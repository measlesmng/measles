#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
# Load required libraries
library(shinydashboard)
library(tidyverse)
library(googlesheets4)
library(plotly)
library(gsheet)
library(slider)
library(leaflet)
library("sf")
library(zoo)
library(rsconnect)

#setwd("C:/Users/batzo/OneDrive/Documents/shiny/test4/")
#getwd()
library(googlesheets4)
gs4_deauth()
options(rsconnect.packrat = FALSE)
# Яг энэ сесс дээр шалгахад:
getOption("rsconnect.packrat")
# > [1] FALSE  ✅ гэж гарах ёстой
readRDS("measles.RDS") -> data
readRDS("map1.rds") -> map1
# Load data from Google Sheets
# Option 1: using the full URL
sheet_url <- "https://docs.google.com/spreadsheets/d/1m8TOXucgRqP13_vYqUVhLmThrK33XWXdOrv5PsDOqew/edit#gid=919168862"

# Read sheet by gid (or name, if known)
df <- googlesheets4::read_sheet(sheet_url, sheet = "cases") %>% 
  janitor::clean_names()
linelist <- df[, c(1:19, 25)]
linelist$date_of_report <- as.Date(linelist$date_of_report, format = "%d-%b-%y")
# Prepare data




############## EPI CURVES AT RURAL #####################
# STEP 1: Summarize daily cases
daily_counts_rural <- linelist %>% filter(adm1_mn != "Улаанбаатар") %>%
  count(date_of_report, name = "new_cases", adm1_mn) %>%
  arrange(date_of_report)

daily_counts_rural %>% drop_na(adm1_mn) -> daily_counts_rural

tail(linelist$date_of_report,1) -> last

adm1_list <- daily_counts_rural %>% # аймгуудыг нэрсийг ялгаж авах
  drop_na(adm1_mn) %>%
  distinct(adm1_mn) %>%
  pull(adm1_mn)


date_seq <- seq.Date(
  from = as.Date("2025-02-12"),
  to = as.Date(last),
  by = "day"
)
df <- data.frame(date_of_report = date_seq)
full_grid <- expand.grid(
  date_of_report = date_seq,
  adm1_mn = adm1_list,
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)


daily_counts_rural <- full_grid %>%
  left_join(daily_counts_rural, by = c("date_of_report", "adm1_mn")) %>%
  arrange(adm1_mn, date_of_report)


daily_counts_rural[is.na(daily_counts_rural)] <- 0

# STEP 2: Calculate 7-day rolling average
daily_counts_rural <- daily_counts_rural %>%
  mutate(avg_7day = slide_dbl(
    new_cases,
    .before = 6, .complete = FALSE,
    .f = ~mean(.x, na.rm = TRUE)
  ))
last_day <- daily_counts_rural %>% 
  filter(date_of_report == max(date_of_report))





################# MAPPING BY NEW CASES ########################

# STEP 1: Summarize daily cases
daily_counts <- linelist %>% 
  count(date_of_report, name = "new_cases", city_province) %>%
  arrange(date_of_report)

daily_counts %>% drop_na(city_province) -> daily_counts

tail(linelist$date_of_report,1) -> last

adm1_list <- daily_counts %>% # аймгуудыг нэрсийг ялгаж авах
  drop_na(city_province) %>%
  distinct(city_province) %>%
  pull(city_province)


date_seq <- seq.Date(
  from = as.Date("2025-02-12"),
  to = as.Date(last),
  by = "day"
)
df <- data.frame(date_of_report = date_seq)
full_grid <- expand.grid(
  date_of_report = date_seq,
  city_province = adm1_list,
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)


daily_counts <- full_grid %>%
  left_join(daily_counts, by = c("date_of_report", "city_province")) %>%
  arrange(city_province, date_of_report)


daily_counts[is.na(daily_counts)] <- 0

daily_counts %>% 
  group_by(city_province) %>%
  summarise(cum_cases = sum(new_cases)) -> sum_data

readxl::read_excel("population.xlsx") -> population2024
left_join(sum_data, population2024, by = "city_province") -> sum_data

sum_data %>% mutate(incidence = cum_cases*10000/population) -> sum_data

map1$ADM1_EN[map1$ADM1_EN == "Omnogovi"] <- "Umnugovi"
map1$ADM1_EN[map1$ADM1_EN == "Arxangai"] <- "Arkhangai"
map1$ADM1_EN[map1$ADM1_EN == "To'v"] <- "Tuv"
map1$ADM1_EN[map1$ADM1_EN == "Hentii"] <- "Khentii"
map1$ADM1_EN[map1$ADM1_EN == "Khovsgol"] <- "Khuvsgul"
map1$ADM1_EN[map1$ADM1_EN == "Ovorkhangai"] <- "Uvurkhangai"
map1$ADM1_EN[map1$ADM1_EN == "Bayan-Olgii"] <- "Bayan-Ulgii"

left_join(map1, sum_data, by = c("ADM1_EN" = "city_province")) -> map1
map1$incidence <- round(map1$incidence, digits = 1)


# Аймгаар нь шинэ өвчлөл, түвшингөө харуулж, газрын зурагтай нэгтгэх


pal <- colorBin("Reds", map1$cum_cases, bins = c(0, 50, 100, 150, 200, 250, 300, Inf), reverse = FALSE)
state_pop <- paste("<br/>Аймаг:", map1$ADM1_EN, 
                   "<strong/>", "<br/>Total cases:",map1$cum_cases,"</strong>",
                   sep = " ")
# Label-д харуулах текстийг бэлтгэх
state_label <- paste0(map1$ADM1_EN, ": ", map1$cum_cases, " ")

# Leaflet map
leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lng = 106.91, lat = 47.92, zoom = 5) %>%
  addPolygons(data = map1, 
              weight = 2, 
              color = "black",
              smoothFactor = 0.3,
              opacity = 1,
              fillOpacity = 0.75,
              fillColor = ~ pal(cum_cases),
              label = state_label,         # ⬅ энэ нь үргэлж харагдана
              labelOptions = labelOptions(
                noHide = TRUE,             # ⬅байнга харагдуулах
                direction = "center",      # ⬅төвд байрлуулах
                textOnly = TRUE,
                style = list(
                  "font-weight" = "regular",
                  "font-size" = "10px",
                  "color" = "black",
                  "background-color" = "transparent", # ар тал нь ил тод
                  "border" = "none"                   # хүрээгүй
                )
              )) %>%
  addLegend(pal = pal, 
            values = map1$cum_cases,
            title = "New cases",
            position = "bottomright") -> map_totalcases



###################### MAP BY INCIDENCE ####################
pal <- colorBin("Reds", map1$incidence, bins = c(0, 10, 20, 30, 40, 50, 60, Inf), reverse = FALSE)
state_pop <- paste("<br/>Аймаг:", map1$ADM1_EN, 
                   "<strong/>", "<br/>Total cases:",map1$incidence,"</strong>",
                   sep = " ")
# Label-д харуулах текстийг бэлтгэх
state_label <- paste0(map1$ADM1_EN, ": ", map1$incidence, " ")

# Leaflet map
leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lng = 106.91, lat = 47.92, zoom = 5) %>%
  addPolygons(data = map1, 
              weight = 2, 
              color = "black",
              smoothFactor = 0.3,
              opacity = 1,
              fillOpacity = 0.75,
              fillColor = ~ pal(incidence),
              label = state_label,         # ⬅ энэ нь үргэлж харагдана
              labelOptions = labelOptions(
                noHide = TRUE,             # ⬅байнга харагдуулах
                direction = "center",      # ⬅төвд байрлуулах
                textOnly = TRUE,
                style = list(
                  "font-weight" = "regular",
                  "font-size" = "10px",
                  "color" = "black",
                  "background-color" = "transparent", # ар тал нь ил тод
                  "border" = "none"                   # хүрээгүй
                )
              )) %>%
  addLegend(pal = pal, 
            values = map1$incidence,
            title = "incidence per 10 000",
            position = "bottomright") -> map_incidence


##################DAILY COUNTS ##################
# STEP 1: Summarize daily cases
daily_counts <- linelist %>%
  count(date_of_report, name = "new_cases") %>%
  arrange(date_of_report)

tail(linelist$date_of_report,1) -> last

date_seq <- seq.Date(
  from = as.Date("2025-02-12"),
  to = as.Date(last),
  by = "day"
)

df <- data.frame(date_of_report = date_seq)
left_join(df, daily_counts, by = "date_of_report") -> daily_counts
daily_counts[is.na(daily_counts)] <- 0

# STEP 2: Calculate 7-day rolling average
daily_counts <- daily_counts %>%
  mutate(avg_7day = slide_dbl(
    new_cases,
    .before = 6, .complete = FALSE,
    .f = ~mean(.x, na.rm = TRUE)
  ))
last_day <- daily_counts %>% 
  filter(date_of_report == max(date_of_report))




######################### DAILY COUNTS IN UB ###########
# STEP 1: Summarize daily cases
daily_counts_ub <- linelist %>% filter(adm1_mn == "Улаанбаатар") %>%
  count(date_of_report, name = "new_cases") %>%
  arrange(date_of_report)

tail(linelist$date_of_report,1) -> last

date_seq <- seq.Date(
  from = as.Date("2025-02-12"),
  to = as.Date(last),
  by = "day"
)

df <- data.frame(date_of_report = date_seq)
left_join(df, daily_counts_ub, by = "date_of_report") -> daily_counts_ub
daily_counts_ub[is.na(daily_counts_ub)] <- 0

# STEP 2: Calculate 7-day rolling average
daily_counts_ub <- daily_counts_ub %>%
  mutate(avg_7day = slide_dbl(
    new_cases,
    .before = 6, .complete = FALSE,
    .f = ~mean(.x, na.rm = TRUE)
  ))
last_day <- daily_counts_ub %>% 
  filter(date_of_report == max(date_of_report))







########################### UI ##########################

# Define UI

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Measles Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Epicurves", tabName = "epicurve", icon = icon("chart-line")),
      menuItem("Map", tabName = "map", icon = icon("globe-asia")),
      menuItem("Age groups", tabName = "age", icon = icon("users")),
      menuItem("Fever&Rash", tabName = "fever_rash", icon = icon("temperature-high")),
      menuItem("Clinical condition", tabName = "clinical", icon = icon("hospital")), 
      menuItem("Vaccination", tabName = "Vaccination", icon = icon("syringe")),
      menuItem("Mortality", tabName = "Mortality", icon = icon("cross"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "epicurve",
              fluidRow(
                column(width = 4,
                       box(title = "Daily Measles Cases at National", status = "primary", solidHeader = TRUE, width = 12,
                           plotOutput("epicurvePlot1", height = 400)
                       ),
                       box(title = "Daily Measles Cases at UB", status = "primary", solidHeader = TRUE, width = 12,
                           plotlyOutput("epicurvePlot_ub", height = 400)
                       )
                ),
                column(width = 8,
                       box(title = "Daily Measles Cases by Provinces", status = "primary", solidHeader = TRUE, width = 12,
                           plotlyOutput("epicurvePlot_rural", height = 900)
                       )
                )
              )
      )
      ,
      tabItem(tabName = "map",
              fluidRow(
                column(
                  width = 8,
                  box(
                    title = "Map of Cases", status = "success", solidHeader = TRUE, width = 12,
                    leafletOutput("measlesMap", height = 400)
                  ),
                  box(
                    title = "Map of Incidence", status = "success", solidHeader = TRUE, width = 12,
                    leafletOutput("measlesMap_inc", height = 400)
                  )
                ),
                column(
                  width = 4,
                  box(
                    title = "Summary Table", status = "primary", solidHeader = TRUE, width = 12,
                    DT::dataTableOutput("measles_table")
                  )
                )
              )
      ),
      tabItem(tabName = "age",
              fluidRow(
                column(width = 6,
                       box(
                         title = "Age groups (Plot)", status = "success", solidHeader = TRUE, width = 12,
                         plotlyOutput("epicurve_age_cat", height = 500)
                       )
                ),
                column(width = 6,
                       box(
                         title = "Summary Table by Age Group", status = "primary", solidHeader = TRUE, width = 12,
                         DT::dataTableOutput("age_group_table")
                       )
                )
              )
      ), 
      tabItem(tabName = "fever_rash",
              fluidRow(
                column(width = 7,
                       box(
                         title = "Daily reported F&R syndrome cases by Provinces", status = "success", solidHeader = TRUE, width = 12,
                         plotlyOutput("fever_rash", height = 900)
                       )
                ),
                column(width = 5,
                       box(
                         title = "Daily reported F&R syndrome cases in UB", status = "primary", solidHeader = TRUE, width = 10,
                         plotlyOutput("fever_rash_ub", height = 900)
                       )
                )
              )
      ),
      tabItem(tabName = "clinical",
              fluidRow(
                column(
                  width = 8,
                  box(
                    title = "Severe and Critical cases", status = "success", solidHeader = TRUE, width = 12,
                    plotlyOutput("severe_critical", height = 400)
                  ),
                  box(
                    title = "Mild and Moderate cases", status = "success", solidHeader = TRUE, width = 12,
                    plotlyOutput("mild_mod", height = 400)
                  )
                ),
                column(
                  width = 4,
                  box(
                    title = "Summary Clinical Table", status = "primary", solidHeader = TRUE, width = 12,
                    DT::dataTableOutput("clinical_table")
                  )
                )
              )
      ), 
      tabItem(tabName = "Vaccination",
              fluidRow(
                # Row 1 - Top Half
                column(width = 6,
                       box(
                         title = "Vaccination coverage by province (by Jul 30, 2025)",
                         width = 12,
                         status = "success",
                         solidHeader = TRUE,
                         leafletOutput("provinceMap", height = 400)
                       )
                ),
                column(width = 6,
                       box(
                         title = "Vaccination coverage - Provinces (by Jul 30, 2025)",
                         width = 12,
                         status = "primary",
                         solidHeader = TRUE,
                         DT::dataTableOutput("provinceTable", height = 400)
                       )
                )
              ),
              fluidRow(
                # Row 2 - Bottom Half
                column(width = 6,
                       box(
                         title = "Vaccination coverage trend (by Jul 30, 2025)",
                         width = 12,
                         status = "primary",
                         solidHeader = TRUE,
                         plotlyOutput("weeklyTrend")
                       )
                ),
                column(width = 6,
                       box(
                         title = "Vaccination coverage - Districts (by Jul 30, 2025)",
                         width = 12,
                         status = "info",
                         solidHeader = TRUE,
                         DT::dataTableOutput("districtTable")
                       )
                )
              )),
      
      tabItem(tabName = "Mortality",
              # Top Row - Full Width Chart
              fluidRow(
                box(
                  title = "Weekly Excess Mortality Percentage - National Level (Jan-Jun)",
                  width = 12,
                  status = "danger",
                  solidHeader = TRUE,
                  plotlyOutput("nationalMortalityPlot", height = 350)
                )
              ),
              
              # Second Row - Two Columns
              fluidRow(
                column(width = 6,
                       box(
                         title = "National Weekly Mortality Comparison (Jan-Jun 2025 vs Jan-Jun 2020-2024)",
                         width = 12,
                         status = "warning",
                         solidHeader = TRUE,
                         plotlyOutput("excessMortalityPercent", height = 350)
                       )
                ),
                column(width = 6,
                       box(
                         title = "Weekly Excess Mortality by Province (Jan to Jun 2025)",
                         width = 12,
                         status = "success",
                         solidHeader = TRUE,
                         plotlyOutput("provinceFacetMortality", height = 350)  # adjustable
                       )
                )
              ))
      
      
      
    )
    
    
    
    
    
  )
)



# Define server logic
server <- function(input, output) {
  
  
  output$epicurvePlot1 <- renderPlot({
    
    
    # ggplot хувилбар
    p <- ggplot(daily_counts, aes(x = date_of_report)) +
      geom_col(aes(y = new_cases),
               fill = "steelblue", color = "steelblue", linewidth = 1.5) +
      geom_line(aes(y = avg_7day),
                color = "red", linewidth = 1) +
      labs(title = "Epidemic Curve in Mongolia",
           x = "Date",
           y = "Number of new cases") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "5 days")
    
    # ggplotly хөрвүүлэлт
    #ggplotly(p, tooltip = c("x", "y"))
    p
    
    
  })  
  
  
  
}

# Run the app
shinyApp(ui = ui, server = server)
