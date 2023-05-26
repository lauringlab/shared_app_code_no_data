library(sf)
library(shiny)
library(tidyverse)
library(lubridate)
library(shinydashboard)
library(shinyWidgets)
library(gridExtra)
library(grid)
library(shinybusy)
library(leaflet)
library(leaflet.extras)
library(gt)
library(reshape2)
#
#
#
#
#
#
#
#
# read in zipcode outlines
tx <- st_read("data/cb_2018_us_zcta510_500k/cb_2018_us_zcta510_500k.shp", stringsAsFactors=FALSE)
# read in zipcode list
zip_mi <- read.csv("data/mi_zip_list.csv")
# filter full zipcode list (tx = all of us) down to michigan only
tx_mi <- filter(tx, as.numeric(GEOID10) %in% as.numeric(zip_mi$Zip.Code))
#zip_data1 <- read.csv("/Users/juliegil/Documents/LauringLab_Code/code_for_misapphire_private_data_refresh/zip_data.csv") # read in strain/case information by zip code
zip_data1 <- read.csv("zip_data.csv")
# filter out strain/case information to only include michigan zip codes
zip_data_mi <- filter(zip_data1, PATIENT_ZIP %in% unique(tx_mi$GEOID10))

# read in county outlines
county_geo <- readRDS(file = "data/county_map_bones.rds")
county_geo$NAME <- gsub(" County, Michigan", "", county_geo$NAME) # edit county names to only contain name and remove "County, Michigan"

#county_data1 <- read.csv("/Users/juliegil/Documents/LauringLab_Code/code_for_misapphire_private_data_refresh/county_data.csv")
county_data1 <- read.csv("county_data.csv")

patient_data <- read.csv("patient_details.csv") %>% arrange(desc(WEEK))
#patient_data <- read.csv("/Users/juliegil/Documents/LauringLab_Code/code_for_misapphire_private_data_refresh/patient_details.csv") %>% arrange(desc(WEEK))

# landing page stats
one_set <- filter(patient_data, TYPE == "ByAge")
total_samples <- sum(one_set$COUNT, na.rm = TRUE)
average_per_week <- one_set %>% group_by(WEEK) %>% summarize(total_per_week = sum(COUNT, na.rm = TRUE))
average_per_week <- round(mean(average_per_week$total_per_week), 0)
start_point <- filter(one_set, WEEK == min(one_set$WEEK))
start_point <- paste0(month.abb[min(start_point$MONTH)], " ", substr(unique(start_point$WEEK), 1, 4))
end_point <- filter(one_set, WEEK == max(one_set$WEEK))
end_point <- paste0(month.abb[max(end_point$MONTH)], " ", substr(unique(end_point$WEEK), 1, 4))
# patient_data <- patient_data %>% mutate(PATIENT_RACE = case_when(PATIENT_RACE == "W" ~ "White or Caucasian", 
#                                                                  PATIENT_RACE == "B" ~ "Black or African American", 
#                                                                  PATIENT_RACE == "O" ~ "Other", 
#                                                                  PATIENT_RACE == "U" ~ "Unknown", 
#                                                                  PATIENT_RACE == "A" ~ "Asian", 
#                                                                  PATIENT_RACE == "M" ~ "Multiple", 
#                                                                  PATIENT_RACE == "I" ~ "American Indian and Alaska Native", 
#                                                                  PATIENT_RACE == "D" ~ "Patient Refused", 
#                                                                  PATIENT_RACE == "P" ~ "Native Hawaiian and Other Pacific Islander", 
#                                                                  T ~ PATIENT_RACE))

covid_cases_all <- read.csv("data/state_covid_data_combined.csv")
#covid_cases_all <- read.csv("/Users/juliegil/Documents/git_synced_code/misapphire_private/data/state_covid_data_combined.csv")

region_crosswalk <- read.csv("data/michigan_merc_region_county_crosswalk_populations_byCounty_withDetCity.csv")


# Define UI for application that draws a histogram
ui <- shinyUI(
        fluidPage(
            navbarPage(HTML("MI-SAPPHIRE at University of Michigan"), 
                       tabPanel("About", 
                                titlePanel(""), 
                                sidebarLayout(position = "right", 
                                              sidebarPanel(includeMarkdown("about_sidebar.md")), 
                                              mainPanel(fluidRow(column(12, includeMarkdown("about.md")))), 
                                              ),
                                br(), 
                                h6(paste0("Reported data includes information from ", start_point, " to ", end_point, ", for a total of ", total_samples , " samples (averaging ", average_per_week, " per week)."))),
                       navbarMenu("Charts & Tables",
                                  tabPanel("Strains",
                                           add_busy_bar(color = "#F9E784"),
                                           titlePanel(h4("SARS-CoV-2 Samples")), 
                                           sidebarLayout(
                                               
                                               position = "right",
                                               
                                               # Sidebar panel for inputs ----
                                               sidebarPanel(id="sc2_strains",
                                                            width = 3, 
                                                            radioButtons("presentation_prop", h6(strong("Presentation:")), choices = c("Count", "Percent"), selected = "Count"), 
                                                            radioButtons("presentation_cov", h6(strong("Coverage Line:")), choices = c("On", "Off"), selected = "Off"), 
                                                            h6("Note: Coverage Line is only available on Count presentation choice for Overall & 2S Region"), 
                                                            
                                                            pickerInput("presentation_select", h6(strong("Weeks:")), 
                                                                        choices = unique(patient_data$WEEK), options = list(`actions-box` = TRUE), multiple = T,
                                                                        selected = unique(patient_data$WEEK)[1:12])
                                                            
                                                            
                                               ), 
                                               mainPanel(
                                                   fluidRow(column(12, plotOutput(outputId = "overall_strain_plot"))),
                                                   #fluidRow(h6(paste0("Data last updated ", label_date)))
                                                   fluidRow(column(12, plotOutput(outputId = "region_strain_plot"))), 
                                                   
                                                   fluidRow(column(12, plotOutput(outputId = "county_strain_plot", height = 2000))), 
                                                   fluidRow(column(12, plotOutput(outputId = "county_strain_plot2", height = 700)))
                                                   
                                               )
                                           )
                                  ),
                                  tabPanel("Coverage by County",
                                           add_busy_bar(color = "#F9E784"),
                                           titlePanel(h4("County Coverage - Sequences vs. Confirmed Case Totals")), 
                                           sidebarLayout(
                                               
                                               position = "right",
                                               
                                               # Sidebar panel for inputs ----
                                               sidebarPanel(id="county_cov",
                                                            width = 3, 
                                                            h6("Coverage is calculated as the number of samples available & sequenced from a county,
                                                            divided by the total cases in the county, and multiplied by 100."), 
                                                            pickerInput("county_select", h6(strong("Weeks:")), 
                                                                        choices = unique(patient_data$WEEK), options = list(`actions-box` = TRUE), multiple = T,
                                                                        selected = unique(patient_data$WEEK)[1:12])
                                                            
                                                            #radioButtons("map_1_1_region_add", h6(strong("Region Highlight:")), choices = c("Off", "1", "2N", "2S", "3", "5", "6", "7", "8"), selected = "Off")
                                                            
                                               ), 
                                               mainPanel(
                                                   fluidRow(column(12, plotOutput("county_coverage_table")))
                                                   #fluidRow(h6(paste0("Data last updated ", label_date)))
                                               )
                                           )
                                  ), 
                                  tabPanel("Strains by Demographics",
                                           add_busy_bar(color = "#F9E784"),
                                           titlePanel(h4("SARS-CoV-2 Samples")), 
                                           sidebarLayout(
                                               
                                               position = "right",
                                               
                                               # Sidebar panel for inputs ----
                                               sidebarPanel(id="strains_demos_side",
                                                            width = 3, 
                                                            radioButtons("demo_presentation_prop", h6(strong("Presentation:")), choices = c("Count", "Percent"), selected = "Count"), 
                                                            radioButtons("demo_type_choice", h6(strong("Demographic Breakout:")), choices = c("Age", "Sex", "Race"), selected = "Age"), 
                                                            pickerInput("demo_presentation_select", h6(strong("Weeks:")), 
                                                                        choices = unique(patient_data$WEEK), options = list(`actions-box` = TRUE), multiple = T,
                                                                        selected = unique(patient_data$WEEK)[1:12])
                                                            #radioButtons("map_1_1_region_add", h6(strong("Region Highlight:")), choices = c("Off", "1", "2N", "2S", "3", "5", "6", "7", "8"), selected = "Off")
                                                            
                                               ), 
                                               mainPanel(
                                                   fluidRow(column(12, plotOutput("overall_demo_strain_plot")))
                                                   #fluidRow(h6(paste0("Data last updated ", label_date)))
                                               )
                                           )
                                  )#, 
                                  # tabPanel("Strains by Demographics",
                                  #          add_busy_bar(color = "#F9E784"),
                                  #          titlePanel(h4("SARS-CoV-2 Samples")), 
                                  #          sidebarLayout(
                                  #              
                                  #              position = "right",
                                  #              
                                  #              # Sidebar panel for inputs ----
                                  #              sidebarPanel(id="cr_strains_demo_side",
                                  #                           width = 3, 
                                  #                           dateRangeInput("cr_strains_demo_date", h6(strong("Date Range:")), 
                                  #                                          start = as_date(min(patient_data$WEEK)), 
                                  #                                          end = as_date(max(patient_data$WEEK))), 
                                  #                           # selectInput("cr_strains_demo_county", h6(strong("County Selection:")), 
                                  #                           #             choices = c("All", "Jackson", "Lenawee", "Livingston", 
                                  #                           #                         "Monroe", "Oakland", "Washtenaw", "Wayne", "Other Michigan", 
                                  #                           #                         "Other Non-Michigan")), 
                                  #                           checkboxGroupInput("cr_strains_demo_cross", 
                                  #                                              h3("Select Demographic Grouping:"), 
                                  #                                              choices = c("Age", "Sex", "Race"),
                                  #                                              selected = c("Age", "Sex", "Race")),
                                  #                           h6("Cells are highlighted in pink if the percentage value is > 50%"), 
                                  #                           pickerInput("cr_strains_select", h6(strong("Weeks:")), 
                                  #                                       choices = unique(patient_data$WEEK), options = list(`actions-box` = TRUE), multiple = T,
                                  #                                       selected = unique(patient_data$WEEK)[1:10])
                                  #                           #radioButtons("map_1_1_region_add", h6(strong("Region Highlight:")), choices = c("Off", "1", "2N", "2S", "3", "5", "6", "7", "8"), selected = "Off")
                                  #                           
                                  #              ), 
                                  #              mainPanel(
                                  #                  fluidRow(column(12, gt_output("cr_strains_demo_table")))
                                  #                  #fluidRow(h6(paste0("Data last updated ", label_date)))
                                  #              )
                                  #          )
                                  # )
                       ),
                       navbarMenu("Maps", 
                                  tabPanel("ZIP Code Level",
                                           add_busy_bar(color = "#F9E784"),
                                           titlePanel(h4("Zip Code Level Geography")), 
                                           sidebarLayout(
                                               
                                               position = "right",
                                               
                                               # Sidebar panel for inputs ----
                                               sidebarPanel(id="zip_map",
                                                            width = 3, 
                                                            
                                                            radioButtons("zip_map_strain_choice", h6(strong("Strain Selection:")), choices = unique(patient_data$CLADETYPE), selected = unique(patient_data$CLADETYPE)[1]), 
                                                            # dateRangeInput("county_map_strain_date", h6(strong("Date Range:")), 
                                                            #                start = as_date(min(patient_data$WEEK)), 
                                                            #                end = as_date(max(patient_data$WEEK))), 
                                                            radioButtons("zip_map_pres_choice", h6(strong("Presentation:")), 
                                                                         choices = c("Count", "Percent"), 
                                                                         selected = "Count"), 
                                                            pickerInput("zip_map_select", h6(strong("Weeks:")), 
                                                                        choices = unique(patient_data$WEEK), options = list(`actions-box` = TRUE), multiple = T,
                                                                        selected = unique(patient_data$WEEK)[1:10])
                                               ), 
                                               mainPanel(
                                                   fluidRow(column(12, leafletOutput(outputId = "zip_code_map_base"))),
                                                   #fluidRow(h6(paste0("Data last updated ", label_date)))
                                                   
                                                   br(), 
                                                   br(),
                                                   fluidRow(column(12, gt_output("zip_map_variant_table")))
                                               )
                                           )
                                  ), 
                                  tabPanel("County Level",
                                           add_busy_bar(color = "#F9E784"),
                                           titlePanel(h4("County Level Geography")), 
                                           sidebarLayout(
                                               
                                               position = "right",
                                               
                                               # Sidebar panel for inputs ----
                                               sidebarPanel(id="county_map",
                                                            width = 3,  
                                                            radioButtons("county_map_strain_choice", h6(strong("Strain Selection:")), choices = unique(patient_data$CLADETYPE), selected = unique(patient_data$CLADETYPE)[1]), 
                                                            # dateRangeInput("county_map_strain_date", h6(strong("Date Range:")), 
                                                            #                start = as_date(min(patient_data$WEEK)), 
                                                            #                end = as_date(max(patient_data$WEEK))), 
                                                            radioButtons("county_map_pres_choice", h6(strong("Presentation:")), 
                                                                         choices = c("Count", "Percent"), 
                                                                         selected = "Count"), 
                                                            pickerInput("county_map_select", h6(strong("Weeks:")), 
                                                                        choices = unique(patient_data$WEEK), options = list(`actions-box` = TRUE), multiple = T,
                                                                        selected = unique(patient_data$WEEK)[1:10])
                                               ), 
                                               mainPanel(
                                                   fluidRow(column(12, leafletOutput(outputId = "county_map_base", height = 600))), 
                                                   br(), 
                                                   br(),
                                                   fluidRow(column(12, gt_output("county_map_variant_table")))
                                                   
                                                   #fluidRow(h6(paste0("Data last updated ", label_date)))
                                               )
                                           )
                                  )
                                  
                       )
                       
                       
                       
            )
        )
    )

# Define server logic required to draw a histogram
server <- function(input, output) {

    overall_strain_coverage_data <- reactive({
        pd <- filter(patient_data, TYPE == "ByAge") %>% group_by(WEEK) %>% summarize(count = sum(COUNT, na.rm = TRUE))
        pd <- as.data.frame(pd)
        pd <- merge(pd, filter(covid_cases_all, COUNTY == "State"), by.x = c("WEEK"), by.y = c("case_week"), all = TRUE)
        #pd <- pd %>% mutate(coverage = round((count / confirmed_cases)*100, 1))
        pd <- pd %>% mutate(coverage = case_when(is.na(round((count / confirmed_cases)*100, 1)) ~ 0, 
                                                 T ~ round((count / confirmed_cases)*100, 1)))
        pd <- filter(pd, WEEK %in% input$presentation_select)
    })
    
    overall_strain_plot_data <- reactive({
        
        if (input$presentation_prop == "Count"){
            patient_data_grouped <- filter(patient_data, TYPE == "ByAge") %>% group_by(WEEK, CLADETYPE) %>% summarize(count = sum(COUNT, na.rm = TRUE))
            pdg_monthmax <- filter(patient_data, TYPE == "ByAge") %>% group_by(WEEK) %>% summarize(MONTHmax = max(as.numeric(MONTH), na.rm = TRUE))
            patient_data_grouped <- merge(patient_data_grouped, pdg_monthmax, by =c("WEEK"))
            patient_data_grouped <- as.data.frame(patient_data_grouped)
            patient_data_grouped <- filter(patient_data_grouped, WEEK %in% input$presentation_select)
        } else if (input$presentation_prop == "Percent"){
            patient_data_grouped <- filter(patient_data, TYPE == "ByAge") %>% group_by(WEEK, CLADETYPE) %>% summarize(count2 = sum(COUNT, na.rm = TRUE))
            patient_data_grouped2 <- filter(patient_data, TYPE == "ByAge") %>% group_by(WEEK) %>% summarize(total = sum(COUNT, na.rm = TRUE))
            patient_data_grouped <- merge(patient_data_grouped, patient_data_grouped2, by = c("WEEK")) %>% mutate(count = round((count2/total)*100, 1))
            pdg_monthmax <- filter(patient_data, TYPE == "ByAge") %>% group_by(WEEK) %>% summarize(MONTHmax = max(as.numeric(MONTH), na.rm = TRUE))
            patient_data_grouped <- merge(patient_data_grouped, pdg_monthmax, by = c("WEEK"))
            patient_data_grouped <- filter(patient_data_grouped, WEEK %in% input$presentation_select)
        } else {
            x <- 0
        }
    })
    
    
    
    region_strain_coverage_data <- reactive({
        region2s <- filter(region_crosswalk, PH_Region == "Region 2S")
        patient_data_region <- filter(county_data1, tolower(PATIENT_COUNTY) %in% tolower(region2s$County.Name))
        
        pd <- patient_data_region %>% group_by(collection_week) %>% summarize(MONTHmax = max(as.numeric(max_month), na.rm = TRUE), count = sum(count, na.rm = TRUE))
        pd <- as.data.frame(pd)
        pd <- merge(pd, filter(covid_cases_all, COUNTY == "Region 2S"), by.x = c("collection_week"), by.y = c("case_week"), all = TRUE)
        pd <- pd %>% mutate(coverage = case_when(is.na(round((count / confirmed_cases)*100, 1)) ~ 0, 
                                                 T ~ round((count / confirmed_cases)*100, 1)))
        pd <- filter(pd, collection_week %in% input$presentation_select)
    })
    
    region_strain_plot_data <- reactive({
        
        region2s <- filter(region_crosswalk, PH_Region == "Region 2S")
        patient_data_region <- filter(county_data1, tolower(PATIENT_COUNTY) %in% tolower(region2s$County.Name))
        
        if (input$presentation_prop == "Count"){
            patient_data_grouped <- patient_data_region %>% group_by(collection_week, extra_clade) %>% summarize(count = sum(count, na.rm = TRUE))
            patdattot <- patient_data_region %>% group_by(collection_week) %>% summarize(MONTHmax = max(as.numeric(max_month)), total = sum(count, na.rm = TRUE))
            patient_data_grouped <- merge(patient_data_grouped, patdattot, by = c("collection_week"), all.x = TRUE)
            patient_data_grouped <- filter(patient_data_grouped, collection_week %in% input$presentation_select)
        } else if (input$presentation_prop == "Percent"){
            patient_data_grouped <- patient_data_region %>% group_by(collection_week, extra_clade) %>% summarize(MONTHmax = max(as.numeric(max_month), na.rm = TRUE), count2 = sum(count, na.rm = TRUE))
            patient_data_grouped2 <- patient_data_region %>% group_by(collection_week) %>% summarize(MONTHmax = max(as.numeric(max_month), na.rm = TRUE), total = sum(count, na.rm = TRUE))
            patient_data_grouped <- merge(patient_data_grouped, patient_data_grouped2, by = c("collection_week", "MONTHmax")) %>% mutate(count = round((count2/total)*100, 1))
            patient_data_grouped <- filter(patient_data_grouped, collection_week %in% input$presentation_select)
        } else {
            x <- 0
        }
    })
    
    county_strain_plot_data <- reactive({
        
        region2s <- filter(region_crosswalk, PH_Region == "Region 2S")
        top_counties <- filter(county_data1, collection_week %in% input$presentation_select) %>% group_by(PATIENT_COUNTY) %>% summarize(MONTHmax = max(as.numeric(max_month), na.rm = TRUE), person_count = sum(count, na.rm = TRUE))
        top_counties <- filter(top_counties, person_count > 80)
        patient_data_county <- county_data1 %>% mutate(county_group = case_when(tolower(PATIENT_COUNTY) %in% tolower(region2s$County.Name) ~ str_to_title(PATIENT_COUNTY),
                                                                                tolower(PATIENT_COUNTY) %in% tolower(top_counties$PATIENT_COUNTY) ~ str_to_title(PATIENT_COUNTY),
                                                                                tolower(PATIENT_COUNTY) %in% tolower(region_crosswalk$County.Name) ~ "Other MI Counties",
                                                                                T ~ "Non MI Counties"))
        
        if (input$presentation_prop == "Count"){
            patient_data_grouped <- patient_data_county %>% group_by(county_group, collection_week, extra_clade) %>% summarize(count = sum(count, na.rm = TRUE))
            patient_data_grouped2 <- patient_data_county %>% group_by(county_group, collection_week) %>% summarize(MONTHmax = max(as.numeric(max_month), na.rm = TRUE), total = sum(count, na.rm = TRUE))
            patient_data_grouped <- merge(patient_data_grouped, patient_data_grouped2, by = c("county_group", "collection_week"), all.x = TRUE)
            patient_data_grouped <- filter(patient_data_grouped, collection_week %in% input$presentation_select) %>% group_by(collection_week) %>% mutate(MONTHmax = max(MONTHmax, na.rm = TRUE))
        } else if (input$presentation_prop == "Percent"){
            patient_data_grouped <- patient_data_county %>% group_by(county_group, collection_week, extra_clade) %>% summarize(MONTHmax = max(as.numeric(max_month), na.rm = TRUE), count2 = sum(count, na.rm = TRUE))
            patient_data_grouped2 <- patient_data_county %>% group_by(county_group, collection_week) %>% summarize(MONTHmax = max(as.numeric(max_month), na.rm = TRUE), total = sum(count, na.rm = TRUE))
            patient_data_grouped <- merge(patient_data_grouped, patient_data_grouped2, by = c("county_group", "collection_week", "MONTHmax")) %>% mutate(count = round((count2/total)*100, 1))
            patient_data_grouped <- filter(patient_data_grouped, collection_week %in% input$presentation_select) %>% group_by(collection_week) %>% mutate(MONTHmax = max(MONTHmax, na.rm = TRUE))
        } else {
            x <- 0
        }
    })
    
    output$overall_strain_plot <- renderPlot({
       
        ospd <- overall_strain_plot_data()
        cov_overall <- overall_strain_coverage_data()
        
        #print(tail(cov_overall, n = 20))
        
        if (input$presentation_prop == "Count"){
            if (input$presentation_cov == "On"){
                ggplot(ospd, aes(x = WEEK, y = count, fill = CLADETYPE)) + 
                    geom_bar(stat = "identity") + 
                    geom_line(data = cov_overall, aes(x = WEEK, y = coverage*100, group = COUNTY), linetype="dotted", color = "black", size = 1, alpha = 0.5, inherit.aes = FALSE) + 
                geom_point(data = cov_overall, aes(x = WEEK, y = coverage*100, group = COUNTY), color = "black", size = 2, alpha = 0.5, inherit.aes = FALSE) + 
                  #geom_point(aes(x = WEEK, y = -10, shape = as.character(MONTHmax)), alpha = 0.0, size = 3, inherit.aes = FALSE) + 
                geom_text(aes(x = WEEK, y = (max(ospd$count)/24)*-1, label = month.abb[MONTHmax]), inherit.aes = FALSE, color = "gray50") + 
                    theme_bw() + 
                    scale_fill_manual(values = c("#B5CA8D", "#8BB174", "#426B69", "#222E50", "#654C4F",
                                                 "#7FB7BE", "#D3F3EE", "#6D98BA", "#505A5B", "#000000",
                                                 "#00487C", "#4BB3FD", "#027BCE", "#644536", "#DDF093",
                                                 "#BCA3AC", "#E5CEDC", "#F3EAF4", "#FF5E5B", "#31493C",
                                                 "#D35269", "#FEC3A6", "#C17767", "#D8CC34", "#533747")) + 
                    labs(title = "Overall", 
                         x = "Sample Collection Week", 
                         y = input$presentation_prop, 
                         fill = "SARS-CoV-2 Clade", 
                         caption = paste0("Max Week Coverage = ", max(cov_overall$coverage), "%;\nNote: Data for the most recent 3 weeks is considered incomplete due to the cadence of testing")) + 
                    theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
              scale_y_continuous(
                "Count", 
                sec.axis = sec_axis(~ . * .010, name = "Coverage (%)")
              )
            } else {
                ggplot(ospd, aes(x = WEEK, y = count, fill = CLADETYPE)) + 
                    geom_bar(stat = "identity") + 
                    #geom_point(aes(x = WEEK, y = -10, shape = as.character(MONTHmax)), alpha = 0.0, size = 3, inherit.aes = FALSE) + 
                    geom_text(aes(x = WEEK, y = (max(ospd$count)/24)*-1, label = month.abb[MONTHmax]), inherit.aes = FALSE, color = "gray50") + 
                    #geom_line(data = cov_overall, aes(x = collection_week, y = coverage, group = COUNTY), color = "black", size = 1, alpha = 0.5, inherit.aes = FALSE) + 
                    theme_bw() + 
                    geom_text(data = cov_overall, aes(x = WEEK, y = count, label = paste0("(", count, ")")), nudge_y = 10, inherit.aes = FALSE) + 
                    scale_fill_manual(values = c("#B5CA8D", "#8BB174", "#426B69", "#222E50", "#654C4F",
                                                 "#7FB7BE", "#D3F3EE", "#6D98BA", "#505A5B", "#000000",
                                                 "#00487C", "#4BB3FD", "#027BCE", "#644536", "#DDF093",
                                                 "#BCA3AC", "#E5CEDC", "#F3EAF4", "#FF5E5B", "#31493C",
                                                 "#D35269", "#FEC3A6", "#C17767", "#D8CC34", "#533747")) + 
                    labs(title = "Overall", 
                         x = "Sample Collection Week", 
                         y = input$presentation_prop, 
                         fill = "SARS-CoV-2 Clade",
                         caption = "Note: Data for the most recent 3 weeks is considered incomplete due to the cadence of testing") + 
                    theme(axis.text.x = element_text(angle = 60, hjust = 1))
            }
        } else {
            ggplot(ospd, aes(x = WEEK, y = count, fill = CLADETYPE)) + 
                geom_bar(stat = "identity") + 
            #geom_point(aes(x = WEEK, y = -10, shape = as.character(MONTHmax)), alpha = 0.0, size = 3, inherit.aes = FALSE) + 
            geom_text(aes(x = WEEK, y = (max(ospd$count)/24)*-1, label = month.abb[MONTHmax]), inherit.aes = FALSE, color = "gray50") + 
                #geom_line(data = ospd, aes(x = case_week, y = coverage)) + 
                theme_bw() + 
                scale_fill_manual(values = c("#B5CA8D", "#8BB174", "#426B69", "#222E50", "#654C4F",
                                             "#7FB7BE", "#D3F3EE", "#6D98BA", "#505A5B", "#000000",
                                             "#00487C", "#4BB3FD", "#027BCE", "#644536", "#DDF093",
                                             "#BCA3AC", "#E5CEDC", "#F3EAF4", "#FF5E5B", "#31493C",
                                             "#D35269", "#FEC3A6", "#C17767", "#D8CC34", "#533747")) + 
                labs(title = "Overall", 
                     x = "Sample Collection Week", 
                     y = input$presentation_prop, 
                     fill = "SARS-CoV-2 Clade", 
                     caption = "Note: Data for the most recent 3 weeks is considered incomplete due to the cadence of testing") + 
                theme(axis.text.x = element_text(angle = 60, hjust = 1))
        }
        
    })
    
    
    output$region_strain_plot <- renderPlot({
        
        rspd <- region_strain_plot_data()
        cov_region <- region_strain_coverage_data()
        if (input$presentation_prop == "Count"){
            if (input$presentation_cov == "On"){
                ggplot(rspd, aes(x = collection_week, y = count, fill = extra_clade)) + 
                    geom_bar(stat = "identity") + 
                    geom_line(data = cov_region, aes(x = collection_week, y = coverage*100, group = COUNTY), linetype="dotted", color = "black", size = 1, alpha = 0.5, inherit.aes = FALSE) +
                geom_point(data = cov_region, aes(x = collection_week, y = coverage*100, group = COUNTY), color = "black", size = 2, alpha = 0.5, inherit.aes = FALSE) + 
                    geom_text(aes(x = collection_week, y = (max(rspd$count)/24)*-1, label = month.abb[MONTHmax]), inherit.aes = FALSE, color = "gray50") + 
                    theme_bw() + 
                    scale_fill_manual(values = c("#B5CA8D", "#8BB174", "#426B69", "#222E50", "#654C4F",
                                                 "#7FB7BE", "#D3F3EE", "#6D98BA", "#505A5B", "#000000",
                                                 "#00487C", "#4BB3FD", "#027BCE", "#644536", "#DDF093",
                                                 "#BCA3AC", "#E5CEDC", "#F3EAF4", "#FF5E5B", "#31493C",
                                                 "#D35269", "#FEC3A6", "#C17767", "#D8CC34", "#533747")) + 
                    labs(title = "Public Health Region 2S", 
                         x = "Sample Collection Week", 
                         y = input$presentation_prop, 
                         fill = "SARS-CoV-2 Clade", 
                         caption = paste0("Max Week Coverage = ", max(cov_region$coverage), "%\nRegion placement based on residential address\n", 
                                          "Note: Data for the most recent 3 weeks is considered incomplete due to the cadence of testing")) + 
                    theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
                scale_y_continuous(
                  "Count", 
                  sec.axis = sec_axis(~ . * .010, name = "Coverage (%)")
                )
            } else {
                ggplot(rspd, aes(x = collection_week, y = count, fill = extra_clade)) + 
                    geom_bar(stat = "identity") + 
                geom_text(aes(x = collection_week, y = (max(rspd$count)/24)*-1, label = month.abb[MONTHmax]), inherit.aes = FALSE, color = "gray50") + 
                geom_text(data = rspd, aes(x = collection_week, y = total, label = paste0("(", total, ")")), nudge_y = 10, inherit.aes = FALSE) +
                    #geom_line(data = cov_region, aes(x = collection_week, y = coverage, group = COUNTY), color = "black", size = 1, alpha = 0.5, inherit.aes = FALSE) + 
                    theme_bw() + 
                    scale_fill_manual(values = c("#B5CA8D", "#8BB174", "#426B69", "#222E50", "#654C4F",
                                                 "#7FB7BE", "#D3F3EE", "#6D98BA", "#505A5B", "#000000",
                                                 "#00487C", "#4BB3FD", "#027BCE", "#644536", "#DDF093",
                                                 "#BCA3AC", "#E5CEDC", "#F3EAF4", "#FF5E5B", "#31493C",
                                                 "#D35269", "#FEC3A6", "#C17767", "#D8CC34", "#533747")) +
                    labs(title = "Public Health Region 2S", 
                         x = "Sample Collection Week", 
                         y = input$presentation_prop, 
                         fill = "SARS-CoV-2 Clade", 
                         caption = "Region placement based on residential address\nNote: Data for the most recent 3 weeks is considered incomplete due to the cadence of testing") + 
                    theme(axis.text.x = element_text(angle = 60, hjust = 1))
            }
        } else {
            ggplot(rspd, aes(x = collection_week, y = count, fill = extra_clade)) + 
                geom_bar(stat = "identity") + 
            geom_text(aes(x = collection_week, y = (max(rspd$count)/24)*-1, label = month.abb[MONTHmax]), inherit.aes = FALSE, color = "gray50") + 
                theme_bw() + 
                scale_fill_manual(values = c("#B5CA8D", "#8BB174", "#426B69", "#222E50", "#654C4F",
                                             "#7FB7BE", "#D3F3EE", "#6D98BA", "#505A5B", "#000000",
                                             "#00487C", "#4BB3FD", "#027BCE", "#644536", "#DDF093",
                                             "#BCA3AC", "#E5CEDC", "#F3EAF4", "#FF5E5B", "#31493C",
                                             "#D35269", "#FEC3A6", "#C17767", "#D8CC34", "#533747")) +
                labs(title = "Public Health Region 2S", 
                     x = "Sample Collection Week", 
                     y = input$presentation_prop, 
                     fill = "SARS-CoV-2 Clade", 
                     caption = "Region placement based on residential address\nNote: Data for the most recent 3 weeks is considered incomplete due to the cadence of testing") + 
                theme(axis.text.x = element_text(angle = 60, hjust = 1))
        }
        
    })
    
    output$county_strain_plot <- renderPlot({
        
        cspd <- county_strain_plot_data()
        cspd <- filter(cspd, !county_group %in% c("Other MI Counties", "Non MI Counties"))
        #cspd$county_group <- factor(cspd$county_group, levels = c("Monroe", "Washtenaw", "Wayne",
        #                                                          "Other MI Counties", "Non MI Counties"))
        ggplot(cspd, aes(x = collection_week, y = count, fill = extra_clade)) + 
            geom_bar(stat = "identity") + 
          geom_text(aes(x = collection_week, y = (max(cspd$count)/24)*-1, label = month.abb[MONTHmax]), inherit.aes = FALSE, color = "gray50") +
          geom_text(data = cspd, aes(x = collection_week, y = total, label = paste0("(", total, ")")), nudge_y = 5, inherit.aes = FALSE) +
            theme_bw() + 
            scale_fill_manual(values = c("#B5CA8D", "#8BB174", "#426B69", "#222E50", "#654C4F",
                                         "#7FB7BE", "#D3F3EE", "#6D98BA", "#505A5B", "#000000",
                                         "#00487C", "#4BB3FD", "#027BCE", "#644536", "#DDF093",
                                         "#BCA3AC", "#E5CEDC", "#F3EAF4", "#FF5E5B", "#31493C",
                                         "#D35269", "#FEC3A6", "#C17767", "#D8CC34", "#533747")) + 
            labs(title = "County Groupings",
                 subtitle = "Counties with 80+ Samples",
                 x = "Sample Collection Week", 
                 y = input$presentation_prop, 
                 fill = "SARS-CoV-2 Clade", 
                 caption = "County placement based on residential address\nNote: Data for the most recent 3 weeks is considered incomplete due to the cadence of testing") + 
            theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
            facet_wrap(.~county_group, ncol = 1)
        
    })
    
    output$county_strain_plot2 <- renderPlot({
        
        cspd2 <- county_strain_plot_data()
        cspd2 <- filter(cspd2, county_group %in% c("Other MI Counties", "Non MI Counties"))
        #cspd$county_group <- factor(cspd$county_group, levels = c("Monroe", "Washtenaw", "Wayne",
        #                                                          "Other MI Counties", "Non MI Counties"))
        ggplot(cspd2, aes(x = collection_week, y = count, fill = extra_clade)) + 
            geom_bar(stat = "identity") + 
          geom_text(aes(x = collection_week, y = (max(cspd2$count)/24)*-1, label = month.abb[MONTHmax]), inherit.aes = FALSE, color = "gray50") +
          geom_text(data = cspd2, aes(x = collection_week, y = total, label = paste0("(", total, ")")), nudge_y = 1, inherit.aes = FALSE) +
            theme_bw() + 
            scale_fill_manual(values = c("#B5CA8D", "#8BB174", "#426B69", "#222E50", "#654C4F",
                                         "#7FB7BE", "#D3F3EE", "#6D98BA", "#505A5B", "#000000",
                                         "#00487C", "#4BB3FD", "#027BCE", "#644536", "#DDF093",
                                         "#BCA3AC", "#E5CEDC", "#F3EAF4", "#FF5E5B", "#31493C",
                                         "#D35269", "#FEC3A6", "#C17767", "#D8CC34", "#533747")) + 
            labs(title = "County Data Categories",
                 subtitle = "Fewer than 80 Samples Available per County",
                 x = "Sample Collection Week", 
                 y = input$presentation_prop, 
                 fill = "SARS-CoV-2 Clade", 
                 caption = "County placement based on residential address\nNote: Data for the most recent 3 weeks is considered incomplete due to the cadence of testing") + 
            theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
            facet_wrap(.~county_group, ncol = 1, scales = "free")
        
    })
    
    
    output$county_coverage_table <- renderPlot({
        
        region2s <- filter(region_crosswalk, PH_Region == "Region 2S")
        top_counties <- county_data1 %>% group_by(PATIENT_COUNTY) %>% summarize(person_count = sum(count, na.rm = TRUE))
        top_counties <- filter(top_counties, person_count > 80)
        
        patient_data_county <- county_data1 %>% mutate(county_group = case_when(tolower(PATIENT_COUNTY) %in% tolower(region2s$County.Name) ~ str_to_title(PATIENT_COUNTY),
                                                                                tolower(PATIENT_COUNTY) %in% tolower(top_counties$PATIENT_COUNTY) ~ str_to_title(PATIENT_COUNTY),
                                                                                tolower(PATIENT_COUNTY) %in% tolower(region_crosswalk$County.Name) ~ "Other MI Counties",
                                                                                T ~ "Non MI Counties"))
        
        patient_data_grouped <- patient_data_county %>% group_by(county_group, collection_week) %>% summarize(count = sum(count, na.rm = TRUE))
        pdg2 <- patient_data_county %>% group_by(collection_week) %>% summarize(MONTHmax = max(as.numeric(max_month), na.rm = TRUE))
        patient_data_grouped <- merge(patient_data_grouped, pdg2, by = c("collection_week"))
        county_case_data <- filter(covid_cases_all, tolower(COUNTY) %in% tolower(unique(patient_data_grouped$county_group)))
        
        #county_case_data$COUNTY <- toupper(county_case_data$COUNTY)
        pd <- merge(patient_data_grouped, county_case_data, by.x = c("collection_week", "county_group"), by.y = c("case_week", "COUNTY"), all = TRUE)
        
        pd <- filter(pd, county_group != "Non MI Counties" & county_group != "Other MI Counties" & county_group != "")
        
        pd <- pd %>% mutate(coverage = case_when(is.na(round((count / confirmed_cases)*100, 1)) ~ 0, 
                                                 T ~ round((count / confirmed_cases)*100, 1)))
        
        pd <- filter(pd, collection_week %in% input$county_select)
        
        ggplot(pd, aes(x = collection_week, y = coverage, group = county_group, color = county_group)) + 
            geom_line(size = 1, alpha = 0.7) + 
          geom_point(data = pd, aes(x = collection_week, y = coverage, group = county_group, color = county_group), size = 3, alpha = 0.7) + 
          geom_text(aes(x = collection_week, y = (max(pd$coverage)/24)*-1, label = month.abb[MONTHmax]), inherit.aes = FALSE, color = "gray50") +
          scale_color_manual(values = c("#B5CA8D", "#8BB174", "#426B69", "#222E50", "#654C4F",
                                        "#7FB7BE", "#D3F3EE", "#6D98BA", "#505A5B", "#000000",
                                        "#00487C", "#4BB3FD", "#027BCE", "#644536", "#DDF093",
                                        "#BCA3AC", "#E5CEDC", "#F3EAF4", "#FF5E5B", "#31493C",
                                        "#D35269", "#FEC3A6", "#C17767", "#D8CC34", "#533747"))  + 
            theme_bw() + 
            theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
            labs(x = "Sample Collection Week", 
                 y = "Cases Covered with Genetic Sample (%)", 
                 color = "County")
        
    })
    
    
    overall_strain_by_demos_plot_data <- reactive({
        
        if (input$demo_presentation_prop == "Count"){
            if (input$demo_type_choice == "Age"){
                patient_data_grouped <- filter(patient_data, TYPE == "ByAge" & WEEK %in% input$demo_presentation_select)
                #patient_data$approx_age_group <- ifelse(patient_data$APPROX_AGE < 18, "0-17", "18+")
                #patient_data_grouped <- patient_data %>% group_by(approx_age_group, collection_week, extra_clade) %>% summarize(count = length(unique(PATIENT_MRN)))
               
                #names(patient_data_grouped)[names(patient_data_grouped) == 'approx_age_group'] <- 'demo_group'
                patient_data_grouped <- as.data.frame(patient_data_grouped)
            } else if (input$demo_type_choice == "Sex"){
                patient_data_grouped <- filter(patient_data, TYPE == "BySex" & WEEK %in% input$demo_presentation_select)
                # patient_data_grouped <- patient_data %>% group_by(PATIENT_SEX, collection_week, extra_clade) %>% summarize(count = length(unique(PATIENT_MRN)))
                # 
                # names(patient_data_grouped)[names(patient_data_grouped) == 'PATIENT_SEX'] <- 'demo_group'
                patient_data_grouped <- as.data.frame(patient_data_grouped)
            } else {
                # choice is Race
                patient_data_grouped <- filter(patient_data, TYPE == "ByRace" & WEEK %in% input$demo_presentation_select)
                # patient_data_grouped <- patient_data %>% group_by(PATIENT_RACE, collection_week, extra_clade) %>% summarize(count = length(unique(PATIENT_MRN)))
                # names(patient_data_grouped)[names(patient_data_grouped) == 'PATIENT_RACE'] <- 'demo_group'
                patient_data_grouped <- as.data.frame(patient_data_grouped)
            }
            
        } else if (input$demo_presentation_prop == "Percent"){
            if (input$demo_type_choice == "Age"){
              patient_data_grouped <- filter(patient_data, TYPE == "ByAge" & WEEK %in% input$demo_presentation_select)
              patient_data_grouped2 <- filter(patient_data, TYPE == "ByAge" & WEEK %in% input$demo_presentation_select) %>% group_by(DEMO, WEEK) %>% summarize(total = sum(COUNT, na.rm = TRUE))
                #patient_data$approx_age_group <- ifelse(patient_data$APPROX_AGE < 18, "0-17", "18+")
                #patient_data_grouped <- patient_data %>% group_by(approx_age_group, collection_week, extra_clade) %>% summarize(count2 = length(unique(PATIENT_MRN)))
                #patient_data_grouped2 <- patient_data %>% group_by(approx_age_group, collection_week) %>% summarize(total = length(unique(PATIENT_MRN)))
                patient_data_grouped <- merge(patient_data_grouped, patient_data_grouped2, by = c("DEMO", "WEEK")) %>% mutate(perc = round((COUNT/total)*100, 1))
                #names(patient_data_grouped)[names(patient_data_grouped) == 'approx_age_group'] <- 'demo_group'
                patient_data_grouped <- as.data.frame(patient_data_grouped)
            } else if (input$demo_type_choice == "Sex"){
              patient_data_grouped <- filter(patient_data, TYPE == "BySex" & WEEK %in% input$demo_presentation_select)
              patient_data_grouped2 <- filter(patient_data, TYPE == "BySex" & WEEK %in% input$demo_presentation_select) %>% group_by(DEMO, WEEK) %>% summarize(total = sum(COUNT, na.rm = TRUE))
              #patient_data$approx_age_group <- ifelse(patient_data$APPROX_AGE < 18, "0-17", "18+")
              #patient_data_grouped <- patient_data %>% group_by(approx_age_group, collection_week, extra_clade) %>% summarize(count2 = length(unique(PATIENT_MRN)))
              #patient_data_grouped2 <- patient_data %>% group_by(approx_age_group, collection_week) %>% summarize(total = length(unique(PATIENT_MRN)))
              patient_data_grouped <- merge(patient_data_grouped, patient_data_grouped2, by = c("DEMO", "WEEK")) %>% mutate(perc = round((COUNT/total)*100, 1))
              #names(patient_data_grouped)[names(patient_data_grouped) == 'approx_age_group'] <- 'demo_group'
              patient_data_grouped <- as.data.frame(patient_data_grouped)
            } else {
              patient_data_grouped <- filter(patient_data, TYPE == "ByRace" & WEEK %in% input$demo_presentation_select)
              patient_data_grouped2 <- filter(patient_data, TYPE == "ByRace" & WEEK %in% input$demo_presentation_select) %>% group_by(DEMO, WEEK) %>% summarize(total = sum(COUNT, na.rm = TRUE))
              #patient_data$approx_age_group <- ifelse(patient_data$APPROX_AGE < 18, "0-17", "18+")
              #patient_data_grouped <- patient_data %>% group_by(approx_age_group, collection_week, extra_clade) %>% summarize(count2 = length(unique(PATIENT_MRN)))
              #patient_data_grouped2 <- patient_data %>% group_by(approx_age_group, collection_week) %>% summarize(total = length(unique(PATIENT_MRN)))
              patient_data_grouped <- merge(patient_data_grouped, patient_data_grouped2, by = c("DEMO", "WEEK")) %>% mutate(perc = round((COUNT/total)*100, 1))
              #names(patient_data_grouped)[names(patient_data_grouped) == 'approx_age_group'] <- 'demo_group'
              patient_data_grouped <- as.data.frame(patient_data_grouped)
            }
                
        } else {
            x <- 0
        }
      
      
      
      return(patient_data_grouped)
    })
    
    output$overall_demo_strain_plot <- renderPlot({
        
        osbdpd <- overall_strain_by_demos_plot_data() 
        #cov_overall <- overall_strain_coverage_data()
        if (input$demo_type_choice == "Age"){
          osbdpd$DEMO <- factor(osbdpd$DEMO, levels = c("Under 18", "18-29", "30-49", "50+"))
        } else if (input$demo_type_choice == "Sex"){
          osbdpd <- osbdpd %>% mutate(DEMO = case_when(DEMO == "F" ~ "Female", 
                                                       DEMO == "M" ~ "Male", 
                                                       T ~ "Unknown"))
        } else if (input$demo_type_choice == "Race"){
          #print(unique(osbdpd$DEMO))
          osbdpd$DEMO <- factor(osbdpd$DEMO, levels = c("White or Caucasian", "Black or African American", "Asian", 
                                                        "American Indian and Alaska Native", "Other", "Unknown"))
        } else {
          z1 <- 1
        }
        
        #print(head(osbdpd))
        #print(tail(cov_overall, n = 20))
        
        if (input$demo_presentation_prop == "Count"){
                ggplot(osbdpd, aes(x = WEEK, y = COUNT, fill = CLADETYPE)) + 
                    geom_bar(stat = "identity", color = "white") + 
                    #geom_line(data = cov_overall, aes(x = collection_week, y = coverage, group = COUNTY), color = "black", size = 1, alpha = 0.5, inherit.aes = FALSE) + 
                    theme_bw() + 
                scale_fill_manual(values = c("#B5CA8D", "#8BB174", "#426B69", "#222E50", "#654C4F",
                                             "#7FB7BE", "#D3F3EE", "#6D98BA", "#505A5B", "#000000",
                                             "#00487C", "#4BB3FD", "#027BCE", "#644536", "#DDF093",
                                             "#BCA3AC", "#E5CEDC", "#F3EAF4", "#FF5E5B", "#31493C",
                                             "#D35269", "#FEC3A6", "#C17767", "#D8CC34", "#533747")) +
                    labs(title = paste0("Overall - ", input$demo_presentation_prop),
                         subtitle = paste0("Grouped by ", input$demo_type_choice),
                         x = "Sample Collection Week", 
                         y = input$demo_presentation_prop, 
                         fill = "SARS-CoV-2 Clade") + 
                    theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
                facet_wrap(.~DEMO)
        } else if (input$demo_presentation_prop == "Percent"){
          
          
            ggplot(osbdpd, aes(x = WEEK, y = perc, fill = CLADETYPE)) + 
                geom_bar(stat = "identity", color = "white") + 
                #geom_line(data = ospd, aes(x = case_week, y = coverage)) + 
                theme_bw() + 
                scale_fill_manual(values = c("#B5CA8D", "#8BB174", "#426B69", "#222E50", "#654C4F",
                                             "#7FB7BE", "#D3F3EE", "#6D98BA", "#505A5B", "#000000",
                                             "#00487C", "#4BB3FD", "#027BCE", "#644536", "#DDF093",
                                             "#BCA3AC", "#E5CEDC", "#F3EAF4", "#FF5E5B", "#31493C",
                                             "#D35269", "#FEC3A6", "#C17767", "#D8CC34", "#533747")) + 
                labs(title = paste0("Overall - ", input$demo_presentation_prop), 
                     subtitle = paste0("Grouped by ", input$demo_type_choice),
                     x = "Sample Collection Week", 
                     y = input$demo_presentation_prop, 
                     fill = "SARS-CoV-2 Clade") + 
                theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
                facet_wrap(.~DEMO)
        }
        
    })
    
    
    # cross_table_data <- reactive({
    #     if (input$cr_strains_demo_county == "All"){
    #         patient_data <- filter(patient_data, COLL_DATE >= input$cr_strains_demo_date[1] & 
    #                                  COLL_DATE <= input$cr_strains_demo_date[2])
    #     } else if (input$cr_strains_demo_county == "Other Michigan"){
    #         patient_data <- filter(patient_data, COLL_DATE >= input$cr_strains_demo_date[1] & 
    #                                  COLL_DATE <= input$cr_strains_demo_date[2] & PATIENT_COUNTY %in% toupper(region_crosswalk$County.Name) & 
    #                                    !PATIENT_COUNTY %in% c("Jackson", "Lenawee", "Livingston", 
    #                                                           "Monroe", "Oakland", "Washtenaw", "Wayne"))
    #     } else if (input$cr_strains_demo_county == "Other Non-Michigan"){
    #         patient_data <- filter(patient_data, COLL_DATE >= input$cr_strains_demo_date[1] & 
    #                                  COLL_DATE <= input$cr_strains_demo_date[2] & !PATIENT_COUNTY %in% toupper(region_crosswalk$County.Name))
    #     } else {
    #         patient_data <- filter(patient_data, COLL_DATE >= input$cr_strains_demo_date[1] & 
    #                                  COLL_DATE <= input$cr_strains_demo_date[2] & PATIENT_COUNTY == toupper(input$cr_strains_demo_county))
    #     }
    #     
    #     patient_data$approx_age_group <- ifelse(patient_data$APPROX_AGE < 18, "0-17", "18+")
    #     
    #     if (setequal(input$cr_strains_demo_cross, c("Age", "Sex", "Race"))){
    #         patient_data_grouped <- patient_data %>% group_by(extra_clade, approx_age_group, PATIENT_SEX, PATIENT_RACE) %>% summarize(total = length(unique(PATIENT_MRN)))
    #         patient_data_grouped <- reshape2::dcast(patient_data_grouped, approx_age_group + PATIENT_SEX + PATIENT_RACE ~ extra_clade, value.var = c("total"))
    #     } else if (setequal(input$cr_strains_demo_cross, c("Age", "Sex"))){
    #         patient_data_grouped <- patient_data %>% group_by(extra_clade, approx_age_group, PATIENT_SEX) %>% summarize(total = length(unique(PATIENT_MRN)))
    #         patient_data_grouped <- reshape2::dcast(patient_data_grouped, approx_age_group + PATIENT_SEX ~ extra_clade, value.var = c("total"))
    #     }  else if (setequal(input$cr_strains_demo_cross, c("Age", "Race"))){
    #         patient_data_grouped <- patient_data %>% group_by(extra_clade, approx_age_group, PATIENT_RACE) %>% summarize(total = length(unique(PATIENT_MRN)))
    #         patient_data_grouped <- reshape2::dcast(patient_data_grouped, approx_age_group + PATIENT_RACE ~ extra_clade, value.var = c("total"))
    #     } else if (setequal(input$cr_strains_demo_cross, c("Sex", "Race"))){
    #         patient_data_grouped <- patient_data %>% group_by(extra_clade, PATIENT_SEX, PATIENT_RACE) %>% summarize(total = length(unique(PATIENT_MRN)))
    #         patient_data_grouped <- reshape2::dcast(patient_data_grouped, PATIENT_SEX + PATIENT_RACE ~ extra_clade, value.var = c("total"))
    #     } else if (setequal(input$cr_strains_demo_cross, c("Sex"))){
    #         patient_data_grouped <- patient_data %>% group_by(extra_clade, PATIENT_SEX) %>% summarize(total = length(unique(PATIENT_MRN)))
    #         patient_data_grouped <- reshape2::dcast(patient_data_grouped, PATIENT_SEX ~ extra_clade, value.var = c("total"))
    #     } else if (setequal(input$cr_strains_demo_cross, c("Race"))){
    #         patient_data_grouped <- patient_data %>% group_by(extra_clade, PATIENT_RACE) %>% summarize(total = length(unique(PATIENT_MRN)))
    #         patient_data_grouped <- reshape2::dcast(patient_data_grouped, PATIENT_RACE ~ extra_clade, value.var = c("total"))
    #     } else if (setequal(input$cr_strains_demo_cross, c("Age"))){
    #         patient_data_grouped <- patient_data %>% group_by(extra_clade, approx_age_group) %>% summarize(total = length(unique(PATIENT_MRN)))
    #         patient_data_grouped <- reshape2::dcast(patient_data_grouped, approx_age_group ~ extra_clade, value.var = c("total"))
    #     } else {
    #         x <- "None"
    #     }
    #     patient_data_grouped[is.na(patient_data_grouped)] <- 0
    #     
    #     
    #     if("PATIENT_SEX" %in% colnames(patient_data_grouped)){
    #         x <- 0
    #     } else {
    #         patient_data_grouped$PATIENT_SEX <- ""
    #     }
    #     if("PATIENT_RACE" %in% colnames(patient_data_grouped)){
    #         x <- 0
    #     } else {
    #         patient_data_grouped$PATIENT_RACE <- ""
    #     }
    #     if("approx_age_group" %in% colnames(patient_data_grouped)){
    #         x <- 0
    #     } else {
    #         patient_data_grouped$approx_age_group <- ""
    #     }
    #     
    #     #print(ncol(patient_data_grouped))
    #     if("Alpha" %in% colnames(patient_data_grouped)){
    #         x <- 0
    #     } else {
    #         patient_data_grouped$Alpha <- 0
    #     }
    #     if("Delta" %in% colnames(patient_data_grouped)){
    #         x <- 0
    #     } else {
    #         patient_data_grouped$Delta <- 0
    #     }
    #     if("Omicron" %in% colnames(patient_data_grouped)){
    #         x <- 0
    #     } else {
    #         patient_data_grouped$Omicron <- 0
    #     }
    #     if("Other" %in% colnames(patient_data_grouped)){
    #         x <- 0
    #     } else {
    #         patient_data_grouped$Other <- 0
    #     }
    #     
    #     patient_data_grouped <- patient_data_grouped %>% mutate(total = Alpha + Delta + Omicron + Other) %>% arrange(-total)
    #     
    #     patient_data_grouped <- patient_data_grouped %>% mutate(AlphaPercent = paste0(round((Alpha / total)* 100, 1), "%"), 
    #                                                             DeltaPercent = paste0(round((Delta / total)* 100, 1), "%"), 
    #                                                             OmicronPercent = paste0(round((Omicron / total)* 100, 1), "%"),
    #                                                             OtherPercent = paste0(round((Other / total)* 100, 1), "%"))
    #     
    #     patient_data_grouped <- patient_data_grouped %>% select(approx_age_group, PATIENT_SEX, PATIENT_RACE, Alpha, AlphaPercent, Delta, DeltaPercent, Omicron, OmicronPercent, Other, OtherPercent, total)
    #     
    #     return(patient_data_grouped)
    # })
    # 
    # output$cr_strains_demo_table <- render_gt({
    #     print(input$cr_strains_demo_cross == c("Age", "Sex"))
    #     pdg <- cross_table_data()
    #     
    #     pdg %>% gt() %>% tab_style(
    #         style = list(
    #             cell_fill(color = "#EAD7D7")
    #         ),
    #         locations = cells_body(
    #             columns =  vars(`AlphaPercent`),
    #             rows = as.numeric(gsub("%", "", `AlphaPercent`)) > 50
    #         )) %>% 
    #         tab_style(
    #             style = list(
    #                 cell_fill(color = "#EAD7D7")
    #             ),
    #             locations = cells_body(
    #                 columns =  vars(`DeltaPercent`),
    #                 rows = as.numeric(gsub("%", "", `DeltaPercent`)) > 50
    #             )) %>%
    #         tab_style(
    #             style = list(
    #                 cell_fill(color = "#EAD7D7")
    #             ),
    #             locations = cells_body(
    #                 columns =  vars(`OmicronPercent`),
    #                 rows = as.numeric(gsub("%", "", `OmicronPercent`)) > 50
    #             )) %>%
    #         tab_style(
    #             style = list(
    #                 cell_fill(color = "#EAD7D7")
    #             ),
    #             locations = cells_body(
    #                 columns =  vars(`OtherPercent`),
    #                 rows = as.numeric(gsub("%", "", `OtherPercent`)) > 50
    #             )) %>%
    #         cols_label(
    #         total = "Total", 
    #         PATIENT_SEX = "Sex", 
    #         PATIENT_RACE = "Race", 
    #         approx_age_group = "Age", 
    #         AlphaPercent = "", 
    #         DeltaPercent = "", 
    #         OmicronPercent = "", 
    #         OtherPercent = ""
    #     ) %>% 
    #         tab_style(
    #             style = cell_text(
    #                 size = "small"
    #             ),
    #             locations = cells_body(
    #                 columns = everything(),
    #                 rows = everything()
    #             )
    #         ) %>%
    #         tab_style(
    #             style = cell_text(
    #                 size = "small"
    #             ),
    #             locations = cells_column_labels(
    #                 columns = everything()
    #             )
    #         )
    #     
    # })
    
    output$zip_code_map_base <- renderLeaflet({
        leaflet() %>% addTiles() %>% addPolygons(data = tx_mi$geometry,
                                  fillColor = "#b1caf2",
                                  color = "#000000", # you need to use hex colors
                                  #label = lapply(map_2_labs, htmltools::HTML),
                                  fillOpacity = 0.7,
                                  weight = 1,
                                  smoothFactor = 0.2) 
      
      #print(head(county_strain_map_data()))
      patient_data_date <- filter(zip_data1, collection_week %in% input$zip_map_select)
      #patient_data1 <- patient_data_date %>% group_by(extra_clade, PATIENT_COUNTY) %>% summarize(sam_count = length(unique(PATIENT_MRN)))
      patient_data_total <- filter(zip_data1, collection_week %in% input$zip_map_select) %>% group_by(PATIENT_ZIP) %>% summarize(sam_total = sum(count, na.rm = TRUE))
      patient_data1 <- merge(patient_data_date, patient_data_total, by = c("PATIENT_ZIP"))
      patient_data1 <- patient_data1 %>% mutate(percent = round((count/sam_total)*100, 1))
      patient_data2 <- filter(patient_data1, extra_clade == input$zip_map_strain_choice)
      
      #county_geo$NAME <- toupper(county_geo$NAME)
      #summary(county_geo)
      zip_strain_merge <- merge(tx_mi, patient_data2, by.x = c("GEOID10"), by.y = c("PATIENT_ZIP"), all.x = TRUE)
      
      
      zip_strain_merge <- st_transform(zip_strain_merge, "+proj=longlat +ellps=WGS84 +datum=WGS84")
      #county_strain_merge <- as.data.frame(county_strain_merge)
      
      date_addition_1 <- month.abb[unique(filter(zip_data1, collection_week == input$zip_map_select[1])$max_month)]
      #print(date_addition_1)
      date_addition_2 <- month.abb[unique(filter(zip_data1, collection_week == input$zip_map_select[length(input$zip_map_select)])$max_month)]
      
      # map_1_labs <- lapply(seq(nrow(county_strain_merge)), function(i){
      #     paste0('<p> County: ', county_strain_merge[i, "NAME"], '</p>', 
      #            input$county_map_strain_choice, '<p> Sample Count: ', county_strain_merge[i, "sam_count"], '</p>') 
      # })
      
      # covid_cases_county2_date <- filter(covid_cases_all, case_week %in% input$zip_map_select)
      # case <- filter(covid_cases_all, case_week %in% input$county_map_select) %>% group_by(COUNTY) %>% summarize(total_cases = sum(confirmed_cases, na.rm = TRUE))
      # case$COUNTY <- toupper(case$COUNTY)
      
      if (input$zip_map_pres_choice == "Count"){ 
        
        pall<-colorNumeric(c("#EAD7D7","#BD4F6C"), as.numeric(patient_data1$count))
        
        map_1_labs <- zip_strain_merge %>% select(GEOID10, extra_clade, count)
        #map_1_labs <- merge(map_1_labs, case, by.x = c("NAME"), by.y = c("COUNTY"), all.x = TRUE)
        map_1_labs <- as.data.frame(map_1_labs)
        map_1_labs <-  map_1_labs %>% mutate(X = paste0('<p> <b>ZIP Code:</b> ', str_to_title(GEOID10), '</p>', 
                                                        '<p> <b>', input$zip_map_strain_choice, ' Sample Count:</b> ', count, '</p>'))
        map_1_labs <- as.list(map_1_labs$X)
        
        title_label <- paste0('<center><p><b>', input$zip_map_strain_choice, " Counts by Michigan ZIP Code</b></p></center>", 
                              '<center><p>Week ', input$zip_map_select[length(input$zip_map_select)], " (", date_addition_2,  ") to Week ", input$zip_map_select[1], " (", date_addition_1,  ")</p></center>")
        out_map <- leaflet() %>% addTiles() %>% addPolygons(data = zip_strain_merge$geometry,
                                                            fillColor = pall(zip_strain_merge$count),
                                                            color = "#000000", # you need to use hex colors
                                                            label = lapply(map_1_labs, htmltools::HTML),
                                                            fillOpacity = 0.7,
                                                            weight = 1,
                                                            smoothFactor = 0.2) %>% 
          addLegend("bottomright", pal = pall, values = patient_data1$count,
                    title = paste0("Count of<br>", input$zip_map_strain_choice),
                    opacity = 1) %>% 
          addControl(title_label, position = "topright")
        
      } else if (input$zip_map_pres_choice == "Percent"){
        
        pall<-colorNumeric(c("#EAD7D7","#BD4F6C"), as.numeric(patient_data1$percent))
        
        map_1_labs <- zip_strain_merge %>% select(GEOID10, extra_clade, percent, count, sam_total)
        #map_1_labs <- merge(map_1_labs, case, by.x = c("GEOID10"), by.y = c("COUNTY"), all.x = TRUE)
        map_1_labs <- as.data.frame(map_1_labs)
        map_1_labs <-  map_1_labs %>% mutate(X = paste0('<p> <b>ZIP Code:</b> ', str_to_title(GEOID10), '</p>', 
                                                        '<p> <b>', input$zip_map_strain_choice, ' Sample Percent:</b> ', percent, '% (', count, '/', sam_total, ')</p>'))
        map_1_labs <- as.list(map_1_labs$X)
        
        title_label <- paste0('<center><p><b>', input$zip_map_strain_choice, " Percents by Michigan ZIP Code</b></p></center>", 
                              '<center><p>Week ', input$zip_map_select[length(input$zip_map_select)], " (", date_addition_2,  ") to Week ", input$zip_map_select[1], " (", date_addition_1,  ")</p></center>")
        out_map <- leaflet() %>% addTiles() %>% addPolygons(data = zip_strain_merge$geometry,
                                                            fillColor = pall(zip_strain_merge$percent),
                                                            color = "#000000", # you need to use hex colors
                                                            label = lapply(map_1_labs, htmltools::HTML),
                                                            fillOpacity = 0.7,
                                                            weight = 1,
                                                            smoothFactor = 0.2) %>% 
          addLegend("bottomright", pal = pall, values = patient_data1$percent,
                    title = paste0("Percent of<br>", input$zip_map_strain_choice),
                    opacity = 1) %>% 
          addControl(title_label, position = "topright")
      } else {
        x = 0
      }
      
      return(out_map)
      
      
    })
    
    
    # county_strain_map_data <- reactive({
    #     
    #     county_geo$NAME <- toupper(county_geo$NAME)
    #     county_strain_merge <- merge(county_geo, patient_data, by.x = c("NAME"), by.y = c("PATIENT_COUNTY"), all.y = TRUE)
    #     county_strain_merge <- county_strain_merge %>% group_by(geometry, extra_clade, NAME) %>% summarize(sam_count = length(unique(PATIENT_MRN)))
    # })
    output$zip_map_variant_table <- render_gt({
      patient_data_date <- filter(zip_data1, collection_week %in% input$zip_map_select & extra_clade == input$zip_map_strain_choice)
      
      #patient_data_date <- filter(county_data1, collection_week %in% c("2021-32", "2021-33") & extra_clade == "Delta")
      
      zips <- unique(patient_data_date$PATIENT_ZIP)
      combined_lines <- data.frame()
      combined_lines <- rbind(combined_lines, c("i", "final_lineage_list"))
      
      for (i in zips){
        lineage_list <- c()
        lineages <- filter(patient_data_date, PATIENT_ZIP == i)$pieces2
        for (each in lineages){
          cut_up <- str_split(each, ", ")
          for (each_cut in cut_up){
            lineage_list <- c(lineage_list, each_cut)
          }
        }
        
        final_lineage_list <- toString(unique(lineage_list))
        new_row <- c(i, final_lineage_list)
        combined_lines <- rbind(combined_lines, new_row)
      }
      
      #
      
      #combined_lines <- rbind(colnames(combined_lines), combined_lines)
      colnames(combined_lines) <- c("i", "final_lineage_list")
      combined_lines <- filter(combined_lines, i != "i")
      patient_data_date <- merge(patient_data_date, combined_lines, by.x = c("PATIENT_ZIP"), by.y = c("i"), all.x = TRUE)
      
      print(head(patient_data_date))
      
      patient_data_date %>% arrange(desc(count)) %>% select(PATIENT_ZIP, final_lineage_list) %>% distinct() %>% gt() %>%
        cols_label(
          PATIENT_ZIP = "ZIP Code", 
          final_lineage_list = "Lineages"
        ) %>% 
        tab_header(
          title = md("Variant Lineage Details"),
          subtitle = md(input$zip_map_strain_choice)
        )
    })
    
    
    output$county_map_variant_table <- render_gt({
      patient_data_date <- filter(county_data1, collection_week %in% input$county_map_select & extra_clade == input$county_map_strain_choice)
      
      #patient_data_date <- filter(county_data1, collection_week %in% c("2021-32", "2021-33") & extra_clade == "Delta")
      
      counties <- unique(patient_data_date$PATIENT_COUNTY)
      combined_lines <- data.frame()
      for (i in counties){
        lineage_list <- c()
        lineages <- filter(patient_data_date, PATIENT_COUNTY == i)$pieces2
        for (each in lineages){
          cut_up <- str_split(each, ", ")
          for (each_cut in cut_up){
            lineage_list <- c(lineage_list, each_cut)
          }
        }
        
        final_lineage_list <- toString(unique(lineage_list))
        new_row <- c(i, final_lineage_list)
        combined_lines <- rbind(combined_lines, new_row)
      }
      
      combined_lines <- rbind(colnames(combined_lines), combined_lines)
      colnames(combined_lines) <- c("i", "final_lineage_list")
      patient_data_date <- merge(patient_data_date, combined_lines, by.x = c("PATIENT_COUNTY"), by.y = c("i"), all.x = TRUE)
      
      patient_data_date %>% arrange(desc(count)) %>% select(PATIENT_COUNTY, final_lineage_list) %>% distinct() %>% gt() %>%
                cols_label(
                PATIENT_COUNTY = "County", 
                final_lineage_list = "Lineages"
            ) %>% 
        tab_header(
          title = md("Variant Lineage Details"),
          subtitle = md(input$county_map_strain_choice)
        )
    })
    
    output$county_map_base <- renderLeaflet({
        
        #print(head(county_strain_map_data()))
        patient_data_date <- filter(county_data1, collection_week %in% input$county_map_select)
        #patient_data1 <- patient_data_date %>% group_by(extra_clade, PATIENT_COUNTY) %>% summarize(sam_count = length(unique(PATIENT_MRN)))
        patient_data_total <- filter(county_data1, collection_week %in% input$county_map_select) %>% group_by(PATIENT_COUNTY) %>% summarize(sam_total = sum(count, na.rm = TRUE))
        patient_data1 <- merge(patient_data_date, patient_data_total, by = c("PATIENT_COUNTY"))
        patient_data1 <- patient_data1 %>% mutate(percent = round((count/sam_total)*100, 1))
        patient_data2 <- filter(patient_data1, extra_clade == input$county_map_strain_choice)
        
        county_geo$NAME <- toupper(county_geo$NAME)
        #summary(county_geo)
        county_strain_merge <- merge(county_geo, patient_data2, by.x = c("NAME"), by.y = c("PATIENT_COUNTY"), all.x = TRUE)
        
        
        county_strain_merge <- st_transform(county_strain_merge, "+proj=longlat +ellps=WGS84 +datum=WGS84")
        #county_strain_merge <- as.data.frame(county_strain_merge)
        
        date_addition_1 <- month.abb[unique(filter(county_data1, collection_week == input$county_map_select[1])$max_month)]
        #print(date_addition_1)
        date_addition_2 <- month.abb[unique(filter(county_data1, collection_week == input$county_map_select[length(input$county_map_select)])$max_month)]
        
        # map_1_labs <- lapply(seq(nrow(county_strain_merge)), function(i){
        #     paste0('<p> County: ', county_strain_merge[i, "NAME"], '</p>', 
        #            input$county_map_strain_choice, '<p> Sample Count: ', county_strain_merge[i, "sam_count"], '</p>') 
        # })
        
        covid_cases_county2_date <- filter(covid_cases_all, case_week %in% input$county_map_select)
        case <- filter(covid_cases_all, case_week %in% input$county_map_select) %>% group_by(COUNTY) %>% summarize(total_cases = sum(confirmed_cases, na.rm = TRUE))
        case$COUNTY <- toupper(case$COUNTY)
        
        if (input$county_map_pres_choice == "Count"){ 
            
            pall<-colorNumeric(c("#EAD7D7","#BD4F6C"), as.numeric(patient_data1$count))
            
                map_1_labs <- county_strain_merge %>% select(NAME, extra_clade, count)
                map_1_labs <- merge(map_1_labs, case, by.x = c("NAME"), by.y = c("COUNTY"), all.x = TRUE)
                map_1_labs <- as.data.frame(map_1_labs)
                map_1_labs <-  map_1_labs %>% mutate(X = paste0('<p> <b>County:</b> ', str_to_title(NAME), '</p>', 
                                                                '<p> <b>', input$county_map_strain_choice, ' Sample Count:</b> ', count, '</p>', 
                                                                '<p> <b>Confirmed Case Count:</b> ', total_cases, '</p>'))
                map_1_labs <- as.list(map_1_labs$X)
                
                title_label <- paste0('<center><p><b>', input$county_map_strain_choice, " Counts by Michigan County</b></p></center>", 
                                      '<center><p>Week ', input$county_map_select[length(input$county_map_select)], " (", date_addition_2,  ") to Week ", input$county_map_select[1], " (", date_addition_1,  ")</p></center>")
                out_map <- leaflet() %>% addTiles() %>% addPolygons(data = county_strain_merge$geometry,
                                                                    fillColor = pall(county_strain_merge$count),
                                                                    color = "#000000", # you need to use hex colors
                                                                    label = lapply(map_1_labs, htmltools::HTML),
                                                                    fillOpacity = 0.7,
                                                                    weight = 1,
                                                                    smoothFactor = 0.2) %>% 
                    addLegend("bottomright", pal = pall, values = patient_data1$count,
                              title = paste0("Count of<br>", input$county_map_strain_choice),
                              opacity = 1) %>% 
                    addControl(title_label, position = "topright")
                
        } else if (input$county_map_pres_choice == "Percent"){
            
            pall<-colorNumeric(c("#EAD7D7","#BD4F6C"), as.numeric(patient_data1$percent))
            
                map_1_labs <- county_strain_merge %>% select(NAME, extra_clade, percent, count, sam_total)
                map_1_labs <- merge(map_1_labs, case, by.x = c("NAME"), by.y = c("COUNTY"), all.x = TRUE)
                map_1_labs <- as.data.frame(map_1_labs)
                map_1_labs <-  map_1_labs %>% mutate(X = paste0('<p> <b>County:</b> ', str_to_title(NAME), '</p>', 
                                                                '<p> <b>', input$county_map_strain_choice, ' Sample Percent:</b> ', percent, '% (', count, '/', sam_total, ')</p>', 
                                                                '<p> <b>Confirmed Case Count:</b> ', total_cases, '</p>'))
                map_1_labs <- as.list(map_1_labs$X)
                
                title_label <- paste0('<center><p><b>', input$county_map_strain_choice, " Percents by Michigan County</b></p></center>", 
                                      '<center><p>Week ', input$county_map_select[length(input$county_map_select)], " (", date_addition_2,  ") to Week ", input$county_map_select[1], " (", date_addition_1,  ")</p></center>")
                out_map <- leaflet() %>% addTiles() %>% addPolygons(data = county_strain_merge$geometry,
                                                                    fillColor = pall(county_strain_merge$percent),
                                                                    color = "#000000", # you need to use hex colors
                                                                    label = lapply(map_1_labs, htmltools::HTML),
                                                                    fillOpacity = 0.7,
                                                                    weight = 1,
                                                                    smoothFactor = 0.2) %>% 
                    addLegend("bottomright", pal = pall, values = patient_data1$percent,
                              title = paste0("Percent of<br>", input$county_map_strain_choice),
                              opacity = 1) %>% 
                    addControl(title_label, position = "topright")
        } else {
            x = 0
        }
        
        return(out_map)

    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
