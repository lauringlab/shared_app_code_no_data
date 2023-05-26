#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinybusy)
library(shinyWidgets)
library(tidyverse)
library(lubridate)
library(ggplot2)

# get coverage file names
basic_cov_files <- list.files("~/r_shiny_apps_jg/coverage/coverage_exam/data/basic_coverage_files/", pattern = "*.csv")


#full_plate_file2 <- read.csv("~/r_shiny_apps_jg/coverage/coverage_exam/data/basic_coverage_files/20230127_IAV_Illumina_Run_44.coverage.csv")
#print(head(full_plate_file2))
#print(colnames(full_plate_file2))

key_set <- read.csv("~/r_shiny_apps_jg/coverage/coverage_exam/data/KEY.csv") %>% arrange(PlateNames)
comp_stepwise <- readRDS("~/r_shiny_apps_jg/coverage/coverage_exam/data/stepwise/stepwise.rds")
primer_info <- readRDS("~/r_shiny_apps_jg/coverage/coverage_exam/data/primers/artic_v4_primer_bed_2.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(
        tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style-page.css")), 
        navbarPage(HTML("Sequencing Run Coverage"), 
                           navbarMenu("Full Run",
                                      tabPanel("SARS-CoV-2",
                                               add_busy_bar(color = "#450920"),
                                               titlePanel(h4(strong("SARS-CoV-2 FULL RUN COVERAGE"))), 
                                               
                                                sidebarPanel(id = "full_run_sidebar",
                                                    selectInput("select_run_FC", h5("Select Plate Run:"), 
                                                                choices = unique(key_set$PlateNames), 
                                                                selected = unique(key_set$PlateNames)[-1]), # default select the most recent run available
                                                    #radioButtons("select_window_FC", h5("Window Size:"),
                                                    #             choices = c(100, 400, 1000), selected = 400), 
                                                    radioGroupButtons(
                                                        inputId = "select_window_FC",
                                                        label = h5("Window Size:"),
                                                        choices = c(100, 400, 1000),
                                                        selected = 400
                                                    )
                                                ),
                                        
                                                # Show a plot of the generated distribution
                                                mainPanel(
                                                   plotOutput(outputId = "coverage_boxplot_FC")
                                                
                                                )
                                            ), 
                                      tabPanel("Influenza A")
                                        ), 
                           navbarMenu("Cut-offs",
                                      tabPanel("SARS-CoV-2",
                                               add_busy_bar(color = "#450920"),
                                               titlePanel(h4(strong("SARS-CoV-2 Coverage Cut-offs"))),

                                               
                                               sidebarPanel(id = "cut_off_sidebar",
                                                       selectInput("select_run_CO", h5("Select Plate Run:"), 
                                                                   choices = unique(key_set$PlateNames), 
                                                                   selected = unique(key_set$PlateNames)[-1]), # default select the most recent run available
                                                       #checkboxGroupInput("select_cutoff_CO", 
                                                    #                      h5("Cut-off Options:"), 
                                                    #                      choices = list(10, 20, 200, 400, 500, 1000),
                                                    #                      selected = c(10, 20, 200, 400, 500, 1000))
                                                    checkboxGroupButtons(
                                                           inputId = "select_cutoff_CO",
                                                           label = h5("Cut-off Options:"),
                                                           choices = c(10, 20, 200, 400, 500, 1000),
                                                           selected = c(10, 20, 200, 400, 500, 1000)
                                                       )
                                                ),

                                               # Show a plot of the generated distribution
                                               mainPanel(
                                                   plotOutput("coverage_lines_CO")
                                               )
                                      ),
                                      tabPanel("Influenza A")
                                    ),
                        navbarMenu("Individual Samples",
                                   tabPanel("SARS-CoV-2",
                                            add_busy_bar(color = "#450920"),
                                            titlePanel(h4(strong("SARS-CoV-2 Sample Coverage"))),

                                            sidebarPanel(id = "ind_run_sidebar",
                                                #htmlOutput("plate_selector"),#add selectinput boxs
                                                selectInput("select_run_IND", h5("Select Plate Run:"), 
                                                            choices = unique(key_set$PlateNames), 
                                                            selected = unique(key_set$PlateNames)[-1]),  # default select the most recent run available
                                                
                                                htmlOutput("sample_selector")# from objects created in server
                                            ),

                                            # Show a plot of the generated distribution
                                            mainPanel(
                                                plotOutput("coverage_IND")
                                            )
                                   ),
                                   tabPanel("Basic Coverage - Illumina", 
					add_busy_bar(color = "#450920"),
                                            titlePanel(h4(strong("Sample Coverage"))),

                                            sidebarPanel(id = "basic_sidebar",
                                                #htmlOutput("plate_selector"),#add selectinput boxs
                                                selectInput("select_org_basic", h5("Select Organism:"), 
                                                            choices = c("IAV", "SC2"), 
                                                            selected = "IAV"),  # default select the most recent run available
                                                
                                                htmlOutput("plate_selector_basic"), # from objects created in server
						htmlOutput("sample_selector_basic")
                                            ),

                                            # Show a plot of the generated distribution
                                            mainPanel(
                                                plotOutput("basic_plotoutput", height = 800)
                                            )
				   )
                                ),
                        navbarMenu("Primers",
                                   tabPanel("SARS-CoV-2",
                                            add_busy_bar(color = "#450920"),
                                            titlePanel(h4(strong("SARS-CoV-2 Sample Primer Investigation"))),
                                            
                                            sidebarPanel(id = "primer_run_sidebar",
                                                #htmlOutput("plate_selector"),#add selectinput boxs
                                                selectInput("select_run_P", h5("Select Plate Run:"), 
                                                            choices = unique(key_set$PlateNames), 
                                                            selected = unique(key_set$PlateNames)[-1]),  # default select the most recent run available
                                                
                                                htmlOutput("sample_selector_p"), # from objects created in server
                                                
                                                numericInput("p_position_start", 
                                                             h5("(0) Genome Position Start:"), 
                                                             value = 0), 
                                                
                                                numericInput("p_position_end", 
                                                             h5("(29903) Genome Position End:"), 
                                                             value = 29903)
                                            ),
                                            
                                            # Show a plot of the generated distribution
                                            mainPanel(
                                                plotOutput("coverage_P_primers", height = "75px", 
                                                           click = clickOpts(id = "primer_click", clip = TRUE)),
                                                verbatimTextOutput("click_primer_write"),
                                                plotOutput("coverage_P")
                                            )
                                   ),
                                   tabPanel("Influenza A")
                        )
                        
                            )
                    )


# Define server logic required to draw a histogram
server <- function(input, output){
    
    output$plate_selector_basic <- renderUI({
        
	plate_options <- c()
	for (i in basic_cov_files){
		if (grepl(input$select_org_basic, i)){
			plate_options <- c(plate_options, i)
		}
	}
	#plate_options <- filter(basic_cov_files, grepl(input$select_org_basic, basic_cov_files))

       
        selectInput(
            inputId = "plate_select_basic",
            label = h5("Select Plate to Examine:"),
            choices = plate_options,
            selected = plate_options[1]
        )

	#print(unique(data_available$SampleNames))
	#print(class(data_available))

    })
    
	output$sample_selector_basic <- renderUI({
		
		full_plate_file2 <- read.csv(paste0("~/r_shiny_apps_jg/coverage/coverage_exam/data/basic_coverage_files/", input$plate_select_basic))
		
	       
		checkboxGroupButtons(
		    inputId = "sample_select_basic",
		    label = h5("Select Samples to Examine:"),
		    choices = unique(as.character(full_plate_file2$ID)),
		    selected = unique(as.character(full_plate_file2$ID))[1:2], 
		    direction = "vertical"
		)

		#print(unique(data_available$SampleNames))
		#print(class(data_available))

	    })
    

	output$basic_plotoutput <- renderPlot({

		full_plate_file <- read.csv(paste0("~/r_shiny_apps_jg/coverage/coverage_exam/data/basic_coverage_files/", input$plate_select_basic))
		fpl <- filter(full_plate_file, ID %in% unique(input$sample_select_basic))

		if (input$select_org_basic == "IAV"){
			fpl <- fpl %>% mutate(count = case_when(as.numeric(cov) > 30 ~ 1,
		                                                      T ~ 0)) %>% group_by(ID, chr) %>% mutate(total = sum(as.numeric(cov)),
		                                                                                                average = mean(as.numeric(cov)),
		                                                                                                full = case_when(chr == "A_PB1" ~ 2341, 
		                                                                                                                 chr == "A_PB2" ~ 2341, 
		                                                                                                                 chr == "A_PA" ~ 2233, 
		                                                                                                                 chr == "A_HA_H3" ~ 1778, 
		                                                                                                                 chr == "A_HA_H1" ~ 1778,
		                                                                                                                 chr == "A_NA_N1" ~ 1778,
		                                                                                                                 chr == "A_MP" ~ 1027, 
		                                                                                                                 chr == "A_NA_N2" ~ 1413, 
		                                                                                                                 chr == "A_NP" ~ 1565,
		                                                                                                                 chr == "A_NS" ~ 890,
		                                                                                                                 T ~ 0),
		                                                                                                count2 = sum(count))
		} else if (input$select_org_basic == "SC2"){

			fpl <- fpl %>% mutate(count = case_when(as.numeric(cov) > 30 ~ 1,
                                                          T ~ 0)) %>% group_by(ID, chr) %>% mutate(total = sum(as.numeric(cov)),
                                                                                                    average = mean(as.numeric(cov)),
                                                                                                    full = 29903,
                                                                                                    count2 = sum(count))


		} else {
			print("no recognized organism")
		}


		fpl <- as.data.frame(fpl) 
        
        
        	fpl <- fpl %>% mutate(calc1 = paste0(round(total / full, 1), "x"), 
                                      calc2 = paste0(round((count2 / full)*100, 1), "%"))


		dL <- fpl %>% select(ID, chr, calc1, calc2) %>% distinct()
		dL2 <- fpl %>% group_by(ID, chr) %>% summarize(x = max(pos) - (0.15 * max(pos)), 
							       y = max(cov) - (0.1 * max(cov)))

		dL <- merge(dL, dL2, by = c("ID", "chr"))
		rm(dL2)
        	dL <- dL %>% mutate(color_rule = case_when(as.numeric(gsub("%", "", calc2)) >= 10 ~ "30x Limit", 
                                                   T ~ "Under"))


		ggplot(fpl, aes(x = pos, y = cov)) + 
			geom_bar(stat = "identity") + 
			theme_bw() + 
			labs(x = "GENOME POSITION", 
                 	     y = "COVERAGE",
                 	     color = "", 
                 	     caption = "[OVERALL GENOME AVERAGE COVERAGE DEPTH]; [PERCENT OF GENOME COVERED AT 30x OR MORE]") + 
                 	geom_text(data = dL, mapping = aes(x = x, y = y, label = paste0(calc1, "; ", calc2), color = color_rule), fontface = "bold") + 
            		scale_color_manual(values = c("#450920", "#EA5D93")) + 
			facet_wrap(.~ID + chr, ncol = 3, scales = "free") + 
			theme(legend.position = "none")


	})



    output$sample_selector <- renderUI({#creates County select box object called in ui
        
        data_available <- filter(key_set, PlateNames == input$select_run_IND)
	data_available$SampleNames <- as.character(data_available$SampleNames)

        #creates a reactive list of available counties based on the State selection made
        control_wells <- filter(data_available, grepl("NC_", SampleNames) | grepl("HeLa", SampleNames))
	control_wells$SampleNames <- as.character(control_wells$SampleNames)
        
        # checkboxGroupInput("sample_select_IND", 
        #                    h5("Select Samples to Examine:"), 
        #                    choices = unique(data_available$SampleNames),
        #                    selected = unique(control_wells$SampleNames))
        
        checkboxGroupButtons(
            inputId = "sample_select_IND",
            label = h5("Select Samples to Examine:"),
            choices = unique(data_available$SampleNames),
            selected = unique(control_wells$SampleNames), 
            direction = "vertical"
        )

	#print(unique(data_available$SampleNames))
	#print(class(data_available))

    })
    
    output$sample_selector_p <- renderUI({#creates County select box object called in ui
        
        data_available2 <- filter(key_set, PlateNames == input$select_run_P)
        
        selectInput("sample_select_P", h5("Select Sample:"), 
                    choices = unique(data_available2$SampleNames),
                    selected = unique(data_available2$SampleNames)[1])
    })
    
    
    FC_covid_dat <- reactive({
        
        fc_covid_data <- readRDS(paste0("~/r_shiny_apps_jg/coverage/coverage_exam/data/", input$select_run_FC, ".rds"))
        
        fc_covid_data <- as.data.frame(fc_covid_data)
        
        fc_covid_data <- fc_covid_data %>%
            mutate(window = cut_interval(as.numeric(Position), length = as.numeric(input$select_window_FC))) %>%
            group_by(FileName, window) %>%
            summarize(mean_cov = mean(as.numeric(Coverage))) 

    })
    
    
    CO_covid_base <- reactive({
        
        co_covid_data <- readRDS(paste0("~/r_shiny_apps_jg/coverage/coverage_exam/data/", input$select_run_CO, ".rds"))
        
        co_covid_data <- as.data.frame(co_covid_data)
        
    })
    
    CO_covid_dat <- reactive({
        
        stepwise_data <- filter(comp_stepwise, threshold %in% input$select_cutoff_CO & PlateName == input$select_run_CO)
        # set threshold column as factor in proper order
        stepwise_data$threshold <- factor(stepwise_data$threshold, levels = input$select_cutoff_CO)
        
        return(stepwise_data)
    })
    
    
    IND_covid_base <- reactive({
        
        ind_covid_data2 <- readRDS(paste0("~/r_shiny_apps_jg/coverage/coverage_exam/data/", input$select_run_IND, ".rds"))
        
        ind_covid_data2 <- as.data.frame(ind_covid_data2)
        ind_covid_data2$FileName <- as.character(ind_covid_data2$FileName)
	ind_covid_data2$Coverage <- as.numeric(ind_covid_data2$Coverage)
	ind_covid_data2$Position <- as.numeric(ind_covid_data2$Position)

        ind_covid_data <- filter(ind_covid_data2, FileName %in% input$sample_select_IND)
        
        ind_covid_data <- ind_covid_data %>% mutate(count = case_when(as.numeric(Coverage) > 30 ~ 1,
                                                          T ~ 0)) %>% group_by(FileName) %>% mutate(total = sum(as.numeric(Coverage)),
                                                                                                    average = mean(as.numeric(Coverage)),
                                                                                                    full = 29903,
                                                                                                    count2 = sum(count))
        
        ind_covid_data <- as.data.frame(ind_covid_data) 
        # ind_covid_data$calc1 <- paste0(round(ind_covid_data$total / ind_covid_data$full, 1), "x")
        # ind_covid_data$calc2 <- paste0(round(ind_covid_data$count2 / ind_covid_data$full, 1), "%")
        
        ind_covid_data <- ind_covid_data %>% mutate(calc1 = paste0(round(total / full, 1), "x"), 
                                                    calc2 = paste0(round((count2 / full)*100, 1), "%"))
    })
    
    P_covid_base <- reactive({
        
        p_covid_data <- readRDS(paste0("~/r_shiny_apps_jg/coverage/coverage_exam/data/", input$select_run_IND, ".rds"))
        
        p_covid_data2 <- filter(p_covid_data, FileName == input$sample_select_P)
        
        p_covid_data2 <- as.data.frame(p_covid_data2)
        
    })
    
    output$coverage_boxplot_FC <- renderPlot({
        #Coverage plot
        data_windows <- FC_covid_dat() 
        
        data_window_color <- data_windows %>% group_by(window) %>% summarize(low_limit = median(as.numeric(mean_cov))) %>% arrange(low_limit)
        
        ggplot(data_windows, aes(x = window, y = mean_cov, group = window)) +
            geom_boxplot() + 
            geom_point(data = head(data_window_color, 5), aes(x = window, y = low_limit - 10), fill = "#450920", color = "#450920", shape = 25) + 
            labs(title = "", 
                 y = "READ DEPTH", 
                 x = "GENOME POSITION", 
                 caption = "TRIANGLES INDICATE FIVE LOWEST MEDIAN COVERAGES WITHIN GENOME WINDOWS") + 
            theme_classic() + 
            theme(axis.title.y = element_text(vjust=1.2), 
                  legend.position = "none", 
                  text = element_text(size = 15), 
                  axis.text.x = element_text(size = 5, angle = 45, hjust = 1), 
                  axis.text.y = element_text(size = 10), 
                  plot.background = element_rect(colour = "black", fill=NA, size=3))
    })
    
    output$coverage_lines_CO <- renderPlot({
        
        stepwise_data_plot <- CO_covid_dat()
        
        data_in <- CO_covid_base()
        # Color scheme
        stepplot_colors <- c("#450920", "#7E103B", "#B51754", "#E3266F", 
                             "#EA5D93", "#F193B7", "#F8C9DB")
        
        # generate plot
        
        ggplot(as.data.frame(stepwise_data_plot), aes(x = step, y = num_samples, color = threshold)) +
            geom_step(size = 2, alpha = 0.6) +
            theme_classic() +
            theme(text = element_text(size = 10), 
                  plot.background = element_rect(colour = "black", fill=NA, size=3)) +
            labs(x = "FRACTION OF GENOME ABOVE DEPTH CUTOFF", 
                 y = "NUMBER OF SAMPLES", 
                 color = "CUTOFF") + 
            ylim(c(0, length(unique(data_in$FileName)) + 1)) +
            scale_color_manual(values = stepplot_colors) 
    })
    
    output$coverage_IND <- renderPlot({
        
        data_IDs <- IND_covid_base()
        
        #print(colnames(data_IDs))
        #print(summary(data_IDs))
        
        dL <- data_IDs %>% select(FileName, calc1, calc2) %>% distinct()
        dL$x <- max(data_IDs$Position) - (0.15 * max(data_IDs$Position)) 
        dL$y <- max(data_IDs$Coverage) - (0.1 * max(data_IDs$Coverage))
        dL <- dL %>% mutate(color_rule = case_when(as.numeric(gsub("%", "", calc2)) >= 10 ~ "30x Limit", 
                                                   T ~ "Under"))
        
        ggplot(data_IDs, aes(x = Position, y = Coverage, group = FileName)) + 
            geom_line(color = "gray50", alpha = 0.7) + 
            geom_smooth(color = "gray40") + 
            labs(x = "GENOME POSITION", 
                 y = "COVERAGE",
                 color = "", 
                 caption = "[OVERALL GENOME AVERAGE COVERAGE DEPTH]; [PERCENT OF GENOME COVERED AT 30x OR MORE]") + 
            geom_text(data = dL, mapping = aes(x = x, y = y, label = paste0(calc1, "; ", calc2), color = color_rule), fontface = "bold") + 
            scale_color_manual(values = c("#450920", "#EA5D93")) + 
            facet_wrap(. ~ FileName, ncol = 3) + 
            theme_bw() + 
            theme(legend.position = "none", 
                  plot.background = element_rect(colour = "black", fill=NA, size=3))
        
    })
    
    
    output$click_primer_write <- renderText({
        #print(input$primer_click$x)
        
        if (length(input$primer_click$x) == 0){
            primer_name <- "None"
        } else {
            
            close <- function(x, value, tol=NULL){
                if(!is.null(tol)){
                    x[abs(x-value) <= tol]
                } else {
                    x[order(abs(x-value))]
                }
            }
            
            centers_only <- filter(primer_info, side == "CENTER" & as.numeric(flight) == round(as.numeric(input$primer_click$y), 0))
            name_points <- close(centers_only$value, input$primer_click$x)[1:2]
            
            
            primer_name <- unique(filter(centers_only, value %in% name_points)$name)
        }
        
        
        return(paste0("Primer Selected: ", primer_name))
    })
    
    output$coverage_P_primers <- renderPlot({
        
            ggplot(primer_info, aes(x = value, y = as.factor(flight), color = side, group = name)) + 
                geom_line(size = 3) + 
                scale_color_manual(values = c("#EA5D93", "#450920", "#450920")) + 
                theme_classic() + 
                theme(legend.position = "none", 
                      plot.background = element_rect(colour = "black", fill=NA, size=3)) + 
                labs(x = "", 
                     y = "") + 
                xlim(input$p_position_start, input$p_position_end)
        
            
    })
    
    output$coverage_P <- renderPlot({
        
        one_sample <- P_covid_base()
        

        if (length(input$primer_click$x) == 0){
            primer_name <- "None"
        
        } else {
            
            close <- function(x, value, tol=NULL){
                if(!is.null(tol)){
                    x[abs(x-value) <= tol]
                } else {
                    x[order(abs(x-value))]
                }
            }
            
            centers_only <- filter(primer_info, side == "CENTER" & as.numeric(flight) == round(as.numeric(input$primer_click$y), 0))
            name_points <- close(centers_only$value, input$primer_click$x)[1:2]
            
            user_highlight <- filter(centers_only, value %in% name_points)
            primer_name <- unique(user_highlight$name)
        }
        
        if(primer_name == "None"){
                    ggplot(filter(one_sample, Position >= input$p_position_start & Position <= input$p_position_end), aes(x = Position, y = Coverage, group = FileName)) + 
                        geom_line(color = "gray20", alpha = 0.7) + 
                        geom_smooth(color = "gray60") + 
                        theme_classic() + 
                        theme(plot.background = element_rect(colour = "black", fill=NA, size=3)) +
                        labs(x = "GENOME POSITION", 
                             y = "COVERAGE", 
                             color = "")
        } else {
            
            user_highlight$y_range <- max(filter(one_sample, Position >= input$p_position_start & Position <= input$p_position_end)$Coverage)
            
            user_highlight$min_x <- min(user_highlight$value)
            user_highlight$max_x <- max(user_highlight$value)
            
            ggplot(filter(one_sample, Position >= input$p_position_start & Position <= input$p_position_end), aes(x = Position, y = Coverage), group = FileName) + 
                geom_line(color = "gray20", alpha = 0.7) + 
                geom_smooth(color = "gray60") + 
                geom_area(inherit.aes = FALSE, data = user_highlight, aes(y = y_range, x = value, fill = name), alpha = 0.3) + 
                scale_fill_manual(values = "#F8C9DB") + 
                theme_classic() + 
                theme(legend.position = "none", 
                      plot.background = element_rect(colour = "black", fill=NA, size=3)) +
                labs(x = "GENOME POSITION", 
                     y = "COVERAGE", 
                     color = "")
        }
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

