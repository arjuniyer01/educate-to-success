library(shiny)
library(tidyverse)
library(dplyr)

ed_data_filtered <- read.csv("ed_data_filtered.csv", header = T, sep = ",")
hdi_data <- read.csv("HDI.csv")
correlation_table <- read.csv("correlation_table.csv")

ui <- shinyUI(navbarPage("Education Data",
                         tabPanel("Indicator vs. HDI",
                                  # Sidebar layout with input and output definitions ----
                                  sidebarLayout(
                                    # Sidebar panel for inputs ----
                                    sidebarPanel(
                                      # Input:
                                      selectInput("indicator1",
                                                  label = h3("Select Indicator"),
                                                  choices = ed_data_filtered$Indicator.Name,
                                                  selected = 1),
                                      selectInput("year1",
                                                  label = h3("Select Year"),
                                                  choices = seq(1990, 2015, 1),
                                                  selected = 2008),
                                      h5("Datapoints Available for each year:"),
                                      tableOutput('datapoints_table'),
                                      
                                    ),
                                    # Main panel for displaying outputs ----
                                    mainPanel(
                                      plotOutput("plot1",
                                                 hover = hoverOpts(
                                                   id = "plot_hover"
                                                 )),
                                      h4("Hover over the plot to lookup countries!"),
                                      tableOutput("hover_info"),
                                      
                                    )
                                  ),
                                  ),
                   tabPanel("Indicator trends by country",
                            # Sidebar layout with input and output definitions ----
                            sidebarLayout(
                              
                              # Sidebar panel for inputs ----
                              sidebarPanel(
                                
                                # Input:
                                selectInput("indicator2",
                                            label = h3("Select Indicator"),
                                            choices = ed_data_filtered$Indicator.Name,
                                            selected = 1),
                                selectInput("countries2", label = h3("Select Countries"),
                                            choices = ed_data_filtered$Country.Name,
                                            selected = "Norway",
                                            multiple = T)
                                
                                
                              ),
                              
                              # Main panel for displaying outputs ----
                              mainPanel(
                                
                                # Output:
                                plotOutput("plot2")
                                
                              )
                            )),
                   tabPanel("Correlation Table",
                            # Sidebar layout with input and output definitions ----
                            sidebarLayout(
                              
                              # Sidebar panel for inputs ----
                              sidebarPanel(
                                
                                # Output:
                                h4("Correlation table used for making observations"),
                                h6("1) The table contains: Pearson correlation factor and the corresponding p-value in brackets."),
                                h6("2) Averaged correlation is also measured by looking at the correlation between average HDI and average indicator value for each country between 1990-2015."),
                                h6("3) The time frame has been limited to 1990-2015 since this is where the data exists for both education and HDI datasets."),
                                h6("4) Shows '0' when not enough datapoints available.")
                              ),
                              
                              # Main panel for displaying outputs ----
                              mainPanel(
                                
                                # Output:
                                tableOutput("corr_table"),
                                
                              )
                            )),
))



# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  
  display_table <- reactive({
    ed_data_filtered %>%
      filter(Indicator.Name == input$indicator1) %>%
      pivot_longer(cols = c(X1990:X2015), names_to = "Year") %>%
      group_by(Year) %>%
      summarise(Available.Datapoints = sum(!is.na(value))) %>% 
      mutate(Year = substr(Year, 2, 5)) %>% 
      arrange(desc(Available.Datapoints))
  })
  
  output$datapoints_table <- renderTable({display_table()})
  output$corr_table <- renderTable({correlation_table})
  
  ed_temp <- reactive({
    ed_data_filtered %>% 
      select(Indicator.Name, c(paste0("X", input$year1)), Country.Name) %>% 
      filter(Indicator.Name == input$indicator1) %>% 
      rowwise() %>% 
      mutate(indicator = mean(c_across(paste0("X", input$year1)), na.rm=T)) %>% 
      select(Indicator.Name, indicator, Country.Name)
  })
  
  hdi_temp <- reactive({
    hdi_data %>% 
      select(Country, c(paste0("X", input$year1))) %>% 
      rowwise() %>% 
      mutate(hdi = mean(c_across(paste0("X", input$year1)), na.rm=T)) %>% 
      left_join(ed_temp(), by=c("Country" = "Country.Name")) %>% 
      select(Country, hdi, indicator)
  })
  
  output$plot1 <- renderPlot({
    hdi_temp() %>% 
      ggplot(aes(hdi, indicator)) +
      geom_point() +
      geom_smooth() +
      ggtitle(paste0(input$indicator1, " (",  input$year1,")")) +
      ylab("Value") +
      xlab(paste0("Human Development Index (HDI)", " (",  input$year1,")"))
    
  })
  
  output$plot2 <- renderPlot({
    ed_data_filtered %>%
      filter(Indicator.Name == input$indicator2) %>%
      select(Country.Name, c(X1970:X2015)) %>%
      filter(Country.Name %in%  input$countries2) %>%
      pivot_longer(cols = c(X1970:X2015), names_to = "year") %>% 
      ggplot(aes(year, value, group=Country.Name)) +
      geom_smooth(aes(color=Country.Name)) +
      geom_point(aes(color=Country.Name)) +
      scale_x_discrete(guide = guide_axis(check.overlap = TRUE))
  })
  
  output$hover_info <- renderTable({
           hdi_temp()[which.min(abs(input$plot_hover$x-hdi_temp()$hdi)+abs(input$plot_hover$y-hdi_temp()$indicator)),]
  })
  
}


shinyApp(ui = ui, server = server)
