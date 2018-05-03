# Load necessary libraries 
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(plotly)
library(shiny)

# Reading in datasets 
fah_income <-read_csv('FAH_income1.csv')
fah_educ <-read_csv('FAH_educ1.csv')
fah_gender <-read_csv('FAH_gender1.csv')
fah_race <-read_csv('FAH_race1.csv')
fc_perc <-read_csv('fc_perc.csv')

# Data wrangling
fah_gender <-fah_gender %>%
  rename(Facet = Gender) %>%
  filter(!str_detect(string = Food, pattern = "total")) 

fah_income <- fah_income %>%
  rename(Facet = Socio) %>%
  filter(!str_detect(string = Food, pattern = "total"))

fah_educ <- fah_educ %>%
  rename(Facet = Education) %>%
  filter(!str_detect(string = Food, pattern = "total"))

fah_race <- fah_race %>%
  rename(Facet = Race) %>%
  filter(!str_detect(string = Food, pattern = "total"))

perc_tidy <-fc_perc %>%
  rename(Year = year) %>%
  gather(MeatEggNut, Dairy, Fruit, Vegetable, FlourCereal, AddedFats, CaloricSweeteners, 
         key = "Food", value = "Percent") %>% 
  mutate(Year = as.integer(Year))

# Added four demographic datasets to list
all_data <- list(Gender = fah_gender, Race = fah_race, Education = fah_educ, Income = fah_income)

##### UI Side ######

# Use fluid page layout (rows and columns)
ui <- fluidPage(
  
  # Title
  titlePanel("What are People Eating in the U.S.?"),
  
  h5("Stat 41 Group 8 presents the app: 'What are 
     People Eating in the U.S.?'. Our Shiny application 
     demonstrates what types of food people from different 
     demographics are eating. Change the year and type of 
     demographic using the inputs in the sidebar and notice how 
     the plot reacts. Then select the catagories you would like 
     to compare using the checkbox input. For our second graph, 
     use the slider to examine how the diet of the U.S population 
     has changed over the years. Finally, type which food groups 
     you would like to compare into the search bar. 
     For more information on the data, see the full datasets - 
     which are also interactive - under tabs 'Plot One Data' and
     'Plot Two Data'. See the link below for our code. Enjoy!
     "),
  tags$a(href="https://github.com/clevitt1/What-are-People-Eating-in-the-US-Code/blob/master/APP/app.R", 
         "App Code"),
  
  br(),
  br(),
  
  # Generate row with sidebar
  sidebarLayout(
    
    # Define sidebar
    sidebarPanel(width = 4,
                 
                 
                 # Title for first plot
                 h4("Food Consumed by Demographics"),
                 
                 # First Widget - Asks user to select year 
                 selectInput(inputId = "Year",
                             label = "Choose a year to view:",
                             choices = list("1994-98", 
                                            "2003-04", 
                                            "2005-06",
                                            "2007-08"),
                             selected = "1994-98"),
                 
                 # Second Widget - Asks user to select a dataset
                 selectInput(inputId = "dataDem",
                             label = "Choose a dataset to view:",
                             choices = names(all_data),
                             selected = "Gender"),
                 
                 # Third Widget - Asks user to select which variables to compare 
                 conditionalPanel(
                   condition = "input.dataDem == 'Gender'",
                   checkboxGroupInput(inputId = "facetGen", 
                                      label = "Choose which genders you would like to compare:",
                                      choices = list("Girls (1-20)" = "Girls", 
                                                     "Women (21+)" = "Women", 
                                                     "Boys (1-20)" = "Boys", 
                                                     "Men (21+)" = "Men"),
                                      selected = list("Women","Men"))),
                 
                 conditionalPanel(
                   condition = "input.dataDem == 'Race'",
                   checkboxGroupInput(inputId = "facetRace", 
                                      label = "Choose which races you would like to compare:",
                                      choices = list("White", 
                                                     "Black", 
                                                     "Hispanic",
                                                     "Other"),
                                      selected = list("White","Black", "Hispanic"))),
                 conditionalPanel(
                   condition = "input.dataDem == 'Education'",
                   checkboxGroupInput(inputId = "facetEduc", 
                                      label = "Choose which education levels you would like to compare:",
                                      choices = list("LessThanHighSchool", 
                                                     "HighSchool", 
                                                     "College"),
                                      selected = list("LessThanHighSchool","College"))),
                 conditionalPanel(
                   condition = "input.dataDem == 'Income'",
                   checkboxGroupInput(inputId = "facetInc", 
                                      label = "Choose which income levels you would like to compare:",
                                      choices = list("Low-Income", 
                                                     "High-Income"),
                                      selected = list("Low-Income","High-Income"))),
                 
                 # Break between two plots 
                 h6("--------------------------"),
                 
                 # Title for second plot
                 h4("Food Consumed Over Time"),
                 
                 # Fourth Widget - Asks user to select duraction 
                 sliderInput(inputId = "range",
                             label = "Choose the range in years that you would like to plot:",
                             min = floor(min(perc_tidy$Year)), max = ceiling(max(perc_tidy$Year)),
                             value = c(1970, 2010),
                             sep = "", 
                             round = T),
                 
                 # Fifth Widget - Asks user to type in food categories for plot 2
                 textInput(inputId = "names",
                           label = "Enter the food categories you would like to compare:",
                           value = "Dairy"),
                 
                 # Specifies how user should enter text
                 h6("Options include: Dairy, Fruit, Vegetable, FlourCereal, MeatEggNut, AddedFats, and CaloricSweeteners. Be sure to capitalize and insert a single space between foods. Commas are not accepted."),
                 
                 # Break
                 br(),
                 
                 # Data Source
                 helpText("Data Source: United States Department of Agriculture (USDA)")
    ),
    
    # Put output graphic in main panel
    mainPanel(width = 8,
              
              # Output: Tabset w/ plot, summary, and table ----
              tabsetPanel(type = "tabs",
                          tabPanel("Graphs", plotlyOutput("plot1"), plotlyOutput("plot2")),
                          tabPanel("Plot One Data", tableOutput("table1")), 
                          tabPanel("Plot Two Data", tableOutput("table2")))
    )
  )
  )

##### Server Side #####
server <- function(input, output){
  
  # Reactive table for plot 1
  finalDataByYear <- reactive({
    if(input$dataDem == "Education"){
      all_data[[input$dataDem]] %>%
        filter(Year %in% input$Year) %>%
        filter(Facet %in% input$facetEduc)
    }else if(input$dataDem == "Income"){
      all_data[[input$dataDem]] %>%
        filter(Year %in% input$Year) %>%
        filter(Facet %in% input$facetInc)
    }else if(input$dataDem == "Race"){
      all_data[[input$dataDem]] %>%
        filter(Year %in% input$Year) %>%
        filter(Facet %in% input$facetRace)
    }else if(input$dataDem == "Gender"){
      all_data[[input$dataDem]] %>%
        filter(Year %in% input$Year) %>%
        filter(Facet %in% input$facetGen)
    } 
    
  })
  
  # Outputs table 2 - Food Consumption Based on Demographics
  output$plot1 <- renderPlotly({
    req(input$facetGen, input$facetRace, input$facetEduc, input$facetInc, input$Year)
    ggplot(data = finalDataByYear(), 
           mapping = aes_string(x = "Food", y = "Amount")) + 
      geom_bar(stat = "identity", aes(fill = Facet)) +
      facet_grid(~Facet) + 
      theme(panel.background = element_rect(fill = "#fff68f", size = 3),
            text = element_text(family = "Palatino", color = "#666547"),
            legend.position = "none",
            axis.text.x = element_blank()) +
      labs(title = paste("Food Consumed by", input$dataDem, input$Year)) +
      ylab("Average Pounds per Person") + xlab(NULL) + 
      annotate("text", x = "Milk_2perc", y = 80, label = "Scroll over bar for more info", col = "#474747", size = 3) +
      annotate("text", x = "Milk_2perc", y = 75, label = "Food is ordered alphabetically", col = "#474747", size = 3)
  })  
  
  # Reactive table for plot 2
  finalPerc <- reactive({
    req(input$names)
    perc_tidy %>%
      filter(Year >= input$range[1], Year <= input$range[2]) %>%
      filter(Food %in% c(unlist(str_split(input$names, " ")))) 
  })
  
  # Outputs table 2 - Percentage of Food Consumed Over Time 
  output$plot2 <- renderPlotly({
    ggplot(data = finalPerc(), 
           mapping = aes_string(x = "Year", y = "Percent")) +
      geom_line(aes(color = Food)) +
      theme(panel.background = element_rect(fill = "#e6e6fa", size = 3),
            text = element_text(family = "Palatino", color = "#666547"),
            legend.position = "none") +
      ggtitle("Food Consumed Over Time") +
      ylab("Percentage") +
      xlab("Year")
  })
  
  # Outputs datatable 1
  output$table1 <- renderTable({
    finalDataByYear() 
  })
  
  # Outputs datatable 2
  output$table2 <- renderTable({
    finalPerc()
  })
  
}

shinyApp(ui=ui, server=server) 