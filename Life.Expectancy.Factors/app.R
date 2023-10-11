#### Academic Honesty ####
#### Making it nicer ####


# The packages required
library(shiny)
library(shinythemes)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(DT)
library(scales)
library(shinyjs)
library(maps)
library(reshape2)
library(RColorBrewer)
library(ggiraph)
library(graphics)
library(magick)
library(grid)
library(mosaic)
library(gapminder)
library(gganimate)
library(gifski)


# The UI for application
ui <- fluidPage(# Application title    
                titlePanel("Health on the Atlas Explorer: Dive into World Life Expectancy"),
                
                # Sidebar for the user inputs 
                sidebarLayout(
                  sidebarPanel(
                    style = "height: 85vh; overflow-y: auto;",
                    # Input for years -->
                    sliderInput(inputId = "userYears", label = "Years", sep = "",
                                min = 2000, max = 2015, value = c(2000, 2015)), hr(), br(),
                    
                    hr(), br(),
                    
                    # Option for filtering countries by continent -->
                    checkboxGroupInput(inputId = "filter1", label = "Filters by Continents", 
                                       choices = list("Asia" = "Asia", "Africa" = "Africa", 
                                                      "Europe" = "Europe", 
                                                      "North America" = "North America", 
                                                      "Oceania" = "Oceania", 
                                                      "South America" = "South America")),
                    br(),
                    
                    # Option for filtering countries by development group -->
                    checkboxGroupInput(inputId = "filter2", label = "Filters by Develpment Status", 
                                       choices = list("Less Developed" = "Least.Developed",
                                                      "More Developed" = "Developing.and.Developed"),
                                       selected = c("Least.Developed", "Developing.and.Developed")),
                    br(),
                    
                    # Input for the country selected -->
                    selectInput(inputId = "userCountry", label = "Select the country", 
                                choices = as.factor(append(data$Country, "All", 0)), selected = "All"), 
                    hr(), br(),
                    
                    # Choosing the comparison against Life Expectancy -->       
                    radioButtons(inputId = "compareTo", label = " Life Expectancy vs ___________", 
                                 choices = list("Population of the country" = "Population", 
                                                "GDP per Capita" = "GDP.per.capita", 
                                                "Beer Consumption per Capita" = "Beer.consumption", 
                                                "Unemployment (% of Labor Force)" = "Unemployment", 
                                                "Inflation (Annual %)" = "Inflation", 
                                                "Expenditure on Health (% of GDP)" = "Health.expenditure", 
                                                "Expenditure on Military (% of GDP)" = "Military.expenditure", 
                                                "CO2 Emission (Metric tons per capita)" = "CO2.emissions", 
                                                "Forest Area (% of land area)" = "Forest.area", 
                                                "People practicing open defecation (% of population)" = "Open.defecation", 
                                                "People using at least basic drinking water (% of population)" = "Potable.water",
                                                "Obesity (% of population)" = "Obesity",
                                                "All" = "All")), 
                    hr(), 
                    
                    # Option to add the Line of Best Fit and labels for years in the graph -->
                    checkboxInput("smooth", label = "Add smoother", value = FALSE),
                    checkboxInput("labelYear", label = "Add labels for years", value = FALSE),
                    hr(), br()), 
                  
                  # Writing the UI for the Main Panel to display the graph 
                  mainPanel(
                    tabsetPanel(id = "tabs",
                                tabPanel("Summary Table", dataTableOutput("countryTable"), value = "table"),
                                tabPanel("Visuals", plotOutput("visuals", height = "650px")),
                                tabPanel("Over the Years", imageOutput("animatedOverYears", height = "650px"), 
                                         value = "animatedOverYears"),
                    )
                  )
                )
)


# The Server for application
server <- function(input, output, session) {
  
  
  ## 0a. Handling the reactive data -->
  check = 1
  
  activeData <- reactive({
    
    currentData <- filter(data, Year >= input$userYears[1] & Year <= input$userYears[2])
    # Filtering the years not wanted by the user
    
    if(!is.null(input$filter1)){
      currentData <- filter(currentData, Continent %in% input$filter1)
    }
    # Filtering countries by continents
    
    if(!is.null(input$filter2) & input$filter2[1] == "Developing.and.Developed"){
      currentData <- filter(currentData, Least.Developed == 0)
    }
    else if(!is.null(input$filter2) & input$filter2[1] == "Least.Developed" & length(input$filter2) == 1){
      currentData <- filter(currentData, Least.Developed == 1)
    }
    
    # Filtering countries by development groups
    
    return(currentData)
    
  })
  
  # Updating the data based on the filters 
  observe({
    
    if(!is.null(input$userCountry)){
      updateSelectInput(session, "userCountry", label = "Select the Country", 
                        choices = as.factor(append(activeData()$Country, "All", 0)), 
                        selected = input$userCountry)
    }
    else{
      updateSelectInput(session, "userCountry", label = "Countries", 
                        choices = as.factor(activeData()$Country))
    }
  })
  
  
  ## 0b. Updating Radio choices based on the tab selected -->
  
  filteredRadioChoices <- reactive({
    
    if (input$tabs == "animatedOverYears") {
      choices = list("GDP per Capita" = "GDP.per.capita",
                     "Beer Consumption per Capita" = "Beer.consumption",
                     "Unemployment (% of Labor Force)" = "Unemployment",
                     "Inflation (Annual %)" = "Inflation",
                     "Expenditure on Health (% of GDP)" = "Health.expenditure",
                     "Expenditure on Military (% of GDP)" = "Military.expenditure",
                     "CO2 Emission (Metric tons per capita)" = "CO2.emissions",
                     "Forest Area (% of land area)" = "Forest.area",
                     "People practicing open defecation (% of population)" = "Open.defecation",
                     "People using at least basic drinking water (% of population)" = "Potable.water",
                     "Obesity (% of population)" = "Obesity")
    } 
    else {
      # If any other tab is selected, include the "All" option
      choices = list("Population of the country" = "Population",
                     "GDP per Capita" = "GDP.per.capita",
                     "Beer Consumption per Capita" = "Beer.consumption",
                     "Unemployment (% of Labor Force)" = "Unemployment",
                     "Inflation (Annual %)" = "Inflation",
                     "Expenditure on Health (% of GDP)" = "Health.expenditure",
                     "Expenditure on Military (% of GDP)" = "Military.expenditure",
                     "CO2 Emission (Metric tons per capita)" = "CO2.emissions",
                     "Forest Area (% of land area)" = "Forest.area",
                     "People practicing open defecation (% of population)" = "Open.defecation",
                     "People using at least basic drinking water (% of population)" = "Potable.water",
                     "Obesity (% of population)" = "Obesity",
                     "All" = "All"
      )
    }
    return(choices)
  })
  
  # Update the radioButtons choices based on the selected tab
  observe({
    choices <- filteredRadioChoices()
    updateRadioButtons(session, "compareTo", choices = choices)
  })
  
  
  ## 1. Generating the table of data for the requested country -->
  output$countryTable <- renderDataTable({ 
    
    if(!is.null(input$userCountry) & input$userCountry %in% data$Country){
      currentData <- filter(activeData(), Country == input$userCountry)
    }
    else{
      currentData <- activeData()
    }
    # Selecting the country of user's choice
    
    if(input$compareTo == "All"){
      dataCountry <- currentData %>% 
        select(-Country.code, -Least.Developed, -Developing.and.Developed)
    }
    else {
      dataCountry <- currentData %>% 
        select(Country, Continent, Year, Life.Expectancy, input$compareTo)
    }
    datatable(dataCountry)
  })
  
  
  ## 2. Generating the Visuals -->
  
  output$visuals <- renderPlot({
    
    Plot <- generatePlot(activeData(), 2)
    # The function that generates the graph basd on the user inputs and the data provided to it
    
    if(input$smooth){
      print(Plot + geom_smooth(color = "darkgray", alpha = 0.3))
    }
    else{
      print(Plot)
    }
    # Adding the smooth to the graph, based on the user's choice
    
  })
  
  # FUNCTION generatePlot()
  
  generatePlot <- function(visualData, size1){
    
    if(!is.null(input$userCountry) & input$userCountry == "All"){
      visualData <- group_by(visualData, Year) %>% 
        summarize(across(everything(), list(mean), .names = "{.col}", na.rm = TRUE))
    }
    else if(!is.null(input$userCountry)){
      visualData <- filter(visualData, Country == input$userCountry)
    }
    # Checks is there's a country selected by the user or not
    
    
    # 2 different graph options --> All/Continents/Countries --- All variables || One variable
    
    # The path for one variable
    
    if(input$compareTo != "All") {
      Plot <- ggplot(visualData, 
                     aes_string(x = input$compareTo, y = "Life.Expectancy", colour = as.factor(visualData$Year))) +
        geom_point(size = size1) +
        scale_x_continuous(labels = label_comma(),  # To have commas and not scientfic notation for labels
                           limits = c(min(visualData[,input$compareTo], na.rm = TRUE), 
                                      max(visualData[,input$compareTo], na.rm = TRUE))) + 
        labs(color = "Year") + # For better vision only. Doesn't add any other information to the graph
        theme_linedraw() + # Choosing a theme for the graph display
        theme(legend.position = "bottom", 
              legend.key.size = unit(1.5, "cm"), 
              legend.text = element_text(size = (5 * size1)), 
              legend.title = element_text(size = (5*size1)), 
              axis.title = element_text(size = (7 * size1)),
              axis.text = element_text(size = (12))) # Adding customizations to the theme
      
      if(input$labelYear){
        Plot <- Plot + geom_label(aes(label = visualData$Year), nudge_y = 0.2) 
        # Adding the year on the points in the scatterplot
      }
    }
    else{
      temp <- visualData %>% mutate_at(c("Population", "GDP.per.capita", "Beer.consumption", 
                                         "Unemployment", "Inflation", "Health.expenditure", 
                                         "Military.expenditure", "CO2.emissions", "Forest.area", 
                                         "Open.defecation", "Potable.water", "Obesity"),
                                       ~(scale(.) %>% as.vector)) %>%
        # SCALING to STANDARDIZE all the variables for uniformity along the x-axis

                pivot_longer(cols = c("Population", "GDP.per.capita", "Beer.consumption", 
                                      "Unemployment", "Inflation", "Health.expenditure", 
                                      "Military.expenditure", "CO2.emissions", "Forest.area", 
                                      "Open.defecation", "Potable.water", "Obesity"), 
                             names_to = "Variable", 
                             values_to = "Scaled.Value")
      
        # Converting to a long format data
      
      Plot <- ggplot(temp, aes(x = Scaled.Value, y = Life.Expectancy, colour = as.factor(Year))) +
        geom_point() +
        scale_x_continuous(labels = label_comma()) + # To have commas and not scientfic notation for labels
        labs(color = "Year") + # For better vision only. Doesn't add any other information to the graph
        theme_linedraw() + # Choosing a theme for the graph display
        theme(legend.position = "bottom", 
              legend.key.size = unit(1.5, "cm"), 
              legend.text = element_text(size = (5*size1)), 
              legend.title = element_text(size = (5*size1)), 
              axis.title = element_text(size = (7*size1)),
              axis.text = element_text(size = (12))) + # Adding customizations to the theme
        facet_wrap(~Variable) # Adding all the variables for comparison to the graph
      
      if(input$labelYear){
        Plot <- Plot + geom_label(aes(label = temp$Year), nudge_y = 0.2) 
        # Adding the year on the points in the scatterplot
      }
      
    }
    
    return(Plot)    
  }
  
  
  ## 3. Animated Plot -->
  
  output$animatedOverYears <- renderImage({
    # Generating the plot based on the user's input
    
    if(!is.null(input$userCountry) & input$userCountry %in% data$Country){
      currentData <- filter(activeData(), Country == input$userCountry)
    }
    else{
      currentData <- activeData()
    }
    
    Plot <- ggplot(currentData, aes_string(x = input$compareTo, y = "Life.Expectancy", 
                                            size = "Population", colour = "Country")) +
      geom_point(alpha = 0.6, show.legend = FALSE) +
      scale_colour_manual(values = country_colors) + # Using the vector country_colors in package gapminder
      scale_size(range = c(2, 12)) +
      scale_x_log10() + # Logging X to standardize the variation of the X values
      facet_wrap(~Continent) + # Divide the graph by continents to micromanage the trends visible
      labs(title = 'Year: {frame_time}', y = 'Life Expectancy') + 
      gganimate::transition_time(as.integer(Year)) +
      ease_aes('linear')
    
    # Converting the animated plot to a gif
    
    animation <- animate(Plot)
    
    # Defining a temporary file path
    gif_path <- tempfile(fileext = ".gif")
    # Saving the animation to the temporary file
    anim_save(gif_path, animation)
    # Returning this saved gif in the temporary filepath
    list(src = gif_path, contentType = 'image/gif')}, deleteFile = TRUE)
  
}

# Run the application 
shinyApp(ui = ui, server = server)
