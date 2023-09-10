library(shiny)
library(tidyverse)
library(shinyjs) #allows one to integrate javascript code.
library(DT)
library(expss)
library(lubridate)
library(RMariaDB)#required by all the functions.
library(methods)
library(dplyr)
library(jsonlite) #suggested due to error messages.
library(logging)  #debugging  #debugging shiny
library(htmlwidgets)

#new library
library(shinycssloaders)
library(plotly)#allow hover states. 

###############
# import data #
###############


##################
#Lynca J. Kaminka#
##################

county_yearly <- readRDS("data/county_yearly.rds")
city_df_final <- readRDS("data/city_df_final.rds")

df_list <- c('county_yearly', 'city_df_final')
df_list_names <- c('View stats by county', 'View stats by cities')

names(df_list) <- df_list_names


#for county_yearly_data
######################



#observations the user can choose to display. 
#objects_choice_values  <- c("County_Case_Counts", "Deaths_Last_2_Weeks", "Total_Positive_Tests")
#objects_choice_names <- c("Case Counts", "Deaths Last Two Weeks", "Total Positive Tests")

objects_choice_values <- c("Total_Case_Counts", "Total_Tests", 
                           "PerCapita_New_Case_Counts", "PerCapita_New_Total_Tests", 
                           "PerCapita_New_Positive_Tests", "PerCapita_Deaths_Last_2_Weeks")

objects_choice_names <- c("Total Case Counts", "Total Tests Performed", 
                          "Per Capita New Case Counts (100000)", "Per Capita New Total Tests(100000)",
                          "Per Capita New Positive Tests (100000)", "Per Capita Deaths (100000)")
names(objects_choice_values) <- objects_choice_names

#which county to pick, they can pick multiple of them.  
county_choices_1 <- unique(county_yearly$County)

#the user will have to choice between displaying values from 2021 and ones from 2022.
time_choices_1 <- unique(county_yearly$ReportYear)


# for city_df_final  
#################


#observations the user can choose to display.  

items_choice_values <- c("AverageDailyRate", "TotalPositiveTests", "TestingRate", "PerCapita_Case_Counts")
items_choice_names <- c( "Average Daily Rate",  "Total Positive Tests", " Testing Rate", "Per Capita Case Counts (10000)")


#the user will be able to pick with County to display.
county_choices_2 <- unique(city_df_final$County)

#and which town(s) among the choice
city_choices <- unique(city_df_final$Town)

names(items_choice_values) <- items_choice_names

time_choices_2 <- unique(city_df_final$ReportYear)


#user_interface.

ui <- fluidPage(
  titlePanel("COVID-19 statistics for Massachussets broken down by County."),
  useShinyjs(), #for the interactivity towards user's input(?) 
  sidebarLayout(
    
    sidebarPanel(                          
      
      selectInput(inputId = "dataset", 
                  label = "Which statistics would you like to see:", 
                  choices = df_list, 
                  selected = NULL), 
      #for the data frame that shows the stats for all the counties. 
      conditionalPanel(
        
        condition = "input.dataset == 'county_yearly'", 
        
        radioButtons (inputId = "timeframe_1"
                      , label = "Choose a period:"
                      , choices = time_choices_1
                      , selected = NULL
                      ), 
        
        selectizeInput(inputId = "id_county"
                       , label = "Identify which counties you want to see represented:"
                       , choices = county_choices_1
                       , selected = NULL
                       , multiple = TRUE),
        
        selectInput(inputId = "linegraph_1"
                    , label = "Choose the data of interest to plot:"
                    , choices = objects_choice_values
                    , selected = NULL)
      ), 
      
      #for the data frame that shows the information per county.
      conditionalPanel(
        
        condition = "input.dataset == 'city_df_final'", 
        
        radioButtons (inputId = "timeframe_2"
                      , label = "Choose a period,"
                      , choices = time_choices_2
                      , selected = NULL
                     ), 
        
        selectizeInput(inputId = "county_id"
                       , label = "Identify which county you want to see represented:"
                       , choices = county_choices_2
                       , selected = NULL
                       , multiple = FALSE),
        
        selectizeInput(inputId = "city_id"
                       , label = "Identify which cities you want to see represented:"
                       ,  choices = NULL 
                      # , selected = NULL
                       , multiple = TRUE),
        
        selectInput(inputId = "linegraph_2"
                    , label = "Choose data of interest to plot:"
                    , choices = items_choice_values
                    , selected = NULL )
        

      )
    ), 

    
    mainPanel(
      #if the user chooses to view the covid stats for all counties, then display the corresponding plot.
      conditionalPanel(
        condition = "input.dataset == 'county_yearly'",
        withSpinner(plotOutput(outputId = "linegraph_county_yearly"))
        
      ), 
      #if the user chooses to view the covid stats by county, then display the corresponding plot.
      conditionalPanel(
        condition = "input.dataset == 'city_df_final'",
        withSpinner(plotOutput(outputId = "linegraph_city_df_final"))
       
        
      )
      
    )
  )
)


server <- function(input, output, session) {
  
  #selecting the right data to display, and making it reactive to the user's input.
  mydata <- reactive(
    {
      if(input$dataset == 'county_yearly'){
        return (filter(county_yearly, ReportYear %in% input$timeframe_1 , County %in% input$id_county))
        
      } else if(input$dataset == 'city_df_final') {
        return(filter(city_df_final, ReportYear %in% input$timeframe_2,  County %in% input$county_id, Town %in% input$city_id))
        
      }
      
    }
    

  )
  
  #code to make the town variable reactive to the county chosen. So that the user is restricted in which towns they can represent. 
  observe({
    x<- filter(city_df_final, ReportYear %in% input$timeframe_2 , County %in% input$county_id) %>% 
        ungroup()%>%
        distinct(Town)%>%
        select(Town)%>% 
        as.vector()  
      
    updateSelectizeInput(session, inputId = "city_id", choices = x)
  })
  




# for the data frame that shows all counties in general. 
  output$linegraph_county_yearly <- renderPlot({
    
    ggplot(data = mydata(), aes_string(x = "date", y =  input$linegraph_1)) +
      
     #County
      geom_line(aes(color = County)) +
      geom_point(aes(color = mydata()$County)) + 
      labs(x = "Months of the year", y = objects_choice_names[objects_choice_values == input$linegraph_1],
           color = "County", 
           title = items_choice_names[items_choice_values == input$linegraph_2], 
           subtitle =  mydata()$ReportYear)

    
  })
  
#for the data frame that depends on the county.  
  output$linegraph_city_df_final <- renderPlot({
     ggplot(data = mydata(), aes(x = date, y = !!sym(input$linegraph_2))) +                        #            
    
      geom_line(aes(color= mydata()$Town)) + 

      geom_point(aes(color= mydata()$Town)) + 
      labs(x = "Months of the year", y = items_choice_names[items_choice_values == input$linegraph_2], 
           color = "Cities/Towns represented:", 
           title = items_choice_names[items_choice_values == input$linegraph_2], 
           subtitle =  mydata()$ReportYear)
   
                      
  })
}





############
# server   #
############


####################
# call to shinyApp #
####################
shinyApp(ui = ui, server = server)