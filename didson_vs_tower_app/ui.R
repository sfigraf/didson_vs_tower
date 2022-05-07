## To do:
# add compare tab functionality, filters
#plot proxy for outliers

library(shiny)
library(tidyverse)
library(lubridate)
library(plotly)
library(readxl)
library(shinycssloaders)
library(shinythemes)

source("functions/didson_wrangle.R")
source("functions/tower_wrangle.R")


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  theme = shinytheme("united"),
  titlePanel("DIDSON and Tower Data"),
  hr(),
  fluidRow(column(width = 2, offset = 0,
                  fileInput("didsoninput1", "Input DIDSON",accept = c(".xlsx")),
                  textInput("didson_sheet1", "Input Sheet Name"),
                  ),
           column(width = 2,
                  fileInput("towerinput1", "Input Tower Data",accept = c(".xlsx")),
                  textInput("tower_sheet1", "Input Sheet Name"),
                  
                  offset = 0)
           ), #end of fluidROw
  
  
  navbarPage(title = "Data",
    tabPanel("DIDSON Sonar Data",
             
             sidebarLayout(
               sidebarPanel(
                 
                 dateRangeInput("didson_drangeinput1", "Select a Date Range:",
                                start = "2020-08-01", 
                                end = Sys.Date()
                                
                                ),#end of date range input
                 
                 sliderInput("didson_slider1", "Hour of Day",
                             min = 0,
                             max = 23,  
                             value = c(0,23),
                             step = 1,
                             #timeFormat = "%T",
                             #animate = animationOptions(interval = 500, loop = FALSE)
                 ),
               ), #end of sidebar panel 
              
               
               # Show a plot of the generated distribution
               mainPanel(
                 tabsetPanel(
                   tabPanel("Daily Data",
                           withSpinner(plotlyOutput("didson_dailyplot1")) 
                            ),
                   tabPanel("Hourly",
                            withSpinner(plotlyOutput("didson_hourlyplot1"))
                            ),
                 ),# end of didson tabset panel
                 
               ) #end of didson mainpanel
             ) #end of didson sidebar layout
             
             ), #end of didson tabpanel
    tabPanel("Tower Count Data",
             sidebarLayout(
               sidebarPanel(
                 dateRangeInput("tower_drangeinput1", "Select a Date Range:",
                                start = "2020-08-01", 
                                end = Sys.Date()
                                
                 ),#end of date range input
                 
                 sliderInput("tower_slider1", "Hour of Day",
                             min = 0,
                             max = 23,  
                             value = c(0,23),
                             step = 1,
                             #timeFormat = "%T",
                             #animate = animationOptions(interval = 500, loop = FALSE)
                 ),
               ),
               
               # Show a plot of the generated distribution
               mainPanel(
                 tabsetPanel(
                   tabPanel("Daily Data",
                            withSpinner(plotlyOutput("tower_dailyplot1")) 
                   ),
                   tabPanel("Hourly",
                            withSpinner(plotlyOutput("tower_hourlyplot1"))
                   ),
                 ),# end of tower tabset panel
                 
               ) #end of tower mainpanel
             ) #end of tower sidebar layout
             
    ), #end of tower tabpanel
            
    tabPanel("Comparisons",), #end of comparisons tabpanel
  ), #end of navbar page

    # Application title
    

    # Sidebar with a slider input for number of bins
    
)) # end of shinyUI and FluidPage
