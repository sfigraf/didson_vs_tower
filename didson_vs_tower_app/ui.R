

library(shiny)
library(tidyverse)
library(lubridate)
library(plotly)
library(readxl)
library(shinycssloaders)

source("functions/didson_wrangle.R")
source("functions/tower_wrangle.R")


# Define UI for application that draws a histogram
shinyUI(fluidPage(
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
                 sliderInput("bins",
                             "Number of bins:",
                             min = 1,
                             max = 50,
                             value = 30)
               ),
               
               # Show a plot of the generated distribution
               mainPanel(
                 tabsetPanel(
                   tabPanel("Daily Data",
                           withSpinner(plotOutput("didson_dailyplot1")) 
                            ),
                   tabPanel("Hourly",
                            withSpinner(plotOutput("didson_hourlyplot1"))
                            ),
                 ),# end of didson tabset panel
                 
               ) #end of didson mainpanel
             ) #end of didson sidebar layout
             
             ), #end of didson tabpanel
    tabPanel("Tower Count Data",
             sidebarLayout(
               sidebarPanel(
                 sliderInput("bins",
                             "Number of bins:",
                             min = 1,
                             max = 50,
                             value = 30)
               ),
               
               # Show a plot of the generated distribution
               mainPanel(
                 tabsetPanel(
                   tabPanel("Daily Data",
                            withSpinner(plotOutput("tower_dailyplot1")) 
                   ),
                   tabPanel("Hourly",
                            withSpinner(plotOutput("tower_hourlyplot1"))
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
