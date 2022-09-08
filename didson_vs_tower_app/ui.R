## To do:
#"chage comparisons daterange input to slider"
#play around with bar width on paired plot
#graph hour on that x axis instread of date_time?
#update UI so date range changes based on file inputs
# cut down stuff that is necessary in app vs uneecessary
# format data from other years to be like 2021 so it's compatible with app
# red highlight outliers: try  and use modeling basics chapter https://r4ds.had.co.nz/model-basics.html
#plot proxy for outliers and not redrawing plot when filters change?
# update slider input when datatable is able to render 


library(shiny)
library(tidyverse)
library(lubridate)
library(plotly)
library(ggpmisc) #for displaying rr on the scatterplots
library(readxl)
library(shinycssloaders)
library(shinyWidgets) # for pickerinput
library(shinythemes)

source("functions/didson_wrangle.R")
source("functions/tower_wrangle.R")
source("functions/rsq_function.R")
#rsq <- function(x, y) summary(lm(y~x))$r.squared



# Define UI for application that draws a histogram
shinyUI(fluidPage(
  theme = shinytheme("united"),
  titlePanel("DIDSON and Tower Data"),
  hr(),
  fluidRow(column(width = 2, offset = 0,
                  fileInput("didsoninput1", "Input DIDSON",accept = c(".xlsx")),
                  pickerInput(
                    inputId = "didson_picker1",
                    label = "Select Sheet:",
                    choices = NULL,
                    selected = NULL,
                    multiple = FALSE,
                    options = pickerOptions(container = "body") #makes it so picker dropdown isn't behind navbar
                  ), #end of pickerINput
                  
                  ),
           column(width = 2,
                  fileInput("towerinput1", "Input Tower Data",accept = c(".xlsx")),
                  pickerInput(
                    inputId = "tower_picker1",
                    label = "Select Sheet:",
                    choices = NULL,
                    selected = NULL,
                    multiple = FALSE,
                    options = pickerOptions(container = "body")
                  ), #end of pickerINput
                  offset = 0)
           ), #end of fluidROw
  

# DIDSON UI ---------------------------------------------------------------

  
  navbarPage(title = "Data",
    tabPanel("DIDSON Sonar Data",
             
             sidebarLayout(
               sidebarPanel(
                 sliderInput("didson_slider2", "Date",
                             min = as.Date("2020-08-01"), 
                             max = Sys.Date(),  
                             value = c(as.Date("2021-04-15"), as.Date("2021-09-01")),
                             step = 1,
                             timeFormat = "%d %b %y",
                             #animate = animationOptions(interval = 500, loop = FALSE)
                 ),
                 # dateRangeInput("didson_drangeinput1", "Select a Date Range:",
                 #                start = "2020-08-01", 
                 #                end = Sys.Date()
                 #                
                 #                ),#end of date range input
                 
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
    

# Tower UI ----------------------------------------------------------------

    
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

# Comparisons Tab Panel ---------------------------------------------------

            
    tabPanel("Comparisons",
             sidebarLayout(
               sidebarPanel(
                 sliderInput("didson_tower_slider2", "Date",
                             min = as.Date("2020-08-01"), 
                             max = Sys.Date(),  
                             value = c(as.Date("2021-04-15"), as.Date("2021-09-01")),
                             step = 1,
                             timeFormat = "%d %b %y",
                             #animate = animationOptions(interval = 500, loop = FALSE)
                 ),
                 dateRangeInput("didson_tower_drangeinput1", "Select a Date Range:",
                                start = "2020-07-25", 
                                end = Sys.Date()
                                
                 ),#end of date range input
                 
                 sliderInput("didson_tower_slider1", "Hour of Day",
                             min = 0,
                             max = 23,  
                             value = c(0,23),
                             step = 1,
                             #timeFormat = "%T",
                             #animate = animationOptions(interval = 500, loop = FALSE)
                 ),
               ), #end of sidebar panel
               mainPanel(
                 tabsetPanel(
                   tabPanel("Daily Data",
                            withSpinner(plotlyOutput("didson_tower_dailyplot1")),
                            withSpinner(plotlyOutput("didson_tower_dailyplot2"))
                   ),
                   tabPanel("Hourly",
                            withSpinner(plotlyOutput("didson_tower_hourlyplot1")),
                            withSpinner(plotlyOutput("didson_tower_hourlyplot2"))
                   ),
                   tabPanel("Paired Comparisons",
                            withSpinner(plotlyOutput("didson_tower_pairedplot1")),
                            withSpinner(plotlyOutput("didson_tower_pairedplot2"))
                   ),
                 ), #end of tabset panel
               ),#end of mainpanel
             ),# end of sidebar layout
             
             ), #end of comparisons tabpanel
  ), #end of navbar page

    # Application title
    

    # Sidebar with a slider input for number of bins
    
)) # end of shinyUI and FluidPage
