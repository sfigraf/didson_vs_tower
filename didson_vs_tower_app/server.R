#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

# Data read-ins -----------------------------------------------------------

  
  didson_sheets_name <- reactive({
    if (!is.null(input$didsoninput1)) {
      return(excel_sheets(path = input$didsoninput1$datapath))  
    } else {
      return(NULL)
    }
  })
  
  didson_raw_data <- reactive({
    if (!is.null(input$didsoninput1) && 
        (input$didson_sheet1 %in% didson_sheets_name())) {
      data <- read_excel(input$didsoninput1$datapath, 
                         sheet = input$didson_sheet1,
                         col_types = c("numeric", 
                                       "date", "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "text", "text", "numeric", "numeric", 
                                       "numeric") 
                         
                         )
      
      return(data)
    } else {
      return(NULL)
    }
  })

  #didson reactive
  
  didson_prepped <- reactive({
    validate(
      need(!is.null(didson_raw_data() ), "Please upload a data set")
    )
    didson_function(didson_raw_data() )
  })
  
  output$didson_dailyplot1 <- renderPlot({
    didson_prepped()$daily %>%
      ggplot(aes(x = Date1, y = daily_passage)) +
      geom_line() +
      theme_classic() +
      labs(title = "Daily Total DIDSON Passage Counts")
    
    
  })
  
  output$didson_hourlyplot1 <- renderPlot({

    didson_prepped()$hourly %>%
      ggplot(aes(x = date_time, y = didson_total_passage)) +
        geom_line() +
        theme_classic() +
        labs(title = "DIDSON Hourly Total Passage")
  })
  

# Tower upload logic ------------------------------------------------------
  tower_sheets_name <- reactive({
    if (!is.null(input$towerinput1)) {
      return(excel_sheets(path = input$towerinput1$datapath))  
    } else {
      return(NULL)
    }
  })
  
  tower_raw_data <- reactive({
    if (!is.null(input$towerinput1) && 
        (input$tower_sheet1 %in% tower_sheets_name())) {
      data <- read_excel(input$towerinput1$datapath, 
                         sheet = input$tower_sheet1,
                         col_types = c("numeric", "text", "date", 
                                       "numeric", "numeric", "numeric")
                         
      )
      
      return(data)
    } else {
      return(NULL)
    }
  })
  
  #tower reactive
  
  tower_prepped <- reactive({
    validate(
      need(!is.null(tower_raw_data() ), "Please upload a data set")
    )
    tower_function(tower_raw_data() )
  })
  
  output$tower_dailyplot1 <- renderPlot({
    tower_prepped()$daily %>%
      ggplot(aes(x = Date1, y = daily_passage)) +
      geom_line() +
      theme_classic() +
      labs(title = "Daily Total tower Passage Counts",caption = "Both banks sum" )
    
    
  })
  
  output$tower_hourlyplot1 <- renderPlot({
    
    tower_prepped()$hourly %>%
      ggplot(aes(x = date_time, y = Count, color = Bank)) +
      geom_line() +
      theme_classic() +
      labs(title = "Tower Hourly Total Passage")
  })
  

})
