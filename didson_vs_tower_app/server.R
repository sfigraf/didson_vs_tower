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

# DIDSON read-ins -----------------------------------------------------------

  
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
  
  #could honestly put this reactive in 
  didson_prepped <- reactive({
    validate(
      need(!is.null(didson_raw_data() ), "Please upload a data set")
    )
    didson_list <- list("daily" = didson_function(didson_raw_data())$daily, "hourly" = didson_function(didson_raw_data())$hourly )
    return(didson_list)
  })
  
  didson_filtered <-reactive({
    filtered_daily <- didson_prepped()$daily %>%
      filter(
        Date1 >= input$didson_drangeinput1[1] & Date1 <= input$didson_drangeinput1[2],
      )
    
    filtered_hourly <- didson_prepped()$hourly %>%
      filter(
        Date >= input$didson_drangeinput1[1] & Date <= input$didson_drangeinput1[2],
        hour(date_time) >= input$didson_slider1[1] & hour(date_time) <= input$didson_slider1[2],
      )
    
    didson_filtered_list <- list("didson_filtered_daily" = filtered_daily, "didson_filtered_hourly" =  filtered_hourly)
    return(didson_filtered_list)
  })  
  
  output$didson_dailyplot1 <- renderPlot({
    didson_filtered()$didson_filtered_daily %>%
      ggplot(aes(x = Date1, y = daily_passage)) +
      geom_line() +
      theme_classic() +
      labs(title = "Daily Total DIDSON Passage Counts")
    
    
  })
  
  output$didson_hourlyplot1 <- renderPlot({

    didson_filtered()$didson_filtered_hourly %>%
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
  
  tower_prepped <- reactive({
    validate(
      need(!is.null(tower_raw_data() ), "Please upload a data set")
    )
    tower_list <- list("daily" = tower_function(tower_raw_data())$daily, "hourly" = tower_function(tower_raw_data())$hourly )
    return(tower_list)
  })
  
  # tower_data_list <- list("daily" = tower_function(data)$daily, "hourly" = tower_function(data)$hourly)
  # 
  # return(tower_data_list)#tower reactive
  
  
  tower_filtered <- reactive({
    filtered_daily <- tower_prepped()$daily %>%
      filter(
        Date1 >= input$tower_drangeinput1[1] & Date1 <= input$tower_drangeinput1[2],
      )
    
    filtered_hourly <- tower_prepped()$hourly %>%
      filter(
        date2 >= input$tower_drangeinput1[1] & date2 <= input$tower_drangeinput1[2],
        hour(date_time) >= input$tower_slider1[1] & hour(date_time) <= input$tower_slider1[2],
      )
    
    tower_filtered_list <- list("tower_filtered_daily" = filtered_daily, "tower_filtered_hourly" =  filtered_hourly)
    return(tower_filtered_list)
    
   
  
  })
  
  
  
  output$tower_dailyplot1 <- renderPlot({
    tower_filtered()$tower_filtered_daily %>%
      ggplot(aes(x = Date1, y = daily_passage)) +
      geom_line() +
      theme_classic() +
      labs(title = "Daily Total tower Passage Counts",caption = "Both banks sum" )
    
    
  })
  
  output$tower_hourlyplot1 <- renderPlot({
    
    tower_filtered()$tower_filtered_hourly %>%
      ggplot(aes(x = date_time, y = Count, color = Bank)) +
      geom_line() +
      theme_classic() +
      labs(title = "Tower Hourly Total Passage")
  })
  

})
