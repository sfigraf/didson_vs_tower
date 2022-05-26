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

# UI Updates --------------------------------------------------------------

observeEvent(input$didsoninput1,{
  updatePickerInput(session, "didson_picker1",
                   selected = NULL, 
                   choices = didson_sheets_name()
  )
})  
  
  observeEvent(input$towerinput1,{
    updatePickerInput(session, "tower_picker1",
                      selected = NULL, 
                      choices = tower_sheets_name()
    )
  }) 

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
        (input$didson_picker1 %in% didson_sheets_name())) {
      data <- read_excel(input$didsoninput1$datapath, 
                         sheet = input$didson_picker1,
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
  
  output$didson_dailyplot1 <- renderPlotly({
    didson_filtered()$didson_filtered_daily %>%
      ggplot(aes(x = Date1, y = daily_passage)) +
      geom_line() +
      theme_classic() +
      labs(title = "Daily Total DIDSON Passage Counts")
    
    
  })
  
  output$didson_hourlyplot1 <- renderPlotly({

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
        (input$tower_picker1 %in% tower_sheets_name())) {
      data <- read_excel(input$towerinput1$datapath, 
                         sheet = input$tower_picker1,
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
    tower_list <- list("daily" = tower_function(tower_raw_data())$daily, "hourly" = tower_function(tower_raw_data())$hourly, "hourly_condensed" = tower_function(tower_raw_data())$hourly_condensed  )
    return(tower_list)
  })
  
  
  
  
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
  
  
  
  output$tower_dailyplot1 <- renderPlotly({
    tower_filtered()$tower_filtered_daily %>%
      ggplot(aes(x = Date1, y = daily_passage)) +
      geom_line() +
      theme_classic() +
      labs(title = "Daily Total tower Passage Counts",caption = "Both banks sum" )
    
    
  })
  
  output$tower_hourlyplot1 <- renderPlotly({
    
    tower_filtered()$tower_filtered_hourly %>%
      ggplot(aes(x = date_time, y = Count, color = Bank)) +
      geom_line() +
      theme_classic() +
      labs(title = "Tower Hourly Total Passage")
  })
  

# Comparison Logic --------------------------------------------------------

  tower_didson_prepped <- reactive({
    validate(
      need(!is.null(tower_raw_data()) & !is.null(didson_raw_data() ), "Please upload both data sets")
    )
    #daily
    didson_tower_daily <- bind_rows(tower_prepped()$daily, didson_prepped()$daily )
    #hourly stuff
    didson_tower_hourly <- left_join(tower_prepped()$hourly_condensed, didson_prepped()$hourly, by = "date_time")
    didson_tower_hourly_long <- pivot_longer(data = didson_tower_hourly, cols = !c(date_time, Date, Hour), names_to = "type", values_to = "passage")
    
    
    joined_list <- list("daily" = didson_tower_daily,
                        "hourly_wide" = didson_tower_hourly,
                        "hourly_long" = didson_tower_hourly_long 
                        )
    return(joined_list)
  })
  
  didson_tower_filtered <- reactive({
    filtered_daily <- tower_didson_prepped()$daily %>%
      filter(
        Date1 >= input$didson_tower_drangeinput1[1] & Date1 <= input$didson_tower_drangeinput1[2],
      )
    
    filtered_hourly_long <- tower_didson_prepped()$hourly_long %>%
      filter(
        Date >= input$didson_tower_drangeinput1[1] & Date <= input$didson_tower_drangeinput1[2],
        hour(date_time) >= input$didson_tower_slider1[1] & hour(date_time) <= input$didson_tower_slider1[2],
      )
    
    filtered_hourly_wide <- tower_didson_prepped()$hourly_wide %>%
      filter(
        Date >= input$didson_tower_drangeinput1[1] & Date <= input$didson_tower_drangeinput1[2],
        hour(date_time) >= input$didson_tower_slider1[1] & hour(date_time) <= input$didson_tower_slider1[2],
      )
    
    tower_filtered_list <- list("daily" = filtered_daily, "hourly_long" =  filtered_hourly_long,
                                "hourly_wide" = filtered_hourly_wide)
    return(tower_filtered_list)
    
  })

# Compare Plots -------------------------------------------------------

  
  output$didson_tower_dailyplot1 <- renderPlotly({
    didson_tower_filtered()$daily %>%
      ggplot(aes(x = Date2, y = daily_passage, color = Type)) +
      geom_line() +
      theme_classic() +
      labs(title = "Didson vs Tower Daily Escapement Compare", caption = "Tower counts are from left bank only. Data not collected on tower between 0-4 AM")
    
    
  })
  
  output$didson_tower_hourlyplot1 <- renderPlotly({

    didson_tower_filtered()$hourly_long %>%
      ggplot(aes(x = date_time, y = passage, color = type)) +
      geom_line() +
      theme_classic() +
      labs(title = "Hourly Comparison Tower and DIDSON")
  })
  
  output$didson_tower_hourlyplot2 <- renderPlotly({
    
    formula1 <- y ~ x
    rsq1 <- rsq(didson_tower_filtered()$hourly_wide$didson_total_passage, didson_tower_filtered()$hourly_wide$tower_total_count)
    
    plot <- didson_tower_filtered()$hourly_wide %>%
      ggplot(aes(x = didson_total_passage, y = tower_total_count)) +
      geom_smooth(method = "lm", se=FALSE, color="black", formula = formula1) +
      geom_point() +
      theme_classic() +
      labs(title = "DIDSON vs Tower", caption = paste(round(rsq1,2)))
    
    plot1 <- ggplotly(plot)
    plot2 <- plot1 %>%
      add_annotations(
        #positioning on the graph
        x = max(plot1$x$layout$xaxis$range)*.1,
        y = max(plot1$x$layout$yaxis$range)*.9,
        text = paste("R^2 =",round(rsq1,2)),
        showarrow = F
      ) %>% 
      #this ensures the TeX is read/displayed how I want it
      config(mathjax = 'cdn')
    #layout(annotations = rsq1)
    plot2
  })
  
  

})
