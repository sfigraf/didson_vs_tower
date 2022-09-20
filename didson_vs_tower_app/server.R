

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
  # update tower didson comparison date
  observeEvent(input$sliderupdate_button1 ,{
    #|| 
    updateSliderInput(session,
                      "didson_tower_slider2",
                      min = as.Date(min(tower_didson_prepped()$paired_long$Date) -1),
                      max = as.Date(max(tower_didson_prepped()$paired_long$Date) +1),
                      value = c(as.Date(min(tower_didson_prepped()$paired_long$Date) -1),
                                as.Date(max(tower_didson_prepped()$paired_long$Date) +1)
                                )
                      ) # end of updatesliderinput
  })
  
  # DIDSON Year/date update
  observeEvent(input$didsonui_year_button1,{
    
    updatePickerInput(session, 
                      "didson_picker2", 
                      choices = sort(unique(didson_raw_data()$Year)))
    
  })
  
  
  observeEvent(input$didson_picker2,{
    
    # updatePickerInput(session, 
    #                   "didson_picker2", 
    #                   choices = unique(didson_raw_data()$Year))
    updateSliderInput(session,
                      "didson_slider2",
                      min = as.Date(min(didson_prepped()$daily$Date1) -1),
                      max = as.Date(max(didson_prepped()$daily$Date1) +1),
                      value = c(as.Date(min(didson_prepped()$daily$Date1) -1),
                                as.Date(max(didson_prepped()$daily$Date1) +1)
                      )
    ) # end of updatesliderinput
  })
  
  # TOWER Year/date update
  observeEvent(input$towerui_year_button1,{
    
    updatePickerInput(session, 
                      "tower_picker2", 
                      choices = sort(unique(tower_raw_data()$Year)))
    
  })
  
  
  observeEvent(input$tower_picker2,{
    
    # updatePickerInput(session, 
    #                   "tower_picker2", 
    #                   choices = unique(tower_raw_data()$Year))
    updateSliderInput(session,
                      "tower_slider2",
                      min = as.Date(min(tower_prepped()$daily$Date1) -1),
                      max = as.Date(max(tower_prepped()$daily$Date1) +1),
                      value = c(as.Date(min(tower_prepped()$daily$Date1) -1),
                                as.Date(max(tower_prepped()$daily$Date1) +1)
                      )
    ) # end of updatesliderinput
  })

# DIDSON read-ins -----------------------------------------------------------

  ## Warning: Error in endsWith: non-character object(s)
  #solved because i was calling input$didsoninput1 for endswith, not input$didsoninput1$name
  didson_sheets_name <- reactive({
    if (!is.null(input$didsoninput1) && (endsWith(input$didsoninput1$name, ".xlsx"))) {
      return(excel_sheets(path = input$didsoninput1$datapath))  
    } else {
      return(NULL)
    }
  })
  
  didson_raw_data <- reactive({
    if (!is.null(input$didsoninput1) && 
        (input$didson_picker1 %in% didson_sheets_name()) &&
      (endsWith(input$didsoninput1$name, ".xlsx"))) {
      data <- read_excel(input$didsoninput1$datapath, 
                         sheet = input$didson_picker1,
                         col_types = c("numeric", 
                                       "date", "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "text", "text", "numeric", "numeric", 
                                       "numeric") 
                         
                         )
      
      return(data)
    } else if (!is.null(input$didsoninput1) &&
               (endsWith(input$didsoninput1$name, ".csv"))) {
      didson_all <- read_csv(input$didsoninput1$datapath)
      # this is readying the csv file to be put into the didson_clean function
      didson_all_2 <- didson_all %>%
        mutate(
          Hour = as.numeric(str_sub(Hour, start = 1, end = 2)),
          Date = dmy(paste(Date,Year)),
          #date_time = dmy_hm(paste(Date,Year,new_hr,Minute))
        )
      return(didson_all_2)
    }
    else {
      return(NULL)
    }
  })

  #didson reactive
  
  #could honestly put this reactive in 
  didson_prepped <- reactive({
    validate(
      need(!is.null(didson_raw_data() ), "Please upload a data set")
    )
    
    year_filtered <- didson_raw_data() %>%
      filter(
        Year == input$didson_picker2
      )
    
    didson_list <- list("daily" = didson_function(year_filtered)$daily, "hourly" = didson_function(year_filtered)$hourly, "paired_didson" =  didson_function(year_filtered)$paired_didson)
    return(didson_list)
  })
  
  didson_filtered <- reactive({
    filtered_daily <- didson_prepped()$daily %>%
      filter(
        Date1 >= input$didson_slider2[1] & Date1 <= input$didson_slider2[2]
        
      )
    
    filtered_hourly <- didson_prepped()$hourly %>%
      filter(
        Date >= input$didson_slider2[1] & Date <= input$didson_slider2[2],
        hour(date_time) >= input$didson_slider1[1] & hour(date_time) <= input$didson_slider1[2],
      )
    
    didson_filtered_list <- list("didson_filtered_daily" = filtered_daily, "didson_filtered_hourly" =  filtered_hourly)
    return(didson_filtered_list)
  })  

# DIDSON Plots ------------------------------------------------------------

  
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
  
  #this plot isn't controlled by filters
  output$didson_hourlyplot2 <- renderPlotly({
    
    didson_3 <- didson_prepped()$paired_didson %>%
      # mutate(date2 = ymd(Date),
      #        date_time = ymd_hm(paste(date2, Hour,Minute))) %>%
      replace_na(list(Passage =0)) %>%
      group_by(hour(date_time)) %>%
      summarise(total_passage = sum(Passage)) %>%
      rename(hour1 = `hour(date_time)`) %>%
      mutate(pct = round(prop.table(total_passage),4)*100)
    
    plot <- didson_3 %>%
      ggplot(aes(x = hour1, y = total_passage, group =1, text =  paste0("Percentage of run: ", pct, "%"))) +
      geom_bar(stat = "identity") +
      theme_classic() +
      labs(title = "Hourly total Counts from DIDSON", subtitle = "Data from all read sonar files", caption = "Not controlled by filters on the sidebar. Data is less on some hours because sometimes files aren't read at the XX:50 minute mark, whereas for other hours they often are.")
    
    ggplotly(plot)
  })
  

# Tower upload logic ------------------------------------------------------
  tower_sheets_name <- reactive({
    if (!is.null(input$towerinput1) &&
      (endsWith(input$towerinput1$name, ".xlsx"))) {
      return(excel_sheets(path = input$towerinput1$datapath))  
    } else {
      return(NULL)
    }
  })
  
  tower_raw_data <- reactive({
    
    
    if (!is.null(input$towerinput1) && 
        (input$tower_picker1 %in% tower_sheets_name()) &&
        (endsWith(input$towerinput1$name, ".xlsx"))
        ) {
      data <- read_excel(input$towerinput1$datapath, 
                         sheet = input$tower_picker1,
                         col_types = c("numeric", "text", "date", 
                                       "numeric", "numeric", "numeric")
                         
      )
      return(data)

    } else if (!is.null(input$towerinput1) &&
               (endsWith(input$towerinput1$name, ".csv"))) {
      tower_all <- read_csv(input$towerinput1$datapath)
      # this is readying the csv file to be put into the tower_clean function
      # tower_all <- towers_2021
      tower_all_2 <- tower_all %>%
        select(Year, Location, Date, Hour,  LBank, RBank, Sky, Wind, Precip, Turbidity) %>%
        mutate(date2 = as.Date(dmy(paste(Date, Year))),
               date_time = dmy_h(paste(Date, Year, Hour)))
      return(tower_all_2)
    }
    
    else {
      return(NULL)
    }
  })
  
  tower_prepped <- reactive({
    validate(
      need(!is.null(tower_raw_data() ), "Please upload a data set")
    )
    
    year_filtered <- tower_raw_data() %>%
      filter(
        Year == input$tower_picker2
      )
    ####THIS Seems unnecesary to me....clean it up!
    tower_list <- list("daily" = tower_function(year_filtered)$daily, "hourly" = tower_function(year_filtered)$hourly, "hourly_condensed" = tower_function(year_filtered)$hourly_condensed, "paired_towers" =  tower_function(year_filtered)$paired_towers )
    return(tower_list)
  })
  
  
  
  
  tower_filtered <- reactive({
    filtered_daily <- tower_prepped()$daily %>%
      filter(
        Date1 >= input$tower_slider2[1] & Date1 <= input$tower_slider2[2],
      )
    
    filtered_hourly <- tower_prepped()$hourly %>%
      filter(
        date2 >= input$tower_slider2[1] & date2 <= input$tower_slider2[2],
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
    # req(c(input$didson_picker2, input$tower_picker2))
    validate(
      need(!is.null(tower_raw_data()) & !is.null(didson_raw_data() ), "Please upload both data sets")
    )
    #daily
    didson_tower_daily <- bind_rows(tower_prepped()$daily, didson_prepped()$daily )
    didson_tower_daily_wide <- didson_tower_daily %>%
      pivot_wider(names_from = Type, values_from = daily_passage) 
    #hourly stuff
    didson_tower_hourly <- left_join(tower_prepped()$hourly_condensed, didson_prepped()$hourly, by = "date_time")
    didson_tower_hourly_long <- pivot_longer(data = didson_tower_hourly, cols = !c(date_time, Date, Hour), names_to = "type", values_to = "passage")
    
    #paired 
    paired <- left_join(tower_prepped()$paired_towers,didson_prepped()$paired_didson, by = c("date_time", "date2"))
    
    paired2 <- paired %>%
      filter(Hour >= 4) %>%
      select(date_time, date2, Passage, LBank, Sky, Wind, Precip, Turbidity) %>%
      rename(DIDSON = Passage,
             Lbank_tower = LBank,
             Date = date2) 
    
    #plot ready 
    paired3_long <- paired2 %>%
      pivot_longer(cols = c(DIDSON, Lbank_tower), names_to = "Type", values_to = "passage") %>%
      filter(!is.na(passage))
    
    joined_list <- list("daily_long" = didson_tower_daily,
                        "daily_wide" = didson_tower_daily_wide,
                        "hourly_wide" = didson_tower_hourly,
                        "hourly_long" = didson_tower_hourly_long,
                        "paired_long" = paired3_long, 
                        "paired_wide" = paired2
                        )
    return(joined_list)
  })
  
  didson_tower_filtered <- reactive({
    filtered_daily_long <- tower_didson_prepped()$daily_long %>%
      filter(
        Date1 >= input$didson_tower_slider2[1] & Date1 <= input$didson_tower_slider2[2],
      )
    
    filtered_daily_wide <- tower_didson_prepped()$daily_wide %>%
      filter(
        Date1 >= input$didson_tower_slider2[1] & Date1 <= input$didson_tower_slider2[2],
      )
    
    filtered_hourly_long <- tower_didson_prepped()$hourly_long %>%
      filter(
        Date >= input$didson_tower_slider2[1] & Date <= input$didson_tower_slider2[2],
        hour(date_time) >= input$didson_tower_slider1[1] & hour(date_time) <= input$didson_tower_slider1[2],
      )
    
    filtered_hourly_wide <- tower_didson_prepped()$hourly_wide %>%
      filter(
        Date >= input$didson_tower_slider2[1] & Date <= input$didson_tower_slider2[2],
        hour(date_time) >= input$didson_tower_slider1[1] & hour(date_time) <= input$didson_tower_slider1[2],
      )
    
    
    ### Paired filter logic
    filtered_paired_wide <- tower_didson_prepped()$paired_wide %>%
      filter(
        Date >= input$didson_tower_slider2[1] & Date <= input$didson_tower_slider2[2],
        hour(date_time) >= input$didson_tower_slider1[1] & hour(date_time) <= input$didson_tower_slider1[2],
      )
    
    filtered_paired_long <- tower_didson_prepped()$paired_long %>%
      filter(
        Date >= input$didson_tower_slider2[1] & Date <= input$didson_tower_slider2[2],
        hour(date_time) >= input$didson_tower_slider1[1] & hour(date_time) <= input$didson_tower_slider1[2],
      )
    
    ## make list to return as object
    tower_filtered_list <- list("daily_long" = filtered_daily_long, 
                                "daily_wide" = filtered_daily_wide,
                                "hourly_long" =  filtered_hourly_long,
                                "hourly_wide" = filtered_hourly_wide,
                                "paired_long" = filtered_paired_long,
                                "paired_wide" = filtered_paired_wide)
    return(tower_filtered_list)
    
  })

# Compare Plots -------------------------------------------------------

  
  output$didson_tower_dailyplot1 <- renderPlotly({
    # req(c(input$didson_picker2, input$tower_picker2))
    didson_tower_filtered()$daily_long %>%
      ggplot(aes(x = Date2, y = daily_passage, color = Type)) +
      geom_line() +
      theme_classic() +
      labs(title = "Didson vs Tower Daily Escapement Compare", caption = "Tower counts are from left bank only. Data not collected on tower between 0-4 AM")
    
    
  })
  output$didson_tower_dailyplot2 <- renderPlotly({
    # req(c(input$didson_picker2, input$tower_picker2))
    
    formula1 <- y ~ x
    rsq1 <- rsq(didson_tower_filtered()$daily_wide$DIDSON, didson_tower_filtered()$daily_wide$Tower)
    
    plot <- didson_tower_filtered()$daily_wide %>%
      ggplot(aes(x = DIDSON, y = Tower, text = Date1)) +
      geom_point() +
      geom_smooth(method = "lm", se=FALSE, color="black", formula = formula1) +
      theme_classic() + 
      labs(title = "DIDSON vs Tower Daily")
    
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
  
  
  output$didson_tower_hourlyplot1 <- renderPlotly({
    # req(c(input$didson_picker2, input$tower_picker2))
    didson_tower_filtered()$hourly_long %>%
      ggplot(aes(x = date_time, y = passage, color = type)) +
      geom_line() +
      theme_classic() +
      labs(title = "Hourly Comparison Tower and DIDSON")
  })
  
  output$didson_tower_hourlyplot2 <- renderPlotly({
    # req(c(input$didson_picker2, input$tower_picker2))
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
  
  #### paired plots
  output$didson_tower_pairedplot1 <- renderPlotly({
    # req(c(input$didson_picker2, input$tower_picker2))
    plot <- didson_tower_filtered()$paired_long %>%
      ggplot(aes(x = date_time, y = passage, fill = Type, text = paste("Sky:", Sky,
                                                                       "Wind:", Wind,
                                                                       "Precip:", Precip,
                                                                       "Turbidity:", Turbidity)
                 )) +
      geom_bar(stat = "identity", 
               position = "dodge"
               #width = 1
               ) +
      theme_classic() +
      labs(title = "Paired counts compare")
    ggplotly(plot)
  })
  
  output$didson_tower_pairedplot2 <- renderPlotly({
    
    formula1 <- y ~ x
    
    rsq1 <- rsq(didson_tower_filtered()$paired_wide$DIDSON, didson_tower_filtered()$paired_wide$Lbank_tower)
    
    plot <- didson_tower_filtered()$paired_wide %>%
      ggplot(aes(x = DIDSON, y = Lbank_tower, 
                 # lm line shows up/works if I just list one column here... not sure why it doesn't work with multiple 
                 # paste("DateTime:", date_time,
                 #       "Sky:", Sky,
                 #       "Wind:", Wind,
                 #       "Precip:", Precip,
                 #       "Turbidity:", Turbidity)
                 text = date_time
                 )) +
      geom_point() +
      geom_smooth(method = "lm", se=FALSE, color="black", formula = formula1) +
      theme_classic() +
      labs(title = "Paired counts compare")
    #turns to plotly object so i can add annotations
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
    plot2
  })

# Weather Upload Logic ----------------------------------------------------

  weather_data <- reactive({
    if (!is.null(input$weatherinput1) && 
        
        (endsWith(input$weatherinput1$name, ".csv"))) {
      data <- read_csv(input$weatherinput1$datapath, 
                         
                         col_types = cols(`Precip (in)` = col_number(), 
                                          `Water Temp (C)` = col_number(), 
                                          `Corrected RM 22` = col_number(), 
                                          `Flow (cfs)` = col_number(), ...10 = col_skip(), 
                                          Turbidity...11 = col_skip())
                         
      )
       #getting date to a format usable for plots
      data2 <- data %>%
        drop_na(Date) %>%
        mutate(Date = mdy(Date))
      
      return(data2)
    } 
    else {
      return(NULL)
    }
  })

# Weather Reactivity ------------------------------------------------------

  

# Weather Plots and Table -----------------------------------------------------------

  output$water_temp_plot <- renderPlotly({
    plot <- weather_data() %>%
      ggplot(aes(x = Date, y = `Water Temp (C)`, group = 1)) +
      geom_line() +
      theme_classic() + 
      labs(title = "River Mile Water Temp")
    ggplotly(plot)
  })
  output$precip_plot <- renderPlotly({
    plot <- weather_data() %>%
      ggplot(aes(x = Date, y = `Precip (in)`)) +
      geom_bar(stat = "identity") +
      theme_classic() + 
      labs(title = "River Mile Precip")
    ggplotly(plot)
  })
  output$air_temp_plot <- renderPlotly({
    plot <- weather_data() %>%
      ggplot(aes(x = Date, y = `Air Temp (F)`, group = 1)) +
      geom_line() +
      theme_classic()+ 
      labs(title = "River Mile Air Temp")
    ggplotly(plot)
  })
  output$water_level_plot <- renderPlotly({
    plot <- weather_data() %>%
      ggplot(aes(x = Date, y = `Water Level (RM22)`, group = 1)) +
      geom_line() +
      theme_classic() + 
      labs(title = "River Mile Water Level")
    ggplotly(plot)
  })
  
  output$weather_datatable <- renderDT({
    #req(input_file())
    datatable(weather_data())
  })
  

})
