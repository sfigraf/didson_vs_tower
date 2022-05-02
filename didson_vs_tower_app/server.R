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
    
  })
  
  output$didson_hourlyplot1 <- renderPlot({

    #didson_tower_hourly_long <- pivot_longer(data = didson_prepped()$hourly, cols = !c(date_time, Date, Hour), names_to = "type", values_to = "passage")
    didson_prepped()$hourly %>%
      ggplot(aes(x = date_time, y = didson_total_passage)) +
        geom_line() +
        theme_classic() +
        labs(title = "DIDSON Hourly Passage")
  })

})
