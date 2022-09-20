### function for wrangling tower data

tower_function <- function(tower_data) {
  
  #if date_time isn't already in the data, make it so it is
  if (!"date_time" %in% colnames(tower_data)) {
    
    towers_2021_2<- tower_data %>%
      mutate(date1 = str_sub(Date, 6, -1),
             date2 = as.Date(ymd(paste(Year, date1))),
             date_time = ymd_h(paste(Year, date1, Hour))) 
    #return(towers_2021_2)
  } else {
    #otherwise just proceed as normal
    towers_2021_2 <- tower_data
    #return(towers_2021_2)
  }
  
  
  ###both banks plot
  toweres_collapsed <- towers_2021_2 %>%
    
    select(date_time, date2, LBank, RBank, Sky, Wind, Precip, Turbidity) %>%
    gather(key = "Bank", value = "Count", -c(date_time, date2,Sky, Wind, Precip, Turbidity)) %>%
    filter(!is.na(Count))
  
  
  #ggplotly(plot)
  
  ###totals for both banks summed
  #both banks total by hour
  all_totals <- toweres_collapsed %>%
    group_by(date_time) %>%
    summarise(tower_total_count = sum(Count))
  
#daily totals
  daily_tower_2021totals <- toweres_collapsed %>%
    group_by(date2) %>%
    summarise(total_count = sum(Count))
  
  
  #making new columns readying for combine and graphing
  
  daily_tower_2021totals1 <- daily_tower_2021totals %>%
    rename(daily_passage = total_count,
           Date1 = date2) %>%
    mutate(Year1 = as.character(year(`Date1`)), #allows variabes to be discrete when graphed instead of continuous
           #day1 = paste0(month(`date(date_time)`), "/", day(`date(date_time)`)),
           Date2 = ymd(paste("0001", month(Date1), day(Date1))),
           Type = "Tower"
    )
  
  ### paired ready
  Lbanktowers_2021_2 <- towers_2021_2 %>%
    select(-RBank) %>%
    mutate(
           # For left bank only, the counts start at 50 minutes for even-numbered hours, and at minute 0 for odd-numbered hours
           Minute = case_when((Hour %% 2) == 0 ~ 50,
                              (Hour %% 2) != 0 ~ 0),
           #appending minute to date_time
           date_time = ymd_hm(paste(date(date_time), Hour, Minute))
    ) #end of mutate 
  
  tower_list <- list("hourly" = toweres_collapsed, "daily" = daily_tower_2021totals1, "hourly_condensed" = all_totals, "paired_towers" = Lbanktowers_2021_2)
  return(tower_list)
}

# tower_data <- x
# tower_data <- x %>%
#   filter(Year == 2000)
