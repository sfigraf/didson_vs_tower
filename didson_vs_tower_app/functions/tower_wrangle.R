### function for wrangling tower data

tower_function <- function(tower_data) {
  
  towers_2021_2<- tower_data %>%
    mutate(date1 = str_sub(Date, 6, -1),
           date2 = as.Date(ymd(paste(Year, date1))),
           date_time = ymd_h(paste(Year, date1, Hour))) 
  
  ###both banks plot
  toweres_collapsed <- towers_2021_2 %>%
    select(date_time, date2, LBank, RBank) %>%
    gather(key = "Bank", value = "Count", -c(date_time, date2))
  
  
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
    mutate(Year1 = as.character(year(`Date1`)), #allows variabes to be discrete when grpahed instead of continuous
           #day1 = paste0(month(`date(date_time)`), "/", day(`date(date_time)`)),
           Date2 = ymd(paste("0001", month(Date1), day(Date1))),
           Type = "Tower"
    )
  
  tower_list <- list("hourly" = toweres_collapsed, "daily" = daily_tower_2021totals1, "hourly_condensed" = all_totals)
  return(tower_list)
}
