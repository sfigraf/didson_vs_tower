### function to clean DIDSON data

didson_function <- function(didson_data) {
  #has 2021 in variable names but doesn't mean anything in this function
  
  #daily
  didson_2021_2 <- didson_data %>%
    mutate(Date1 = ymd(Date),
           date_time = ymd_hm(paste(Date1, Hour,Minute)),
           Type = "DIDSON"
    ) %>%
    select(date_time, Upstream, Downstream, Passage) %>%
    replace_na(list(Passage =0, Upstream =0, Downstream =0)) %>%
    group_by(date(date_time)) %>%
    summarise(daily_passage = sum(Passage)) %>%
    mutate(Year1 = as.character(year(`date(date_time)`)), #allows variabes to be discrete when grpahed instead of continuous
           #day1 = paste0(month(`date(date_time)`), "/", day(`date(date_time)`)),
           Date2 = ymd(paste("0001", month(`date(date_time)`), day(`date(date_time)`)))
    )
  
  
  # dataset ready to be joinged for tower compare and also visualization
  didson_all_daily_passage2021 <- didson_2021_2 %>%
    mutate(Type = "DIDSON") %>%
    rename(Date1 = `date(date_time)`)
  
  ## Hourly
  
  didson_hourly <- didson_data %>%
    replace_na(list(Passage =0, Upstream =0, Downstream =0)) %>%
    group_by(Date, Hour) %>%
    summarise(didson_total_passage = sum(Passage))
  
  
  didson_hourly1 <- didson_hourly %>%
    #replace_na(list(Passage =0, Upstream =0, Downstream =0)) %>%
    mutate(date_time = ymd_h(paste(Date, Hour)),
           #count_type = "DIDSON"
    ) %>%
    na.omit()
  
  didson_list <- list("daily" = didson_all_daily_passage2021, "hourly" = didson_hourly1)
  return(didson_list)
}

# x <- didson_function(didson_2021)
 # x$daily
