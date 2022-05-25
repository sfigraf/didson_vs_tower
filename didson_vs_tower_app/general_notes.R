library(readxl)
towers_2021 <- read_excel("LACL Tower Escapement 2021.xlsx", 
                          col_types = c("numeric", "text", "date", 
                                        "numeric", "numeric", "numeric"))
didson_2021 <- read_excel("2021_RM22_DIDSON.xlsx", 
                          sheet = "Counts", col_types = c("numeric", 
                                                          "date", "numeric", "numeric", "numeric", 
                                                          "numeric", "numeric", "numeric", 
                                                          "text", "text", "numeric", "numeric", 
                                                          "numeric"))

towers <- tower_function(towers_2021)
didson <- didson_function(didson_2021)

unique(didson$hourly$Hour)
didson$daily
towers$daily
towers$hourly
didson$hourly
x <- didson_function(didson_2021)$hourly
towers$hourly
# Towers and wrangle ----------------------------------------------




towers_2021_2<- towers_2021 %>%
  mutate(date1 = str_sub(Date, 6, -1),
         date2 = as.Date(ymd(paste(Year, date1))),
         date_time = ymd_h(paste(Year, date1, Hour))) 

#Left Bank
towers_2021_2 %>%
  #group_by(Date)
  ggplot(aes(x = date_time, y = LBank, group = 1)) +
  geom_line() + #Error in base_size/2 : non-numeric argument to binary operator SOLVED because I was doing a pipe after geom_point instead of +
  #geom_line(y = towers_2021_2$RBank) +
  theme_classic()



###both banks plot
toweres_collapsed <- towers_2021_2 %>%
  select(date_time, date2, LBank, RBank) %>%
  gather(key = "Bank", value = "Count", -c(date_time, date2))
plot <- toweres_collapsed %>%
  ggplot(aes(x = date_time, y = Count, color = Bank, group = 1)) +
  geom_line() +
  theme_classic() +
  scale_x_datetime(date_breaks = "months" , date_labels = "%b-%y")

ggplotly(plot)

###totals 
all_totals <- toweres_collapsed %>%
  group_by(date_time) %>%
  summarise(tower_total_count = sum(Count))

all_totals %>%
  ggplot(aes(x = date_time, y = tower_total_count, group =1)) +
  geom_line() +
  theme_classic() +
  labs(title = "Hourly Total Tower Counts", caption = "Both banks sum" )

daily_tower_2021totals <- toweres_collapsed %>%
  group_by(date2) %>%
  summarise(total_count = sum(Count))




daily_tower_2021totals1 <- daily_tower_2021totals %>%
  rename(daily_passage = total_count,
         Date1 = date2) %>%
  mutate(Year1 = as.character(year(`Date1`)), #allows variabes to be discrete when grpahed instead of continuous
         #day1 = paste0(month(`date(date_time)`), "/", day(`date(date_time)`)),
         Date2 = ymd(paste("0001", month(Date1), day(Date1))),
         Type = "Tower"
  )

daily_tower_2021totals1 %>%
  ggplot(aes(x = Date1, y = daily_passage, group =1)) +
  geom_line() +
  theme_classic() +
  labs(title = "Daily Total Tower Counts", caption = "Both banks sum" )


# DIDSON wrangle ----------------------------------------------------------
didson_2021_2 <- didson_2021 %>%
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



didson_all_daily_passage2021 <- didson_2021_2 %>%
  mutate(Type = "DIDSON") %>%
  rename(Date1 = `date(date_time)`)


# daily compare -----------------------------------------------------------



didson_tower2021 <- bind_rows(didson_all_daily_passage2021, daily_tower_2021totals1)

#ratio
d_t_wide <- didson_tower2021 %>%
  pivot_wider(names_from = Type, values_from = daily_passage) %>%
  mutate(dif1 = DIDSON - Tower,
         ratio1 = DIDSON/Tower)

plot <- d_t_wide %>%
  ggplot(aes(x = Date2, y = ratio1)) +
  geom_line() +
  theme_classic() +
  labs(title = "Ratio of DIDSON/Tower Salmon Counts")

ggplotly(plot)

plot <- didson_tower2021 %>%
ggplot(aes(x = Date2, y = daily_passage, color = Type)) +
  geom_line() +
  theme_classic() +
  labs(title = "Didson vs Tower Daily Escapement Compare", caption = "Tower counts are from left bank only. Data not collected on tower between 0-4 AM")
ggplotly(plot)


# Hourly Compare ----------------------------------------------------------
didson_hourly <- didson_2021 %>%
  replace_na(list(Passage =0, Upstream =0, Downstream =0)) %>%
  group_by(Date, Hour) %>%
  summarise(didson_total_passage = sum(Passage))

#turn NA's to 0
#make datetime column

didson_hourly1 <- didson_hourly %>%
  #replace_na(list(Passage =0, Upstream =0, Downstream =0)) %>%
  mutate(date_time = ymd_h(paste(Date, Hour)),
         #count_type = "DIDSON"
  ) %>%
  na.omit()

didson_hourly1 %>%
  ggplot(aes(x = date_time, y = didson_total_passage)) +
  geom_line() +
  theme_classic() +
  labs(title = "DIDSON Hourly Passage")

#compare to towers hourly, compute column of difference ratio
didson_tower_hourly <- left_join(didson_hourly1, all_totals, by = "date_time")



didson_tower_hourly_long <- pivot_longer(data = didson_tower_hourly, cols = !c(date_time, Date, Hour), names_to = "type", values_to = "passage")

#
plot <- didson_tower_hourly_long %>%
  ggplot(aes(x = date_time, y = passage, color = type)) +
  geom_line() +
  theme_classic() +
  labs(title = "Hourly Comparison Tower and DIDSON")

ggplotly(plot)

didson_tower_hourly1 <- didson_tower_hourly %>%
  replace_na(list(tower_total_count = 0)) %>%
  
  mutate(dif1 = didson_total_passage - tower_total_count,
         ratio1 = didson_total_passage/tower_total_count)

#trying to see where the ratios varied siginficantly
didson_tower_hourly1 %>%
  ggplot(aes(x = date_time, y = ratio1)) +
  geom_line()+
  theme_classic() +
  labs(caption = "ANy entries that were NA are now registered to 0. DDIDSON counts by net passage.")

didson_tower_hourly1 %>%
  ggplot(aes(x = date_time, y = dif1)) +
  geom_line()+
  theme_classic()

#identifying outliers by raw differences
lower_bound_difference <- quantile(didson_tower_hourly1$dif1, 0.025) #2.5 % 
upper_bound_difference <- quantile(didson_tower_hourly1$dif1, 0.975) #97.5% of values are below this value

#returns row numbers where these conditions are the case
outlier_ind <- which(didson_tower_hourly1$dif1 < lower_bound_difference | didson_tower_hourly1$dif1 > upper_bound_difference)

#subsets data based on those row numbers
outliers_by_difference <- didson_tower_hourly1[outlier_ind, ]

## Hampel filter: numbers outside 3 median absolute deviations

lower_bound <- median(didson_tower_hourly1$dif1) - 3 * mad(didson_tower_hourly1$dif1, constant = 1)
upper_bound <- median(didson_tower_hourly1$dif1) + 3 * mad(didson_tower_hourly1$dif1, constant = 1)
outlier_ind <- which(didson_tower_hourly1$dif1 < lower_bound | didson_tower_hourly1$dif1 > upper_bound)
outliers_by_difference_hampel <- didson_tower_hourly1[outlier_ind, ]

#ID outliers based on ratio

valid_ratios <- didson_tower_hourly1 %>%
  filter(!ratio1 %in% c(NaN, Inf, -Inf))
lower_bound_ratio <- quantile(valid_ratios$ratio1, 0.025) #2.5 % 
upper_bound_ratio <- quantile(valid_ratios$ratio1, 0.975) #97.5% of values are below this value

outlier_ind <- which(valid_ratios$ratio1 < lower_bound_ratio | valid_ratios$ratio1 > upper_bound_ratio)
outliers_by_ratio <- valid_ratios[outlier_ind, ]

all_equal(outliers_by_difference,outliers_by_ratio)

# these are the only entries that are the same 
#but the small ratios shouldn't really matter very much; only the high percentiles mean there's abig difference between the 2
outliers_by_ratio_and_difference <- intersect(outliers_by_ratio,outliers_by_difference)
# didson_tower_hourly %>%
#   ggplot(aes(x = date_time, y = didson_total_passage)) +
#   geom_line()+
#   geom_line()
#   theme_classic()


###make app:
#user uploads 2 files: didson and tower (same year)
# tabs for general data viz by hour, by day; filters for both
#compare tab: text input filters for percentiles upper bound/lower bound
# graph of ratios; graph of differences; highlight outliers with plotly proxy? datatable of outliers?



