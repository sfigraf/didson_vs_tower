library(readxl)
library(tidyverse)
library(plotly)
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




Lbanktowers_2021_2<- towers_2021 %>%
  select(-RBank) %>%
  mutate(date1 = str_sub(Date, 6, -1),
         # For left bank only, the counts start at 50 minutes for even-numbered hours, and at minute 0 for odd-numbered hours
         Minute = case_when((Hour %% 2) == 0 ~ 50,
                            (Hour %% 2) != 0 ~ 0),
         date2 = as.Date(ymd(paste(Year, date1))),
         date_time = ymd_hm(paste(Year, date1, Hour, Minute))
         ) #end of mutate 


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
  ggplot(aes(x = Date1, y = daily_passage, group = 1)) +
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

## didson for minute
didson_2 <- didson_2021 %>%
  #filter(!is.na(Passage))
  filter(Minute %in% c(50,0)) %>%
  mutate(date2 = ymd(Date),
         date_time = ymd_hm(paste(date2, Hour,Minute)),
         Type = "DIDSON"
  ) %>%
  select(date_time, date2, Passage)


# Paired Counts Compare ---------------------------------------------------

paired <- left_join(Lbanktowers_2021_2,didson_2, by = c("date_time", "date2"))

paired2 <- paired %>%
  filter(Hour >= 4) %>%
  select(date_time, date2, Passage, LBank) %>%
  rename(DIDSON = Passage,
         Lbank_tower = LBank,
         Date = date2)

#plot ready 
paired3 <- paired2 %>%
  pivot_longer(cols = c(DIDSON, Lbank_tower), names_to = "Type", values_to = "passage") %>%
  filter(!is.na(passage))

#cols = !c(DIDSON, Date, Hour), names_to = "type", values_to = "passage")
#bar plot
plot <- paired3 %>%
  ggplot(aes(x = date_time, y = passage, fill = Type)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(title = "Paired counts compare")
ggplotly(plot)

# scatter plot

plot <- paired3 %>%
  ggplot(aes(x = date_time, y = passage, color = Type)) +
  geom_point() +
  theme_classic() +
  labs(title = "Paired counts compare")
ggplotly(plot)

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
d_t_wide %>%
  ggplot(aes(x = DIDSON, y = Tower)) +
  geom_point() +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = formula1) +
  theme_classic() + 
  labs(title = "DIDSON vs Tower Daily")
  


# Hourly Compare ----------------------------------------------------------
didson_hourly <- didson_2021 %>%
  replace_na(list(Passage = 0, Upstream = 0, Downstream = 0)) %>%
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


# this file is in the 
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

library(ggpmisc)

formula1 <- y ~ x
rsq <- function (x, y) cor(x, y) ^ 2
rsq <- function(x, y) summary(lm(y~x))$r.squared

rsq1 <- rsq(didson_tower_hourly$didson_total_passage, didson_tower_hourly$tower_total_count)
## outliers stuff
line_outlier_plot <- function(df, outlier_color = "red", normal_color = "black", drop = FALSE){
  # Assign a label to show if it is an outlier or not
  df$label <- ifelse(df$col > mean(df$col) + 3 * sd(df$col) |
                       df$col < mean(df$col) - 3 * sd(df$col), "Outlier", "Normal")
  
  df$label <- factor(df$label, levels = c("Normal", "Outlier"))
}  
#uses didson_total_passage to predict tower_total_count
model1 <- lm(didson_tower_hourly$tower_total_count ~ didson_tower_hourly$didson_total_passage)
#uses tower_total_count to predict didson_total_passage
model2 <- lm(didson_tower_hourly$didson_total_passage ~ didson_tower_hourly$tower_total_count)

#vector of predicted tower values given didson values
x <- data.frame(as.numeric(model1$coefficients[1]) + as.numeric(model1$coefficients[2])*didson_tower_hourly$didson_total_passage)
colnames(x)[1] <- "didson_total_passage"
y <- left_join(didson_tower_hourly, x, by = "didson_total_passage")

as.numeric(model1$coefficients[1]) + as.numeric(model1$coefficients[2])*933


x2 <- predict.lm(model1, 
           se.fit = TRUE, level = .95)

#uses didson_total_passage to predict tower_total_count
model1 <- lm(didson_tower_hourly$tower_total_count ~ didson_tower_hourly$didson_total_passage)
#uses tower_total_count to predict didson_total_passage
model2 <- lm(didson_tower_hourly$didson_total_passage ~ didson_tower_hourly$tower_total_count)

#this is the predicted value
predicted <- function(model, column_entry) {
  return(as.numeric(model$coefficients[1]) + as.numeric(model$coefficients[2])*column_entry)
} 
#gives 
predicted(model1, 0)
abs(predicted(model1, 970) - actual) > sd(model1$residuals)*2 ~ "outlier"

#95% of data lie within 2 standard deviations of mean
sd(model1$residuals)*2

#when tower predicted value is greater than 2 sd's of , and outlier for didson is within 2 sd's
#problem is that when we reach higher values, the residuals are going to vary more
#
plot(model1$residuals)
didson_tower_hourly11 <- didson_tower_hourly %>%
  mutate(
    #resid12 = lm(didson_total_passage~tower_total_count))$residual
    resid11 = abs(didson_total_passage - predicted(model1, didson_total_passage)) ,
         )

didson_tower_hourly11 %>%
  ggplot(aes(x = tower_total_count, y = resid11)) +
  geom_point()

plot <- didson_tower_hourly %>%
  na.omit() %>%
  ggplot(aes(x = didson_total_passage, y = tower_total_count, color = case_when(
    abs( didson_total_passage - predicted(model1, didson_total_passage)) > sd(model1$residuals)*2 #& 
    #abs(predicted(model2, tower_total_count) - tower_total_count) > sd(model2$residuals)*2 
    ~ "red",
      
      #quantile(tower_total_count, 0.95) & didson_total_passage > quantile(didson_total_passage, .95) ~ "red",
    
  )
  )) +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = formula1) +
  # stat_poly_eq(formula = formula1,
  #              eq.with.lhs = "italic(hat(y))~`=`~",
  #              aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
  #              parse = TRUE) +
  geom_point() +
  # geom_text(aes(label= ifelse(tower_total_count > quantile(tower_total_count, 0.95) & didson_total_passage < quantile(didson_total_passage, .05),
  #                             as.character(tower_total_count),'')),hjust=0,vjust=0) +
  
  theme_classic() +
  labs(caption = paste(round(rsq1,2)))
plot
#library(plotly)
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

excel_sheets(path = "2021_RM22_DIDSON.xlsx")

library(tidyverse)
iris %>%
  ggplot(aes(x = Sepal.Width, y = Sepal.Length)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, formula = "y ~ x", level = .95) +
  geom_text(aes(label= ifelse(Sepal.Length > quantile(Sepal.Length, 0.95) | Sepal.Length < quantile(Sepal.Length, .05),
                              as.character(Sepal.Length),'')),hjust=0,vjust=0) 
  # geom_text(aes(label= ifelse(Sepal.Width > quantile(Sepal.Width, 0.95),
  #                             as.character(Sepal.Width),'')),hjust=0,vjust=0)

mod <- lm(iris$Sepal.Length ~ iris$Sepal.Width)


mod %>%
  ggplot(aes(x = .fitted, y = .resid)) + 
  geom_point()


ggplot(aes(x = iris$Sepal.Width, y = x1$x.residuals)) +
  geom_point()
