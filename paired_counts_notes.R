#### paired script
library(readxl)
library(tidyverse)
library(plotly)
library(lubridate)

towers_2021 <- read_excel("LACL Tower Escapement 2021.xlsx", 
                          col_types = c("numeric", "text", "date", 
                                        "numeric", "numeric", "numeric"))
didson_2021 <- read_excel("2021_RM22_DIDSON.xlsx", 
                          sheet = "Counts", col_types = c("numeric", 
                                                          "date", "numeric", "numeric", "numeric", 
                                                          "numeric", "numeric", "numeric", 
                                                          "text", "text", "numeric", "numeric", 
                                                          "numeric"))

Lbanktowers_2021_2<- towers_2021 %>%
  select(-RBank) %>%
  mutate(date1 = str_sub(Date, 6, -1),
         # For left bank only, the counts start at 50 minutes for even-numbered hours, and at minute 0 for odd-numbered hours
         Minute = case_when((Hour %% 2) == 0 ~ 50,
                            (Hour %% 2) != 0 ~ 0),
         date2 = as.Date(ymd(paste(Year, date1))),
         date_time = ymd_hm(paste(Year, date1, Hour, Minute))
  ) #end of mutate 

didson_2 <- didson_2021 %>%
  #filter(!is.na(Passage))
  filter(Minute %in% c(50,0)) %>%
  mutate(date2 = ymd(Date),
         date_time = ymd_hm(paste(date2, Hour,Minute)),
         Type = "DIDSON"
  ) %>%
  select(date_time, date2, Passage)

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
  geom_bar(stat = "identity" , position = "dodge") +
  theme_classic() +
  labs(title = "Paired counts compare")
ggplotly(plot)

# scatter plot
formula1 <- y ~ x
rsq1 <- rsq(paired2$DIDSON, paired2$Lbank_tower)

plot <- paired2 %>%
  ggplot(aes(x = DIDSON, y = Lbank_tower, text = date_time)) +
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

max(paired2$Date)

