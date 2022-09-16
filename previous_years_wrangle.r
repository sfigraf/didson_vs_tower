### separating files
#all_didson <- read

library(readxl)
library(tidyverse)
library(lubridate)
DIDSON_RM22_2017_2021 <- read_excel("files/DIDSON_RM22_2017-2021.xlsx", 
                                    sheet = "DIDSON Counts", col_types = c("numeric", 
                                                                           "date", "numeric", "numeric", "numeric", 
                                                                           "numeric", "numeric", "text", "text"))

didson_2021 <- read_excel("2021_RM22_DIDSON.xlsx", 
                          sheet = "Counts", col_types = c("numeric", 
                                                          "date", "numeric", "numeric", "numeric", 
                                                          "numeric", "numeric", "numeric", 
                                                          "text", "text", "numeric", "numeric", 
                                                          "numeric"))
library(readr)
DIDSON_RM22_2017_2021 <- read_csv("files/DIDSON_RM22_2017-2021.csv", col_types = c("c","n"))



iso <- DIDSON_RM22_2017_2021 %>%
  filter(Year %in% c(2020))

write_csv(iso, "DIDSON_2020.csv")


### wrangling 

didson_all <- read_csv("files/DIDSON_RM22_2017-2021.csv")

didson_all_2 <- didson_all %>%
  mutate(
    Hour = as.numeric(str_sub(Hour, start = 1, end = 2)),
    Date = dmy(paste(Date,Year)),
    #date_time = dmy_hm(paste(Date,Year,new_hr,Minute))
    )

## TOWERS
library(readxl)
library(readr)
towers_2021 <- read_excel("LACL Tower Escapement 2021.xlsx", 
                          col_types = c("numeric", "text", "date", 
                                        "numeric", "numeric", "numeric"))

#has all counts since 2000
X2022_Newhalen_River_Hourly_counts <- read_csv("files/2022 Newhalen River Master Hourly counts.csv", 
                                               #col_types = cols(Year = col_number())
                                               )
x <- X2022_Newhalen_River_Hourly_counts %>%
  select(Year, Location, Date, Hour,  LBank, RBank, Sky, Wind, Precip, Turbidity) %>%
  mutate(date2 = as.Date(dmy(paste(Date, Year))),
    date_time = dmy_h(paste(Date, Year, Hour)))

x1 <- tower_function(x)
x1$daily$Date1
sort(unique(x1$paired_towers$Year))

