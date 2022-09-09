### separating files
#all_didson <- read

library(readxl)
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

