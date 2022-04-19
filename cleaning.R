
# unzip uspollution.zip and move pollution_us_2000_2016.csv to the folder that
# this script is located in before running this script

library(tidyverse)
library(lubridate)

pollution <- read_csv(file = 'pollution_us_2000_2016.csv')

phoenix <- pollution %>%
  filter(State == 'Arizona', City == 'Phoenix') %>%
  select(`Date Local`, `NO2 Mean`, `O3 Mean`, `SO2 Mean`, `CO Mean`) %>%
  mutate(year = year(`Date Local`),
         month = month(`Date Local`),
         day = day(`Date Local`)) %>%
  group_by(year, month, day) %>%
  summarize(no2 = mean(`NO2 Mean`),
            o3 = mean(`O3 Mean`),
            so2 = mean(`SO2 Mean`),
            co = mean(`CO Mean`)) %>%
  ungroup()
