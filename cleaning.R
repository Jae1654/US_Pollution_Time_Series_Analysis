
# before running this script, 1. unzip uspollution.zip and move
# pollution_us_2000_2016.csv to the folder that this script is located in, and
# then 2. go to Session at the top menu, and do Set Working Directory >
# To Source File Location

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

# we can choose either the NO2, O3, SO2, or CO column as our time-series.
# however we should stick to O3 since that's what Jae's code uses as well.
# please indicate in your model whether you are looking at the entire series or
# just a subset.
