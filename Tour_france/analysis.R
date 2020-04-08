library(data.table)
library(magrittr)
library(skimr)
library(lubridate)

tdf_winners <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-07/tdf_winners.csv')

# Handling date format

tdf_winners[,`:=`(start_date = ymd(start_date),
                  born = ymd(born,truncated = 2),
                  died = ymd(died,truncated = 2))]


