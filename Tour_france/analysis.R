library(data.table)
library(skimr)
library(lubridate)
library(ggplot2)
library(patchwork)
library(roll)
library(forcats)
library(ggthemes)
library(magrittr)


tdf_winners <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-07/tdf_winners.csv')

tdf_winners[,`:=`(start_date = ymd(start_date),
                  born = ymd(born,truncated = 2),
                  bmi = weight/(height^2),
                  distance_r_5 = roll_mean(distance,width = 10,complete_obs = F),
                  died = ymd(died,truncated = 2))]


top_country <- tdf_winners[, .N, by=birth_country
                           ][,birth_country := fct_reorder(birth_country,N)]

distance <- tdf_winners[,.(start_date,distance,distance_r_5)] %>%
  melt.data.table(id.vars = "start_date")


ggplot(data=distance, aes(x=start_date,y=value,colour=variable))+
  geom_line()

ggplot(data = tdf_winners,aes(x=tdf_winners$height,y=tdf_winners$weight))+
  geom_point()
  
ggplot(data=tdf_winners,aes(x=age))+
  geom_histogram(binwidth = 2)

ggplot(data=top_country,aes(y=birth_country,x=N))+
  geom_bar(stat = "identity")
