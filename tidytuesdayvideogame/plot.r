#### Import libraries ####

library(tidyverse)
library(lubridate)
library(ggforce)
library(ggrepel)
library(cowplot)
library(glue)

#### get the data ####

video_games <- 
  readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-30/video_games.csv")

#### some cleaning ####

video_games_cleaned <- video_games %>% 
  mutate(release_date=mdy(str_replace(release_date,pattern = ",",replacement = ""))) %>% 
  arrange(release_date) %>% 
  mutate(Year=as_factor(year(release_date)),
         Month=as_factor(month(release_date)))



#### Grouping & aggegation ####

grouped_average <- video_games_cleaned %>%
  group_by(Year) %>% 
  summarise(average_price_by_year=mean(price,na.rm = T),
            median_price_by_year=median(price,na.rm = T)) %>% 
  ungroup() %>% 
  drop_na() %>% 
  mutate(Year=ymd(Year,truncated = 2))

mean_2008 <- grouped_average %>%
  filter(year(Year)=="2008") %>%
  pull(average_price_by_year) %>% 
  round(digits = 2)


mean_2013 <- grouped_average %>%
  filter(year(Year)=="2013") %>%
  pull(average_price_by_year) %>% 
  round(digits = 2)


#### Plot ####

grouped_average%>% 
  ggplot(aes(x=Year,y=average_price_by_year))+
  scale_x_date(date_breaks = "1 year",date_labels = "%Y",expand = c(0.01,0.01))+
  geom_line(size=1.2,colour="#FFCD29",alpha=.65)+
  geom_point(size=5,color="#FFCD29")+
  geom_mark_circle(data=subset(grouped_average,
                               Year==ymd("2013",truncated = 2)),
                   aes(label=paste(substr(as.character(Year),start = 0,stop = 4),": Highest peak"),
                       description=glue("In 2013, the highest peak of price was observed with a yearly average of {mean_2013}$.")),
                   color="red",
                   fill="red",
                   expand =unit(7,"mm"),
                   label.minwidth = unit(70,"mm"),
                   label.hjust = 0.5,con.colour = "#D4D4D4",label.fill = "#423F3F",label.colour = "#D4D4D4")+
  geom_mark_circle(data=subset(grouped_average,
                               Year==ymd("2008",truncated = 2)),
                   aes(label=paste(substr(as.character(Year),start = 0,stop = 4),": First peak"),
                       description=glue("In 2008, the first peak of price was observed with a yearly average of {mean_2008}$.")),
                   color="red",
                   fill="red",
                   expand =unit(7,"mm"),
                   label.minwidth = unit(70,"mm"),
                   label.hjust = 0.5,con.colour = "#D4D4D4",label.fill = "#423F3F",label.colour = "#D4D4D4")+
  theme_minimal()+
  labs(x="",
       y="",
       caption = "#Tidy tuesday | Source: Steam Spy | @alangel12407606",
       subtitle = "From 2004 to 2013 the average game's prices have increased steadily. Howewer since 2013 prices are decreasing.\n\n",
       title = "Average price by year")+
  theme(plot.background =element_rect(fill = "#423F3F"),
        plot.caption = element_text(size = 10,colour = "#D4D4D4"),
        panel.grid = element_line(colour = "#C78F50",size = 0.01),
        axis.text.x = element_text(size=10,colour = "#FFCD29"),
        plot.title = element_text(hjust = .5,size=20,colour = "#FFCD29"),
        plot.subtitle = element_text(hjust = .5,size = 14,colour = "#D4D4D4"),
        axis.text.y = element_text(size=10,color = "#FFCD29"))
