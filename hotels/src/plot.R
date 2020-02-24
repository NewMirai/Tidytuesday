library(tidyverse)
library(ggtext)
library(ggforce)
library(skimr)
library(lubridate)
library(glue)
library(countrycode)
library(ggtext)
library(forcats)
library(ggtext)
library(ggrepel)
library(showtext)

# add fonts

font_add_google(name = "Playfair Display",family = "pd") 
font_add_google(name = "Roboto",family = "rb") 

# activate fonts

showtext_auto()

hotels <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-11/hotels.csv') %>% drop_na()


plot_df <-  hotels %>% 
  mutate(Continent=countrycode(sourcevar = country,origin = "iso3c",destination = "continent")) %>% 
  drop_na() %>% 
  group_by(arrival_date_year,Continent) %>% 
  summarise(median_adr = median(adr)) %>% 
  ungroup() %>% 
  mutate(arrival_date_year=as_factor(arrival_date_year),
         Continent = if_else(Continent == "Americas","America",Continent))

hotels %>% 
  group_by(arrival_date_year) %>% 
  summarise(med=median(adr))

chart <- plot_df %>% 
  ggplot(aes(x=arrival_date_year,y=median_adr,group=Continent))+
  geom_line(color="#676767",size=0.75)+
  geom_label_repel(data=plot_df %>% filter(arrival_date_year %in% c("2015","2017")),
                  force = 10,label.r = unit(0.9,"mm"),
                  max.iter = 10000,
                  aes(x=arrival_date_year,y=median_adr,label=Continent))+
  geom_point(color="#247CD5",size=4)+
  scale_x_discrete(position = "top",expand=c(0.06,0.05)) +
  scale_y_continuous(expand=c(0.025,1))+
  labs(x="",
       color="Customer type",
       title="Daily rate ranking of Hotel Bookings accross continent",
       subtitle=str_wrap(width = 140,"Shown below is a slopegraph of the median daily rate accross continent from 2015 to 2017. In two years America became the continent with the highest median daily rate while Europe remained last. One can also notice that Asia jump from the fourth to the second place. Overall a increasing trend of daily rate is observed."),
       caption = "#Tidy Tuesday | Source: Antonio, Almeida and Nunes, 2019 | Author: @alangel112407606 ",
       y="Median Daily Rate")+
  theme_bw(base_family = "rb")+
  theme(panel.grid = element_blank(),
        panel.border= element_blank(),
        plot.margin = unit(c(1, 2, 1, 2), "cm"),
        plot.title = element_text(family = "pd",size = 22),
        axis.line.y = element_line(colour = "black",size=1),
        panel.grid.major.x = element_line(colour = "#676767",linetype = "dashed"),
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(size=13,face="bold",vjust = 3),
        axis.text.y = element_text(size=10),
        axis.text.x = element_text(size=14,face="bold",vjust = 1),
        legend.position = "top")