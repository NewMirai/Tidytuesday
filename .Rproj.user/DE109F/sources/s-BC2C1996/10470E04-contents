# @author Alain ANGHELIDI

#### Library import ####

library(tidyverse)
library(readr)
library(RColorBrewer)
library(lubridate)
library(ggchicklet)

#### Data ####

media_franchises <- 
  read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-02/media_franchises.csv")


####  Wrangling ####


top<- media_franchises %>%
  mutate(year_existence=as.numeric(year(Sys.Date())-year_created),
         revenue_normalized=revenue/year_existence,
         revenue_category=as_factor(revenue_category)) %>% 
  distinct(franchise,revenue_normalized,revenue_category,original_media) %>% 
  group_by(franchise,original_media) %>% 
  summarise(revenue_tot=sum(revenue_normalized)) %>% 
  ungroup() %>% 
  mutate(franchise=case_when(
    str_detect(string = as.character(franchise), pattern = "Potter") ~ "Harry Potter",
    TRUE ~ franchise
  )) %>% 
  mutate(franchise=fct_reorder(franchise,revenue_tot,.desc = T)) %>%
  arrange(-revenue_tot) %>%
  head(20)


#### Plot ####

p <- ggplot(data=top,
       aes(x =fct_reorder(franchise,revenue_tot),
           y=revenue_tot,
           fill=original_media))+
  scale_fill_brewer(type ="qual",palette = "Paired")+
  coord_flip()+
  theme_light()+
  geom_chicklet()+
  labs(y="Revenue by year of existence in (bn $)",
       x="",
       title ="Which type of media provide the most revenue by year?",
       fill="Type of media")+
  theme(plot.subtitle = element_text(family = "serif", size = 16, hjust = 0.5, vjust = 1), 
    plot.caption = element_text(vjust = 1,size = 12),
    plot.title = element_text(family = "serif",size = 20,hjust =.5),
    axis.text = element_text(family = "serif",size=14),
    axis.title = element_text(size = 14,family = "serif"),
    axis.text.x = element_text(vjust = 2),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "ivory1", 
        linetype = "solid"), plot.background = element_rect(fill = "#e6f2ff"),
    legend.text = element_text(size=12),
    legend.box.background = element_rect(color="black",size = 1.1),
    legend.title.align = 0.5,
    legend.title = element_text(size=14,face = "bold"),
    legend.background = element_rect(fill = "ivory1", 
        colour =NA)) +
  labs(x = NULL, subtitle = "Top 20 franchise using revenue by year of existence\n", 
    caption = "@alangel12407606
#TidyTuesday")

p
