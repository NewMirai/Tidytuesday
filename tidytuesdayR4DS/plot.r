library(tidyverse)
library(ggTimeSeries)
library(viridis)

r4ds_members <-read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-16/r4ds_members.csv")


#visdat::vis_miss(r4ds_members)


r4ds_members %>% 
  ggplot_calendar_heatmap('date','daily_active_members',monthBorderSize = 1.5,monthBorderColour = "black")+
  scale_fill_viridis(option = "D")+
  theme_minimal()+
  facet_wrap(~Year, ncol = 1,strip.position = "right")+
  theme(panel.spacing = unit(5, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = NA, fill="black"),
        plot.background = element_rect(fill="black"),
        legend.position = "bottom",
        axis.text.x = element_text(size = 12,colour ="#bfbfbf",vjust = -2),
        axis.text.y = element_text(size=12,colour = "#bfbfbf"),
        strip.text = element_text(size=14,colour="#bfbfbf"),
        legend.text = element_text(colour = "#bfbfbf",size=8),
        legend.title = element_text(colour = "#bfbfbf",vjust = .9,size=14))+
  labs(y='',
       fill="Daily active members")
  