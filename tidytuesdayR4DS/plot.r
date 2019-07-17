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
        legend.title = element_text(colour = "#bfbfbf",vjust = .9,size=14),
        plot.title = element_text(size=20,colour="#bfbfbf",hjust = .5),
        plot.subtitle = element_text(size=16,colour = "#bfbfbf",hjust = .5),
        plot.caption = element_text(colour = "#bfbfbf"))+
  labs(y='',
       fill="Daily active members",
       title = "Calendar heatmap of the active members",
       subtitle = "Made with the ggTimeSeries package for super easy calendar heatmap\n",
       caption = "#Tidy tuesday | Source: R4DS Slack | @alangel12407606")


ggsave("calendar_heatmap.png",width = 16,height = 9,dpi = 400)
  