library(tidyverse)
library(lubridate)
library(ggsci)
library(gganimate)
library(ggthemes)

nz_bird <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-19/nz_bird.csv")


nz_bird<- nz_bird %>% 
  mutate(hour=paste0(as.character(hour),":00:00")) %>% 
  mutate(date_time=ymd_hms(paste(date,hour))) %>% 
  select(-date,-hour,date_time,vote_rank,bird_breed) %>% 
  mutate(vote_rank=as.integer(gsub(vote_rank,pattern = "vote_",replacement = ""))) %>% 
  drop_na()

top_loved_bird <- nz_bird %>% 
  group_by(bird_breed) %>% 
  count() %>% 
  arrange(-n) %>% 
  head() %>% 
  ungroup()%>% 
  pull(bird_breed)

plot_data <- nz_bird %>% 
  filter(bird_breed %in% top_loved_bird) %>% 
  group_by(bird_breed,date_time)%>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  group_by(date_time) %>% 
  mutate(ordering=rank(n)) %>% 
  ungroup()


p <- plot_data %>% 
  ggplot(aes(x=ordering,group=bird_breed,color=bird_breed,fill=bird_breed))+
  geom_tile(aes(y=n/2,height=n,width=0.9),alpha=0.9)+
  geom_text(aes(y = n, label = bird_breed), hjust = -0.4)+
  coord_flip(clip = "off", expand = FALSE) +
  scale_color_d3(name="")+
  scale_fill_d3(name="")+
  theme_tufte(14)+
  guides(color=F,fill=F)+
  labs(title='Date time: {closest_state}', x = "",y="Count of votes")+
  theme(plot.title = element_text(size = 22),
        axis.ticks.y = element_blank(),
        axis.text.y  = element_blank())+
  transition_states(states = date_time,transition_length =4,state_length = 10)+
  ease_aes('cubic-in-out')


animate(p, duration = 30, fps = 60,res=100,width=800,height=600)