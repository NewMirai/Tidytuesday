library(tidyverse)
library(naniar)
library(skimr)
library(ggalluvial)
library(showtext)
library(patchwork)
library(ggtext)
library(glue)
library(scales)

commute_mode <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-05/commute.csv")



font_add_google(name = "Rosario",family = "ro")
font_add_google(name = "Open Sans",family = "os") 
font_add_google(name = "Roboto",family = "rb") 


showtext_auto()


commute_mode <- commute_mode %>% 
  drop_na() %>% 
  mutate_if(is.character,as.factor) %>% 
  rename(size = n) %>% 
  select(mode,city_size,state,size)

top_10_state <- commute_mode %>%
  select(state,size) %>% 
  group_by(state) %>% 
  summarise(n=sum(size)) %>% 
  arrange(-n) %>% 
  slice(1:10) %>% 
  mutate(state=as.character(state)) %>% 
  pull(state)

top_10_state_last <- commute_mode %>%
  select(state,size) %>% 
  group_by(state) %>% 
  summarise(n=sum(size)) %>% 
  arrange(n) %>% 
  slice(1:10) %>% 
  mutate(state=as.character(state)) %>% 
  pull(state)

top_state_data <- commute_mode %>% 
  filter(state %in% top_10_state) %>% 
  group_by(mode,city_size,state) %>% 
  summarise(frequency=sum(size))

last_state_data <- commute_mode %>% 
  filter(state %in% top_10_state_last) %>% 
  group_by(mode,city_size,state) %>% 
  summarise(frequency=sum(size))


 p1 <- top_state_data%>% 
  ggplot(aes(y = frequency,axis1 = state,axis2=city_size,axis3=mode))+
    geom_alluvium(aes(fill = mode), width = 1/12,knot.pos = 0.4) +
    geom_stratum(width = 1/4,fill="#FAF9DC") +
    geom_text(stat = "stratum", label.strata = TRUE,color="black",family="os",size=3.5) +
    scale_x_discrete(limits = c(" ", " "," "),expand = c(0.01, 0.01)) +
    scale_y_discrete(limit=c(0,1000000,max(cumsum(top_state_data$frequency))),
                     expand = c(.05, .05),
                     labels=function(x) format(x, big.mark = ",",
                                               scientific = FALSE))+
    scale_fill_manual(values = c("Bike"="#DB1515","Walk"="#1A124D"))+
   theme_minimal()+
   theme(panel.grid.major = element_blank(),
         plot.title = element_markdown(size = 18,face = "bold",family="rb"),
         plot.subtitle = element_markdown(family = "rb",size=12),
         panel.grid.minor = element_blank(),
         axis.ticks.y = element_line(colour = "#DB1515",size=1,linetype = "dashed"),
         axis.ticks.length.y = unit(1.5,"cm"),
         legend.position = "top",
         axis.text.x = element_text(size=14,family = "ro"),
         axis.text.y = element_text(size = 10,hjust = -0.4),
         axis.title.y = element_text(vjust = 2,angle = 90,hjust = -0.5,face = "bold",size=12))+
   labs(y="Number of individuals",
        fill="Mode",
        title="Modes Less Traveled - Bicycling and Walking to Work in the United States: 2008-2012",
        subtitle = glue("Illustrated below is an alluvial diagram showing the contrast between the **10 most
                        populated states <span style='color:#DB1515'>(1st graph)</span>** and the **10 least 
                        populated <span style='color:#DB1515'>(2nd graph)</span>**<br>"))
 
 breaks2 <- c(0,30000,max(cumsum(last_state_data$frequency)))
 
 p2 <- last_state_data %>% 
   ggplot(aes(y = frequency,axis1 = state,axis2=city_size,axis3=mode))+
   geom_alluvium(aes(fill = mode), width = 1/12,knot.pos = 0.4,show.legend = F) +
   geom_stratum(width = 1/4,fill="#FAF9DC") +
   geom_text(stat = "stratum", label.strata = TRUE,color="black",family="os",size=3.5) +
   scale_x_discrete(limits = c("State", "City Size","Mode"),
                    expand = c(0.01, 0.01)) +
   scale_y_discrete(limits=breaks2,
                    expand = c(.05, .05),
                    labels=function(x) format(x, big.mark = ",",
                                              scientific = FALSE))+
   scale_fill_manual(values = c("Bike"="#DB1515","Walk"="#1A124D"))+
   theme_minimal()+
   theme(panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         plot.caption = element_text(face = "bold",vjust = -2),
         plot.title = element_text(size = 16,face = "bold",family="rb",hjust = 0.5),
         legend.position = "top",
         axis.ticks.y = element_line(colour = "#DB1515",size=1,linetype = "dashed"),
         axis.ticks.length.y = unit(1.5,"cm"),
         axis.text.x = element_text(size=14,family = "ro",face="bold"),
         axis.text.y = element_text(size = 10,hjust = -.4),
         axis.title.y = element_text(vjust = 2,angle = 90,hjust = 0.5,face = "bold",size=12))+
   labs(y="",
        fill="Mode",
        caption="Data: ACS Survey | Visualisation by : @alangel12407606")
 
p3 <- p1+p2+plot_layout(ncol = 1, heights = c(2, 2))
  



