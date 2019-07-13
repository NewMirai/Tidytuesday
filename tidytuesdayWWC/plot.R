library(readr)
library(tidyverse)
library(ggridges)
library(viridis)
library(ggthemes)
library(hrbrthemes)
library(ggThemeAssist)
library(ggrepel)
library(visdat)

wwc_outcomes <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-09/wwc_outcomes.csv")
squads <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-09/squads.csv")
codes <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-09/codes.csv")

wwc_outcomes <- left_join(wwc_outcomes, codes, by = "team")


median_age_plot <- squads %>%
  group_by(country) %>% 
  mutate(median_age=median(age))%>% 
  ungroup() %>% 
  mutate( country=case_when(
    country=="China PR" ~ "China",
    country=="US" ~ "USA",
    TRUE ~ country
  )) %>% 
  mutate(country=fct_reorder(country,median_age))


p1 <- ggplot(data = median_age_plot,aes(x=age,y=country,fill=age))+
  stat_density_ridges(quantile_lines = TRUE, quantiles = 2,
                      scale=.95,rel_min_height=0.005,fill="#a3c2c2")+
  scale_x_continuous(breaks = seq(10,45,5))+
  theme_ridges(center_axis_labels = T)+
  labs(x="Age",
       y="",
       title = "Distribution of age per country and ranked by median age",
       subtitle = "We observe that the most experienced team in terms of median age is also the winner of the competition\n",
       caption = "@alangel12407606\n#TidyTuesday")+
theme(axis.title.x = element_text(hjust = .5,size=14,color = "black",vjust = -1),
      plot.title = element_text(size=20,hjust = .5),
      plot.subtitle = element_text(size = 16,hjust = .5),
      axis.text.y = element_text(size=12),
      plot.background = element_rect(fill="#ffffe6"))

p1


p2_data <- squads %>% 
  top_n(50,wt =goals) %>% 
  arrange(-goals) %>% 
  mutate(gp=round(goals/caps,digits = 1)) %>% 
  mutate(player=fct_reorder(player,goals),
         country=as_factor(country)) %>% 
  mutate(player_category=case_when(
    age<=25 ~"- 25",
    age<=30 ~"25-30",
    age<=35 ~"30-35",
    age>35 ~"35+")) %>% 
  arrange(-gp,country)


p2 <- p2_data %>% ggplot(aes(x = player,y=goals,fill=gp))+
  geom_bar(stat = "identity",colour=NA)+
  geom_vline(aes(x = country,y=player),xintercept=0.5)+
  facet_wrap(~player_category,nrow = 1)+
  coord_flip()+
  scale_fill_viridis(option = "A")+
  theme_modern_rc()+
  theme(
    axis.text.x = element_text(angle = 90),
    axis.title.x= element_text(hjust = .5,size=18,vjust = -2),
    legend.position = "right",
    plot.title = element_text(size=20,hjust = .5),
    plot.subtitle = element_text(size=16,hjust = .5),
    legend.direction = "vertical",
    plot.caption = element_text(size=12),
    legend.title = element_text(size=16),
    strip.text = element_text(hjust = .5,color="#f0f5f5",size = 16,vjust = 2))+
  geom_text(hjust=-0.1,label=paste(p2_data$caps,"games"),color=ifelse(p2_data$gp>0.75,"#f0f5f5",NA),size=5)+
  labs(x="",
       y="Goals scored",
       fill="Goal per games ",
       title = "Top 50 scorers by age category",
       subtitle = "This plot higlight the consistency of each scorers by looking at the goal per games indicator.3 players scored more than 0.75 goals per games and are highlighted.
       But if the goal per games stats is the highest for Khadija Shaw, Martha from Brazil with 6 times more  games  shows a much more impressive consistency.\n",
       caption = "@alangel12407606\n#TidyTuesday")


p2
