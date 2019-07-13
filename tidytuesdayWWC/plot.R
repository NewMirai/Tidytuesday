library(readr)
library(tidyverse)
library(ggridges)
library(viridis)
library(ggthemes)
library(hrbrthemes)
library(ggThemeAssist)

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

