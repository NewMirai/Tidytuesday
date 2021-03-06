library(tidyverse)
library(ggforce)
library(ggtext)
library(glue)
library(extrafont)
library(showtext)
library(repurrrsive)
library(patchwork)
library(lubridate)
library(ggtext)
library(waffle)
library(paletteer)
library(hrbrthemes)
library(gganimate)

horror_movies <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-22/horror_movies.csv")



# add fonts

font_add_google(name = "Fjalla One",family = "fj")
font_add_google(name = "Permanent Marker",family = "pm")
font_add_google(name = "Open Sans",family = "os") 

# activate fonts

showtext_auto()



horror_cleaned <- horror_movies %>% 
  select(-cast,-filming_locations,-movie_rating,-plot)


horror_cleaned <- horror_cleaned %>% 
  mutate(release_date= dmy(release_date),
         release_country=as.factor(release_country),
         year=year(release_date)) 

top5_countries_year <- horror_cleaned %>% 
  count(year,release_country) %>% 
  drop_na() %>% 
  arrange(year,-n) %>% 
  group_by(year) %>% 
  slice(1:5) %>% 
  ungroup()



top5_countries_year_2012_usa <- top5_countries_year %>% 
  filter(year == 2012,release_country =="USA") %>% 
  tally(n) %>% 
  pull(n)

top5_countries_year_2017_usa <- top5_countries_year %>% 
  filter(year == 2017,release_country =="USA") %>% 
  tally(n) %>% 
  pull(n)


p1 <- top5_countries_year %>% 
  ggplot(aes(fill=release_country,values= n))+
  geom_waffle(color="white",size=.25,n_rows = 10,flip = T)+
  facet_wrap(~year, nrow= 1, strip.position = "bottom")+
  scale_x_discrete()+
  scale_y_continuous(labels = function(x) x*10,
                     expand = c(0,0))+
  scale_fill_paletteer_d(package = ggsci,palette = dark_uchicago,name=NULL)+
  coord_equal()+
  labs(title = glue("USA is dominating the industry of  <span style='color:#BD0707'>**horror movies**</span> <br>"),
       subtitle =glue("Shown below is a waffle chart showing the top 5 countries
                                by release of horror movies for each year from 2012 to 2017.<br>
                                In 2012, USA released <span style='color:#BD0707'>**{top5_countries_year_2012_usa}**</span> horror movies
                                against <span style='color:#BD0707'>**{top5_countries_year_2017_usa}**</span> in 2017. 
                                It's nearly <span style='color:#BD0707'>**2.5 times**</span> more horrors movies in <span style='color:#BD0707'>**5 years**</span> which have been produced!!<br>
                                Other countries seems to have stabilized their number of horrors movies produced."),
       caption = "#Tidy Tuesday | Source: IMDB Horror Movie Dataset (Kaggle)| Author: @alangel112407606 inspired by https://github.com/hrbrmstr/waffle")+
  theme_minimal(base_family = "fj")+
  theme(panel.grid = element_blank(), 
        axis.ticks.y= element_line(),
        plot.title = element_markdown(size=24,family ="pm"),
        plot.subtitle = element_markdown(size= 12,family = "os"))
  
x11(width = 1920 ,height = 1080)

p1


