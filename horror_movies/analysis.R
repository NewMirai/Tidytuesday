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
library(emojifont)
library(paletteer)
library(hrbrthemes)
library(png)
library(magick)


horror_movies <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-22/horror_movies.csv")



# add fonts

font_add_google(name = "Fjalla One",family = "fj")
font_add_google(name = "Permanent Marker",family = "pm")
font_add_google(name = "Open Sans",family = "os") 

# activate fonts

showtext_auto()

resume_data <- skimr::skim_to_wide(horror_movies)


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
       subtitle = str_wrap(glue("Shown below is a waffle chart showing the top 5 countries
                                by release of horror movies for each year from 2012 to 2017. 
                                In 2012, USA released {top5_countries_year_2012_usa} horror movies
                                against {top5_countries_year_2017_usa} in 2017. It's nearly 2.5 times more horrors movies in 5 years which have been produced!!
                                Other countries seems to stabilize their number of horrors movies produced."),width = 100),
       caption = "#Tidy Tuesday | Source: IMDB Horror Movie Dataset (Kaggle)| Author: @alangel112407606 inspired by https://github.com/hrbrmstr/waffle")+
  theme_minimal(base_family = "fj")+
  theme(panel.grid = element_blank(), 
        axis.ticks.y= element_line(),
        plot.title = element_markdown(size=24,family ="pm"),
        plot.subtitle = element_text(size= 14,family = "os"))
  
x11(width = 1600 ,height = 900)

p1
