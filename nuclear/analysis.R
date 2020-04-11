library(tidyverse)
library(ggforce)
library(lubridate)
library(ggchicklet)
library(hrbrthemes)
library(ggtext)
library(waffle)
library(glue)

nuclear_explosions <-
  read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-20/nuclear_explosions.csv")

france_nuclear <- nuclear_explosions %>% 
  filter(country =="FRANCE") 

fr_nucl_explo <- nrow(france_nuclear)

data_plot <- france_nuclear%>% 
  group_by(purpose,type) %>% 
  tally() %>% 
  ungroup() %>% 
  drop_na() 

proportion <- data_plot %>% 
  group_by(purpose) %>% 
  summarise(sum(n))

weapon_r_exp <- proportion %>% 
  filter(purpose=="WR") %>% 
  pull(`sum(n)`)

weapon_e_exp <- proportion %>% 
  filter(purpose=="WE") %>%
  pull(`sum(n)`)

weapon_s_exp <- proportion %>% 
  filter(purpose=="SE") %>% 
  pull(`sum(n)`)

waffle <- data_plot%>%
  ggplot(aes(fill=purpose,values=n))+
  geom_waffle(color="black",size=1.125)+
  facet_wrap(~type,ncol=4)+
  scale_fill_manual(name=NULL,
                    values = c(
                    "SE"="#4859D4",
                    "WE"="#FFFFFF",
                    "WR"="#DE0000"),
                    labels = c(
                      SE = "<i style='color:#4859D4'>Test to determine the safety in case of accident</i>",
                      WE = "<i style='color:#FFFFFF'>Effects of a nuclear detonation </i>",
                      WR = "<i style='color:#DE0000'>Weapon-related</i>"))+
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0)) +
  coord_equal() +
  theme_ft_rc(grid="")+
  labs(x="",
       y="",
       caption = "#Tidy tuesday | Source: Stockholm International Peace Research Institute | @alangel12407606",
       subtitle = glue("Among the **{fr_nucl_explo}** french nuclear explosions, <span style='color:#DE0000'>**{weapon_r_exp}**</span> are weapon-related, <span style='color:#4859D4'>**{weapon_s_exp}**</span> were used to determine the <br> safety in case of accident and <span style='color:#FFFFFF'>**{weapon_e_exp}**</span> were conducted to observe the effect of detonations"),
       title = "Type and purpose of the French nuclear explosions")+
  theme_enhance_waffle()+
  theme(legend.position = "top",
        legend.text = element_markdown(size=11),
        plot.title = element_text(size=24),
        plot.subtitle = element_markdown(size=14),
        plot.caption = element_text(color="#FFFFFF",size=11))

ggsave("plot.png",plot = waffle,dpi=400,width = 16,height = 11)
