library(data.table)
library(ggplot2)
library(magrittr)
library(ggchicklet)
library(showtext)
library(forcats)
library(paletteer)
library(ggforce)
library(glue)
library(ggalt)
library(ggtext)
library(gganimate)
library(av)

options(scipen = 999)

cran_code <- fread("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-12/loc_cran_packages.csv")

font_add_google(name = "Oswald",family = "ow")
font_add_google(name = "Open Sans",family = "os") 

showtext_auto()


top_5n <- first(cran_code[,.N, by=.(language)],n = 5)[[1]]



plot_data <- cran_code[complete.cases(cran_code),
                       ][,c("language","blank","comment","code")
                         ][,`:=`(blank=mean(blank),comment=mean(comment),code=mean(code)),by="language"
                           ][language %in% top_5n,first(.SD),by="language"]

plot_data <- melt(plot_data,id.vars = 1)

plot_data <- plot_data[order(-value)]

plot_data$language <- fct_relevel(plot_data$language,"Markdown","HTML","R","C++")

p <-  plot_data %>%
      ggplot(aes(x=language,y=value))+
      geom_point(aes(colour=variable,group=1L),size=7)+
      geom_hline(yintercept = mean_code,color="red",size=1.1,linetype=1)+
      annotate_textp(label = "Below average",x = 0.65,y=1,color = "#F0D773",size = 14,family ="os")+
      annotate_textp(label = "Above average",x = 0.79,y=1,color = "#F0D773",size = 14,family ="os")+
      annotate_textp(label = "Average # lines of codes",x = 0.9,y=.18,color = "red",size = 18,family ="os")+
      coord_flip()+
      theme_minimal()+
      scale_color_manual(values = c("blank"= "#D6D6CE","comment"="#10C719","code"="#FFB547"))+
      labs(x="",
           y="# lines",
           title = "Code in CRAN Packages",
           subtitle = glue("Shown below is a comparison between the 5 most used programming language in CRAN of their **average key characteristics**.<br>
                           Using this **dot plot** we can compare lines of <span style='color:#F0D773'>codes</span>,
                           <span style='color:#10C719'>comment</span> and <span style='color:#D6D6D6'>**blank**</span> accross these languages.<br>"),
           caption="Data: CRAN provided by Phillip Massicotte | Visualisation by : @alangel12407606",
           color="Average characteristics")+
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x=element_blank(),
            axis.line.y = element_line(colour ="#D6D6D6",linetype = 1),
            plot.caption = element_text(colour = "#D6D6D6",size=10,family = "os"),
            plot.title = element_text(colour ="#D6D6D6",size=16,family = "ow"),
            plot.subtitle = element_markdown(colour ="#D6D6D6",size=13,family = "os"),
            panel.grid.major.y = element_line(colour ="#D6D6D6",linetype = 2),
            legend.text = element_text(colour = "#D6D6D6",size=12,family = "os"),
            legend.title = element_text(colour = "#D6D6D6",family = "ow",size=14),
            axis.title.x = element_text(colour = "#D6D6D6",size=14,vjust = -1),
            axis.text.x = element_text(colour = "#D6D6D6",size=10),
            axis.text.y = element_text(colour = "#D6D6D6",size=14),
            panel.background = element_rect("#000000"),
            plot.background = element_rect("#000000"))


anim <- p+transition_states(variable,
                    transition_length = 2,
                    state_length = 1)

animate(anim,width=1920,height=1080,res=100,fps = 60)

anim_save('out/plot.gif')
