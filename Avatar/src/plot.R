library(data.table)
library(ggplot2)
library(skimr)
library(tvthemes)
library(magrittr)
library(forcats)
library(glue)
library(showtext)

# Read data
avatar <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-11/avatar.csv')

# Fonts
import_avatar()
font_add_google(name="Open Sans",family="os")
font_add_google(name="Oswald",family="ow")
showtext_auto()

# Extract top 10 chapter by imdb rating for each book
top_10_chapter <- unique(avatar,by = "chapter")[,.SD,.SDcols=c("book","chapter","imdb_rating")
                                      ][order(-imdb_rating),first(.SD,10),by="book"
                                        ][,chapter]

# Extract top 5 rank of the most speaking character in each chapter
plot_dt <- avatar[chapter %chin% top_10_chapter & character != 	"Scene Description"
                    ][,.N,by=.(book,chapter,character,imdb_rating)
                      ][order(-N),first(.SD,5),by="chapter"
                        ][,rank:=frank(-N,ties.method = "min"),by=.(book,chapter)
                          ][,`:=`(chapter = fct_reorder(chapter,.x = imdb_rating),
                                  rank = fct_reorder(as.factor(rank),.x=rank))]
# Adding labels
plot_dt[,label_text:=glue("{plot_dt[,character]}: {plot_dt[,N]} asides")]

plot_dt_fire <- plot_dt[book=="Fire"]

plot_dt_fire %>% 
  ggplot(aes(y=N,x=chapter,fill=rank))+
  geom_bar(position = "identity",stat = "identity")+
  guides(fill=guide_legend(reverse = F))+
  labs(fill="Aside rank",y="# of asides")+
  scale_y_continuous(position = "right")+
  coord_flip()+
  scale_fill_avatar()+
  theme_void()+
  theme(legend.position = "top",
        axis.text = element_text(colour="black"),
        axis.text.y = element_text(size = 20,family = "slayer"),
        axis.text.x = element_text(vjust = 3,family = "os"),
        legend.margin = margin(20,0,20,0),
        legend.title = element_text(margin = margin(0,15,0,0),family = "os"),
        panel.grid.major.x = element_line(colour = "#DEDEDE",size = 0.5,linetype = "dashed"))