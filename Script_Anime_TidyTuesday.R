#Obtención de la data
tidy_anime <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-23/tidy_anime.csv")

#Inspección General
str(tidy_anime)
summary(tidy_anime)
table(tidy_anime$genre)
shoujoai<-tidy_anime[tidy_anime$genre=="Shoujo Ai",]
shoujoai[,1:3]
view(shoujoai)

#Un dataset más reducido (con el que se trabajará)
shoujoai_r<-shoujoai[,c(1,2,17,18,20,21,23)]
summary(shoujoai_r)
dim(shoujoai_r)
df_shoujoai<-as.data.frame(shoujoai_r)
df_shoujoai
df_shoujoai<-na.omit(df_shoujoai)
df_shoujoai<-unique(df_shoujoai)
summary(df_shoujoai)
summary(df_shoujoai[,4:7])

#Correlaciones
library(ggplot2)
library(GGally)
ggcorr(df_shoujoai[,4:7],label=TRUE)
ggcorr(df_shoujoai[,4:7],label=TRUE,method = c("pairwise", "spearman"))

#Take this Lollipop
library(tidyverse)
x<-factor(df_shoujoai[,2])
y<-df_shoujoai[,4]
data<-data.frame(x,y)
#No alcanzan todos en el arca de Noé
data<-data[-c(35,23,13,33,61,34,14,20,39,48,47,64,60,56,55,42,25,31,3,7,8,37,40,44,46,
              10,17,27,38,50,51,54,57,63),]
p = ggplot(data, aes(x=x, y=y)) +
  geom_segment( aes(x=x, xend=x, y=0, yend=y ), color=ifelse(data$x %in% c("Yagate Kimi ni Naru","Citrus"), "orange", "grey"), size=ifelse(data$x %in% c("Yagate Kimi ni Naru","Citrus"), 1.3, 0.7) ) +
  geom_point( color=ifelse(data$x %in% c("Yagate Kimi ni Naru","Citrus"), "orange", "grey"), size=ifelse(data$x %in% c("Yagate Kimi ni Naru","Citrus"), 5, 2) ) +
  theme_light() +
  coord_flip() +
  theme(
    legend.position="none",
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  xlab("") +
  ylab("Score (MyAnimeList)")
p

y<-df_shoujoai[,5]
data<-data.frame(x,y)
#No alcanzan todos en el arca de Noé
data<-data[-c(35,23,13,33,61,34,14,20,39,48,47,64,60,56,55,42,25,31,3,7,8,37,40,44,46,
              10,17,27,38,50,51,54,57,63),]
q = ggplot(data, aes(x=x, y=y)) +
  geom_segment( aes(x=x, xend=x, y=0, yend=y ), color=ifelse(data$x %in% c("Yagate Kimi ni Naru","Citrus"), "orange", "grey"), size=ifelse(data$x %in% c("Yagate Kimi ni Naru","Citrus"), 1.3, 0.7) ) +
  geom_point( color=ifelse(data$x %in% c("Yagate Kimi ni Naru","Citrus"), "orange", "grey"), size=ifelse(data$x %in% c("Yagate Kimi ni Naru","Citrus"), 5, 2) ) +
  theme_light() +
  coord_flip() +
  theme(
    legend.position="none",
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  xlab("") +
  ylab("Rank (MyAnimeList)")
q

y<-df_shoujoai[,6]
data<-data.frame(x,y)
#No alcanzan todos en el arca de Noé
data<-data[-c(35,23,13,33,61,34,14,20,39,48,47,64,60,56,55,42,25,31,3,7,8,37,40,44,46,
              10,17,27,38,50,51,54,57,63),]
r = ggplot(data, aes(x=x, y=y)) +
  geom_segment( aes(x=x, xend=x, y=0, yend=y ), color=ifelse(data$x %in% c("Yagate Kimi ni Naru","Citrus"), "orange", "grey"), size=ifelse(data$x %in% c("Yagate Kimi ni Naru","Citrus"), 1.3, 0.7) ) +
  geom_point( color=ifelse(data$x %in% c("Yagate Kimi ni Naru","Citrus"), "orange", "grey"), size=ifelse(data$x %in% c("Yagate Kimi ni Naru","Citrus"), 5, 2) ) +
  theme_light() +
  coord_flip() +
  theme(
    legend.position="none",
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  xlab("") +
  ylab("Popularity (MyAnimeList)")
r

y<-df_shoujoai[,7]
data<-data.frame(x,y)
#No alcanzan todos en el arca de Noé
data<-data[-c(35,23,13,33,61,34,14,20,39,48,47,64,60,56,55,42,25,31,3,7,8,37,40,44,46,
              10,17,27,38,50,51,54,57,63),]
s = ggplot(data, aes(x=x, y=y)) +
  geom_segment( aes(x=x, xend=x, y=0, yend=y ), color=ifelse(data$x %in% c("Yagate Kimi ni Naru","Citrus"), "orange", "grey"), size=ifelse(data$x %in% c("Yagate Kimi ni Naru","Citrus"), 1.3, 0.7) ) +
  geom_point( color=ifelse(data$x %in% c("Yagate Kimi ni Naru","Citrus"), "orange", "grey"), size=ifelse(data$x %in% c("Yagate Kimi ni Naru","Citrus"), 5, 2) ) +
  theme_light() +
  coord_flip() +
  theme(
    legend.position="none",
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  xlab("") +
  ylab("Favorites (MyAnimeList)")
s

#Por age rating

ggplot(df_shoujoai, aes(x=score, y=favorites, color=rating)) + 
  geom_point(size=6, alpha=0.6)

ggplot(df_shoujoai, aes(x=score, y=popularity, color=rating)) + 
  geom_point(size=6, alpha=0.6)

ggplot(df_shoujoai, aes(x=score, y=rank, color=rating)) + 
  geom_point(size=6, alpha=0.6)

ggplot(df_shoujoai, aes(x=rank, y=favorites, color=rating)) + 
  geom_point(size=6, alpha=0.6)
