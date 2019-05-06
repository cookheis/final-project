library(tidyverse)
library(rvest)
library(lubridate)
library(stringr)
library(imager)

cleaner <- function(x){
  str_replace_all(x, " \\([0-9]+\\)|(\n)", "")
}

cleaner <- function(x){
  str_replace_all(x, "(^[ \t]+|[ \t]+$)|(\n)", "")
}

stat <- "https://www.leagueofgraphs.com/champions/builds"  %>% read_html() %>% html_table(fill=TRUE) %>% .[[1]]

stat_clean <- map_df(stat, cleaner)

stat_clean <-  stat_clean[-c(16,17,33,49,65,81,97,113,129,145),-c(1,8,9,10)]


stat_clean <- stat_clean %>% separate(`Popularity`, into = c('Popularity', 'Popularity2','Popularity3'), sep = ' ')
stat_clean <- stat_clean %>% separate(`Winrate`, into = c('Winrate', 'Winrate2','Winrate3'), sep = ' ')
stat_clean <- stat_clean %>% separate(`BanRate`, into = c('BanRate', 'BanRate2','BanRate3'), sep = ' ')
stat_clean <-  stat_clean[,-c(3,4,6,7,9,10)]
stat_clean[,7]=stat_clean[,1]
colnames(stat_clean)=c("Name","Popularity","Winrate","BanRate","KDA","Pentas/match","position")
stat_clean[,1][[1]] <-gsub("AD Carry|Mid|,|Top|Jungler|Support", "", stat_clean[,1][[1]])
for(i in 1:143){stat_clean[i,7][[1]] <-gsub(stat_clean[i,1][[1]], "", stat_clean[i,7][[1]])}
stat_clean[,7][[1]]=gsub(" ", "", stat_clean[,7][[1]])
a=gsub(" ", "", stat_clean[,1][[1]])
for(i in 1:143){stat_clean[i,7][[1]] <-gsub(a[i], "", stat_clean[i,7][[1]])}
stat_clean##the cleaned table




ggplot(data=stat_clean[1:20,],mapping=aes(x=Name,y=Popularity,fill=Name,group=factor(1)))+
  geom_bar(stat="identity")

ggplot(data=stat_clean[1:20,],mapping=aes(x=Name,y=Winrate,fill=Name,group=factor(1)))+
  geom_bar(stat="identity")

ggplot(data=stat_clean[1:20,],mapping=aes(x=Name,y=BanRate,fill=Name,group=factor(1)))+
  geom_bar(stat="identity")

##input a name in lower case for example
## print a table about Role Popularity and Winrate
champion="kaisa"
stat1 <-  paste0("https://www.leagueofgraphs.com/champions/stats/",champion)  %>% read_html() %>% html_table(fill=TRUE)
data1=stat1[[1]]
data1 <- data1 %>% separate(`Popularity`, into = c('Popularity', 'Popularity2','Popularity3'), sep = ' ')
data1 <- data1 %>% separate(`Winrate`, into = c('Winrate', 'Winrate2','Winrate3'), sep = ' ')
data1 <-  data1[,-c(3,4,6,7)]
data1
## print a table about the best player of this champion
data2=stat1[[2]]

q<-"https://www.leagueofgraphs.com/rankings/summoners/kaisa" %>% read_html() %>% html_table(fill=TRUE)
q1=q[[1]]
data2

##print the best with the champion
stat2 <-  paste0("https://www.leagueofgraphs.com/champions/counters/",champion)  %>% read_html() %>% html_table(fill=TRUE)
data3=stat2[[1]]
data3
##print the best against the champion
data4=stat2[[2]]
data4
## print the countered champion
data5=stat2[[3]]
data5


##find the player's stats
##overview of RECENT RECORDS
player="cookheis"
stat3 <-  paste0("https://lol.mobalytics.gg/summoner/na/",player,"/overview?season=13")  %>% read_html() %>% html_table(fill=TRUE)
data6=stat3[[1]]

##personal rating
stat4 <-  paste0("https://www.leagueofgraphs.com/summoner/na/",player)  %>% read_html()%>% html_table(fill=TRUE)
data7=stat4[[1]]
data7

##most played champions in rank
data8=stat4[[4]]
data8