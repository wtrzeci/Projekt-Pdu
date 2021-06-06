library(xml2)
library(dplyr)
library(magrittr)
library(kableExtra)
library(TSstudio)
library(sqldf)
library(dplyr)
library(data.table)
library(compare)
library(ggplot2)
library(XML)
library(data.table)
library(tidyverse)
data(USgas)
Users<- read.csv('Users.xml.csv')
Votes <- read.csv ('Votes.xml.csv')
Tags <- read.csv('Tags.xml.csv')
Posts <- read.csv("Posts.xml.csv")
PostLinks <- read.csv('PostLinks.xml.csv')
Comments <- read.csv('Comments.xml.csv')
Badges <- read.csv('Badges.xml.csv')
#Pomysl na analizke: zrobimy wykresy aktywnosci, porownamy z terminami sesjii
#Porownamy post z najloepszymi tagami do innyhch
Cytowania <- function(links=PostLinks,Posts=Posts) {
  iloscpolaczen <-select(links,PostId,RelatedPostId,LinkTypeId) %>% filter(LinkTypeId!=3) %>% group_by(PostId) %>%  count(PostId)
  iloscpolaczen2 <- select(links,PostId,RelatedPostId,LinkTypeId) %>% filter(LinkTypeId!=3) %>% group_by(RelatedPostId) %>%  count(RelatedPostId)
  colnames(iloscpolaczen2)[1]<- 'PostId'
  wynik <- merge(iloscpolaczen,iloscpolaczen2,by='PostId',na.remove= T)
  colnames(wynik)[2] <- 'link1'
  colnames(wynik)[3] <- 'link2'
  wynik['cytowania'] <- wynik[2]+wynik[3]
  wynik <- select(wynik,PostId,cytowania)
  Posty <- select(Posts,Id,Score,ViewCount,Title,OwnerUserId)
  colnames(wynik)[1] <- 'Id'
  wynik <- merge(wynik,Posty,'Id')
  wynik <- arrange(wynik,desc(cytowania))
  wynik['LinksToScoreRatio'] <- wynik['cytowania']/wynik['Score']
  wynik 
}
cytowania<- Cytowania(links=PostLinks,Posts=Posts)
#chcemy okreslic najbardziej aktywnych matematykow, zrobimy to sumujac ich posty, komentarze, głosy
aktywność_ludzie <- function (Users=Users ,Comments=Comments){
  uzytkownicy <- select(Users,Id,DisplayName,UpVotes,DownVotes,Location)
  komentarze <- select (Comments, UserId,Id)
  wynik <- komentarze %>% group_by(UserId) %>% count(UserId)
  colnames(wynik) <- c('Id','CommentCount')
  wynik <- inner_join(wynik,uzytkownicy, by= 'Id')
  wynik['Activity'] <- wynik['UpVotes'] + wynik["DownVotes"]+wynik['CommentCount']
  wynik <- select (wynik,Id,DisplayName,Location,CommentCount,UpVotes,DownVotes,Activity)
  wynik <- arrange(wynik,desc(Activity))
  wynik <- head(wynik,10)}

cytowania<- Cytowania(links=PostLinks,Posts=Posts)