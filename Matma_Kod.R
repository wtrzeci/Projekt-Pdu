library(xml2)
library(dplyr)
library(leaflet)
library(maps)
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
ranking <- read.csv('cwurData.csv')
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
aktywnosc_ludzie <- function (Users=Users ,Comments=Comments){
  uzytkownicy <- select(Users,Id,DisplayName,UpVotes,DownVotes,Location)
  komentarze <- select (Comments, UserId,Id)
  wynik <- komentarze %>% group_by(UserId) %>% count(UserId)
  colnames(wynik) <- c('Id','CommentCount')
  wynik <- inner_join(wynik,uzytkownicy, by= 'Id')
  wynik['Activity'] <- wynik['UpVotes'] + wynik["DownVotes"]+wynik['CommentCount']
  wynik <- select (wynik,Id,DisplayName,Location,CommentCount,UpVotes,DownVotes,Activity)
  wynik <- arrange(wynik,desc(Activity))
  wynik
}
aktywnosc <- aktywnosc_ludzie(Users=Users ,Comments=Comments)
znajdowanie_lokalizacji <- function(aktywnosc=aktywnosc)
{
  aktywnosc <- head(aktywnosc,1000)
  aktywnosc$Location <- sub(",([^,]*)$"," &\\1", aktywnosc$Location)
  aktywnosc$Location <- gsub(".*&","",aktywnosc$Location)
  aktywnosc$Location <- gsub("^\\s+|\\s+$", "", aktywnosc$Location)
  aktywnosc$Location[aktywnosc$Location == 'USA'| aktywnosc$Location == 'NY'] <- 'United States'
  aktywnosc$Location[aktywnosc$Location == 'CA'] <- 'Canada'
  aktywnosc$Location[aktywnosc$Location == 'UK'] <- 'United Kingdom'
  wynik <- select(aktywnosc, Location,Id) %>% group_by(Location) %>% count(Location)
  wynik <- arrange(wynik,desc(n))
  wynik <- wynik[-1,]
  wynik <- head(wynik,3)
  barplot(wynik$n,names.arg = wynik$Location)
}
znajdowanie_lokalizacji(aktywnosc=aktywnosc)
#zobaczmy jeszcze ogolne wykresy lokalizacji 
znajdowanie_lokalizacji_pelna<- function(aktywnosc=aktywnosc)
{
  aktywnosc$Location <- sub(",([^,]*)$"," &\\1", aktywnosc$Location)
  aktywnosc$Location <- gsub(".*&","",aktywnosc$Location)
  aktywnosc$Location <- gsub("^\\s+|\\s+$", "", aktywnosc$Location)
  aktywnosc$Location[aktywnosc$Location == 'United States'| aktywnosc$Location == 'NY'] <- 'USA'
  aktywnosc$Location[aktywnosc$Location == 'CA'] <- 'Canada'
  aktywnosc$Location[aktywnosc$Location == 'United Kingdom'] <- 'UK'
  wynik <- select(aktywnosc, Location,Id) %>% group_by(Location) %>% count(Location)
  wynik <- arrange(wynik,desc(n))
  wynik <- wynik[-1,]
  colnames(wynik)[1] <- 'Country'
  colnames(wynik)[2] <- 'NumberOfUsers'
  wynik
}
wynik <- znajdowanie_lokalizacji_pelna(aktywnosc=aktywnosc)
wynik <- head(wynik,3)
barplot(wynik$NumberOfUsers,names.arg = wynik$Country)
#widzimy ze te Indie to jest taka mniej aktywna nacja, jednak ma duzo uzytkownikow
#sprobujemy przedstawic dane na mapie 
wynik <- znajdowanie_lokalizacji_pelna(aktywnosc = aktywnosc)
data(world.cities)
rysowanie_mapy <- function(wynik=wynik)
{mapy <- world.cities %>%
  filter(capital == 1) %>%
  dplyr::select(Country = country.etc, lat, lng = long)
mapy <- inner_join(mapy,wynik,by='Country')
mapy <- arrange(mapy,desc(NumberOfUsers))
mapy <- head(mapy, 20)
leaflet(mapy)%>%
  addTiles()%>%
  addMarkers(label = ~NumberOfUsers) }
#rysujemy ile osob pochodzi z jakich krajow
rysowanie_mapy(wynik = wynik )
#badamy lokalizacje najaktywniejszych
rysowanie_mapy(wynik = head(wynik,1000) )
#chcemy to teraz porownac z mapa topowych uniwersytetow, dane zaczerpniete z 
#rankingu Center for World University Rankings
zamiana_
uniwersytety <- select(ranking, world_rank, country) %>% filter(world_rank<=150)%>%
group_by(country) %>% count(country)
colnames(uniwersytety) <- c('Country','NumberOfUsers')
rysowanie_mapy(wynik = uniwersytety )
 

