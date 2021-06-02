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
#Pytanie: chemy stworzyc liste postow o ktorych jest najglosniej, pojawiaja sie one w innych postach.
#Teza jest taka ze po stworzeniu czegos takiego zobaczymy kotore posty maja najwieksza liczbe wiedzy o kawie
#chemy tez zobaczyc czy moze na forum sa jacys kawowi guru, musimy sie czegos o nich dowiedziec
#robimy sobie taki 'indeks cytowan Postow' razem z ich autorami
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
#znajdzmy najlepszych kawowych guru, i dowiedzmy sie czegos o nich
Kawiarze_cytowania <- function(Users=Users,cytowania=cytowania)
{
  uzytkownicy <- Users
  uzytkownicy <- select(uzytkownicy,Id,Reputation,DisplayName,Location,CreationDate)
  colnames(uzytkownicy)[1] <- 'OwnerUserId'
  wynik <- inner_join(cytowania,uzytkownicy,'OwnerUserId')
  wynik <- select(wynik,cytowania,Score,LinksToScoreRatio,OwnerUserId,DisplayName,Location,CreationDate)
  #chcemy wiedziec ile kawiarzy zdobylo jaka sume pkt i ile mieli cytowan.
  
  wynik <- wynik  %>% group_by(OwnerUserId,DisplayName,Location,CreationDate) %>% summarise(OgolneCytowania=sum(cytowania),Wyniki=sum(Score))
  a <- arrange(wynik,desc(OgolneCytowania)) #Dostalismy najbardziej cytowanych 20
  head(a,20)
}
a <- Kawiarze_cytowania(Users=Users,cytowania=cytowania)
Kawiarze_Score <- function(Users=Users,cytowania=cytowania)
{
  uzytkownicy <- Users
  uzytkownicy <- select(uzytkownicy,Id,Reputation,DisplayName,Location,CreationDate)
  colnames(uzytkownicy)[1] <- 'OwnerUserId'
  wynik <- inner_join(cytowania,uzytkownicy,'OwnerUserId')
  wynik <- select(wynik,cytowania,Score,LinksToScoreRatio,OwnerUserId,DisplayName,Location,CreationDate)
  wynik <- wynik  %>% group_by(OwnerUserId,DisplayName,Location,CreationDate) %>% summarise(OgolneCytowania=sum(cytowania),Wyniki=sum(Score))
  a <- arrange(wynik,desc(Wyniki))
  head(a,20)
}
a <- Kawiarze_cytowania(Users=Users,cytowania=cytowania)
zmiana_daty <- function (a=a){
b <-  list(a$CreationDate)
b <- substr(b[[1]],1,4)
b <- as.list(b)
a$CreationDate <- gsub("\\T.*", "", a$CreationDate)
a
}
#rysowanie wykresow : tutaj w zaleznosci od Score 
a <- Kawiarze_Score(Users=Users,cytowania=cytowania)
a <- zmiana_daty(a=a) #zmieniamy daty na nam przyjazne
a <- select(a,OgolneCytowania,Wyniki)
#plot(a$Wyniki,a$OgolneCytowania,xlab='Cytowania',ylab = 'Wyniki')
#al = par("usr")
#segments(al[1], al[3], al[2], al[4], col='blue')
a <- Kawiarze_cytowania(Users=Users,cytowania=cytowania)
a <- zmiana_daty(a=a) #zmieniamy daty na nam przyjazne
#plot(a$OgolneCytowania,a$Wyniki,xlab='Cytowania',ylab = 'Wyniki')
#al = par("usr")
#segments(al[1], al[3], al[2], al[4], col='blue')
#Wyniosek Z tego taki ze najslabiej cytowane posty maja często o wiele lepsze wyniki
#Widzimy ze posty do ktorych ludzie sie odwoluja nie maja duzego wplywu na ich like




#PYTANIE 2
#Chcemy porownać ceny kawy na WallStreet w stosunku do aktywnosci na forum naszych kochanych kawoszy
#uzyjemy paczki danych zawierajacej ceny kawy na przestrzeni lat
Ceny_Kawy <- Coffee_Prices
#znajdzmy teraz ile jakich postow bylo robionych w jakich czasach 
Znajdowanie_wykresu_daty <- function(Posts=Posts)
{
  Posty <- Posts
  Posty$CreationDate <- gsub('.{16}$', '', Posty$CreationDate)
  wynik <- Posty
  wynik <- select(Posty,CreationDate) %>%  group_by(CreationDate) %>% count(CreationDate)
  wynik <- wynik %<>% mutate(CreationDate= as.Date(CreationDate, format= "%Y-%m-%s"))
  colnames(wynik)[2] <- 'LiczbaPostow'
  arrange(wynik,desc(CreationDate))
}
daty<- Znajdowanie_wykresu_daty(Posts=Posts)
rysowanie_aktywnosc <-  function(daty=daty)
{
daty <- daty$LiczbaPostow
daty <- ts(daty, start=c(2015, 1, 1), end=c(2021, 1,1), frequency=12) 
ts_plot(daty,title="Wykres aktywnosci",
        Xtitle="przedzial czasowy",
        Ytitle="ilosc postow w danym dniu",
        )
}
Ceny_Kawy <- Ceny_Kawy
wykres_Ceny <- function(Ceny_Kawy=Ceny_Kawy)
{ts_plot(Ceny_Kawy)}



#zauwazylismy teraz z o dziwo ( kto by sie spodziewal) nie ma korelacji pomiedzy aktywnoscia na forum a cena kawy
#jednak zauwazylismy ze forum umiera, sprawdzmy teraz kiedy nasi spolecznicy ostatnio cos postowali
#Pytanie kiedy nasi najlepsi uzytkownicy ostatnio postowali
a <- Kawiarze_Score(Users=Users,cytowania=cytowania)
last_activity <- function(a=a){
a <- Kawiarze_Score(Users=Users,cytowania=cytowania)
uzytkownicy <- Users
colnames(uzytkownicy)[1] <- "OwnerUserId"
wynik <- inner_join(a,uzytkownicy,"OwnerUserId")
wynik <- select(wynik,DisplayName.x,LastAccessDate)
wynik$LastAccessDate <- gsub('.{19}$', '', wynik$LastAccessDate)
wynik
}

a <- last_activity()
wykres_aktywnosc <- function(a=a)
{a <- a %>% group_by(LastAccessDate) %>% count(LastAccessDate)
a$LastAccessDate <- as.numeric(a$LastAccessDate)
barplot(a$n,names.arg = a$LastAccessDate, xlab = "Lata", ylab = 'Ilosc osob', main = 'Wykres odchodzących ludzi ( top 20 cytowanych) od czasu')}
#widzimy teraz ze ludzie ktorzy sa czesto cytowani odeszli niedawno, sprawdzmy teraz calosc
wykres_odchodzenie_wszyscy <- function(Users=Users)
{
  uzytkownicy <- Users
  uzytkownicy$LastAccessDate <- gsub('.{19}$', '', uzytkownicy$LastAccessDate)
  uzytkownicy <- uzytkownicy %>% group_by(LastAccessDate) %>% count(LastAccessDate)
  barplot(uzytkownicy$n,names.arg = uzytkownicy$LastAccessDate, xlab = "Lata", ylab = 'Ilosc osob', main = 'Wykres odchodzących ludzi (wszystkich ) od czasu')
}
#sprawdzmy teraz ile ludzi ubywa
wykres_ubywania <- function(Users=Users)
{
  uzytkownicy <- Users
  uzytkownicy$LastAccessDate <- gsub('.{16}$', '', uzytkownicy$LastAccessDate)
  uzytkownicy <- uzytkownicy %>% group_by(LastAccessDate) %>% count(LastAccessDate)
  colnames(uzytkownicy) <- c('LastAccessDate','LastSeenAccounts')
  dochodzenie <- Users
  dochodzenie$CreationDate <- gsub('.{16}$', '', dochodzenie$CreationDate)
  dochodzenie <- dochodzenie %>% group_by(CreationDate) %>% count(CreationDate)
  colnames(dochodzenie) <- c('LastAccessDate','NewAccounts')
  wynik <- inner_join(dochodzenie,uzytkownicy,by='LastAccessDate')
  wynik['HowMuchLeft'] <- wynik['NewAccounts'] - wynik['LastSeenAccounts']
  wynik <- wynik$HowMuchLeft
  wynik<- ts(wynik, start=c(2015, 1, 1), end=c(2021, 1,1), frequency=12) 
  ts_plot(wynik,Xtitle="Lata",Ytitle='Ilosc kont ktore ubyly', title='Ubywanie czlonkow')
  
}
#wniosek z tego taki ze to forum umiera. Jednak ci uzytkownicy z 'JADRA' nie odchodza.Popatrzmy jeszcze na reakcje
wykres_komentarze_czas <-function (Votes=Votes)
{
  wynik <- Votes
  wynik <- select(wynik, CreationDate )
  wynik$CreationDate <- gsub('.{16}$', '', wynik$CreationDate)
  wynik <- wynik %>% group_by(CreationDate) %>% count(CreationDate)
  wynik <- wynik$n
  wynik <- ts(wynik, start=c(2015, 1, 1), end=c(2021, 1,1), frequency=12) 
  ts_plot(wynik,Xtitle="Lata",Ytitle='Ilosc komentarzy', title='Komentarze od Czasu')
}
