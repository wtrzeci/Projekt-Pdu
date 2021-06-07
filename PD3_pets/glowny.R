Users<- read.csv('Users.xml.csv')
Votes <- read.csv ('Votes.xml.csv')
Tags <- read.csv('Tags.xml.csv')
Posts <- read.csv("Posts.xml.csv")
PostHistory <- read.csv("PostHistory.xml.csv")
PostLinks <- read.csv('PostLinks.xml.csv')
Comments <- read.csv('Comments.xml.csv')
Badges <- read.csv('Badges.xml.csv')

# Załadujmy potrzebne biblioteki
library(dplyr)
library(sqldf)
library(data.table)
library(knitr)
library(TSstudio)


#### Pytanie 1 #####
# Wszystkie serwisy Stack Exchenge są tworzone przez nikogo innego jak
# użytkowników. Jak wiadomo wkład poszczególnych członków społeczności jest
# niezwykle zróżnicowany. Są więc ustalone pewne kryteria, dzięki którym 
# możemy wyłonić najważniejszych użytkowników, którzy mają ogromny wpływ
# na działanie serwisu. Domyślnie użytkownicy są porównywani przy pomocy
# reputacji. Teoretycznie im większa reputacja, tym większym zaufaniem jest
# obdarzony użytkownik i tym większy jest jego wkład w rozwój serwisu.
# Co jednak gdy nieco zmienimy kryteria wyboru najlepszych członków społeczności?

# Na początku zobaczmy 10 użytkowników z największą reputacją.

najlepsi_reputacja <- function(Users){
  sqldf::sqldf("SELECT DisplayName, Reputation
                FROM Users
                ORDER BY Reputation DESC 
                LIMIT 10")
}

naj_rep <- najlepsi_reputacja(Users)
knitr::kable(naj_rep)

# Znajdźmy 10 użytkowników z największą ilością odznak - przyznawanych
# za osiągnięcia na forum.

najlepsi_odznaki <- function(Badges, Users){
  ile_odznak <- sqldf::sqldf("SELECT DisplayName, SumaWszystkie
         FROM
             (
                 SELECT COUNT(*) as SumaWszystkie, UserId
                 FROM Badges
                 GROUP BY UserId
             ) AS Tab1
         JOIN
             (
                 SELECT DisplayName, Id
                 FROM Users
             ) AS Tab2
             ON Tab1.UserId = Tab2.Id
         ORDER BY SumaWszystkie DESC
         LIMIT 10")
}

naj_odz <- najlepsi_odznaki(Badges, Users)
knitr::kable(naj_odz)

# Znajdźmy 10 użytkowników z największą ilością złotych odznak - przyznawanych
# za duże osiągnięcia na forum.

najlepsi_zlote_odznaki <- function(Badges){
  ile_odznak <- sqldf::sqldf("SELECT DisplayName, SumaZlote
         FROM
             (
                 SELECT COUNT(*) as SumaZlote, UserId
                 FROM Badges
                 WHERE Badges.Class = 1
                 GROUP BY UserId
             ) AS Tab1
         JOIN
             (
                 SELECT DisplayName, Id
                 FROM Users
             ) AS Tab2
             ON Tab1.UserId = Tab2.Id
         ORDER BY SumaZlote DESC
         LIMIT 10")
}

naj_zl_odz <- najlepsi_zlote_odznaki(Badges)
knitr::kable(naj_zl_odz)

# Zobaczmy czyje pytania najbardziej przyciągnęły internautów do serwisu Pets.
# W tym celu znajdźmy sumę wyświetleń opublikowanych postów dla użytkowników.

najlepsi_wyswietlenia <- function(Users, Posts){
  sqldf::sqldf("
  SELECT
      Users.DisplayName, Tab.ViewScore
  FROM (
          SELECT
             OwnerUserId,
             SUM(ViewCount) AS ViewScore
          FROM Posts
          WHERE PostTypeId = 1
          GROUP BY OwnerUserId
  ) AS Tab
  JOIN Users ON Users.Id = Tab.OwnerUserId
  ORDER BY Tab.ViewScore DESC
  LIMIT 10")
}

naj_wys <- najlepsi_wyswietlenia(Users, Posts)
knitr::kable(naj_wys)

# Możemy zauważyć, że wielu użytkowników się pojawia w więcej niż jednej tabeli.
# Wynika stąd, że aktywność najbardziej cenionych członków społeczności Pets
# musi być wszechstronna.

# Połączmy otrzymane tabele
polaczona <- dplyr::full_join(naj_rep, naj_odz, by = "DisplayName")
polaczona2 <- dplyr::full_join(polaczona, naj_zl_odz, by = "DisplayName")
polaczona3 <- dplyr::full_join(polaczona2, naj_wys, by = "DisplayName")
knitr::kable(polaczona3)

# Znajdźmy osoby, które znalazły się we wszystkich 4 rankingach popularności
wybrani <- dplyr::filter(polaczona3, !is.na(Reputation) & !is.na(SumaWszystkie) & !is.na(SumaZlote) & !is.na(ViewScore))
knitr::kable(wybrani)

# Jak widzimy są 4 osoby, które znalazły się we wszystkich rankingach popularności.



