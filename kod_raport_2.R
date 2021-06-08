Users<- read.csv('Users.xml.csv')
Posts <- read.csv("Posts.xml.csv")
Badges <- read.csv('Badges.xml.csv')
Posts_math <- read.csv('Posts_math.xml.csv')
PostsMath <- read.csv("Posts_math.xml.csv")
PostsKawa <- read.csv("Posts_kawa.xml.csv")
PostsPets <- read.csv("Posts.xml.csv")
Spolecznosciowe <- read.csv("users-by-social-media-platform.csv")

# Załadujmy potrzebne biblioteki
library(dplyr)
library(sqldf)
library(data.table)
library(knitr)
library(TSstudio)
library(mosaic)
library(xtable)
library(kableExtra)
library(tidyr)

najlepsi_reputacja <- function(Users){
  sqldf::sqldf("SELECT DisplayName, Reputation
                FROM Users
                ORDER BY Reputation DESC 
                LIMIT 10")
}


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


najlepsze_wszystko <- function(naj_odz, naj_rep, naj_wys, naj_zl_odz){
  
  # Połączmy otrzymane tabele
  polaczona <- dplyr::full_join(naj_rep, naj_odz, by = "DisplayName")
  polaczona2 <- dplyr::full_join(polaczona, naj_zl_odz, by = "DisplayName")
  polaczona3 <- dplyr::full_join(polaczona2, naj_wys, by = "DisplayName")
  knitr::kable(polaczona3)
  
  # Znajdźmy osoby, które znalazły się we wszystkich 4 rankingach popularności
  wybrani <- dplyr::filter(polaczona3, !is.na(Reputation) & !is.na(SumaWszystkie) & !is.na(SumaZlote) & !is.na(ViewScore))
  wybrani
}



#### Answer Ratio ####

tabelka_questions_answers <- function(Posts){
  posts_new <- Posts
  posts_new$CreationDate <- gsub('.{19}$', '', posts_new$CreationDate)
  pytania <- sqldf("
                 SELECT COUNT(*) as Questions, CreationDate as Year
                 FROM posts_new
                 WHERE posts_new.PostTypeId = 1
                 GROUP BY CreationDate")
  odpowiedzi <- sqldf("
                 SELECT COUNT(*) as Answers, CreationDate as Year
                 FROM posts_new
                 WHERE posts_new.PostTypeId = 2
                 GROUP BY CreationDate
                      ")
  tabelka <- merge(pytania, odpowiedzi, by = "Year")
  
  # Tworzymy kolumne AnswerRatio
  tabelka["AnswerRatio"] <- tabelka["Answers"] / tabelka["Questions"]
  tabelka
}

questions_answers_per_year_pets <- function(Posts){
  
  tabelka <- tabelka_questions_answers(Posts)
  
  ggplot(tabelka, aes(x=Year, y=Quantity, group = 1)) + 
    geom_line(aes(y = Questions, color = "Questions"), size = 1.2) + 
    geom_point(aes(y = Questions, color = "Questions"), size = 1.6) +
    geom_line(aes(y = Answers, color= "Answers"), size = 1.2) +
    geom_point(aes(y = Answers, color= "Answers"), size = 1.6) +
    geom_line(aes(y = AnswerRatio, color  = "AnswerRatio"), size = 1.2) +
    geom_point(aes(y = AnswerRatio, color  = "AnswerRatio"), size = 1.6)
}

questions_answers_per_year_math <- function(Posts_math){
  tabelka <- tabelka_questions_answers(Posts_math)
  
  ggplot(tabelka, aes(x=Year, y=Quantity, group = 1)) + 
    geom_line(aes(y = Questions, color = "Questions"), size = 1.2) + 
    geom_point(aes(y = Questions, color = "Questions"), size = 1.6) +
    geom_line(aes(y = Answers, color= "Answers"), size = 1.2) +
    geom_point(aes(y = Answers, color= "Answers"), size = 1.6) +
    geom_line(aes(y = AnswerRatio, color  = "AnswerRatio"), size = 1.2) +
    geom_point(aes(y = AnswerRatio, color  = "AnswerRatio"), size = 1.6)
}

answers_ratio_plot <- function(Posts, Posts_math){
  
  tabelka1 <- tabelka_questions_answers(Posts)
  tabelka1 <- tabelka1[,c(1,4)]
  colnames(tabelka1)[2] <- "Pets"
  tabelka2 <- tabelka_questions_answers(Posts_math)
  tabelka2 <- tabelka2[,c(1,4)]
  colnames(tabelka2)[2] <- "Math"
  
  tabelka <- merge(tabelka1, tabelka2, by = "Year")
  
  ggplot(tabelka, aes(x=Year, y=AnswerRatio, group = 1)) + 
    geom_line(aes(y = Pets, color = "Pets"), size = 1.2) + 
    geom_point(aes(y = Pets, color = "Pets"), size = 1.6) +
    geom_line(aes(y = Math, color= "Math"), size = 1.2) +
    geom_point(aes(y = Math, color= "Math"), size = 1.6) 
  
  
}

#### Aktywność na serwisach ####

Znajdowanie_wykresu_daty <- function(Posts){
  Posty <- Posts
  Posty$CreationDate <- gsub('.{16}$', '', Posty$CreationDate)
  wynik <- Posty
  wynik <- select(Posty,CreationDate) %>%  group_by(CreationDate) %>% count(CreationDate)
  wynik <- wynik %>% mutate(CreationDate= as.Date(CreationDate, format= "%Y-%m-%s"))
  colnames(wynik)[2] <- 'LiczbaPostow'
  arrange(wynik,desc(CreationDate))
}

rysowanie_aktywnosc <-  function(daty){
  daty <- daty$LiczbaPostow
  daty <- ts(daty, start=c(2015, 1, 1), end=c(2021, 1,1), frequency=12) 
  ts_plot(daty,title="Wykres aktywnosci",
          Xtitle="przedzial czasowy",
          Ytitle="ilosc postow w danym dniu",
  )
}

wykresy_dla_serwisow <- function(PostsPets, PostsKawa, PostsMath){
  # PETS
  daty_pets <- Znajdowanie_wykresu_daty(PostsPets)
  wykres_pets <- rysowanie_aktywnosc(daty_pets)
  wykres_pets
  
  # KAWA
  daty_kawa <- Znajdowanie_wykresu_daty(PostsKawa)
  wykres_kawa <- rysowanie_aktywnosc(daty_kawa)
  wykres_kawa
  
  # MATH
  daty_math <- Znajdowanie_wykresu_daty(PostsMath)
  wykres_math <-rysowanie_aktywnosc(daty_math)
  wykres_math
}



generowanie_tabelka_do_wykresu <- function(PostsPets, PostsKawa, PostsMath){
  
  daty1 <- Znajdowanie_wykresu_daty(PostsPets)
  daty2 <- Znajdowanie_wykresu_daty(PostsKawa)
  daty3 <- Znajdowanie_wykresu_daty(PostsMath)
  daty1 <- daty1$LiczbaPostow
  daty2 <- daty2$LiczbaPostow
  daty3 <- daty3$LiczbaPostow
  daty1 <- ts(daty1, start=c(2015, 1, 1), end=c(2021, 1,1), frequency=12)
  daty2 <- ts(daty2, start=c(2015, 1, 1), end=c(2021, 1,1), frequency=12)
  daty3 <- ts(daty3, start=c(2015, 1, 1), end=c(2021, 1,1), frequency=12)
  daty1dt <- as.data.frame(daty1)
  colnames(daty1dt)[1] <- "Pets"
  daty2dt <- as.data.frame(daty2)
  colnames(daty2dt)[1] <- "Kawa"
  daty3dt <- as.data.frame(daty3)
  colnames(daty3dt)[1] <- "Mathematica"
  lata <- as.data.frame(seq(from = 2015, to = 2021, length.out = 73))
  colnames(lata)[1] <- "Lata"
  tabelka <- cbind(lata, daty1dt, daty2dt, daty3dt)
  tabelka
}




wykres_aktywnosc_pets_coffee <- function(tabelka){
  
  ggplot(tabelka, aes(x=Lata, y=Aktywność)) + 
    geom_line(aes(y = Pets, color = "Pets"), size = 1.3) + 
    geom_line(aes(y = Kawa, color= "Coffee"), size = 1.3)
  
}  


wykres_aktywnosc_wszystkie <- function(tabelka){
  
  gf_line(Mathematica ~ Lata, data = tabelka, color = ~ Pets, size = ~ Kawa) %>% 
    gf_theme(legend.position = "left") %>% 
    gf_labs(title = "Wykresy aktywności Pets, Coffee i Mathematica", caption = "
          Im szerszy wykres, tym większa aktywność na serwisie Coffee.
          Im jaśniejszy wykres, tym większa aktywność na serwisie Pets.")
}


wykres_social_media_trzy <- function(Spolecznosciowe){
  colnames(Spolecznosciowe)[4] <- "Wyniki"
  tabelka_fb_twitter_insta <- dplyr::filter(Spolecznosciowe, (Entity == "Facebook" | Entity == "Twitter" | Entity == "Instagram") & Year >= 2013)
  
  ggplot(tabelka_fb_twitter_insta, aes(x=Year, y=Users, group=Entity)) +
    geom_line(aes(y=Wyniki, color=Entity)) +
    geom_point(aes(y=Wyniki, color=Entity), size = 1.3)
}


wykres_social_media_wszystkie <- function(Spolecznosciowe){
  colnames(Spolecznosciowe)[4] <- "Wyniki"
  
  ggplot(Spolecznosciowe, aes(x=Year, y=Users, group=Entity)) +
    geom_line(aes(y=Wyniki, color=Entity)) +
    theme(legend.position="top")
}


