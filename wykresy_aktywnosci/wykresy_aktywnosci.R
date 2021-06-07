# Porównamy aktywność na forach Pets, Mathematica i Coffee
# wczytajmy potrzebne dane i biblioteki

PostsPets <- read.csv("Posts_pets.xml.csv")
PostsMath <- read.csv("Posts_math.xml.csv")
PostsKawa <- read.csv("Posts_kawa.xml.csv")

library(dplyr)
library(data.table)
library(knitr)
library(TSstudio)
library(tidyr)
library(mplot)
library(SmarterPoland)
library(mosaic)

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

#### PETS ####
daty_pets <- Znajdowanie_wykresu_daty(PostsPets)
wykres_pets <- rysowanie_aktywnosc(daty_pets)
wykres_pets

#### KAWA ####
daty_kawa <- Znajdowanie_wykresu_daty(PostsKawa)
wykres_kawa <- rysowanie_aktywnosc(daty_kawa)
wykres_kawa

#### MATH ####
daty_math <- Znajdowanie_wykresu_daty(PostsMath)
wykres_math <-rysowanie_aktywnosc(daty_math)
wykres_math

# Jak widzimy aktywność na forum Mathematica była zdecydowanie największa
# i co więcej w przeciwieństwie do pozostałych serwisów dalej jest spora.
# Wykresy serwisu Pets i Coffee są podobne - widać jak aktywność systematycznie
# maleje.

# Dodatkowo można zauważyć, że wykres dla derwisu Mathematica cyklicznie
# maleje i rośnie.
daty1 <- daty_pets
daty2 <- daty_kawa
daty3 <- daty_math
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

# Porównajmy wykresy serwisów Pets, Coffee i Mathematica
gf_line(Mathematica ~ Lata, data = tabelka, color = ~ Pets, size = ~ Kawa) %>% 
  gf_theme(legend.position = "left") %>% 
  gf_labs(title = "Wykresy aktywności Pets, Coffee i Mathematica", caption = "
          Im szerszy wykres, tym większa aktywność na serwisie Coffee.
          Im jaśniejszy wykres, tym większa aktywność na serwisie Pets.")




