
daty1 <- daty_pets
daty2 <- daty_kawa
daty3 <- daty_math
daty1 <- daty1$LiczbaPostow
daty2 <- daty2$LiczbaPostow
daty3 <- daty3$LiczbaPostow
daty1 <- ts(daty1, start=c(2015, 1, 1), end=c(2021, 1,1), frequency=1)
daty2 <- ts(daty2, start=c(2015, 1, 1), end=c(2021, 1,1), frequency=1)
daty3 <- ts(daty3, start=c(2015, 1, 1), end=c(2021, 1,1), frequency=1)
daty1dt <- as.data.frame(daty1)
colnames(daty1dt)[1] <- "Pets"
daty2dt <- as.data.frame(daty2)
colnames(daty2dt)[1] <- "Kawa"
daty3dt <- as.data.frame(daty3)
colnames(daty3dt)[1] <- "Mathematica"
lata <- as.data.frame(2015:2021)
colnames(lata)[1] <- "Lata"
tabelka <- cbind(lata, daty1dt, daty2dt, daty3dt)

m_wykres <- mPlot(
  tabelka,
  default = "scatter",
  system = system_choices()[1],
  show = FALSE,
  title = "",
  data_text = substitute(data)
)


m_wykres <- mPlot(x = 'Lata', y = c('Pets', 'Kawa'),
                  data = tabelka, pointSize = 0, lineWidth = 1, system = "ggplot2")
m_wykres$set(width = 750, height = 590)

# Porównajmy wykresy serwisów Pets i Mathematica
gf_line(Pets ~ Lata, data = tabelka, color = ~ Mathematica, size = ~ Pets) %>% 
  gf_theme(legend.position = "left") %>% 
  gf_labs(title = "Wykresy aktywności Pets i Mathematica", caption = "
          Im szerszy wykres, tym większa aktywność na serwisie Pets.
          Im jaśniejszy wykres, tym większa aktywność na serwisie Mathematica.")

# Porównajmy wykresy serwisów Coffee i Mathematica
gf_line(Kawa ~ Lata, data = tabelka, color = ~ Mathematica, size = ~ Kawa) %>% 
  gf_theme(legend.position = "left") %>% 
  gf_labs(title = "Wykresy aktywności Coffee i Mathematica", caption = "
          Im szerszy wykres, tym większa aktywność na serwisie Coffee.
          Im jaśniejszy wykres, tym większa aktywność na serwisie Mathematica.")

# Porównajmy wykresy serwisów Pets, Coffee i Mathematica
gf_line(Mathematica ~ Lata, data = tabelka, color = ~ Pets, size = ~ Kawa) %>% 
  gf_theme(legend.position = "left") %>% 
  gf_labs(title = "Wykresy aktywności Pets, Coffee i Mathematica", caption = "
          Im szerszy wykres, tym większa aktywność na serwisie Coffee.
          Im jaśniejszy wykres, tym większa aktywność na serwisie Pets.")