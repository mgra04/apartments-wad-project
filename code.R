library(tidyverse)
library(openxlsx)
library(ggplot2)
library(ggcorrplot)
install.packages("dummy")
library(dummy)
library(car)
baza <- read.xlsx("C:/Users/jasiu/OneDrive - Akademia Górniczo-Hutnicza im. Stanisława Staszica w Krakowie/Studia/IFS1-3/Analiza danych/Koło naukowe/Apartments project/apartments (2).xlsx")

#===============================================
#Opis bazy
#===============================================
#Baza zawiera dane wszystkich ofert mieszkań na sprzedaż z otodom.pl z dnia 
#04.01.2026. Zebrane dane i ich opis:
#id - unikalny identyfikator oferty
#source - link do oferty
#city - stała wartość "Cracow"
#district - nazwa dzielnicy (18 różnych dzielnic)
#latitude - szerokość geograficzna
#longitude - szerokość geograficzna
#total_price - całkowita cena mieszkania (zł)
#area - powierzchnia w m^2
#rooms - ilość pokoi
#floor - piętro
#total_floors - łączna liczba pięter bloku
#finishing_state - stan wykończenia. Jedna z 3 wartości (ready_to_use, to_completion, to_renovate)
#market_type - Rynek piertowny bądź wtórny. Jedna z 2 wartości (primary, secondary)
#advertiser_type - Rodzaj sprzedawcy. Jenda z 3 wartości (developer, agency, private)
#build_year - rok budowy
#has_elevator - czy jest winda. Wartości true or false
#heating_type - rodzaj ogrzewania
#avaiable_from - data od kiedy mieszkanie jest dostępne
#created_at - Data zapisania oferty do bazy

#Dane dodane w trakcie analizy:
#price_per_m - cena za m^2

#===============================================
#Wstępna eksploracja
#===============================================

#1. Sprawdzenie wszystkich typów danych w bazie
typ_danych <- data.frame(
  kolumna = names(baza),
  typ = sapply(baza, class)
)
print(typ_danych)
# Ad.1
# wszystkie dane poza "has_elevator" ma typ character co uniemożliwia
# sporą część dalszych analiz
# trzeba zamienić typ kolumn:
# latitude, longitude, total_price, area, rooms, floor, total_floors, build_year
# na numeric
baza <- baza %>%
  mutate(across(
    c(latitude, longitude, total_price, area, rooms, floor, total_floors, build_year), 
    as.numeric
  ))
typ_danych <- data.frame(
  kolumna = names(baza),
  typ = sapply(baza, class)
)
print(typ_danych)

baza <- baza %>%
  mutate (price_per_m = total_price / area)

#-------------------------------------------------------------------------

hist(baza$price_per_m,
     breaks = 80)

baza_norm <- baza %>% 
  filter(price_per_m <=35000)
baza_norm %>% 
  count(baza_norm$price_per_m >= 25000)
baza_norm <- baza_norm %>% 
  filter(price_per_m < 25000)

hist(baza_norm$price_per_m,
     breaks = 30,
     main = "Histogram cen za m^2 mieszkań",
     xlab = "Cena za m^2")
skewness(baza_norm$price_per_m)

hist(baza_norm$price_per_m[baza_norm$price_per_m >= 10000],
     breaks = 30,
     main = "Histogram cen za m^2 mieszkań",
     xlab = "Cena za m^2")

baza_norm <- baza_norm %>% 
  mutate(log_price_m = log(price_per_m))

hist(baza_norm$log_price_m[baza_norm$log_price_m >= 9.2],
     breaks = 30,
     main = "Histogram cen za m^2 mieszkań log scaled",
     xlab = "log(Cena za m^2)")

baza_norm <- baza_norm %>% 
  filter(log_price_m >=9.2)

hist(baza_norm$price_per_m,
     breaks = 30,
     main = "Histogram cen za m^2 mieszkań",
     xlab = "Cena za m^2")

skewness(baza_norm$price_per_m)
skewness(baza_norm$log_price_m)

probka2 <- baza_norm%>% 
  filter(log_price_m >=9.2)

probka2 <- probka2 %>% pull(log_price_m)

skewness(probka2)


baza_norm %>% 
  count(total_price >= 3000000)

baza_norm <- baza_norm %>% 
  filter(total_price < 3000000)

hist(baza_norm$total_price)

baza_norm %>% 
  count(total_price >= 2000000)

hist(baza_norm$total_price[baza_norm$total_price < 2000000])

baza_norm %>% 
  count(total_price >= 1500000)

skewness(baza_norm$total_price)

probka <- baza_norm %>% 
  filter(total_price < 1500000)

probka <- probka %>% pull(total_price)

skewness(probka)

baza_norm <- baza_norm %>% 
  mutate(log_price = log(total_price))

probka3 <- baza_norm %>% pull(log_price)

skewness(probka3)

hist(probka3)

probka3 <- baza_norm %>% 
  filter(log_price < 14.75 & log_price > 12.5 )

probka3 <- probka3 %>% pull(log_price)

hist(probka3)

baza_filtered <- baza_norm

Q1 <- quantile(baza_filtered$area, 0.25)
Q3 <- quantile(baza_filtered$area, 0.75)
IQR <- Q3 - Q1

baza_filtered <- baza_filtered %>% 
  filter(log_price < 14.75 & log_price > 12.5 ) %>% 
  filter(log_price >= 12.75) %>% 
  filter(log_price < 14.35) %>% 
  filter(total_price <= 1500000) %>% 
  filter(price_per_m < 21500 | price_per_m > 22000) %>% 
  dplyr::select(-id, -source, -city, -avaiable_from, -created_at) %>% 
  filter(area < Q3 + 1.5 * IQR | area > Q1 - 1.5 * IQR)



hist(baza_filtered$log_price)


hist(baza_filtered$log_price)

skewness(baza_filtered$log_price)


hist(baza_filtered$log_price[baza_filtered$log_price > 14.2])

hist(baza_filtered$log_price)

skewness(baza_filtered$log_price)

skewness(baza_filtered$log_price_m)

kurtosis(baza_filtered$log_price)

kurtosis(baza_filtered$log_price_m)

qqnorm(baza_filtered$log_price)
qqline(baza_filtered$log_price)


baza_filtered %>%
  filter(total_price <= 3000000, area <= 200) %>%
  ggplot (aes(x=area, y=total_price)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Zależność Ceny od Powierzchni",
    x = "Powierzchnia",
    y = "Cena",
  ) +
  theme_minimal()

baza_filtered <- baza_filtered %>% 
  filter(total_price <= 1500000)



baza_filtered %>%
  filter(total_price <= 3000000, area <= 200) %>%
  ggplot (aes(x=area, y=total_price)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Zależność powierzchni od ceny",
    x = "Cena",
    y = "Powierzchnia",
  ) +
  theme_minimal()

skewness(baza_filtered$log_price)

skewness(baza_filtered$log_price_m)

kurtosis(baza_filtered$log_price)

kurtosis(baza_filtered$log_price_m)


hist(baza_filtered$price_per_m)

hist(baza_filtered$price_per_m[baza_filtered$price_per_m >= 20000])

baza_filtered <- baza_filtered %>%
  filter(price_per_m < 21500 | price_per_m > 22000)


hist(baza_filtered$price_per_m)

skewness(baza_filtered$log_price_m)
kurtosis(baza_filtered$log_price_m)


hist(baza_filtered$log_price_m[baza_filtered$log_price_m > 10.0])


hist(baza_filtered$log_price)
hist(baza_filtered$log_price_m)

skewness(baza_filtered$log_price)
kurtosis(baza_filtered$log_price)

hist(baza_filtered$log_price,
     breaks = 30,
     main = "Histogram cen całkowitych",
     xlab = "log(Cena)")
mtext(paste("Skosność:", round(skewness(baza_filtered$log_price), 4)), side = 1, line = 4, cex=0.8)
mtext(paste("Kurtoza:", round(kurtosis(baza_filtered$log_price), 4)), side = 1, line = 2, cex=0.8)



model_test <- lm(baza_filtered$log_price_m ~ baza_filtered$area, data=baza_filtered)
summary(model_test)


boxplot(baza_filtered$area)


hist(baza_filtered$area, breaks = 30)

Q1 <- quantile(baza_filtered$area, 0.25, na.rm = TRUE)
Q3 <- quantile(baza_filtered$area, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

baza_filtered <- baza_filtered %>%
  filter(area > lower_bound & area < upper_bound)

boxplot(baza_filtered$area)

boxplot(baza_filtered$log_price_m)

num_vars <- baza_filtered %>% select(where(is.numeric))

par(mfrow = c(1, 1))
for (v in names(num_vars)) {
  boxplot(num_vars[[v]], main = v)
}


Q1 <- quantile(baza_filtered$log_price_m, 0.25, na.rm = TRUE)
Q3 <- quantile(baza_filtered$log_price_m, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
baza_filtered <- baza_filtered %>% 
  dplyr::filter(log_price_m > lower_bound, log_price_m < upper_bound)



Q1 <- quantile(baza_filtered$log_price, 0.25, na.rm = TRUE)
Q3 <- quantile(baza_filtered$log_price, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

baza_filtered <- baza_filtered %>%
  filter(log_price >= lower_bound, log_price <= upper_bound)






boxplot(baza_filtered$log_price, main="Bez outlierów")




Q1 <- quantile(baza_filtered$build_year, 0.25, na.rm = TRUE)
Q3 <- quantile(baza_filtered$build_year, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

sum(baza_filtered$build_year < lower_bound | 
      baza_filtered$build_year > upper_bound, 
    na.rm = TRUE)


sum(baza_filtered$build_year == 0, na.rm = TRUE)

baza_filtered <- baza_filtered %>%
  mutate(build_year = ifelse(build_year == 0, NA, build_year))


baza_filtered <- baza_filtered %>%
  filter(!is.na(build_year))

par(mfrow = c(1, 1))
for (v in names(num_vars)) {
  boxplot(num_vars[[v]], main = v)
}

baza_filtered$build_year[4428] <- 1980
baza_filtered$build_year[4428] <- 1980
which(baza_filtered$build_year == 980)
baza_filtered$build_year[which(baza_filtered$build_year == 980)] <- 1980
baza_filtered$build_year[which(baza_filtered$build_year == 1)] <- NA
baza_filtered$build_year[which(baza_filtered$build_year == 0)] <- NA
baza_filtered <- baza_filtered %>%
  filter(!is.na(build_year))
baza_filtered <- baza_filtered %>%
  filter(build_year != 1) %>% 
  filter(build_year != 0) %>% 
  filter(build_year != 2)
boxplot(baza_filtered$build_year)

colSums(is.na(baza_filtered)) > 0
sum(is.na(baza_filtered$finishing_state))

baza_filtered <- baza_filtered %>%
  filter(!is.na(finishing_state))
sum(is.na(baza_filtered$heating_type))


mode_val <- names(sort(table(baza_filtered$heating_type), decreasing = TRUE))[1]

baza_filtered <- baza_filtered %>%
  mutate(heating_type = ifelse(is.na(heating_type), mode_val, heating_type))

table(baza_filtered$heating_type, useNA = "ifany")
table(baza_filtered$finishing_state, useNA = "ifany")
table(baza_filtered$market_type, useNA = "ifany")

baza_filtered <- baza_filtered %>%
  mutate(has_elevator = as.numeric(has_elevator))

baza_filtered$heating_type <- as.factor(baza_filtered$heating_type)
baza_filtered$finishing_state <- as.factor(baza_filtered$finishing_state)
baza_filtered$district <- as.factor(baza_filtered$district)
baza_filtered$market_type <- as.factor(baza_filtered$market_type)
baza_filtered$advertiser_type <- as.factor(baza_filtered$advertiser_type)
install.packages("fastDummies")
library(fastDummies)

baza_model <- dummy_cols(
  baza_filtered,
  select_columns = c("heating_type", "finishing_state", "district", "market_type", "advertiser_type"),
  remove_first_dummy = TRUE,   # usuwa kategorię referencyjną
  remove_selected_columns = TRUE
)

baza_model <- baza_model %>% 
  select(-c(latitude, longitude, total_price, price_per_m))

baza_model_m <- baza_model %>% 
  select(-c(log_price))

baza_model <- baza_model %>% 
  select(-log_price_m)



model_price_m <- lm(formula = log_price_m ~ ., data = baza_model_m)
summary(model_price_m)

vif(model_price_m)

v_model_m <- vif(model_price_m)
subset(v_model_m, v_model_m > 10)

names(baza_filtered) <- gsub(" ", "_", names(baza_filtered))
names(baza_model) <- gsub(" ", "_", names(baza_model))
names(baza_model_m) <- gsub(" ", "_", names(baza_model_m))

model_price_m <- lm(formula = log_price_m ~ . - district_Dębniki - district_Prądnik_Biały - district_Podgórze - district_Podgórze_Duchackie, data=baza_model_m)
summary(model_price_m)

baza_model_m$area <- log(baza_model_m$area)
model_price_m <- lm(formula = log_price_m ~ . - district_Dębniki - district_Prądnik_Biały - district_Podgórze - district_Podgórze_Duchackie, data=baza_model_m)
summary(model_price_m)


model_price <- lm(formula = log_price ~ ., data = baza_model)
summary(model_price)

# Podstawowy zestaw wykresów diagnostycznych
# Ustawia układ 2x2, żeby widzieć wszystkie naraz
plot(model_price)
par(mfrow = c(2, 2))  # Dzieli ekran na 2 wiersze i 2 kolumny
plot(model_price)

najdziwniejsze <- which.max(hatvalues(model_price))
baza_model[najdziwniejsze,]
baza_model <- baza_model[-6545, ]
baza[najdziwniejsze,]


baza_model <- baza_model[-najdziwniejsze,]
baza_model_m <- baza_model_m[-najdziwniejsze,]


baza_model$area <- log(baza_model$area)
baza_model <- rename(baza_model, area_log = area)

model_price <- lm(formula = log_price ~ ., data = baza_model)
summary(model_price)
plot(model_price)

v_model_m <- vif(model_price)
subset(v_model_m, v_model_m > 5)

vif(model_price)

library(corrplot)
M <- cor(baza_model, use = "complete.obs")

# 2. Rysujemy wykres
corrplot(M, 
         method = "color",       # Kwadraty z kolorami         # Tylko dolna połówka
         tl.col = "black",       # Kolor tekstu
         tl.cex = 0.4,           # <--- WAŻNE: Mała czcionka (zmniejsz do 0.4 jeśli wciąż za duże)
         tl.srt = 45,            # Obrót napisów
         diag = FALSE,           # Ukryj przekątną
         title = "Mapa Korelacji Cen Mieszkań", 
         mar = c(0,0,2,0)        # Margines na tytuł
)

kor <- baza_model[, c("finishing_state_to_completion", "market_type_secondary")]
cor_matrix <- cor(kor, use="complete.obs")
corrplot(cor_matrix, method = "circle", addCoef.col="black")


model_price_r <- lm(formula = log_price ~ .+finishing_state_to_completion:market_type_secondary, data = baza_model)
summary(model_price)
plot(model_price_r)
anova(model_price, model_price_r)
model_price_r2 <- lm(formula = log_price ~ .+finishing_state_to_completion:market_type_secondary +build_year:market_type_secondary, data = baza_model)
summary(model_price_r2)
plot(model_price_r2)
anova(model_price, model_price_r2)

baza_model <- baza_model[-5646,]
baza_model <- baza_model[-5648,]
model_price_r2 <- lm(formula = log_price ~ .+finishing_state_to_completion:market_type_secondary +build_year:market_type_secondary , data = baza_model)
plot(model_price_r2)

baza_model[5646, ]
install.packages("olsrr")
library(olsrr)
ols_vif_tol(model_price_r2)
