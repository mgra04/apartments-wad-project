library(tidyverse)
library(openxlsx)
library(ggplot2)
library(ggcorrplot)
install.packages("dummy")
library(dummy)
library(car)
library(moments)
baza <- read.xlsx("C:/Users/jasiu/OneDrive - Akademia Górniczo-Hutnicza im. Stanisława Staszica w Krakowie/Studia/IFS1-3/Analiza danych/Koło naukowe/Apartments project/apartments (2).xlsx")

baza <- baza %>%
  mutate(across(
    c(latitude, longitude, total_price, area, rooms, floor, total_floors, build_year), 
    as.numeric
  ))
#zamiana typów character na numeric, niezbędna do dalszej analizy

baza <- baza %>%
  mutate (price_per_m = total_price / area)
#dodanie zmiennej price_per_m w celu ułatwienia zobrazowania cen

baza <- baza %>% 
  filter(price_per_m < 25000)

#odfiltrowanie obserwacji z ceną za m^2 powyzej 25000 zł
#Nie było ich dużo, a znacząco zaburzały rozkład tej zmiennej
#Oraz skośność, co mogłoby prowadić do późniejszych błędów w modelu
#tu możnaby pokazac histogram tej wartosci
baza<- baza %>% 
  mutate(log_price = log(total_price)) %>% 
  mutate(log_price_m = log(price_per_m))
#Dodanie logarytmu ceny całkowitej oraz ceny za m^2, dzięki czemu 
#rozkład będzie znacznie stabilniejszy i bliższy normalnemu
#Ułatwia to dalszą analizę


Q1 <- quantile(baza$area, 0.25)
Q3 <- quantile(baza$area, 0.75)
IQR <- Q3 - Q1
#definicja zakresów potrzebnych do następnego etapu filtrowania
#Zakresy te wzięły się z wykresu pudełkowego powierzchni

boxplot(baza$area)

#na którym widać, iż jest sporo obserwacji wykraczających poza 
#wartości mediany

baza<- baza %>%  
  filter(log_price < 14.75 & log_price > 12.5 ) %>% 
  #odfiltrowanie obserwacji z mocno odstającą ceną
  # w celu redukcji skośności i poprawy kurtozy
  filter(log_price >= 12.75) %>% 
  #tutaj to samo
  #możnaby przy tym gdzieś pokazac znowu ten histogram
  filter(log_price_m >=9.2) %>%
  filter(log_price < 14.35) %>% 
  filter(total_price <= 1500000) %>% 
  #Odfiltrowanie skrajnie wysokich cen
  #mogących zaburzać dokładność modelu
  filter(price_per_m < 21500 | price_per_m > 22000) %>% 
  #znowu odfiltrowanie odstających cen na m^2
  dplyr::select(-id, -source, -city, -avaiable_from, -created_at) %>% 
  #usunięcie niepotrzebnych w dalszej analizie kolumn
  filter(area < Q3 + 1.5 * IQR | area > Q1 - 1.5 * IQR)
  #filtrowanie na podstawie odstawania obserwacji o więcej niż
#1.5 IQR


#histogram log price w celu pokazania osiągniętych wartosci 
#kurtozy i skośności
hist(baza$log_price,
     breaks = 30,
     main = "Histogram cen całkowitych",
     xlab = "",
     ylab = "Częstotliwość")



mtext("Logarytm ceny za metr kwadratowy", side = 1, line = 2, cex = 0.9)
#ładniejszy opis osi X
mtext(
  paste("Skosność:", round(skewness(baza$log_price), 4)),
  side = 1, line = 3, cex = 0.8
)
# Skosność

mtext(
  paste("Kurtoza:", round(kurtosis(baza$log_price), 4)),
  side = 1, line = 4, cex = 0.8
)
# Kurtoza


Q1 <- quantile(baza$log_price_m, 0.25, na.rm = TRUE)
Q3 <- quantile(baza$log_price_m, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

#Przygotowanie zakresów w celu usunięcia odstających 
#obserwacji znalezionych na wykresie pudełkowym 
#logarytmu ceny za m^2

num_vars <- baza %>% dplyr::select(where(is.numeric))

par(mfrow = c(1, 1))
for (v in names(num_vars)) {
  boxplot(num_vars[[v]], main = v)
}
#utworzenie wykresów pudełkowych dla każdej zmiennej numerycznej
#w bazie

lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
baza <- baza %>% 
  dplyr::filter(log_price_m > lower_bound, log_price_m < upper_bound)

#usunięcie odstających obserwacji w logarytmie ceny za m^2

Q1 <- quantile(baza$log_price, 0.25, na.rm = TRUE)
Q3 <- quantile(baza$log_price, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

baza <- baza %>%
  filter(log_price >= lower_bound, log_price <= upper_bound)


#To samo dla logarytmu ceny całkowitej

Q1 <- quantile(baza$build_year, 0.25, na.rm = TRUE)
Q3 <- quantile(baza$build_year, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

sum(baza$build_year < lower_bound | 
      baza$build_year > upper_bound, 
    na.rm = TRUE)

#Usunięcie obserwacji odstających dla roku zbudowania

sum(baza$build_year == 0, na.rm = TRUE)
#sprawdzenie ile jest zerowych wartości roku budowy

baza <- baza %>%
  mutate(build_year = ifelse(build_year == 0, NA, build_year))
#zamiana tych wartosci na NA

baza <- baza %>%
  filter(!is.na(build_year))
#usunięcie wartości NA z build year

baza$build_year[which(baza$build_year == 980)] <- 1980
#Poprawa prawdopobobnego błędu znalezionego w jednej obserwacji
baza$build_year[which(baza$build_year == 1)] <- NA
#zamiana wartości 1 w roku wybudowania na NA
baza$build_year[which(baza$build_year == 0)] <- NA
#To samo dla wartości 0

baza <- baza %>%
  filter(!is.na(build_year))
#odfiltrowanie wartosci NA z build year (krok powtórzony dla pewności)
baza <- baza %>%
  filter(build_year != 1) %>% 
  filter(build_year != 0) %>% 
  filter(build_year != 2)
#Odfiltrowanie wartości year build równych 0, 1 lub 2 

colSums(is.na(baza)) > 0
#sprawdzenie które kolumny jeszcze wciąż mają NA
sum(is.na(baza$finishing_state))
#sprawdzenie ile NA ma konkretna kolumna

baza <- baza %>%
  filter(!is.na(finishing_state))
#odfiltrowanie obserwacji z wartościami NA w finishing state
sum(is.na(baza$heating_type))
#sprawdzenie ilosci NA w heating type

mode_val <- names(sort(table(baza$heating_type), decreasing = TRUE))[1]

baza <- baza %>%
  mutate(heating_type = ifelse(is.na(heating_type), mode_val, heating_type))
#z powodu ogromnej ilości NA, brakujące wartości wypełniliśmy
#modą

baza <- baza %>%
  mutate(has_elevator = as.numeric(has_elevator))
#przekształcenie zmiennej logicznej has_elevator na binarną

baza$heating_type <- as.factor(baza$heating_type)
baza$finishing_state <- as.factor(baza$finishing_state)
baza$district <- as.factor(baza$district)
baza$market_type <- as.factor(baza$market_type)
baza$advertiser_type <- as.factor(baza$advertiser_type)
#przekształcenie zmiennych kategorialnych na factory

model1 <- lm(formula = log_price ~ . + area * district - price_per_m - log_price_m, data = baza)
summary(model1)
par(mfrow = c(2, 2))  # Dzieli ekran na 2 wiersze i 2 kolumny
plot(model1)
par(mfrow = c(1,1))
#pierwszy model z relacją
#area_log - district
#decyzja o kodowaniu hot one aby 
#ułatwić interpretację modelu
#Mimo niewiarygodnie wysokiego R^2
#Jakość modelu nie jest najlepsza, prawdopodobnie z powodu zbyt dużej
#ilości zmiennych wyjaśniających, oraz z powodu obecności zmiennych
#kategorialnych
install.packages("fastDummies")
library(fastDummies)
lapply(baza[c("heating_type", "finishing_state", "district", "market_type", "advertiser_type")],
       function(x) sort(unique(x))[1])

#sprawdzenie które kolumny zostaną użyte jako referencje 
#Heating_type - boiler_room
#Finishing_state - ready_to_use
#district - Bieńczyce
#market_type - primary
#advertiser_type - agency

# wyniku działania pakietu fastDummies
baza <- dummy_cols(
  baza,
  select_columns = c("heating_type", "finishing_state", "district", "market_type", "advertiser_type"),
  remove_first_dummy = TRUE,   # usuwa kategorię referencyjną
  remove_selected_columns = TRUE
)
#kodowanie jeden do wielu zmiennych kategorialnych

baza <- baza %>% 
  dplyr::select(-c(latitude, longitude, total_price, price_per_m, log_price_m))
#usunięcie kolumn zbędnych w dalszej analizie
#Z racji iż model price_per_m okazał się być niezwykle ciężki
#do dalszego użycia (trudno było poprawiać jego jakość)
#zdecydowaliśmy się na model wyjaśniający total_price 
# w formie logarytmowanej
names(baza) <- gsub(" ", "_", names(baza))
#pozybcie się spacji w nazwach zmiennych
#wykresy diagnostyczne modelu
reszty <- resid(model1)
baza <- baza[abs(reszty) <= 0.4, ]
#usuniecie reszt wiekszych niz 0.4
leverage_values <- hatvalues(model1)
id_punktu <- which.max(leverage_values)

leverage_values[id_punktu]

baza[id_punktu, ]
#ustalenie punktu o najwyższym leverage value
#Jest to prawdopodobnie blad - jak mieszkanie z 2008 roku moze mieć 
#piec kaflowy?
#Usuwamy

baza <- baza[-id_punktu, ]

baza$area <- log(baza$area)
#decyzja o logarytmizacji powierzchni
#w celu poprawy rozkladu, a tym samym jakości modelu
baza <- rename(baza, area_log = area)
#zmiana nazwy kolumny w celu poprawy czytelności


#podejrzenie silnej korelacji market_type_secondary z year_build
cor_matrix <- cor(baza[ , c("market_type_secondary", "build_year")])
corrplot::corrplot(
  cor_matrix,
  method = "color",
  addCoef.col = "black",
  tl.col = "black",
  type = "upper",
  diag = FALSE,
  main = "Korelacja: market_type_secondary vs build_year"
)
#potwierdzenie tej korelacji

baza$build_year <- cut(baza$build_year, 
                                   breaks = c(-Inf, 1945, 1989, 2010, Inf), 
                                   labels = c("Kamienica", "PRL", "Transformacja", "Nowe"))
#zamiana build year na przedzialy w celu 
#zanegowania efektu tej korelacji
#ponadto relacja log_price z build_year
# nie jest liniowa
#dzieki temu model lepiej moze zrozumieć że
#stare kamienice i nowe apartamenty mogą być jednocześnie drogie 
#mimo rożnicy wieku

model2 <- lm(formula = log_price ~ ., data = baza)
summary(model2)
par(mfrow = c(2, 2))  # Dzieli ekran na 2 wiersze i 2 kolumny
plot(model2)
par(mfrow = c(1,1))

v_model_m <- vif(model2)
subset(v_model_m, v_model_m > 5)
#wyświetlenie czynników o VIF >5)
#Zmienne dzielnicowe niestety muszą zostać, mimo tak
#silnego wpływu na model
#natomiast usunięciu podległa zmienna heating_type_urban, 
#gdyż niewiele wnosi ona do wytłumaczenia ceny
ggplot(baza, aes(x = factor(heating_type_urban), y = log_price)) +
  stat_summary(fun = mean, geom = "point", size = 3, color = "red") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  labs(
    x = "Heating type: urban",
    y = "Log price",
    title = "Średnie log_price z przedziałami ufności"
  ) +
  theme_minimal()

#jak widać tutaj na wykresie


#ponowne sprawdzenie modelu
v_model_m <- vif(model2)
subset(v_model_m, v_model_m > 5)


#kolejny etap - usunięcie zmiennych nie wnoszących nic do modelu
#tutaj będą to zmienne advertiser_type oraz market_type_secondary
#i heating type gas, other i tiled stove, oraz total floors

model3 <- lm(formula = log_price ~ .-advertiser_type_developer -advertiser_type_private -market_type_secondary-heating_type_gas-heating_type_other-heating_type_tiled_stove, data = baza)
summary(model3)
par(mfrow = c(2, 2))  # Dzieli ekran na 2 wiersze i 2 kolumny
plot(model3)
par(mfrow = c(1,1))
#kolejna poprawa jakosci oraz dopasowania modelu, 
#qq residuals są lepsze, zarowno jak i residuals vs leverage
reszty <- resid(model3)
print(which.max(reszty))
baza <- baza[rownames(baza) != "171", ]
baza <- baza[rownames(baza) != "169", ]
#usuwanie maksymalnej reszty

reszty_std <- rstandard(model3)
indeksy_qq <- order(abs(reszty_std), decreasing = TRUE)[1:10]
top10_qq <- baza[indeksy_qq, ]
print(top10_qq)
#wybor oraz podejrzenie obserwacji o największych błędach
# w celu decyzji co nalezy zrobić
#Wiersze 171 oraz 3321 to duplikaty
# nalezy je usunąć
#Wiersz 4423 - anomalia w postaci jednopokojowego 50m mieszkania
#usunięcie duplikatu
baza <- baza[which(rownames(baza) != "171"), ]
#


cooks <- cooks.distance(model3)
indeksy_leverage <- order(cooks, decreasing = TRUE)[1:10]
top10_leverage <- baza[indeksy_leverage, ]
print(top10_leverage)
#wyswietlenie 10 obserwacji o największym wpływie na model
#wiersz 4497 - dziwna kombinacja stanu i epoki
#do wykończenia z lat 1990-2015?
#nietypowy przypadek -> do usuniecia
#4884 -> prawdopodobinie dom. Heating type other oraz wysoka powierzchnia
#moze oznaczać pompę 
#ciepła - jest to opcja premium, stąd cena
#Do usunięcia
for_deletion <- c("4884", "4497")
baza <- baza[rownames(baza) != "4884", ]
baza <- baza[rownames(baza) != "4497", ]
#usunięcie wspomnianych przypadków
model3 <- lm(formula = log_price ~ .-advertiser_type_developer -advertiser_type_private -market_type_secondary-heating_type_gas-heating_type_other-heating_type_tiled_stove, data = baza)
summary(model3)
par(mfrow = c(2, 2))  # Dzieli ekran na 2 wiersze i 2 kolumny
plot(model3)
par(mfrow = c(1,1))
#końcowy model

reszty <- resid(model3)

hist(reszty)
#pokazanie rozkładu reszt modelu

coefs <- coef(model3)[-1]
#Pobranie zmiennych bez interceptu

tabela_final <- data.frame(
  Zmienna = names(coefs),
  Beta = coefs
) %>%
  #utworzenie tabeli zawierającej zmienne oraz ich wpływ w procentach
  mutate(
    Wplyw_Procentowy = ifelse(
      Zmienna == "area_log",
      round(Beta * 1, 2), 
      round((exp(Beta) - 1) * 100, 2) 
    )
    #logika obliczania wpływu procentowego
    #Nie obliczamy logarytmu dla area_log
  ) %>%
  select(Zmienna, Wplyw_Procentowy) %>%
  arrange(desc(abs(Wplyw_Procentowy))) # Sortowanie od najsilniejszych czynników
#wybieramy kolumny oraz sortujemy


referencje <- data.frame(
  Zmienna = c(
    "heating_type_boiler_room",
    "finishing_state_ready_to_use",
    "district_Bieńczyce", "build_year"
  ),
  Wplyw_Procentowy = 0
)
#utworzenie tabeli ze zmiennymi referencyjnymi

tabela_final <- bind_rows(tabela_final, referencje) %>%
  mutate(
    Zmienna = gsub("district_", "Dzielnica: ", Zmienna),
    Zmienna = gsub("heating_type_", "Ogrzewanie: ", Zmienna),
    Zmienna = gsub("finishing_state_", "Standard: ", Zmienna),
    Zmienna = gsub("market_type_", "Rynek: ", Zmienna),
    Zmienna = gsub("advertiser_type_", "Ogłoszeniodawca: ", Zmienna),
    Zmienna = gsub("build_year", "Epoka: ", Zmienna)
  ) %>%
  #utworzenie tabeli finalnej 
  arrange(Zmienna, desc(abs(Wplyw_Procentowy)))

tabela_final$Zmienna[tabela_final$Zmienna == "Epoka: "] <- "Epoka: Kamienica"
#Zmiana nazwy "Epoka" na "Epoka: Kamienica" dla czytelności
#grupowanie po zmiennej
print(tabela_final, row.names = FALSE)
#wyświetlanie tabeli

#===================================
#To samo dla pozostałych dwóch modeli
#===================================
coefs <- coef(model2)[-1]
#Pobranie zmiennych bez interceptu

tabela_final <- data.frame(
  Zmienna = names(coefs),
  Beta = coefs
) %>%
  #utworzenie tabeli zawierającej zmienne oraz ich wpływ w procentach
  mutate(
    Wplyw_Procentowy = ifelse(
      Zmienna == "area_log",
      round(Beta * 1, 2), 
      round((exp(Beta) - 1) * 100, 2) 
    )
    #logika obliczania wpływu procentowego
    #Nie obliczamy logarytmu dla area_log
  ) %>%
  select(Zmienna, Wplyw_Procentowy) %>%
  arrange(desc(abs(Wplyw_Procentowy))) # Sortowanie od najsilniejszych czynników
#wybieramy kolumny oraz sortujemy


referencje <- data.frame(
  Zmienna = c(
    "heating_type_boiler_room",
    "finishing_state_ready_to_use",
    "district_Bieńczyce", "build_year", "Rynek, Ogłoszeniodawca:"
  ),
  Wplyw_Procentowy = 0
)
#utworzenie tabeli ze zmiennymi referencyjnymi

tabela_final <- bind_rows(tabela_final, referencje) %>%
  mutate(
    Zmienna = gsub("district_", "Dzielnica: ", Zmienna),
    Zmienna = gsub("heating_type_", "Ogrzewanie: ", Zmienna),
    Zmienna = gsub("finishing_state_", "Standard: ", Zmienna),
    Zmienna = gsub("market_type_", "Rynek: ", Zmienna),
    Zmienna = gsub("advertiser_type_", "Ogłoszeniodawca: ", Zmienna),
    Zmienna = gsub("build_year", "Epoka: ", Zmienna)
  ) %>%
  #utworzenie tabeli finalnej 
  arrange(Zmienna)

tabela_final$Zmienna[tabela_final$Zmienna == "Epoka: "] <- "Epoka: Kamienica"
#Zmiana nazwy "Epoka" na "Epoka: Kamienica" dla czytelności
#grupowanie po zmiennej
print(tabela_final, row.names = FALSE)

#======================================

#======================================

coefs <- coef(model3)[-1]
#Pobranie zmiennych bez interceptu

tabela_final <- data.frame(
  Zmienna = names(coefs),
  Beta = coefs
) %>%
  #utworzenie tabeli zawierającej zmienne oraz ich wpływ w procentach
  mutate(
    Wplyw_Procentowy = ifelse(
      Zmienna == "area_log",
      round(Beta * 1, 2), 
      round((exp(Beta) - 1) * 100, 2) 
    )
    #logika obliczania wpływu procentowego
    #Nie obliczamy logarytmu dla area_log
  ) %>%
  select(Zmienna, Wplyw_Procentowy) %>%
  arrange(desc(abs(Wplyw_Procentowy))) # Sortowanie od najsilniejszych czynników
#wybieramy kolumny oraz sortujemy


referencje <- data.frame(
  Zmienna = c(
    "heating_type_boiler_room",
    "finishing_state_ready_to_use",
    "district_Bieńczyce", "build_year"
  ),
  Wplyw_Procentowy = 0
)
#utworzenie tabeli ze zmiennymi referencyjnymi

tabela_final <- bind_rows(tabela_final, referencje) %>%
  mutate(
    Zmienna = gsub("district_", "Dzielnica: ", Zmienna),
    Zmienna = gsub("heating_type_", "Ogrzewanie: ", Zmienna),
    Zmienna = gsub("finishing_state_", "Standard: ", Zmienna),
    Zmienna = gsub("market_type_", "Rynek: ", Zmienna),
    Zmienna = gsub("advertiser_type_", "Ogłoszeniodawca: ", Zmienna),
    Zmienna = gsub("build_year", "Epoka: ", Zmienna)
  ) %>%
  #utworzenie tabeli finalnej 
  arrange(Zmienna, desc(abs(Wplyw_Procentowy)))

tabela_final$Zmienna[tabela_final$Zmienna == "Epoka: "] <- "Epoka: Kamienica"
#Zmiana nazwy "Epoka" na "Epoka: Kamienica" dla czytelności
#grupowanie po zmiennej
print(tabela_final, row.names = FALSE)
