library(tidyverse)
library(openxlsx)
baza <- read.xlsx("C:/WAD/Dane/apartments.xlsx")

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

#2. Histogram dla total_price
summary(baza$total_price)
#mediana 819 000, a wartość maksymalna 16 500 000 => histogram bez warunku
#ograniczającego wyświetlanie skrajnie drogich mieszkań może być ciężki do
#interpretacji
hist(baza$total_price)
#Wykres prawoskośny, ale przez skrajne wyniki jest ciężki do głębszej interpretacji

#Histogram mieszkań z ceną do 3 000 000
hist(baza$total_price[baza$total_price <= 3000000],
     breaks = 30,
     main = "Histogram cen mieszkań do 3mln zł",
     xlab = "Cena")
#Wyraźna dominacja mieszkań w cenach 500 000 - 1 000 000
#Poniżej 500 000 i powyżej 1 000 000 wyraźny spadek ilości ofert

#3. Dodanie zmiennej price_per_m
baza <- baza %>%
  mutate (price_per_m = total_price / area)

summary(baza$price_per_m)
#Znowu wartość maksymalna: 79800 w momencie kiedy 3 kwartyl to 18000
hist(baza$price_per_m,
     breaks = 30,
     main = "Histogram cen za m^2 mieszkań",
     xlab = "Cena za m^2")
#Histogram pokazuje, że ceny za m^2 powyżej 35000 to pojedyńcze przypadki
#Można je wyciąć
hist(baza$price_per_m[baza$price_per_m <= 35000],
     breaks = 30,
     main = "Histogram cen za m^2 mieszkań do 35000 zł",
     xlab = "Cena za m^2")
#12000 - 18000 większość ofert co w sumie wskazywały nam kwartyle

#4. Sprawdzenie ilości ofert na dzielnice + wstępna analiza w cenach
oferty_na_dzielnice <- baza %>%
  group_by(district) %>%
  summarise(liczba_ofert = n()) %>%
  arrange(desc(liczba_ofert))
print(oferty_na_dzielnice)
#Ogólnie duże zróżnicowanie w ilości ofert. Wyraźna dominacja na Prądniku 
#Białym (2115 🥷🤙) Dalej Podgórze (1408), Dębniki (1209), Podgórze Duchackie 
#(932) Najmniej ofert W Bieńczycach (105)

analiza_dzielnic_total_price <- baza %>%
  group_by(district) %>%
  summarise(
    liczba_ofert = n(),
    Min = min(total_price, na.rm = TRUE), #Nie ma braków w cenie, ale niech zostanie
    `1st Qu.` = quantile(total_price, 0.25, na.rm = TRUE),
    Median = median(total_price, na.rm = TRUE),
    Mean = mean(total_price, na.rm = TRUE),
    `3rd Qu.` = quantile(total_price, 0.75, na.rm = TRUE),
    Max = max(total_price, na.rm = TRUE)
    ) %>%
  arrange(desc(liczba_ofert))
print(analiza_dzielnic_total_price)
#DO INTERPRETACJI!
baza %>%
  filter(total_price <= 3000000) %>%
  ggplot(aes(x = reorder(district, total_price, FUN = median), y = total_price)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  coord_flip() +
  scale_y_continuous(labels = scales::comma) + #żeby w cenach nie było np. 1e+06
  labs(
    title = "Rozkład cen mieszkań w dzielnicach",
    x = "Dzielnica",
    y = "Cena"
  ) +
  theme_minimal()
#Zwierzyniec wydaje się być dzielnicą "Premium", pudełko zaczyna się od ceny
# 1 000 000 i wyraźnie odstaje od reszty. Różnice w cenach również są największe
# (najdłuższe pudełko). Najkorzystniejsze oferty znajdują się w Bieńczycach,
# Nowej Hucie i Mistrzejowicach

#Ad 4. Sprawdźmy jeszcze dla ceny za m^2
analiza_dzielnic_price_per_m <- baza %>%
  group_by(district) %>%
  summarise(
    liczba_ofert = n(),
    Min = min(price_per_m, na.rm = TRUE),
    `1st Qu.` = quantile(price_per_m, 0.25, na.rm = TRUE),
    Median = median(price_per_m, na.rm = TRUE),
    Mean = mean(price_per_m, na.rm = TRUE),
    `3rd Qu.` = quantile(price_per_m, 0.75, na.rm = TRUE),
    Max = max(price_per_m, na.rm = TRUE)
  ) %>%
  arrange(desc(liczba_ofert))
print(analiza_dzielnic_price_per_m)
#DO INTERPRETACJI!
baza %>%
  filter(price_per_m <= 25000) %>%
  ggplot(aes(x = reorder(district, price_per_m , FUN = median), y = price_per_m)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  coord_flip() +
  labs(
    title = "Rozkład cen za m^2 mieszkań w dzielnicach",
    x = "Dzielnica",
    y = "Cena"
  ) +
  theme_minimal()
#Teraz już to wygląda tak jak można było się tego spodziewać. Najdroższe mieszkania
#w Starym mieście. W Zwierzyńcu musi być więcej mieszkań o większej powierzchni
#przez co w cenie całkowitej cena był na samej górze. Jak podniesiemy ograniczenie
#w cenie bądź nawet usuniemy to możemy zaobserować sporo outlinerów w Starym Mieście
#gdzie ceny sięgają nawet powyżej 50000zł za m^2. 3 Mieszkania z najwyższą ceną
#za m^2 znajdują się w Zwierzyńcu 62000-70000 i jedno to nasze 79800 które
#zobaczyliśmy w summary(baza$price_per_m)

#5. Sprawdzenie ilości ofert ze względu na rodzaj sprzedawcy (osoba prywatna, agencja, deweloper)
oferty_na_sprzedawce <- baza %>%
  group_by(advertiser_type) %>%
  summarise(liczba_ofert = n()) %>%
  arrange(desc(liczba_ofert))
print(oferty_na_sprzedawce)
#:OOOOO
#1 developer               5395
#2 agency                  5036
#3 private                  430
#Deweloperzy w formie. Odnośnie sporej ilości ofert od agencji, może to wynikać 
#z kosztów jakie ponosi osoba prywatna wystawiając mieszkanie na sprzedaż na 
#otodom bądź po prostu wygody (agencja ich wyręcza)

analiza_sprzedawcow_price_per_m <- baza %>%
  group_by(advertiser_type) %>%
  summarise(
    liczba_ofert = n(),
    Min = min(price_per_m, na.rm = TRUE),
    `1st Qu.` = quantile(price_per_m, 0.25, na.rm = TRUE),
    Median = median(price_per_m, na.rm = TRUE),
    Mean = mean(price_per_m, na.rm = TRUE),
    `3rd Qu.` = quantile(price_per_m, 0.75, na.rm = TRUE),
    Max = max(price_per_m, na.rm = TRUE)
  ) %>%
  arrange(desc(liczba_ofert))
print(analiza_sprzedawcow_price_per_m)
#brak większych różnic w średniej i medianie
#średnia w agencji zawyżona przez to, że oni mają to nieszczęsne mieszkanie
#z ceną 79800 za m^2
#Median   Mean 
#15781. 16421. <- developer 
#16000  17078. <- agency
#15777. 16748. <- private

#6. Wykres rozrzutu ceny i powierzchni z podziałem na dzielnice
baza %>%
  filter(total_price <= 3000000, area <= 200) %>%
  ggplot (aes(x=area, y=total_price, colour = district)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Zależność powierzchni od ceny",
    x = "Cena",
    y = "Powierzchnia",
    color = "Dzielnica"
  ) +
  theme_minimal()
#Nieczytelny... 
#Ad.6 Spróbujmy bez podziału
baza %>%
  filter(total_price <= 3000000, area <= 200) %>%
  ggplot (aes(x=area, y=total_price)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Zależność powierzchni od ceny",
    x = "Powierzchnia",
    y = "Cena",
  ) +
  theme_minimal()
#piękna chmurka. Im większa powierzchnia tym cena jest bardziej zróżnicowana.

#7. Sprawdzenie normalności rozkładu
#Na historgramach już można było dostrzec, że nie jest on normalny, ale ja się
#dalej łudzę, że jest
#Problem: można przekazać makymalnie 5000 wartości
probka_total_price <- sample(baza$total_price, 5000)
shapiro.test(probka_total_price)
#p-value < 2.2e-16... Oj jak boli. p wyraźnie mniejsze od 0.05 <=> brak rozkładu
#normalnego. w=0.52

#Jeszcze sprawdźmy dla ceny za metr.
probka_price_per_m <- sample(baza$price_per_m, 5000)
shapiro.test(probka_price_per_m)
#p-value < 2.2e-16 <=> brak normalności. w=0.84 <=> dane bardziej "symetryczne"

#Ad.7 Wnioski:
#Trzeba będzie używać mediany bądź odfiltrować odstające wyniki które psują średnią
#Raczej skupiamy się na testach nieparametrycznych... Chyba, że to odfiltrowanie
#nas uratuje. Można popróbować. Teoretycznie jak się patrzy na histogramy ceny
#całkowitej i ceny za m^2 to fragmenty wykresu wydają się mieć rozkład normalny

baza <- baza %>%
  mutate (price_per_m = total_price / area) %>% 
  mutate(log_price = log(total_price)) %>% 
  mutate(log_price_m = log(price_per_m))

baza_filtered <- baza %>% 
  filter(price_per_m < 25000) %>% 
  filter(log_price < 14.75 & log_price > 12.5 ) %>% 
  filter(log_price >= 12.75) %>% 
  filter(log_price_m >=9.2) %>% 
  filter(log_price < 14.35) %>% 
  filter(total_price <= 1500000) %>% 
  filter(price_per_m < 21500 | price_per_m > 22000)

#===============================================
#Propozycje modeli docelowych
#===============================================

#1. Regresja liniowa
#   a) zmienna wyjaśniana: total_price
#   b) zmienna wyjaśniana: price_per_m
#Uzasadnienie:
#Wykresy pokazują, że cena całkowita i za m^2 wyraźnie zależy od dzielnicy
#Wykres rozrzutu pokazuje, że cena rośnie wraz z rosnącą powierzchnią. Linia
#trendu jest wyraźnie pochylona (około 45 procent). Ciekawe jest to, że wraz
#z rosnącą powierzchnią cena jest coraz bardziej rozproszona. Może uda stworzyć
#nam się model który będzie w stanie trafnie przewidywać cene.
#W przypadku price_per_m odpada nam area do użycia jako zmienna objaśniająca.
#Model wydaje się być wtedy ciekawszy bo trzeba szukać czy inne zmienne będą
#w stanie objaśnić nam price_per_m

#2. Analiza skupień
#Ciężko obecnie nie pisać ogólnie, ale uda znaleźć się zmienne które najlepiej
#różnicują oferty i będziemy w stanie wyróżnić pewne grupy. Np.
#Budżetowe: Małe i stare mieszkania z niską ceną za m^2
#Inwestycyjne: małe i nowe mieszkania z wysoką ceną za m^2
#Rodzinne stare: duże i stare mieszkania z niską ceną za m^2
#itd.
#Uwaga:
#Tutaj już nie uciekniemy od filtrowania bazy z wyników odstających. W regresji
#liniowej odstępstwa nas interesują bo to potencjalna okazja bądź zdzierstwo

#===============================================
#Relacje pomiędzy parami zmiennych
#===============================================
#1. Przygotowanie bazy
baza_clean <- baza %>%
  filter(!is.na(build_year)) %>%
  filter(build_year > 1550 & build_year <= 2026) %>%
  filter(rooms != 0)

#2. Korelacja ceny całkowitej i powierzchni
cor.test(baza_clean$total_price, baza_clean$area, method = "spearman", exact = FALSE)
#rho 0.8227432 p-value < 2.2e-16
#Pierwszy przykład oczywisty, ale no... Wyraźna korelacja dodatnia =>
# powierzchnia jest głównym czynnikiem całkowitą cene

#3. Korelacja ceny za m^2 i roku budowy
cor.test(baza_clean$price_per_m, baza_clean$build_year, method = "spearman", exact = FALSE)
#rho 0.009556547 p-value = 0.3856
#Niska korelacja, wysoka wartośc p => wynik nieistotny statystycznie.
#Problem przez metode. Rok budowy wiele razy się powtórzył więc spearman który
#opiera się na nadawaniu "rang" mógł zgłupieć... Jest obawa, że to źle dobrana
#metoda do "zadania".

#4.Zależność ceny za m^2 od stanu wykończenia
kruskal.test(price_per_m ~ finishing_state, data = baza_clean)
#chi-squared = 312.2, p-value < 2.2e-16
#p < 0.05 => Ceny za m^2 w zależności od stanu wykończenia się różnią

#===============================================
#Przygotowanie pod regresje liniową
#===============================================
baza_clean <- baza_filtered %>%
  filter(!is.na(build_year)) %>%
  filter(build_year > 1550 & build_year <= 2026) %>%
  filter(rooms != 0) %>%
  filter(!is.na(finishing_state)) %>%
  select(-id, -source, -city, -avaiable_from, -created_at) %>%
  mutate(
    district = as.factor(district),
    finishing_state = as.factor(finishing_state),
    market_type = as.factor(market_type),
    advertiser_type = as.factor(advertiser_type),
    has_elevator = as.numeric(has_elevator)
  )

summary(baza_clean$district)
hist(baza_clean$log_price)

ggplot(baza_clean, aes(x = area, y = total_price)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", color = "red") +
  labs(
    title = "Wpływ powierzchni na cenę całkowitą",
    x = "Powierzchnia",
    y = "Cena",
  ) +
  theme_minimal()

ggplot(baza_clean, aes(x = area, y = log_price)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", color = "red") +
  labs(
    title = "Wpływ powierzchni na cenę całkowitą (log)",
    x = "Powierzchnia",
    y = "Cena",
    ) +
  theme_minimal()

plot(log_price~area, data=baza_clean)
plot(log_price~district, data=baza_clean)

model_tp_area <- lm(total_price ~ area, data = baza_clean)
model_tp_area_base <- lm(total_price ~ area, data = baza)
summary(model_tp_area)
summary(model_tp_area_base)
#zwiększenie area o 1 powoduje wzrost ceny o 11930.5
#jest bardzo istotna statystycznie
#wyjania 65.86% wariancji
#p-value: < 2.2e-16

par(mfrow=c(2,2))
par(mar=c(3,3,3,3))
plot(model_tp_area) # to interpretuję
plot(model_tp_area_base)
# WNIOSKI
# 1. Dla wyższych cen jednorodność wariancji się rozjeżdża - heteroskedastyczność
# 2. reszty modelu nie mają rozkładu normalnego
# 3. Scale-Location - idzie w góre. Potwierdza problem z pierwszego wykresu
# dla wyższych cen większy rozrzut błędów

model_tp_2 <- lm(total_price ~ area + build_year, data = baza_clean)
summary(model_tp_2)
#zwiększenie build_year o 1 powoduje wzrost ceny o 228.38
#0.000154 <=> istotne statystycznie
#mały wzrost w wyjaśnionej wariancji

model_tp_3 <- lm(total_price ~ area + build_year + district + finishing_state, data = baza_clean)
summary(model_tp_3)
plot(model_tp_3)

model_log <- lm(log_price ~ area + build_year + district + finishing_state, data = baza_clean)
summary(model_log)
plot(model_log)

model_log_2 <- lm(log_price ~ area*district + build_year + latitude + longitude + finishing_state, data = baza_clean)
summary(model_log_2)
library(car)
vif(model_log_2, type = "predictor")
plot(model_log_2)


#Czyszczenie najbadziej niedoszacowanych ofert dla model_log_2

baza_clean <- baza_clean %>%
  mutate(residual = resid(model_log_2))

baza_clean_filtered <- baza_clean %>%
  arrange(residual) %>%
  slice(-(1:100)) %>%
  select(-residual)

model_log_2 <- lm(log_price ~ area*district + build_year + latitude + longitude + finishing_state, data = baza_clean_filtered)
summary(model_log_2)
plot(model_log_2)

#==================================
#Analiza skupień - metoda PAM
#==================================
library(cluster)
library(factoextra)

#1. Dane do tworzenia klastrów:
wybrane <- select(baza_clean_filtered,c("log_price", "area", "build_year"))
#2 Standaryzacja
wybrane_stand <- scale(wybrane)

#3.Sprawdźmy dla 4 klastów
wynik <- pam(wybrane_stand,4)
fviz_cluster(wynik,data = wybrane_stand)

#4. Ile klastrów wybrać
fviz_nbclust(wybrane_stand, pam, method = "wss")
gap <- clusGap(wybrane_stand, pam, K.max = 8, B=500)
fviz_gap_stat(gap)

#5. 6 klastrów
wynik2 <- pam(wybrane_stand,6)
fviz_cluster(wynik2,data = wybrane_stand)

#7. Jak interpretować wymiary?
res.pca <- prcomp(wybrane_stand)
fviz_pca_var(res.pca, col.var = "black")

#Dim1 (oś pozioma) - log_price + area
#Lewo - mieszkania większe i droższe
#Prawo - mieszkania mniejsze i tańsze
#Dim2 (oś pionowa) - build_year
#Góra - wyższy rok budowy
#Dół - niższy rok budowy

#7. Analiza klastrów:
#Klastry 1, 4, 2 - nowsze mieszkania których różni powierzchnia i cena
#Klaster 6 - Stare budownictwo. Zawiera stare mieszkania bez podziału na powierzchnię i cenę
#Klaster 3 - Małe i mikro mieszkania. Zarówno starsze jak i nowsze. 
#Klaster 5 - Mieszkania premium. Duże, drogie i w znacznej większości nowsze
