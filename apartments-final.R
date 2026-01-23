#===============================================
#Biblioteki (do pobrania jeśli nie masz)
#===============================================
install.packages("nortest")
#===============================================
#Biblioteki
#===============================================
library(tidyverse)
library(openxlsx)
library(nortest)
library(e1071)
#===============================================
#Baza
#===============================================
baza <- read.xlsx("C:/WAD/Dane/apartments.xlsx")

#===============================================
#Opis bazy
#===============================================
#Baza zawiera dane wszystkich ofert mieszkań na sprzedaż w Krakowie z otodom.pl 
#z dnia 04.01.2026. Zebrane dane i ich opis:
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
#log_price - zlogarytmowana cena całkowita
#log_price_per_m - zlogarytmowana cena za metr kwadratowy
#log_area - zlogarytmowana powierzchnia

#===============================================
#Wstępna eksploracja
#===============================================

#1. Sprawdzenie typów danych
typy_danych <- data.frame(
  kolumna = names(baza),
  typ = sapply(baza, class)
)

print(typy_danych)

#Ad. 1. wszystkie dane poza has_elevator (logical) mają typ character

#2. Decyzja odnośnie dostosowania typów w bazie w celu umożliwienia dalszej analizy
# numeric -> latidute, longitude, total_price, area, rooms, floor, total_floors
# build_year, has elevator
# factor -> district, finishing_state, market_type, advertiser_type, heating_type
baza <- baza %>%
  mutate(across(
    c(latitude, longitude, total_price, area, rooms, floor, total_floors, build_year), 
    as.numeric
  )) %>%
  mutate(across(
    c(district, finishing_state, market_type, advertiser_type, heating_type),
    as.factor
  ))

#3. Czyszczenie bazy ze zbędnych danych
#id - wystraczy zostawić source które też jest unikalne i pozwala podejrzeć ofertę
#city - stała wartość "Cracow" <=> nic nie wnosi
#available_from - dużo braków i dziwnych wartości dla niektórych mieszkań jak
#dostępność od 2024 roku

baza <- baza %>%
  select(-id, -city, -avaiable_from)

#4. Dodanie nowych zmiennych
#price_per_m, log_price, log_price_m
baza <- baza %>%
  mutate(price_per_m = total_price / area) %>% 
  mutate(log_price = log(total_price)) %>% 
  mutate(log_price_m = log(price_per_m)) %>%
  mutate(log_area = log(area))

#5. Analiza zmiennej total_price i log_price
summary(baza$total_price)
#Min.     1st Qu.  Median   Mean    3rd Qu. Max. 
#160000   670000   819000   954493  1030000 16500000
#Mediana dużo mniejsza od średniej <=> problem z wartościami odstającymi
#3 kwartyl ma wartość 1mln, a wartość maksymalna ponad 16mln
#To zaburza średnią, ale podejrzyjmy jeszcze histogram

summary(baza$log_price)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#11.98   13.42   13.62   13.67   13.85   16.62

hist(baza$total_price,
     breaks = 30,
     main = "Histogram cen mieszkań",
     xlab = "Cena")
#Oferty powyżej 3 milionów to pojedyńcze oferty. Można je odfiltrować i przeprowadzać
#dalszą analizę dla mieszkań z ceną do 3mln
hist(baza$total_price[baza$total_price <= 3000000],
     breaks = 30,
     main = "Histogram cen mieszkań do 3mln zł",
     xlab = "Cena")
#Wyraźna prawoskośność. Większość mieszkań w cenach 0.5mln - 1mln
#brak rozkładu normalnego => problem z wykonaniem testów parametrycznych

hist(baza$log_price,
     breaks = 30,
     main = "Histogram zlogarytmowanych cen mieszkań",
     xlab = "Zlogarytmowana cena")
#Wygląda to lepiej. Zwłaszcza, że nie odfiltrowaliśmy jeszcze outlinerów.
#Dalej jesteśmy daleko od rozkładu normalnego, ale wykres daje przestrzeń
#do podjęcia walki

#Przeprowadzimy jeszcze testy do ostatecznego potwierdzenia

#shapriro.test jest ograniczony do 5000 argumetnów więc trzeba posłużyć się innym

#Test Kolmogorowa-Smirnowa
ks.test(baza$total_price, "pnorm")
#Problem z wybranym testem - nie jest odporny na powtarzające się wartości

#Test Lillieforsa
#dla total_price
lillie.test(baza$total_price)
#D = 0.20929, p-value < 2.2e-16

#dla log_price
lillie.test(baza$log_price)
#D = 0.088214, p-value < 2.2e-16
#p value w obu przypadkach znacząco mniejsze od 0.05. Nawet nie jesteśmy blisko
#rozkładu normalnego.

#Sprawdzenie skośności
skewness(baza$total_price)
#[1] 8.579692
#Wynik potwierdza to co pokazał histogram. silna skośność dodatnia (prawostronna)
#Co oznacza większą ilość mieszkań w niższej, standardowej cenie
skewness(baza$log_price)
#[1] 1.191421
#Skośność w dalszym ciągu jest silna, ale widać duży spadek w porównaniu
#skośnośći dla niezlogarytmowanej ceny

#6. Analiza zmiennej area (zlogarytmowane wartości dopiero później po odfiltrowaniu
#bazy)
summary(baza$area)
#Oj... 3 kwartyl to 66m^2, a max to 1415 m^2. Zdecydowanie jest to kolejna zamienna
#która wymaga odfiltrowania ze skrajnych wartości po skończonym etapie analizy
#pojedyńczych zmiennych. Co ciekawe najmniejsze mieszkanie ma powierzchnię
#12m^2 <=> patologia (moim zdaniem. Niekórzy mają większą cele. Mam nadzieje, że
#to biuro przypadkiem wrzucone do kategorii mieszkań)

hist(baza$area,
     breaks = 30,
     main = "Histogram powierzchni mieszkań",
     xlab = "powierzchnia")
#ciężkie do interetacji. Powyżej około 250m^2 znikoma ilość ofert

hist(baza$area[baza$area <= 250],
     breaks = 30,
     main = "Histogram powierzchni mieszkań",
     xlab = "powierzchnia")
#Nawet powyżej 150m^2 znikoma ilość ofert
hist(baza$area[baza$area <= 150],
     breaks = 30,
     main = "Histogram powierzchni mieszkań",
     xlab = "powierzchnia")
#Widać wyraźną dominacje mieszkań z powierzchnią 30-70m^2. Poza tym zakresem
#ilosć ofert jest niższa, ale nie są to pojedyńczne przypadki. Trzeba będzie się
#zastanowić nad wyborem zakresu danych w finalnej bazie.

#7. Sprawdzenie ilości ofert na dzielnice
oferty_na_dzielnice <- baza %>%
  group_by(district) %>%
  summarise(liczba_ofert = n()) %>%
  arrange(desc(liczba_ofert))
print(oferty_na_dzielnice)
#duże zrożnicowanie. Zakres 105-2115 🥷🤙
summary(oferty_na_dzielnice)

#8. Analiza cen mieszkań ze względu na dzielnicę:
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
  arrange(desc(Median))
print(analiza_dzielnic_total_price)
#Jest problem z odstającymi wartościami więc posłużę się medianą
#Top 3 mediany (jedyne które przekroczyły 1mln):
#a) Zwierzyniec: 1732745
#b) Grzegórzki: 1100000
#c) Stare Miasto: 1084038
#Min 3 mediany:
#a) Bieńczyce: 589000
#b) Nowa Huta: 628894
#c) Mistrzejowice: 

#9. Analiza ceny za m^2 mieszkań ze względu na dzielnicę:
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
  arrange(desc(Median))
print(analiza_dzielnic_price_per_m)
#Top 3 mediany:
#Stare Miasto: 21557
#Grzegórzki: 21243
#Zwierzyniec: 19302
#Min 3 mediany:
#swoszowice: 13163
#Bieżanów-Prokocim: 13175
#Nowa Huta: 13362

#10. Analiza powierzchni mieszkań ze względu na dzielnicę:
analiza_dzielnic_area <- baza %>%
  group_by(district) %>%
  summarise(
    liczba_ofert = n(),
    Min = min(area, na.rm = TRUE), #Nie ma braków w powierzchni, ale niech zostanie
    `1st Qu.` = quantile(area, 0.25, na.rm = TRUE),
    Median = median(area, na.rm = TRUE),
    Mean = mean(area, na.rm = TRUE),
    `3rd Qu.` = quantile(area, 0.75, na.rm = TRUE),
    Max = max(area, na.rm = TRUE)
  ) %>%
  arrange(desc(Median))
print(analiza_dzielnic_area)
#Bardzo ciekawy wynik. Mediana powierzchni mieszkań w Zwierzyńcu to aż 91.3m^2
#na 201 ofert. Kolejne są mieszkania na Bieżanowie 57m^2. Jest drastyczna 
#róznica, aż 24.3m^2. Najmniejsze mieszkania mamy w Bieńczycach: 43.5m^2

#11. Ilość ofert ze względu na rodzaj sprzedawcy (osoba prywatna, agencja, deweloper)
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

#12. Analiza ceny za m^2 ze względu na sprzedawcę
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
#średnia w agencji może być zawyżona prze oferty odstające. Np. Mają oni to 
#nieszczęsne mieszkanie z ceną 79800 za m^2
#Median   Mean 
#15781. 16421. <- developer 
#16000  17078. <- agency
#15777. 16748. <- private

#===============================================
#Wykresy - baza przed filtrowaniem (ewentualne filtrowanie wpisane w kod
#odpowiedzialny za utworzenie wykresu)
#===============================================

#1. Boxplot cen mieszkań do 3mln ze względu na dzielnice
baza %>%
  filter(total_price <= 3000000) %>%
  ggplot(aes(x = reorder(district, total_price, FUN = median), y = total_price)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  coord_flip() + #dla lepszej czytelności wykresu
  scale_y_continuous(labels = scales::comma) + #żeby w cenach nie było np. 1e+06
  labs(
    title = "Rozkład cen mieszkań do 3mln w dzielnicach",
    x = "Dzielnica",
    y = "Cena"
  ) +
  theme_minimal()
#Zwierzyniec wydaje się być dzielnicą "Premium", pudełko zaczyna się od ceny
# 1 000 000 i wyraźnie odstaje od reszty. To wynika z tego co sprawdziliśmy
# wcześniej (mediana mieszkań w Zwierzyńcu to 91.3m^2).
# Różnice w cenach również są największe (najdłuższe pudełko). 
# Najkorzystniejsze oferty pod względem ceny znajdują się w Bieńczycach,
# Nowej Hucie i Mistrzejowicach

#2. Boxplot cen za m^2 mieszkań (do 25000 za m^2) ze względu na dzielnice
baza %>%
  filter(price_per_m <= 25000) %>%
  ggplot(aes(x = reorder(district, price_per_m , FUN = median), y = price_per_m)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  coord_flip() +
  labs(
    title = "Rozkład cen za m^2 (do 25000zł) mieszkań w dzielnicach",
    x = "Dzielnica",
    y = "Cena za m^2"
  ) +
  theme_minimal()
#Najdroższe mieszkania w Starym mieście. Jak podniesiemy ograniczenie
#w cenie bądź nawet usuniemy to możemy zaobserować sporo outlinerów w Starym Mieście
#gdzie ceny sięgają nawet powyżej 50000zł za m^2. 3 Mieszkania z najwyższą ceną
#za m^2 znajdują się w Zwierzyńcu 62000-70000 i jedno 79800 które
#zobaczyliśmy w summary(baza$price_per_m)

#3. Wykres rozrzutu ceny i powierzchni z podziałem na dzielnice
# cena do 3 000 000 i powierzchnia do 200 m^2
baza %>%
  filter(total_price <= 3000000, area <= 200) %>%
  ggplot (aes(x=area, y=total_price, colour = district)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Zależność ceny (do 3mln) od powierzchni (do 200m^2)",
    x = "Powierzchnia",
    y = "Cena",
    color = "Dzielnica"
  ) +
  theme_minimal()
#Trochę nieczytelne, ale można zaobserwować, że w dzielnicy Grzegórzki linia
#trendu jest najbardziej stroma, a w swoszowicach najmniej

#Ad.3 Spróbujmy jeszcze bez podziału
baza %>%
  filter(total_price <= 3000000, area <= 200) %>%
  ggplot (aes(x=area, y=total_price)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Zależność ceny (do 3mln) od powierzchni (do 200m^2)",
    x = "Powierzchnia",
    y = "Cena",
    color = "Dzielnica"
  ) +
  theme_minimal()
#piękna chmurka. Możemy zaobserwować, że wraz ze wzrostem powierzchni cena,
#staje się coraz bardziej zróżnicowana/nieprzewidalna (odstaje od lini trendu)

#===============================================
#Filtrowanie bazy. Walka o normalność
#===============================================
#1.Filtrowanie bazy
baza_filtered <- baza %>%
  filter(
    total_price >= 500000 & total_price <= 1380000,
    area >= 25 & area <= 110,
    price_per_m >= 7000 & price_per_m <= 28000,
    build_year > 1550 & build_year <= 2026,
    rooms != 0,
    total_floors >= floor,
  )
#stracone 3950 ofert

#2.Analiza log_price po filtrowaniu
summary(baza_filtered$log_price)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#13.12   13.42   13.59   13.60   13.77   14.14 
#Wygląda to obiecująco

hist(baza_filtered$log_price,
     breaks = 30,
     main = "Histogram zlogarytmowanych cen mieszkań",
     xlab = "Zlogarytmowana cena")
#Wygląda to już dużo lepiej

lillie.test(baza_filtered$log_price)
#p-value < 2.2e-16
#Daleko od rozkładu normalnego, ale może to wynikać z:
#niestyntetycznych danych
#dużej ilości argumentów
#sprawdżmy skośność
skewness(baza_filtered$log_price)
#[1] 0.1461568
#Udało się zbić skośność z ponad 1 do 0.146. Jest to spore zbliżenie do rozkładu
#symetrycznego. Mieści się w zakresie 0-0.5
#sprawdźmy jeszcze kurtozę
kurtosis(baza_filtered$log_price)
#[1] -0.6931759
#kurtoza ujemna, ale dosyć bliska zera. Rozkład jest lekko spłaszczony

#3.Analiza log_area po filtrowaniu
summary(baza_filtered$log_area)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#3.219   3.748   3.932   3.930   4.130   4.700 

hist(baza_filtered$log_area,
     breaks = 30,
     main = "Histogram zlogarytmowanej powierzchni mieszkań",
     xlab = "Zlogarytmowana powierzchnia")
#Histogram ma się jednak nieco gorzej. Wartość około 4.25 "psuje" rozkład

lillie.test(baza_filtered$log_area)
#p-value < 2.2e-16
#nie spodziewałem się niczego innego...

skewness(baza_filtered$log_area)
#[1] -0.05447489
#:000000
#Praktycznie zero. Blisko symetryczności

kurtosis(baza_filtered$log_area)
#[1] -0.4003116
#Rozkład wciąż lekko spłaszczony, ale mniej niż w przypadku log_price.

#===============================================
#Powtórzone wykresy dla odfiltrowanej bazy
#===============================================
#1. Boxplot cen mieszkań ze względu na dzielnice
baza_filtered %>%
  ggplot(aes(x = reorder(district, total_price, FUN = median), y = total_price)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  coord_flip() + #dla lepszej czytelności wykresu
  scale_y_continuous(labels = scales::comma) + #żeby w cenach nie było np. 1e+06
  labs(
    title = "Rozkład cen mieszkań do 1.38mln w dzielnicach",
    x = "Dzielnica",
    y = "Cena"
  ) +
  theme_minimal()

#2. Boxplot cen za m^2 mieszkań ze względu na dzielnice
baza_filtered %>%
  ggplot(aes(x = reorder(district, price_per_m , FUN = median), y = price_per_m)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  coord_flip() +
  labs(
    title = "Rozkład cen za m^2 mieszkań w dzielnicach",
    x = "Dzielnica",
    y = "Cena za m^2"
  ) +
  theme_minimal()

#3. Wykres rozrzutu ceny i powierzchni z podziałem na dzielnice
baza_filtered %>%
  ggplot (aes(x=area, y=total_price, colour = district)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Zależność ceny (do 1.38mln) od powierzchni (do 110m^2)",
    x = "Powierzchnia",
    y = "Cena",
    color = "Dzielnica"
  ) +
  theme_minimal()

#Ad.3 Spróbujmy jeszcze bez podziału
baza_filtered %>%
  ggplot (aes(x=area, y=total_price)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Zależność ceny (do 1.38mln) od powierzchni (do 110m^2)",
    x = "Powierzchnia",
    y = "Cena",
    color = "Dzielnica"
  ) +
  theme_minimal()

#4. Wykres rozrzutu zlogarytmowanej ceny i powierzchni z podziałem na dzielnice
baza_filtered %>%
  ggplot (aes(x=log_area, y=log_price, colour = district)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Zależność ceny (do 1.38mln) od powierzchni (do 110m^2)",
    x = "Powierzchnia",
    y = "Cena",
    color = "Dzielnica"
  ) +
  theme_minimal()

#Ad.4 Spróbujmy jeszcze bez podziału

baza_filtered %>%
  ggplot (aes(x=log_area, y=log_price)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Zależność ceny (do 1.38mln) od powierzchni (do 110m^2)",
    x = "Powierzchnia",
    y = "Cena",
    color = "Dzielnica"
  ) +
  theme_minimal()

#===============================================
#Modele docelowe
#===============================================

#1. Regresja liniowa
#   a) zmienna wyjaśniana: total_price

#2. Analiza skupień

#===============================================
#Relacje pomiędzy parami zmiennych
#===============================================


#===============================================
#Regresja liniowa
#===============================================
#1. Przygotowanie bazy do regresji:
baza_lr <- baza %>%
  filter(
    total_price >= 500000 & total_price <= 1380000,
    area >= 25 & area <= 110,
    price_per_m >= 7000 & price_per_m <= 28000,
    build_year > 1550 & build_year <= 2026,
    rooms != 0,
    total_floors >= floor,
    !is.na(finishing_state),
    !is.na(advertiser_type),
    !is.na(market_type),
  )


#===============================================
#Analiza skupień
#===============================================