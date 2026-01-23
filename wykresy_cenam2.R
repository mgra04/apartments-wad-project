#Zależność ceny mieszkania od metrażu

baza_clean %>%
  filter(total_price <= 3500000, area <= 200) %>%
  ggplot(aes(x = area, y = total_price)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Zależność ceny mieszkania od metrażu",
    x = "Powierzchnia (m^2)",
    y = "Cena całkowita (zł)"
  )

#Zależność ceny za m^2 mieszkania od dzielnicy

baza_clean %>%
  filter(price_per_m < 30000) %>%
  mutate(
    district = reorder(district, price_per_m, FUN = median)
  ) %>%
  ggplot(aes(y = district, x = price_per_m)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") + 
  labs(
    title = "Cena za m^2 mieszkania a dzielnica",
    x = "Cena za m^2",
    y = "Dzielnica"
  ) 
