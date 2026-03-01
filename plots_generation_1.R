library("dplyr")
library("ggplot2")
library("ggridges")
library("forcats")

df <- read.csv("Car_sale_ads.csv")
cars_pln <- df %>% filter(Currency == "PLN")


cars_pln$Year_Group <- cut(cars_pln$Production_year,
                     breaks = c(1915, 1998, 2004, 2009, 2015, 2021),
                     labels = c("1915-1998", "1999-2004", "2005-2009", "2010-2015", "2016-2021"),
                     include.lowest = TRUE)

new_cars <- cars_pln %>% filter(Production_year >= 1999)

wykresik1 <-ggplot(cars_pln, aes(x = Displacement_cm3/1000, y = fct_rev(Year_Group), fill = Year_Group)) +
  geom_density_ridges(alpha = 0.5, scale = 1.2) +
  scale_fill_manual(values = c("orange", "darkorange", "chocolate","saddlebrown", "brown4")) +
  labs(
    title = "Litraż samochodów a ich wiek",
    x = "Pojemność silnika (litry)",
    y = "Liczność",
    fill = "Grupa rocznikowa"
  ) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  theme_minimal() +
  theme(text = element_text(face = "bold"),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 18))+
  xlim(0, 3.5)

print(wykresik1)

transmission_percentages <- new_cars %>%
  filter(Transmission %in% c("Manual", "Automatic")) %>%
  group_by(Production_year, Transmission) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(Production_year) %>%
  mutate(percentage = count / sum(count) * 100)

transmission_percentages <- transmission_percentages %>%
  mutate(Transmission = recode(Transmission,
                               "Manual" = "Manualna",
                               "Automatic" = "Automatyczna"))

transmission_percentages <- transmission_percentages %>%
  mutate(Transmission = factor(Transmission, levels = c("Manualna", "Automatyczna")))

# Wykres
wykresik2 <-ggplot(transmission_percentages, aes(x = Production_year, y = percentage, fill = Transmission)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = c("Manualna" = "blue3", "Automatyczna" = "red3")) +  
  theme_minimal() +
  labs(  title = "Typ skrzyni biegów a rok produkcji",
         subtitle = "Manualna czy automatyczna? Udział procentowy według roku",
         x = "Rok produkcji",
         y = "Procent",
         fill = "Typ skrzyni biegów") +
  scale_x_continuous(expand = c(0, 0))+
  scale_y_continuous(labels = scales::percent_format(scale = 100), expand = c(0, 0)) +
  theme(text = element_text(face = "bold"),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1,size = 10),
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 15, face = "plain")) 

print(wykresik2)
##########

wykresik2 <- ggplot(transmission_percentages, aes(x = Production_year, y = percentage, color = Transmission, group = Transmission)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c("Manualna" = "blue3", "Automatyczna" = "red3")) +
  theme_minimal() +
  labs(
    title = "Typ skrzyni biegów a rok produkcji",
    subtitle = "Manualna czy automatyczna? Udział procentowy według roku",
    x = "Rok produkcji",
    y = "Procent",
    color = "Typ skrzyni biegów"
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1),limits = c(0, 90), expand = c(0, 0)) +
  theme(
    text = element_text(face = "bold"),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 15, face = "plain")
  )
print(wykresik2)
# Same automaty
automatic_percentages <- transmission_percentages %>%
  filter(Transmission == "Automatyczna")
# Tworzenie wykresu


ggplot(automatic_percentages, aes(x = Production_year, y = percentage, fill = Transmission)) +
  geom_bar(stat = "identity", position = "stack") +  # Zmieniamy na 'stack' zamiast 'fill'
  scale_fill_manual(values = c("Automatyczna" = "#ff7f0e")) +  # Kolor pomarańczowy
  theme_minimal() +
  labs(title = "Udział skrzyń automatycznych według roku produkcji",
       x = "Rok produkcji",
       y = "Procent") + 
  scale_y_continuous(labels = scales::percent_format(scale = 1), expand = c(0, 0), limits = c(0, 100)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

#########################

# Calculate the percentages for each Fuel_type by Production_year
fuel_percentages <- new_cars %>%
  filter(Fuel_type %in% c("Gasoline", "Gasoline + LPG", "Diesel", "Electric", "Hybrid")) %>%
  group_by(Production_year, Fuel_type) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(Production_year) %>%
  mutate(percentage = count / sum(count) * 100)


ggplot(fuel_percentages, aes(x = Production_year, y = percentage, color = Fuel_type, group = Fuel_type)) +
  geom_line(size = 1) +  # Line graph instead of bar chart
  scale_color_manual(values = c(
    "Gasoline" = "#1f77b4",
    "Gasoline + LPG" = "#ff7f0e",
    "Diesel" = "#2ca02c",
    "Electric" = "purple",
    "Hybrid" = "red"
  )) +
  theme_minimal() +
  labs(title = "Udział rodzajów paliwa według roku produkcji",
       x = "Rok produkcji",
       y = "Procent",
       color = "Typ paliwa") +
  scale_y_continuous(labels = scales::percent_format(scale = 1), expand = c(0, 0), limits = c(0, 100)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

