if (!require(readxl)) {
  install.packages("readxl")
  library(readxl)
}

if (!require(DBI)) {
  install.packages("DBI")
  library(DBI)
}

if (!require(RMariaDB)) {
  install.packages("RMariaDB")
  library(RMariaDB)
}

if (!require(purrr)) {
  install.packages("purrr")
  library(purrr)
}

if (!require(pdftools)) {
  install.packages("pdftools")
  library(pdftools)
}

if (!require(stringr)) {
  install.packages("stringr")
  library(stringr)
}

if (!require(plotly)) {
  install.packages("plotly")
  library(plotly)
}

library(ggplot2)
library(dplyr)
library(lubridate)

db_user <- '384054_admin'
db_password <- 'qZ9XcWesVVq9tiY'
db_name <- 'tp-groupe-r_airport'
db_host <- 'mysql-tp-groupe-r.alwaysdata.net'
db_port <- 3306

# Connexion à la base de données
mydb <- dbConnect(MariaDB(), user = db_user, password = db_password,
                  dbname = db_name, host = db_host, port = db_port)

                  
flights <- dbGetQuery(mydb, "SELECT * FROM flights")
airports <- dbGetQuery(mydb, "SELECT * FROM airports")
airlines <- dbGetQuery(mydb, "SELECT * FROM airlines")
weather <- dbGetQuery(mydb, "SELECT * FROM weather")
planes <- dbGetQuery(mydb, "SELECT * FROM planes")

summary(flights)
summary(airports)
View(flights)
summary(airlines)
summary(weather)
summary(planes)

# Constater qu’on peut reconstituer l’information sched_dep_time à partir de year, month, day, hour et minute. Transformer alors cette colonne en type datetime. Transformer également dep_time, arr_time et sched_arr_time et supprimer les colonnes year, month, day, hour, minute

flights <- flights %>%
  mutate(
    sched_dep_time = make_datetime(year, month, day, hour, minute),
    dep_time = make_datetime(year, month, day, floor(dep_time / 100), dep_time %% 100),
    arr_time = make_datetime(year, month, day, floor(arr_time / 100), arr_time %% 100),
    sched_arr_time = make_datetime(year, month, day, floor(sched_arr_time / 100), sched_arr_time %% 100)
  ) %>%
  mutate(
    arr_time = if_else(arr_time < sched_dep_time, arr_time + days(1), arr_time),
    sched_arr_time = if_else(sched_arr_time < sched_dep_time, sched_arr_time + days(1), sched_arr_time)
  ) %>%
  select(-year, -month, -day, -hour, -minute)

# Vérification
summary(flights$sched_dep_time)
summary(flights$dep_time)
summary(flights$arr_time)
summary(flights$sched_arr_time)

View(flights)

# Visualisation du trafic mensuel moyen par aéroport d’origine

flights <- flights %>%
  mutate(month_year = floor_date(sched_dep_time, "month"))

monthly_traffic <- flights %>%
  group_by(origin, month_year) %>%
  summarise(
    num_flights = n(),
    days_in_month = n_distinct(day(sched_dep_time))  
  ) %>%
  mutate(
    avg_flights_per_day = num_flights / days_in_month, 
    avg_flights_per_month = avg_flights_per_day * days_in_month  
  )

monthly_avg <- monthly_traffic %>%
  group_by(origin) %>%
  summarise(overall_avg_per_month = mean(avg_flights_per_month))

ggplot(monthly_traffic, aes(x = month_year, y = avg_flights_per_month, color = origin)) +
  geom_line() +
  geom_hline(data = monthly_avg, aes(yintercept = overall_avg_per_month, color = origin),
             linetype = "dashed") +
  facet_wrap(~ origin, scales = "free_y") +
  labs(
    title = "Évolution du trafic mensuel moyen par aéroport d'origine",
    x = "Mois",
    y = "Nombre moyen de vols par mois"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

# Visualisation du taux d’accroissement mensuel du trafic par aéroport d’origine

monthly_traffic <- flights %>%
  mutate(month = as.Date(format(sched_dep_time, "%Y-%m-01"))) %>% 
  group_by(origin, month) %>%
  summarise(total_flights = n(), .groups = 'drop')

monthly_growth <- monthly_traffic %>%
  group_by(origin) %>%
  arrange(month) %>%
  mutate(
    growth_rate = (total_flights - lag(total_flights)) / lag(total_flights) * 100 


ggplot(monthly_growth, aes(x = month, y = growth_rate, color = origin)) +
  geom_line(size = 1) +
  facet_wrap(~ origin) +
  labs(
    title = "Taux d'accroissement mensuel du trafic par aéroport d'origine",
    x = "Mois",
    y = "Taux de croissance (%)",
    color = "Aéroport"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Top 3 aéroports d'origine et de destination avec le plus de trafic

top_origins <- flights %>%
  group_by(origin) %>%
  summarise(total_flights = n()) %>%
  arrange(desc(total_flights)) %>%
  slice_head(n = 3) %>%
  left_join(airports, by = c("origin" = "faa")) %>%
  select(name) 

top_destinations <- flights %>%
  group_by(dest) %>%
  summarise(total_flights = n()) %>%
  arrange(desc(total_flights)) %>%
  slice_head(n = 3) %>%
  left_join(airports, by = c("dest" = "faa")) %>%
  select(name) 

print("Top 3 aéroports d'origine avec le plus de trafic :")
print(top_origins)

print("Top 3 aéroports de destination avec le plus de trafic :")
print(top_destinations)

################################################################################
###### Est-il vrai que le retard à l’arrivée augmente avec la distance ?! ######
################################################################################

delay_distance_by_origin_dest <- flights %>%
  group_by(origin, dest) %>%
  summarise(
    delay_moy = mean(arr_delay, na.rm = TRUE),
    distance = first(distance)  
  ) %>%
  filter(dest != "HNL") %>%
  arrange(desc(distance)) 

View(delay_distance_by_origin_dest)




## retard moyen par distance de vol
ggplot(delay_distance_by_origin_dest, aes(x = distance, y = delay_moy)) +
  geom_point(aes(color = origin), alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Relation entre distance et retard moyen",
       x = "Distance (miles)",
       y = "Retard moyen (min)",
       color = "Aéroport d'origine") +
  theme_minimal()




## retard moyenne par compagnie de vol
delay_by_dest_carrier <- flights %>%
  filter(!is.na(dep_delay) & !is.na(arr_delay)) %>%
  group_by(dest, carrier) %>%
  summarise(
    delay_moy = mean(arr_delay, na.rm = TRUE),   
    n_vols = n()                               
  ) %>%
  arrange(desc(delay_moy)) 

View(delay_by_dest_carrier, 10)




## Destinations les plus touchées par le retard moyen
delay_by_dest <- flights %>%
  filter(!is.na(dep_delay) & !is.na(arr_delay)) %>%
  group_by(dest) %>%
  summarise(
    delay_moy = mean(arr_delay, na.rm = TRUE),  
    n_vols = n()                              
  ) %>%
  arrange(desc(delay_moy)) 

ggplot(delay_by_dest, aes(x = reorder(dest, delay_moy), y = delay_moy)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Destinations les plus touchées par le retard moyen",
    x = "Destination",
    y = "Retard moyen (min)"
  ) +
  theme_minimal()




delay_by_carrier <- flights %>%
  mutate(
    arr_delay = ifelse(is.na(arr_delay), 0, arr_delay),
    dep_delay = ifelse(is.na(dep_delay), 0, dep_delay)
  ) %>%
  group_by(carrier) %>%
  summarise(
    delay_moy = mean(arr_delay, na.rm = TRUE),
    n_vols = n()
  ) %>%
  arrange(desc(delay_moy))

ggplot(delay_by_carrier, aes(x = reorder(carrier, delay_moy), y = delay_moy, fill = carrier)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Compagnies aériennes les plus touchées par le retard moyen",
    x = "Compagnie aérienne",
    y = "Retard moyen (min)",
    fill = "Compagnie"
  ) +
  theme_minimal()



## Retard moyen par heures de départ
flights <- flights %>%
  mutate(dep_hour = hour(sched_dep_time))

delay_by_dep_hour <- flights %>%
  filter(!is.na(dep_delay) & !is.na(arr_delay)) %>%
  group_by(dep_hour) %>%
  summarise(
    delay_moy = mean(dep_delay, na.rm = TRUE),
    n_vols = n()
  )

ggplot(delay_by_dep_hour, aes(x = dep_hour, y = delay_moy)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "Retard moyen par heure de départ",
    x = "Heure de départ",
    y = "Retard moyen (min)"
  ) +
  theme_minimal()




## Retard moyen par heures d'arrivée
flights <- flights %>%
  mutate(arr_hour = hour(sched_arr_time))

delay_by_arr_hour <- flights %>%
  filter(!is.na(dep_delay) & !is.na(arr_delay)) %>%
  group_by(arr_hour) %>%
  summarise(
    delay_moy = mean(arr_delay, na.rm = TRUE),
    n_vols = n()  
  )

ggplot(delay_by_arr_hour, aes(x = arr_hour, y = delay_moy)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "Retard moyen par heure d'arrivée",
    x = "Heure d'arrivée",
    y = "Retard moyen (min)"
  ) +
  theme_minimal()




## Différence de retard moyen entre départ et arrivée
delay_comparison <- delay_by_dep_hour %>%
  left_join(delay_by_arr_hour, by = c("dep_hour" = "arr_hour"), suffix = c("_dep", "_arr"))

delay_comparison <- delay_comparison %>%
  mutate(diff = delay_moy_arr - delay_moy_dep)

ggplot(delay_comparison, aes(x = dep_hour, y = diff)) +
  geom_line(color = "purple", size = 1) +
  geom_point(color = "purple", size = 2) +
  labs(
    title = "Différence de retard moyen entre départ et arrivée",
    x = "Heure de départ",
    y = "Différence de retard moyen (min)"
  ) +
  theme_minimal()


flights <- dbGetQuery(mydb, "SELECT * FROM flights")
weather <- dbGetQuery(mydb, "SELECT * FROM weather")


text <- pdf_text("data/weather.pdf")

# Initialiser une liste pour stocker les données de chaque page
all_data_list <- list()

# Boucler à travers chaque page pour extraire les lignes
for (page_text in text) {
  lines <- str_split(page_text, "\n")[[1]]  # Diviser le texte en lignes
  data_list <- str_split(lines, ",")        # Diviser chaque ligne par des virgules
  all_data_list <- append(all_data_list, data_list[-1])  # Ajouter chaque page (en évitant les en-têtes répétées)
}

# Convertir la liste complète en dataframe
weather <- as.data.frame(do.call(rbind, all_data_list))
colnames(weather) <- c("origin","year","month","day","hour","temp","dewp","humid","wind_dir",
                       "wind_speed","wind_gust","precip","pressure","visib","time_hour")






weather <- weather %>%
  mutate(datetime = make_datetime(year = as.numeric(year), 
                                  month = as.numeric(month), 
                                  day = as.numeric(day), 
                                  hour = as.numeric(hour)))

# Evolution de la température au cours du temps
ggplot(weather, aes(x = datetime, y = as.numeric(temp))) +
  geom_line(color = "blue") +
  labs(
    title = "Evolution de la température au cours du temps",
    x = "Date",
    y = "Température (F)"
  ) +
  theme_minimal()

# Evolution du taux d'humidité au cours du temps
humidity_plot <- ggplot(weather, aes(x = datetime, y = as.numeric(humid))) +
  geom_line(color = "darkgreen") +
  labs(
    title = "Evolution du taux d'humidité au cours du temps",
    x = "Date",
    y = "Humidité (%)"
  ) +
  theme_minimal()

ggplotly(humidity_plot)

# Evolution de la visibilité au cours du temps
visibility_plot <- ggplot(weather, aes(x = datetime, y = as.numeric(visib))) +
  geom_line(color = "purple") +
  labs(
    title = "Evolution de la visibilité au cours du temps",
    x = "Date",
    y = "Visibilité (miles)"
  ) +
  theme_minimal()

ggplotly(visibility_plot)

# Evolution des précipitations au cours du temps
precipitation_plot <- ggplot(weather, aes(x = datetime, y = as.numeric(precip))) +
  geom_line(color = "darkorange") +
  labs(
    title = "Evolution des précipitations au cours du temps",
    x = "Date",
    y = "Précipitations (inches)"
  ) +
  theme_minimal()

ggplotly(precipitation_plot)






flights <- flights %>%
  mutate(year = as.character(year),
         month = as.character(month),
         day = as.character(day),
         hour = as.character(hour))

weather <- weather %>%
  mutate(year = as.character(year),
         month = as.character(month),
         day = as.character(day),
         hour = as.character(hour))

flights_weather <- flights %>%
  left_join(weather, by = c("year", "month", "day", "hour", "origin"))

flights_weather_clean <- flights_weather %>%
  filter(!is.na(temp) & !is.na(humid) & !is.na(precip) & !is.na(visib))

View(flights_weather_clean)




# Retard moyen par rapport à la température
plot_temp_delay <- flights_weather %>%
  filter(!is.na(dep_delay), !is.na(temp)) %>%
  plot_ly(x = ~temp, y = ~dep_delay, type = 'scatter', mode = 'markers', 
          marker = list(color = 'blue', size = 5),
          text = ~paste('Flight:', flight, 'Delay:', dep_delay, 'Temp:', temp)) %>%
  layout(
    title = "Impact de la température sur les retards de départ",
    xaxis = list(title = "Température (°F)"),
    yaxis = list(title = "Retard de départ (minutes)"),
    hovermode = "closest"
  )

plot_temp_delay


# Retard moyen par rapport à l'humidité
plot_humidity_delay <- flights_weather %>%
  filter(!is.na(dep_delay), !is.na(humid)) %>%
  plot_ly(x = ~humid, y = ~dep_delay, type = 'scatter', mode = 'markers', 
          marker = list(color = 'green', size = 5),
          text = ~paste('Flight:', flight, 'Delay:', dep_delay, 'Humidity:', humid)) %>%
  layout(
    title = "Impact de l'humidité sur les retards de départ",
    xaxis = list(title = "Humidité (%)"),
    yaxis = list(title = "Retard de départ (minutes)"),
    hovermode = "closest"
  )

plot_humidity_delay

# Retard moyen par rapport à la visibilité
plot_visib_delay <- flights_weather %>%
  filter(!is.na(dep_delay), !is.na(visib), !visib %in% c("10.0")) %>%
  plot_ly(x = ~visib, y = ~dep_delay, type = 'scatter', mode = 'markers', 
          marker = list(color = 'purple', size = 5),
          text = ~paste('Flight:', flight, 'Delay:', dep_delay, 'Visibility:', visib)) %>%
  layout(
    title = "Impact de la visibilité sur les retards de départ",
    xaxis = list(title = "Visibilité (miles)"),
    yaxis = list(title = "Retard de départ (minutes)"),
    hovermode = "closest"
  )

plot_visib_delay

View(flights_weather %>%
  filter(!is.na(dep_delay), !is.na(visib), !visib %in% c("10.0")) %>%
  group_by(visib) %>%
  summarise(
    delay_moy = mean(dep_delay, na.rm = TRUE),
    n_vols = n()))
  



# Retard moyen par rapport à la visibilité
plot_visib_delay <- flights_weather %>%
  filter(!is.na(arr_delay), !is.na(visib)) %>%
  plot_ly(x = ~visib, y = ~arr_delay, type = 'scatter', mode = 'markers', 
          marker = list(color = 'purple', size = 5),
          text = ~paste('Flight:', flight, 'Arrival Delay:', arr_delay, 'Visibility:', visib)) %>%
  layout(
    title = "Impact de la visibilité sur les retards d'arrivée",
    xaxis = list(title = "Visibilité (miles)"),
    yaxis = list(title = "Retard d'arrivée (minutes)"),
    hovermode = "closest"
  )

plot_visib_delay


# Retard moyen par rapport à la vitesse du vent
plot_wind_speed_delay <- flights_weather %>%
  filter(!is.na(dep_delay), !is.na(wind_speed)) %>%
  plot_ly(x = ~wind_speed, y = ~dep_delay, type = 'scatter', mode = 'markers', 
          marker = list(color = 'orange', size = 5),
          text = ~paste('Flight:', flight, 'Delay:', dep_delay, 'Wind Speed:', wind_speed)) %>%
  layout(
    title = "Impact de la vitesse du vent sur les retards de départ",
    xaxis = list(title = "Vitesse du vent (m/s)"),
    yaxis = list(title = "Retard de départ (minutes)"),
    hovermode = "closest"
  )

plot_wind_speed_delay