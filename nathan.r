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

library(dplyr)
library(ggplot2)

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
View(airports)
summary(airlines)
summary(weather)
summary(planes)

all_origins <- unique(flights$origin)

coverage_by_carrier <- flights %>%
  group_by(carrier) %>%
  summarise(origins_covered = n_distinct(origin),
  covers_all_origins = n_distinct(origin) == length(all_origins))

non_full_coverage <- coverage_by_carrier %>%
  filter(!covers_all_origins)

View(all_origins)
View(coverage_by_carrier)
View(non_full_coverage) 

all_destinations <- unique(flights$dest)

destination_coverage <- flights %>%
  group_by(carrier) %>%
  summarise(destinations_covered = n_distinct(dest),
  covers_all_destinations = n_distinct(dest) == length(all_destinations))

full_destination_coverage <- destination_coverage %>%
  filter(covers_all_destinations)

print(all_destinations)
View(all_destinations)
View(destination_coverage)
View(full_destination_coverage)

library(ggplot2)

ggplot(coverage_by_carrier, aes(x = reorder(carrier, -origins_covered), fill = covers_all_origins)) +
  geom_bar(stat = "identity", aes(y = origins_covered)) +
  labs(title = "Couverture complète des aéroports d'origine par chaque compagnie",
       x = "Compagnie aérienne",
       y = "Nombre d'aéroports d'origine desservis") +
  scale_fill_manual(values = c("TRUE" = "lightgreen", "FALSE" = "coral"),
                    labels = c("TRUE" = "Dessert tous les aéroports", "FALSE" = "Ne dessert pas tous les aéroports")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(destination_coverage, aes(x = reorder(carrier, -destinations_covered), fill = covers_all_destinations)) +
  geom_bar(stat = "identity", aes(y = destinations_covered)) +
  labs(title = "Couverture complète des destinations par chaque compagnie",
       x = "Compagnie aérienne",
       y = "Nombre de destinations desservies") +
  scale_fill_manual(values = c("TRUE" = "lightblue", "FALSE" = "salmon"),
                    labels = c("TRUE" = "Dessert toutes les destinations", "FALSE" = "Ne dessert pas toutes les destinations")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
