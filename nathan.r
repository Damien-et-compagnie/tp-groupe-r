# Combien chaque compagnie a desservi de destination ; combien chaque compagnie a desservie de destination par aéroport d’origine. Réaliser les graphiques adéquats qui synthétisent ces informations ?

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

                  
result <- dbGetQuery(mydb, "SELECT * FROM flights")

View(result)

summary(result)

# Calculer le nombre de destinations par compagnie
dest_per_carrier <- result %>%
  group_by(carrier) %>%
  summarise(num_destinations = n_distinct(dest)) %>%
  arrange(desc(num_destinations))

# Afficher le résultat
print(dest_per_carrier)

# Calculer le nombre de destinations par compagnie et par aéroport d'origine
dest_per_carrier_origin <- result %>%
  group_by(carrier, origin) %>%
  summarise(num_destinations = n_distinct(dest)) %>%
  arrange(carrier, desc(num_destinations))

print(dest_per_carrier_origin)

ggplot(dest_per_carrier, aes(x = reorder(carrier, -num_destinations), y = num_destinations)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Nombre de destinations par compagnie aérienne",
       x = "Compagnie aérienne",
       y = "Nombre de destinations") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(dest_per_carrier_origin, aes(x = carrier, y = num_destinations, fill = origin)) +
  geom_bar(stat = "identity") +
  labs(title = "Nombre de destinations par compagnie et aéroport d'origine",
        x = "Compagnie aérienne",
        y = "Nombre de destinations") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
