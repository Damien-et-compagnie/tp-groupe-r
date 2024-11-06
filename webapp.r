library(DBI)
library(RMariaDB)

# Établir la connexion à la base de données
con <- dbConnect(
  RMariaDB::MariaDB(),
  user = "384054_admin",
  password = "qZ9XcWesVVq9tiY",
  dbname = "tp-groupe-r_airport",
  host = "mysql-tp-groupe-r.alwaysdata.net",  # Par exemple, "localhost" pour un serveur local
  port = 3306                # Par défaut, le port est 3306
)

# Vérification de la connexion
if (!is.null(con)) {
  cat("Connexion réussie !\n")
} else {
  cat("Erreur de connexion.\n")
}


ui <- fluidPage(
  titlePanel("Analyse des données des aéroports"),
)

server <- function(input, output) {}

shinyApp(ui = ui, server = server)