library(shiny)
library(DBI)
library(RMariaDB)
library(dplyr)
library(ggplot2)

# Interface utilisateur de l'application
ui <- fluidPage(
  titlePanel("Analyse des données des aéroports"),
  
  # Graphiques d'origine
  plotOutput("plot_dest_per_carrier"),
  plotOutput("plot_dest_per_carrier_origin"),
  
  # Nouveaux graphiques de couverture
  plotOutput("plot_coverage_by_carrier"),
  plotOutput("plot_destination_coverage"),
  
  # Nouveau graphique pour les compagnies UA, AA, DL
  plotOutput("plot_filtered_flights")
)

# Serveur de l'application
server <- function(input, output, session) {
  # Connexion à la base de données
  con <- dbConnect(
    RMariaDB::MariaDB(),
    user = "384054_admin",
    password = "qZ9XcWesVVq9tiY",
    dbname = "tp-groupe-r_airport",
    host = "mysql-tp-groupe-r.alwaysdata.net",
    port = 3306
  )
  
  # Déconnecte la base de données lorsque l'application s'arrête
  onStop(function() {
    dbDisconnect(con)
  })
  
  # Récupération et traitement des données
  flights <- dbGetQuery(con, "SELECT * FROM flights")
  
  # Calcul pour le premier set de graphiques
  dest_per_carrier <- flights %>%
    group_by(carrier) %>%
    summarise(num_destinations = n_distinct(dest)) %>%
    arrange(desc(num_destinations))
  
  dest_per_carrier_origin <- flights %>%
    group_by(carrier, origin) %>%
    summarise(num_destinations = n_distinct(dest)) %>%
    arrange(carrier, desc(num_destinations))
  
  # Calcul pour le second set de graphiques
  all_origins <- unique(flights$origin)
  
  coverage_by_carrier <- flights %>%
    group_by(carrier) %>%
    summarise(origins_covered = n_distinct(origin),
              covers_all_origins = n_distinct(origin) == length(all_origins))
  
  all_destinations <- unique(flights$dest)
  
  destination_coverage <- flights %>%
    group_by(carrier) %>%
    summarise(destinations_covered = n_distinct(dest),
              covers_all_destinations = n_distinct(dest) == length(all_destinations))
  
  # Graphique 1 : Nombre de destinations par compagnie
  output$plot_dest_per_carrier <- renderPlot({
    ggplot(dest_per_carrier, aes(x = reorder(carrier, -num_destinations), y = num_destinations)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      labs(title = "Nombre de destinations par compagnie aérienne",
           x = "Compagnie aérienne",
           y = "Nombre de destinations") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Graphique 2 : Nombre de destinations par compagnie et par aéroport d'origine
  output$plot_dest_per_carrier_origin <- renderPlot({
    ggplot(dest_per_carrier_origin, aes(x = carrier, y = num_destinations, fill = origin)) +
      geom_bar(stat = "identity") +
      labs(title = "Nombre de destinations par compagnie et aéroport d'origine",
           x = "Compagnie aérienne",
           y = "Nombre de destinations") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Nouveau graphique 1 : Couverture des aéroports d'origine par chaque compagnie
  output$plot_coverage_by_carrier <- renderPlot({
    ggplot(coverage_by_carrier, aes(x = reorder(carrier, -origins_covered), fill = covers_all_origins)) +
      geom_bar(stat = "identity", aes(y = origins_covered)) +
      labs(title = "Couverture complète des aéroports d'origine par chaque compagnie",
           x = "Compagnie aérienne",
           y = "Nombre d'aéroports d'origine desservis") +
      scale_fill_manual(values = c("TRUE" = "lightgreen", "FALSE" = "coral"),
                        labels = c("TRUE" = "Dessert tous les aéroports", "FALSE" = "Ne dessert pas tous les aéroports")) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Nouveau graphique 2 : Couverture des destinations par chaque compagnie
  output$plot_destination_coverage <- renderPlot({
    ggplot(destination_coverage, aes(x = reorder(carrier, -destinations_covered), fill = covers_all_destinations)) +
      geom_bar(stat = "identity", aes(y = destinations_covered)) +
      labs(title = "Couverture complète des destinations par chaque compagnie",
           x = "Compagnie aérienne",
           y = "Nombre de destinations desservies") +
      scale_fill_manual(values = c("TRUE" = "lightblue", "FALSE" = "salmon"),
                        labels = c("TRUE" = "Dessert toutes les destinations", "FALSE" = "Ne dessert pas toutes les destinations")) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Nouveau graphique 3 : Nombre de vols pour les compagnies UA, AA, DL
  output$plot_filtered_flights <- renderPlot({
    flights_filtered <- flights %>%
      filter(carrier %in% c("UA", "AA", "DL"))
    
    ggplot(flights_filtered, aes(x = carrier)) +
      geom_bar(fill = "skyblue") +
      labs(title = "Nombre de vols par compagnie (United, American, Delta)",
           x = "Compagnie aérienne",
           y = "Nombre de vols") +
      theme_minimal()
  })
}

# Lancer l'application Shiny
shinyApp(ui = ui, server = server)
