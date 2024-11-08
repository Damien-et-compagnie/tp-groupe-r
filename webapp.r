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
  plotOutput("plot_filtered_flights"),
  plotOutput("plot_retard_moyen_distance"),
  plotOutput("destination_retard"),
  plotOutput("delay_companie")
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
  
  
  ## retard moyen par distance de vol
  delay_distance_by_origin_dest <- flights %>%
    group_by(origin, dest) %>%
    summarise(
      delay_moy = mean(arr_delay, na.rm = TRUE),
      distance = first(distance)  
    ) %>%
    filter(dest != "HNL") %>%
    arrange(desc(distance)) 
  
  output$plot_retard_moyen_distance <- renderPlot({
    ggplot(delay_distance_by_origin_dest, aes(x = distance, y = delay_moy)) +
      geom_point(aes(color = origin), alpha = 0.7) +
      geom_smooth(method = "lm", se = FALSE, color = "black") +
      labs(title = "Relation entre distance et retard moyen",
           x = "Distance (miles)",
           y = "Retard moyen (min)",
           color = "Aéroport d'origine") +
      theme_minimal()
  })
  
  ## Destinations les plus touchées par le retard moyen
  delay_by_dest <- flights %>%
    filter(!is.na(dep_delay) & !is.na(arr_delay)) %>%
    group_by(dest) %>%
    summarise(
      delay_moy = mean(arr_delay, na.rm = TRUE),  
      n_vols = n()                              
    ) %>%
    arrange(desc(delay_moy)) 

  output$destination_retard <- renderPlot({ggplot(delay_by_dest, aes(x = reorder(dest, delay_moy), y = delay_moy)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      labs(
        title = "Retard moyen par destinations ",
        x = "Destination",
        y = "Retard moyen (min)"
      ) +
      theme_minimal()})
  
  #retard par companie
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
  
  output$delay_companie <- renderPlot({
    ggplot(delay_by_carrier, aes(x = reorder(carrier, delay_moy), y = delay_moy, fill = carrier)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(
        title = "Retard moyen par compagnies aériennes ",
        x = "Compagnie aérienne",
        y = "Retard moyen (min)",
        fill = "Compagnie"
      ) +
      theme_minimal()  
  })
  
}


# Lancer l'application Shiny
shinyApp(ui = ui, server = server)
