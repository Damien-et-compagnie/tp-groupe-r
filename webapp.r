library(shiny)
library(DBI)
library(RMariaDB)
library(dplyr)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Analyse des données des aéroports"),
  
  plotOutput("plot_dest_per_carrier"),
  plotOutput("plot_dest_per_carrier_origin")
)

server <- function(input, output, session) {
  con <- dbConnect(
    RMariaDB::MariaDB(),
    user = "384054_admin",
    password = "qZ9XcWesVVq9tiY",
    dbname = "tp-groupe-r_airport",
    host = "mysql-tp-groupe-r.alwaysdata.net",
    port = 3306
  )
  
  onStop(function() {
    dbDisconnect(con)
  })
  
  result <- dbGetQuery(con, "SELECT * FROM flights")
  
  dest_per_carrier <- result %>%
    group_by(carrier) %>%
    summarise(num_destinations = n_distinct(dest)) %>%
    arrange(desc(num_destinations))
  
  # Calculer le nombre de destinations par compagnie et par aéroport d'origine
  dest_per_carrier_origin <- result %>%
    group_by(carrier, origin) %>%
    summarise(num_destinations = n_distinct(dest)) %>%
    arrange(carrier, desc(num_destinations))
  
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
}

# Lancer l'application Shiny
shinyApp(ui = ui, server = server)
