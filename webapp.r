library(shiny)
library(DBI)
library(RMariaDB)

# Interface utilisateur de l'application
ui <- fluidPage(
  titlePanel("Analyse des données des aéroports"),
  mainPanel(
    tableOutput("tableData")
  )
)

# Serveur de l'application
server <- function(input, output, session) {
    con <- dbConnect(
        RMariaDB::MariaDB(),
        user = "384054_admin",
        password = "qZ9XcWesVVq9tiY",
        dbname = "tp-groupe-r_airport",
        host = "mysql-tp-groupe-r.alwaysdata.net",
        port = 3306
    )
  
  output$tableData <- renderTable({
    dbReadTable(con, "airlines")
  })
  
  onStop(function() {
    dbDisconnect(con)
  })
}

shinyApp(ui = ui, server = server)
