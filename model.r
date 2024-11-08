
if (!require(DBI)) {
  install.packages("DBI")
  library(DBI)
}

if (!require(RMariaDB)) {
  install.packages("RMariaDB")
  library(RMariaDB)
}

if (!require(caret)) {
  install.packages("caret")
  library(caret)
}

if (!require(DMwR)) {
    install.packages(c("zoo","xts","quantmod", "ROCR"))
    install.packages( "C:/Users/nbarb/Downloads/DMwR_0.4.1.tar.gz", repos=NULL, type="source" )
    library(DMwR)
}

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


flights <- flights %>%
  mutate(delay_flag = ifelse(dep_delay > 15, 1, 0)) %>%
  filter(!is.na(flights$dep_time))



flights <- flights %>%
  mutate(carrier = as.factor(carrier)) %>%
  mutate(delay_flag = as.factor(delay_flag))

View(flights)

set.seed(123)
train_index <- createDataPartition(flights$delay_flag, p = 0.8, list = FALSE)
train_data <- flights[train_index, ]
test_data <- flights[-train_index, ]


## J'ai essayé
# training_balanced <- DMwR::SMOTE(delay_flag ~ hour + distance + dep_time + distance + carrier, sample_n(train_data, 100) , k = 1, perc.over = 100, perc.under = 200) 

# View(training_balanced)

model <- glm(delay_flag ~ hour + distance + dep_time + distance + carrier, data = train_data, family = binomial())

summary(model)

predictions <- predict(model, test_data, type = "response")
predicted_classes <- ifelse(predictions > 0.5, 1, 0)

confusionMatrix(factor(predicted_classes), factor(test_data$delay_flag))
