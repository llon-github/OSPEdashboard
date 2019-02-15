library(shiny)
library(shinydashboard)
library(highcharter)
library(dplyr)
library(stringr)
library(purrr)
library(tidyr)
library(DT)
library(readxl)
library(RSQLite)

sqlitePath <- "db//master.db"
query <- paste0("SELECT * FROM catalog")
db <- dbConnect(SQLite(), dbname=sqlitePath)
dbdata <- dbGetQuery(db, query)
dbDisconnect(db)



