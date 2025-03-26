# app.R - Main entry point for Shiny app

# Load required libraries first
library(shiny)
library(DBI)
library(RSQLite)
library(shinyjs)
library(shinyalert)
library(writexl)
library(echarts4r)

# Source all component files (environment = .GlobalEnv ensures functions are available globally)
source("helpers.R", local = FALSE)
source("db.R", local = FALSE)
source("ui.R", local = FALSE)
source("server.R", local = FALSE)

# Create a function that ensures the database is closed when the app stops
onStop(function() {
  if (exists("con") && dbIsValid(con)) {
    dbDisconnect(con)
    cat("Database connection closed.\n")
  }
})

# Run the application
shinyApp(ui = ui, server = server)