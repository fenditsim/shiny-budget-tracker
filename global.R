# global.R - Global variables, constants, and libraries for the Budget Tracker app

# Load required libraries
library(shiny)
library(DBI)
library(RSQLite)
library(shinyjs)
library(shinyalert)  # For pop-up messages
library(writexl)     # For Excel exports
library(echarts4r)   # For donut chart

# Source other files in the correct order, ensuring functions are available globally
source("helpers.R", local = FALSE)
source("db.R", local = FALSE)

# Initialize available fonts
fonts <- c(
  "Arial" = "Arial",
  "Helvetica" = "Helvetica",
  "Verdana" = "Verdana", 
  "Trebuchet MS" = "Trebuchet MS",
  "Times New Roman" = "Times New Roman",
  "Georgia" = "Georgia",
  "SF Pro (iOS)" = "SF Pro Text, -apple-system",
  "San Francisco (iOS)" = "-apple-system, BlinkMacSystemFont",
  "Roboto (Android)" = "Roboto, sans-serif"
)

# Initialize available currencies (alphabetical by code)
currencies <- c(
  "Australian Dollar (A$)" = "AUD",
  "Bulgarian Lev (лв)" = "BGN",
  "Canadian Dollar (C$)" = "CAD",
  "Swiss Franc (CHF)" = "CHF",
  "Chinese Yuan (¥)" = "CNY",
  "Czech Koruna (Kč)" = "CZK",
  "Danish Krone (kr)" = "DKK",
  "Euro (€)" = "EUR",
  "British Pound (£)" = "GBP",
  "Hong Kong Dollar (HK$)" = "HKD",
  "Croatian Kuna (kn)" = "HRK",
  "Hungarian Forint (Ft)" = "HUF",
  "Indian Rupee (₹)" = "INR",
  "Icelandic Króna (kr)" = "ISK",
  "Japanese Yen (¥)" = "JPY",
  "Norwegian Krone (kr)" = "NOK",
  "Polish Złoty (zł)" = "PLN",
  "Romanian Leu (lei)" = "RON",
  "Swedish Krona (kr)" = "SEK",
  "Ukrainian Hryvnia (₴)" = "UAH",
  "US Dollar ($)" = "USD"
)

# Currency symbols for display (alphabetical by code)
currency_symbols <- c(
  "AUD" = "A$",
  "BGN" = "лв",
  "CAD" = "C$",
  "CHF" = "CHF",
  "CNY" = "¥",
  "CZK" = "Kč",
  "DKK" = "kr",
  "EUR" = "€",
  "GBP" = "£",
  "HKD" = "HK$",
  "HRK" = "kn",
  "HUF" = "Ft",
  "INR" = "₹",
  "ISK" = "kr",
  "JPY" = "¥",
  "NOK" = "kr",
  "PLN" = "zł",
  "RON" = "lei",
  "SEK" = "kr",
  "UAH" = "₴",
  "USD" = "$"
)

# Define expense categories
expense_categories <- c('Bills', 'Cash', 'Cheque', 'Home & Property', 'Leisure', 
                        'Other', 'Personal', 'Personal Finance', 'Shopping', 
                        'Supermarket', 'Transport', 'Travel')