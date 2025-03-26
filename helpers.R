# helpers.R - Helper functions for the Budget Tracker app

# Function to safely get currency symbol
get_currency_symbol <- function(currency_code) {
  if (is.null(currency_code) || currency_code == "" || 
      is.null(currency_symbols[[currency_code]]) || 
      length(currency_symbols[[currency_code]]) == 0) {
    return("£") # Default to pound if something goes wrong
  }
  return(currency_symbols[[currency_code]])
}

# Function to safely format amount with currency
format_currency <- function(amount, currency_code = "GBP") {
  symbol <- get_currency_symbol(currency_code)
  return(sprintf("%s%.2f", symbol, amount))
}

# Helper function to get formatted date ranges
get_period_range_text <- function(period_type) {
  end_date <- Sys.Date()
  
  if(period_type == "monthly") {
    # Calculate first and last day of current month
    first_day <- as.Date(format(end_date, "%Y-%m-01"))
    last_day <- as.Date(as.POSIXlt(as.numeric(as.POSIXlt(first_day)) + 32*24*60*60))
    last_day <- as.Date(format(last_day, "%Y-%m-01")) - 1
    
    # Format as "March 1-31, 2025"
    return(sprintf("%s %d-%d, %s", 
                   format(first_day, "%B"),
                   as.numeric(format(first_day, "%d")),
                   as.numeric(format(last_day, "%d")),
                   format(first_day, "%Y")))
  } else if(period_type == "weekly") {
    # Calculate first day of week (Monday)
    days_since_monday <- as.numeric(format(end_date, "%u")) - 1
    first_day <- end_date - days_since_monday
    last_day <- first_day + 6  # Sunday
    
    # If month changes within the week, format as "March 29-April 4, 2025"
    if(format(first_day, "%B") != format(last_day, "%B")) {
      return(sprintf("%s %d-%s %d, %s", 
                     format(first_day, "%B"),
                     as.numeric(format(first_day, "%d")),
                     format(last_day, "%B"),
                     as.numeric(format(last_day, "%d")),
                     format(first_day, "%Y")))
    } else {
      # Same month: "March 24-30, 2025"
      return(sprintf("%s %d-%d, %s", 
                     format(first_day, "%B"),
                     as.numeric(format(first_day, "%d")),
                     as.numeric(format(last_day, "%d")),
                     format(first_day, "%Y")))
    }
  }
  
  # Default fallback
  return("Current Period")
}

# Function to safely escape strings for SQL queries
sql_escape <- function(text) {
  if (is.null(text) || is.na(text)) {
    return("")
  }
  return(gsub("'", "''", as.character(text)))
}