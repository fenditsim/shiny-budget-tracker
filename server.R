# server.R - Server logic for the Budget Tracker app
library(echarts4r)
library(htmlwidgets)

server <- function(input, output, session) {
  # Initialize database connection inside server function
  con <- setup_db()
  
  # Create a reactive value to store the parsed data for importing
  parsed_text_data <- reactiveVal(NULL)
  
  # Modify the preview_text_data_btn observer to store the parsed data
  # Replace the entire preview_text_data_btn observer with this version:
  
  # Preview text data button handler
  observeEvent(input$preview_text_data_btn, {
    # Get the text area content
    text_data <- input$expense_text_data
    
    # Check if text is empty
    if (is.null(text_data) || text_data == "") {
      output$text_preview_ui <- renderUI({
        div(
          class = "alert alert-warning",
          "Please enter some data in the text area."
        )
      })
      shinyjs::hide("import_text_button_container")
      parsed_text_data(NULL)  # Clear any previously stored data
      return()
    }
    
    # Try to parse the text as CSV
    tryCatch({
      # Split the text into lines
      lines <- strsplit(text_data, "\\r?\\n")[[1]]
      lines <- lines[lines != ""] # Remove empty lines
      
      if (length(lines) == 0) {
        output$text_preview_ui <- renderUI({
          div(
            class = "alert alert-warning",
            "No data found. Please enter data in CSV format."
          )
        })
        shinyjs::hide("import_text_button_container")
        parsed_text_data(NULL)  # Clear any previously stored data
        return()
      }
      
      # Print lines for debugging
      cat("Lines to parse:\n")
      for (i in 1:length(lines)) {
        cat(i, ":", lines[i], "\n")
      }
      
      # Check if the header row exists and add it if not
      if (!grepl("date", tolower(lines[1]))) {
        # Add header row
        lines <- c("date,item_name,amount,category", lines)
        cat("Added header row\n")
      }
      
      # Combine lines back to text
      csv_text <- paste(lines, collapse = "\n")
      
      # Parse the CSV text
      con <- textConnection(csv_text)
      df <- read.csv(con, stringsAsFactors = FALSE, 
                     header = TRUE, 
                     sep = ",", 
                     check.names = FALSE)
      close(con)
      
      # Print parsed data for debugging
      cat("Parsed data structure:\n")
      print(str(df))
      print(head(df))
      
      # Check if required columns exist
      required_cols <- c("date", "item_name", "amount", "category")
      missing_cols <- required_cols[!required_cols %in% colnames(df)]
      
      if (length(missing_cols) > 0) {
        output$text_preview_ui <- renderUI({
          div(
            class = "alert alert-danger",
            h5("Error: Missing Columns"),
            p("The following required columns are missing:"),
            tags$ul(
              lapply(missing_cols, function(col) {
                tags$li(col)
              })
            ),
            p("Make sure your data has these columns or add a header row.")
          )
        })
        shinyjs::hide("import_text_button_container")
        parsed_text_data(NULL)  # Clear any previously stored data
        return()
      }
      
      # Validate data types
      validation_errors <- c()
      
      # Check date format
      invalid_dates <- which(!grepl("^\\d{4}-\\d{2}-\\d{2}$", df$date))
      if (length(invalid_dates) > 0) {
        validation_errors <- c(validation_errors, 
                               sprintf("Invalid date format in rows: %s. Use YYYY-MM-DD format.", 
                                       paste(invalid_dates[1:min(length(invalid_dates), 5)], collapse = ", ")))
      }
      
      # Check amount is numeric
      if (!is.numeric(df$amount)) {
        df$amount <- as.numeric(df$amount)
        invalid_amounts <- which(is.na(df$amount))
        if (length(invalid_amounts) > 0) {
          validation_errors <- c(validation_errors, 
                                 sprintf("Invalid amount values in rows: %s. Use numeric values.", 
                                         paste(invalid_amounts[1:min(length(invalid_amounts), 5)], collapse = ", ")))
        }
      }
      
      # Check category is valid
      invalid_categories <- which(!df$category %in% expense_categories)
      if (length(invalid_categories) > 0) {
        validation_errors <- c(validation_errors, 
                               sprintf("Invalid categories in rows: %s. Categories must be one of: %s", 
                                       paste(invalid_categories[1:min(length(invalid_categories), 5)], collapse = ", "),
                                       paste(expense_categories, collapse = ", ")))
      }
      
      # If validation errors exist, show them
      if (length(validation_errors) > 0) {
        output$text_preview_ui <- renderUI({
          div(
            class = "alert alert-danger",
            h5("Validation Errors:"),
            tags$ul(
              lapply(validation_errors, function(error) {
                tags$li(error)
              })
            )
          )
        })
        shinyjs::hide("import_text_button_container")
        parsed_text_data(NULL)  # Clear any previously stored data
        return()
      }
      
      # Store the valid data frame in our reactive value for later use
      parsed_text_data(df)
      cat("Data stored in reactive value, row count:", nrow(df), "\n")
      
      # Show preview table
      preview_rows <- min(nrow(df), 5)  # Preview first 5 rows
      
      # Format the preview data
      preview_df <- df[1:preview_rows, ]
      
      # Make the import button visible
      shinyjs::show("import_text_button_container")
      
      output$text_preview_ui <- renderUI({
        div(
          class = "upload-preview",
          h5(sprintf("Preview (showing %d of %d rows):", preview_rows, nrow(df))),
          div(style = "overflow-x: auto;",
              renderTable({
                preview_df
              }, striped = TRUE, bordered = TRUE, align = "l")
          ),
          p(sprintf("Total records to import: %d", nrow(df))),
          div(
            class = "alert alert-success",
            "Data looks good! Click 'Import Text Data' to add these records to your account."
          )
        )
      })
      
    }, error = function(e) {
      # Hide the import button
      shinyjs::hide("import_text_button_container")
      parsed_text_data(NULL)  # Clear any previously stored data
      
      # Print error for debugging
      cat("Error parsing data:", e$message, "\n")
      
      output$text_preview_ui <- renderUI({
        div(
          class = "alert alert-danger",
          h5("Error Parsing Data"),
          p("There was an error parsing your data:"),
          p(e$message),
          p("Please make sure your data is in proper CSV format.")
        )
      })
    })
  })
  
  # Initialize reactive values
  user_data <- reactiveValues(
    user_id = NULL,
    username = NULL,
    display_name = NULL,
    is_authenticated = FALSE
  )
  
  # Add a reactive value to store theme, font, and currency preferences
  # Explicitly set light theme as default
  app_settings <- reactiveValues(
    theme = "light",
    font = "Arial",
    currency = "GBP",
    currency_symbol = "£"
  )
  
  # Add reactive value to track expense changes
  expense_tracker <- reactiveValues(
    update_counter = 0,
    shown_dialogs = list()  # Add dialog tracking here
  )
  
  # Update currency sign next to Amount automatically
  observeEvent(app_settings$currency, {
    if (user_data$is_authenticated) {
      # Update the Amount label with the current currency symbol
      updateNumericInput(
        session,
        "expense_amount",
        label = paste0("Amount (", app_settings$currency_symbol, ")"),
        value = input$expense_amount  # Preserve current value
      )
      
      updateNumericInput(
        session,
        "income_amount",
        label = paste0("Amount (", app_settings$currency_symbol, ")"),
        value = input$expense_amount  # Preserve current value
      )
      
      # Update this part to use the period from user settings
      period_text <- input$limit_period
      period_text <- paste0(toupper(substr(period_text, 1, 1)), 
                            substr(period_text, 2, nchar(period_text)))
      
      updateNumericInput(
        session,
        "spending_limit",
        label = paste0(period_text, " Spending Limit (", app_settings$currency_symbol, ")"),
        value = input$spending_limit  # Preserve current value
      )
    }
  })
  
  # Update spending limit label when limit period changes
  observeEvent(input$limit_period, {
    if(user_data$is_authenticated) {
      # Get the period text and capitalize first letter
      period_text <- input$limit_period
      period_text <- paste0(toupper(substr(period_text, 1, 1)), 
                            substr(period_text, 2, nchar(period_text)))
      
      # Update the spending limit label with both the period and currency symbol
      updateNumericInput(
        session,
        "spending_limit",
        label = paste0(period_text, " Spending Limit (", app_settings$currency_symbol, ")"),
        value = input$spending_limit  # Preserve current value
      )
    }
  })
  
  # Also update the label when user first logs in
  observeEvent(user_data$is_authenticated, {
    if (user_data$is_authenticated) {
      # Set initial amount label with currency symbol
      updateNumericInput(
        session,
        "expense_amount",
        label = paste0("Amount (", app_settings$currency_symbol, ")"),
        value = 0
      )
      
      # Update this part to use the period from user settings
      period_text <- user_data$limit_period
      period_text <- paste0(toupper(substr(period_text, 1, 1)), 
                            substr(period_text, 2, nchar(period_text)))
      
      updateNumericInput(
        session,
        "spending_limit",
        label = paste0(period_text, " Spending Limit (", app_settings$currency_symbol, ")"),
        value = input$spending_limit  # Preserve current value
      )
    }
  })
  
  # Direct theme application function - more reliable approach
  apply_theme <- function() {
    # Only apply when user is authenticated
    req(user_data$is_authenticated)
    
    if(app_settings$theme == "dark") {
      # Apply dark theme with JavaScript
      shinyjs::runjs("toggleDarkMode(true);")
    } else {
      # Apply light theme with JavaScript
      shinyjs::runjs("toggleDarkMode(false);")
    }
    
    # Apply font
    shinyjs::runjs(sprintf('
      document.body.style.fontFamily = "%s, sans-serif";
    ', app_settings$font));
  }
  
  # Apply theme whenever it changes
  observeEvent(app_settings$theme, {
    if(user_data$is_authenticated) {
      apply_theme()
    }
  })
  
  # Apply font whenever it changes
  observeEvent(app_settings$font, {
    if(user_data$is_authenticated) {
      apply_theme()
    }
  })
  
  # Close database connection when session ends
  session$onSessionEnded(function() {
    if (exists("con") && dbIsValid(con)) {
      dbDisconnect(con)
      cat("Database connection closed.\n")
    }
  })
  
  # Navbar title
  output$navbar_title <- renderText({
    if (user_data$is_authenticated) {
      "Budget Tracker"
    } else {
      ""
    }
  })
  
  # Custom login handler
  observeEvent(input$login_btn, {
    req(input$username, input$password)
    
    # Query the database for this user
    user_query <- sprintf(
      "SELECT id, username, password, display_name, theme, font, currency, spending_limit, limit_period FROM users WHERE username = '%s' LIMIT 1", 
      input$username
    )
    
    user_info <- dbGetQuery(con, user_query)
    
    # Check if user exists and password matches
    if (nrow(user_info) > 0 && user_info$password == input$password) {
      # Login successful
      user_data$user_id <- user_info$id
      user_data$username <- user_info$username
      user_data$display_name <- user_info$display_name
      user_data$is_authenticated <- TRUE
      
      # Set theme, font, and currency preferences
      app_settings$theme <- ifelse(is.na(user_info$theme), "light", user_info$theme)
      app_settings$font <- ifelse(is.na(user_info$font), "Arial", user_info$font)
      app_settings$currency <- ifelse(is.na(user_info$currency), "GBP", user_info$currency)
      
      # Set spending limit preferences
      user_data$spending_limit <- ifelse(is.na(user_info$spending_limit), 1000, user_info$spending_limit)
      user_data$limit_period <- ifelse(is.na(user_info$limit_period), "monthly", user_info$limit_period)
      
      # Safely set currency symbol
      if (!is.null(app_settings$currency) && app_settings$currency != "" && 
          !is.null(currency_symbols[[app_settings$currency]])) {
        app_settings$currency_symbol <- currency_symbols[[app_settings$currency]]
      } else {
        app_settings$currency_symbol <- "£" # Default to pound if something goes wrong
      }
      
      # Hide login panel, show app content
      shinyjs::hide("login-panel")
      shinyjs::show("app-content")
      
      # Update settings fields
      updateTextInput(session, "change_display_name", value = user_data$display_name)
      updateTextInput(session, "view_username", value = user_data$username)
      updateSelectInput(session, "select_theme", selected = app_settings$theme)
      updateSelectInput(session, "select_font", selected = app_settings$font)
      updateSelectInput(session, "select_currency", selected = app_settings$currency)
      updateNumericInput(session, "spending_limit", value = user_data$spending_limit)
      updateSelectInput(session, "limit_period", selected = user_data$limit_period)
      
      # Clear login form
      updateTextInput(session, "username", value = "")
      updateTextInput(session, "password", value = "")
      
      # Activate home tab
      updateTabsetPanel(session, "main_tabs", selected = "home")
      
      # Apply theme immediately
      apply_theme()
      
      # Show success message with shinyalert
      shinyalert(
        title = "Welcome!",
        text = paste0("Hello, ", user_data$display_name, "! You have successfully logged in."),
        type = "success",
        timer = 2000,
        showConfirmButton = FALSE
      )
    } else {
      # Login failed
      shinyalert(
        title = "Login Failed",
        text = "Invalid username or password. Please try again.",
        type = "error"
      )
    }
  })
  
  # Welcome message
  output$welcome_message <- renderText({
    if (user_data$is_authenticated) {
      sprintf("Welcome back, %s!", user_data$display_name)
    } else {
      "Please log in"
    }
  })
  
  # Toggle registration form
  observeEvent(input$register_btn, {
    shinyjs::hide("login-panel")
    shinyjs::show("register-panel")
  })
  
  # Back to login
  observeEvent(input$back_to_login_btn, {
    shinyjs::hide("register-panel")
    shinyjs::show("login-panel")
  })
  
  # Show settings
  observeEvent(input$show_settings_btn, {
    shinyjs::show("settings-panel")
  })
  
  # Hide settings
  observeEvent(input$close_settings_btn, {
    shinyjs::hide("settings-panel")
  })
  
  # Remove account handler
  observeEvent(input$remove_account_btn, {
    # Show confirmation dialog with shinyalert
    shinyalert(
      title = "Remove Account",
      text = "Are you sure you want to remove your account? All your details will be removed once you click yes.",
      type = "warning",
      showCancelButton = TRUE,
      confirmButtonText = "Yes, remove my account",
      cancelButtonText = "No, keep my account",
      confirmButtonCol = "#dc3545",
      callbackR = function(confirmed) {
        if (confirmed) {
          # User confirmed deletion
          tryCatch({
            # First delete all user's expenses
            dbExecute(
              con,
              sprintf(
                "DELETE FROM expenses WHERE user_id = %d",
                user_data$user_id
              )
            )
            
            # Then delete the user account
            dbExecute(
              con,
              sprintf(
                "DELETE FROM users WHERE id = %d",
                user_data$user_id
              )
            )
            
            # Reset user data
            user_data$user_id <- NULL
            user_data$username <- NULL
            user_data$display_name <- NULL
            user_data$is_authenticated <- FALSE
            
            # Show login, hide panels
            shinyjs::hide("settings-panel")
            shinyjs::hide("app-content")
            shinyjs::show("login-panel")
            
            # Show farewell message
            shinyalert(
              title = "Account Removed",
              text = "All your data are deleted - we are sorry to see you go 😢",
              type = "info"
            )
          }, error = function(e) {
            # Error handling
            print(paste("Error removing account:", e$message))
            shinyalert(
              title = "Error",
              text = "There was a problem removing your account. Please try again later.",
              type = "error"
            )
          })
        }
      }
    )
  })
  
  # Create new account
  observeEvent(input$create_account_btn, {
    req(input$new_username, input$new_password, input$new_display_name)
    
    # Check if username exists
    exists <- dbGetQuery(
      con, 
      sprintf("SELECT COUNT(*) as count FROM users WHERE username = '%s'", input$new_username)
    )
    
    if (exists$count > 0) {
      showNotification("Username already exists. Please choose another.", type = "error")
    } else {
      # Escape single quotes in strings
      safe_username <- gsub("'", "''", input$new_username)
      safe_password <- gsub("'", "''", input$new_password)
      safe_display_name <- gsub("'", "''", input$new_display_name)
      
      # Insert new user
      dbExecute(
        con,
        sprintf(
          "INSERT INTO users (username, password, display_name, theme, font, currency) VALUES ('%s', '%s', '%s', 'light', 'Arial', 'GBP')",
          safe_username, safe_password, safe_display_name
        )
      )
      
      # Show success message with shinyalert
      shinyalert(
        title = "Account Created",
        text = "Your account has been created successfully. Please log in.",
        type = "success"
      )
      
      # Reset form and go back to login
      updateTextInput(session, "new_username", value = "")
      updateTextInput(session, "new_password", value = "")
      updateTextInput(session, "new_display_name", value = "")
      
      shinyjs::hide("register-panel")
      shinyjs::show("login-panel")
    }
  })
  
  # Save settings
  observeEvent(input$save_settings_btn, {
    req(user_data$user_id)
    
    changes_made <- FALSE
    appearance_changed <- FALSE
    
    # Update display name
    if (!is.null(input$change_display_name) && input$change_display_name != user_data$display_name) {
      tryCatch({
        # Escape single quotes
        safe_display_name <- gsub("'", "''", input$change_display_name)
        
        dbExecute(
          con,
          sprintf(
            "UPDATE users SET display_name = '%s' WHERE id = %d",
            safe_display_name, user_data$user_id
          )
        )
        user_data$display_name <- input$change_display_name
        changes_made <- TRUE
      }, error = function(e) {
        print(paste("Error updating display name:", e$message))
      })
    }
    
    # Update password if provided
    if (!is.null(input$change_password) && input$change_password != "") {
      tryCatch({
        # Escape single quotes
        safe_password <- gsub("'", "''", input$change_password)
        
        dbExecute(
          con,
          sprintf(
            "UPDATE users SET password = '%s' WHERE id = %d",
            safe_password, user_data$user_id
          )
        )
        # Clear password field
        updateTextInput(session, "change_password", value = "")
        changes_made <- TRUE
      }, error = function(e) {
        print(paste("Error updating password:", e$message))
      })
    }
    
    # Update theme
    if (!is.null(input$select_theme) && input$select_theme != app_settings$theme) {
      tryCatch({
        dbExecute(
          con,
          sprintf(
            "UPDATE users SET theme = '%s' WHERE id = %d",
            input$select_theme, user_data$user_id
          )
        )
        app_settings$theme <- input$select_theme
        changes_made <- TRUE
        appearance_changed <- TRUE
        
        # Apply theme immediately
        apply_theme()
      }, error = function(e) {
        print(paste("Error updating theme:", e$message))
      })
    }
    
    # Update font
    if (!is.null(input$select_font) && input$select_font != app_settings$font) {
      tryCatch({
        dbExecute(
          con,
          sprintf(
            "UPDATE users SET font = '%s' WHERE id = %d",
            input$select_font, user_data$user_id
          )
        )
        app_settings$font <- input$select_font
        changes_made <- TRUE
        appearance_changed <- TRUE
        
        # Apply font immediately
        apply_theme()
      }, error = function(e) {
        print(paste("Error updating font:", e$message))
      })
    }
    
    # Update currency
    if (!is.null(input$select_currency) && input$select_currency != app_settings$currency) {
      tryCatch({
        dbExecute(
          con,
          sprintf(
            "UPDATE users SET currency = '%s' WHERE id = %d",
            input$select_currency, user_data$user_id
          )
        )
        app_settings$currency <- input$select_currency
        # Make sure to use the correct currency symbol based on the selection
        currency_symbol_value <- currency_symbols[[app_settings$currency]]
        if (!is.null(currency_symbol_value) && length(currency_symbol_value) > 0) {
          app_settings$currency_symbol <- currency_symbol_value
        } else {
          # Default to currency code if symbol not found
          app_settings$currency_symbol <- app_settings$currency
        }
        changes_made <- TRUE
        appearance_changed <- TRUE
      }, error = function(e) {
        print(paste("Error updating currency:", e$message))
      })
    }
    
    # Update spending limit - NEW ADDITION - MOVED INSIDE OBSERVE EVENT
    if (!is.null(input$spending_limit) && input$spending_limit != user_data$spending_limit) {
      tryCatch({
        dbExecute(
          con,
          sprintf(
            "UPDATE users SET spending_limit = %.2f WHERE id = %d",
            input$spending_limit, user_data$user_id
          )
        )
        user_data$spending_limit <- input$spending_limit
        changes_made <- TRUE
      }, error = function(e) {
        print(paste("Error updating spending limit:", e$message))
      })
    }
    
    # Update limit period - NEW ADDITION - MOVED INSIDE OBSERVE EVENT
    if (!is.null(input$limit_period) && input$limit_period != user_data$limit_period) {
      tryCatch({
        dbExecute(
          con,
          sprintf(
            "UPDATE users SET limit_period = '%s' WHERE id = %d",
            input$limit_period, user_data$user_id
          )
        )
        user_data$limit_period <- input$limit_period
        changes_made <- TRUE
      }, error = function(e) {
        print(paste("Error updating limit period:", e$message))
      })
    }
    
    if (changes_made) {
      # Show success message with shinyalert
      shinyalert(
        title = "Settings Saved",
        text = "Your settings have been updated successfully.",
        type = "success",
        timer = 2000,
        showConfirmButton = FALSE
      )
      
      # Only return to app content if appearance wasn't changed
      # If appearance was changed, stay on the settings page
      if (!appearance_changed) {
        shinyjs::hide("settings-panel")
        shinyjs::show("app-content")
      }
    }
  })
  
  # Make calculate_current_spending reactive to expense changes
  calculate_current_spending <- reactive({
    req(user_data$user_id, user_data$limit_period)
    
    # Add explicit dependency on expense tracker
    # This ensures recalculation when expenses are added/edited/deleted
    expense_tracker$update_counter
    
    # Define date range based on limit period
    end_date <- Sys.Date()
    
    if(user_data$limit_period == "monthly") {
      # First day of current month
      start_date <- as.Date(format(end_date, "%Y-%m-01"))
    } else if(user_data$limit_period == "weekly") {
      # First day of current week (Monday)
      days_since_monday <- as.numeric(format(end_date, "%u")) - 1
      start_date <- end_date - days_since_monday
    } else {
      # Fallback
      start_date <- end_date - 30
    }
    
    # Query expenses for the period
    # Use dbSendQuery + dbFetch to ensure we're not getting cached results
    con_query <- dbSendQuery(con, sprintf(
      "SELECT SUM(amount) as total FROM expenses WHERE user_id = %d AND date >= '%s' AND date <= '%s'",
      user_data$user_id, start_date, end_date
    ))
    result <- dbFetch(con_query)
    dbClearResult(con_query)
    
    # For debugging - print to console
    cat("Recalculating spending: Counter =", expense_tracker$update_counter, 
        ", Total =", ifelse(is.null(result$total) || is.na(result$total), 0, result$total), "\n")
    
    # Return total or 0 if NULL
    if(is.null(result$total) || is.na(result$total)) return(0)
    return(result$total)
  })
  
  # Add expense
  observeEvent(input$add_expense_btn, {
    req(user_data$user_id, input$expense_date, input$expense_name, 
        input$expense_amount, input$expense_category)
    
    # Escape single quotes in strings by replacing them with two single quotes
    safe_item_name <- gsub("'", "''", input$expense_name)
    safe_category <- gsub("'", "''", input$expense_category)
    
    # Insert expense
    dbExecute(
      con,
      sprintf(
        "INSERT INTO expenses (user_id, date, item_name, amount, category) VALUES (%d, '%s', '%s', %.2f, '%s')",
        user_data$user_id, input$expense_date, safe_item_name, 
        input$expense_amount, safe_category
      )
    )
    
    # Increment the update counter to trigger UI refresh
    expense_tracker$update_counter <- expense_tracker$update_counter + 1
    
    # Reset form
    updateTextInput(session, "expense_name", value = "")
    updateNumericInput(session, "expense_amount", value = 0)
    
    # Show success message with shinyalert - Pass current currency to format_currency
    shinyalert(
      title = "Expense Added",
      text = sprintf("Your expense of %s for %s has been added successfully.",
                     format_currency(input$expense_amount, app_settings$currency), input$expense_name),
      type = "success",
      timer = 2000,
      showConfirmButton = FALSE
    )
  })
  
  # Budget progress output with speedometer-style gauge
  output$budget_progress <- renderUI({
    req(user_data$is_authenticated)
    
    # Get current spending and limit
    current_spending <- calculate_current_spending()
    spending_limit <- user_data$spending_limit
    
    # Calculate percentage (capped at 100%)
    percentage <- min(100, round(current_spending / spending_limit * 100))
    
    # Calculate angle for clockwise rotation from 270° to 90°
    # 0% = 270° (9 o'clock), 50% = 0° (12 o'clock), 100% = 90° (3 o'clock)
    needle_angle <- as.numeric(270 + (percentage * 180 / 100))
    
    # Period text
    period_text <- get_period_range_text(user_data$limit_period)
    
    tagList(
      tags$style(HTML("
      .speedometer {
        position: relative;
        width: 300px;
        height: 150px;
        margin: 0 auto;
        margin-top: 20px;
        margin-bottom: 40px;
      }
      .gauge-bg {
        position: absolute;
        top: 0;
        left: 0;
        width: 300px;
        height: 150px;
        border-radius: 150px 150px 0 0;
        background: linear-gradient(90deg, 
          #28a745 0%, #28a745 60%, 
          #ffc107 60%, #ffc107 80%, 
          #dc3545 80%, #dc3545 100%);
      }
      .gauge-white {
        position: absolute;
        top: 30px;
        left: 30px;
        width: 240px;
        height: 120px;
        border-radius: 120px 120px 0 0;
        background: white;
      }
      .needle-container {
        position: absolute;
        top: 150px;
        left: 150px;
        height: 0;
        width: 0;
      }
      .needle {
        position: absolute;
        top: -120px;
        left: -2px;
        width: 4px;
        height: 120px;
        background: #333;
        transform-origin: bottom center;
        transform: rotate(270deg); /* Start at 9 o'clock */
        transition: transform 1.5s ease-out;
      }
      .needle-pivot {
        position: absolute;
        top: -8px;
        left: -8px;
        width: 16px;
        height: 16px;
        background: #333;
        border-radius: 50%;
      }
      /* Add these styles for dark mode */
      body.dark-mode .needle {
          background: #ff5555; /* Bright reddish color for dark mode */
      }
      
      body.dark-mode .needle-pivot {
          background: #ff5555; /* Matching pivot color for dark mode */
      }
      .percentage {
        position: absolute;
        top: 90px;
        left: 0;
        width: 300px;
        text-align: center;
        font-size: 28px;
        font-weight: bold;
      }
      .labels {
        position: absolute;
        top: 160px;
        left: 0;
        width: 300px;
        display: flex;
        justify-content: space-between;
        padding: 0 30px;
      }
      .low {
        color: #28a745;
        font-weight: bold;
        font-size: 16px;
      }
      .high {
        color: #dc3545;
        font-weight: bold;
        font-size: 16px;
      }
      body.dark-mode .gauge-white {
        background: #1e1e1e;
      }
      body.dark-mode .percentage {
        color: #fff;
      }
    ")),
      
      div(style = "display: flex; justify-content: space-between; margin-bottom: 5px;",
          span(sprintf("Period: %s", period_text)),
          span(sprintf("%s of %s limit", 
                       format_currency(current_spending, app_settings$currency),
                       format_currency(spending_limit, app_settings$currency)))
      ),
      
      div(class="speedometer",
          div(class="gauge-bg"),
          div(class="gauge-white"),
          div(class="needle-container",
              div(id="needle", class="needle"),
              div(class="needle-pivot")
          ),
          div(class="percentage", paste0(percentage, "%")),
          div(class="labels",
              div(class="low", "LOW"),
              div(class="high", "HIGH")
          )
      ),
      
      # JavaScript to animate the needle
      tags$script(HTML(paste0("
      // First reset the needle to the left position (270°)
      var needle = document.getElementById('needle');
      if (needle) {
        needle.style.transition = 'none';
        needle.style.transform = 'rotate(270deg)'; 
        
        // Force a browser reflow
        void needle.offsetWidth;
        
        // Animate to the current percentage (clockwise)
        setTimeout(function() {
          needle.style.transition = 'transform 1.5s ease-out';
          needle.style.transform = 'rotate(", needle_angle, "deg)';
        }, 300);
      }
    ")))
    )
  })
  
  # Create the cost gauge chart
  output$cost_gauge <- renderEcharts4r({
    req(user_data$is_authenticated)
    
    # Add dependency on expense tracker
    expense_tracker$update_counter
    
    # Get current spending and limit
    current_spending <- calculate_current_spending()
    spending_limit <- user_data$spending_limit
    
    # Calculate percentage (capped at 100%)
    percentage <- min(100, round(current_spending / spending_limit * 100))
    
    # Define color based on percentage
    gauge_color <- if(percentage >= 80) "#dc3545" else if(percentage >= 60) "#ffc107" else "#28a745"
    
    # Create data frame for the gauge - critical to include a name for the value
    df <- data.frame(
      name = "COST", 
      value = percentage
    )
    
    # Create the gauge chart with correct parameters
    gauge <- df |>
      e_charts(name) |>
      e_gauge(value, 
              min = 0, 
              max = 100
      ) |>
      e_title("Cost Efficiency Meter") |>
      e_labels(formatter = "{b}: {c}%")
    
    # Apply theme based on app settings
    if(app_settings$theme == "dark") {
      gauge <- gauge |> e_theme("dark")
    }
    
    return(gauge)
  })
  
  # Animate gauge when switching to the Report tab
  observeEvent(input$main_tabs, {
    if(input$main_tabs == "report" && user_data$is_authenticated) {
      # Force a refresh of the gauge with animation
      shinyjs::delay(100, {
        expense_tracker$update_counter <- expense_tracker$update_counter + 1
      })
    }
  })
  
  # Modal control for edit expense
  observeEvent(input$close_edit_modal_btn, {
    shinyjs::hide("edit-expense-modal")
    shinyjs::runjs("hideEditModal();")
  })
  
  observeEvent(input$cancel_edit_btn, {
    shinyjs::hide("edit-expense-modal")
    shinyjs::runjs("hideEditModal();")
  })
  
  # Save edited expense with improved validation
  observeEvent(input$save_edit_btn, {
    req(input$edit_expense_date, input$edit_expense_name, 
        input$edit_expense_amount, input$edit_expense_category)
    
    # More robust expense ID validation
    expense_id_str <- input$edit_expense_id
    
    # Log for debugging (you can remove this in production)
    cat("Expense ID from form:", expense_id_str, "\n")
    
    # Try to convert to integer, catch any errors
    expense_id <- tryCatch({
      id <- as.integer(expense_id_str)
      if (is.na(id) || id <= 0) NA else id
    }, error = function(e) {
      cat("Error converting expense ID:", e$message, "\n")
      NA
    })
    
    if (is.na(expense_id)) {
      shinyalert(
        title = "Error",
        text = "Invalid expense ID. Please try again.",
        type = "error"
      )
      return()
    }
    
    # Update the expense in the database
    tryCatch({
      # Use safer query building with paste0 instead of sprintf
      update_query <- paste0(
        "UPDATE expenses SET date = '", input$edit_expense_date, 
        "', item_name = '", gsub("'", "''", input$edit_expense_name), 
        "', amount = ", input$edit_expense_amount, 
        ", category = '", gsub("'", "''", input$edit_expense_category), 
        "' WHERE id = ", expense_id, 
        " AND user_id = ", user_data$user_id
      )
      
      dbExecute(con, update_query)
      
      # Increment the update counter to trigger UI refresh
      expense_tracker$update_counter <- expense_tracker$update_counter + 1
      
      # Hide the modal
      shinyjs::hide("edit-expense-modal")
      shinyjs::runjs("hideEditModal();")
      
      # Show success message
      shinyalert(
        title = "Expense Updated",
        text = "Your expense has been updated successfully.",
        type = "success",
        timer = 2000,
        showConfirmButton = FALSE
      )
    }, error = function(e) {
      cat("Error updating expense:", e$message, "\n")
      shinyalert(
        title = "Error",
        text = "There was a problem updating the expense. Please try again.",
        type = "error"
      )
    })
  })
  
  # Edit expense handler
  observeEvent(eventExpr = {
    lapply(names(input)[grepl("^edit_expense_", names(input))], function(x) input[[x]])
  }, {
    # Get all button names that match the pattern
    btn_ids <- names(input)[grepl("^edit_expense_", names(input))]
    
    if (length(btn_ids) > 0) {
      # Create a vector of button values
      btn_values <- sapply(btn_ids, function(x) input[[x]])
      
      # Only proceed if any button has been clicked (value > 0)
      if(any(btn_values > 0)) {
        # Find which button has the maximum value (was clicked)
        max_idx <- which.max(btn_values)
        
        # Make sure we got a valid index
        if (length(max_idx) > 0 && !is.na(max_idx) && max_idx > 0) {
          btn_id <- btn_ids[max_idx]
          
          if (!is.null(btn_id) && !is.na(btn_id)) {
            # Extract the expense ID from the button ID
            id_text <- gsub("edit_expense_", "", btn_id)
            
            # Verify we have a numeric ID
            if (!is.na(id_text) && nchar(id_text) > 0) {
              expense_id <- as.integer(id_text)
              
              if (!is.na(expense_id) && expense_id > 0) {
                # Query the expense data safely
                tryCatch({
                  query <- paste0("SELECT * FROM expenses WHERE id = ", expense_id, " AND user_id = ", user_data$user_id)
                  expense_data <- dbGetQuery(con, query)
                  
                  if (nrow(expense_data) > 0) {
                    # Populate the edit form
                    updateDateInput(session, "edit_expense_date", value = as.Date(expense_data$date))
                    updateTextInput(session, "edit_expense_name", value = expense_data$item_name)
                    updateNumericInput(session, "edit_expense_amount", 
                                       label = paste0("Amount (", app_settings$currency_symbol, ")"),
                                       value = expense_data$amount)
                    updateSelectInput(session, "edit_expense_category", selected = expense_data$category)
                    updateTextInput(session, "edit_expense_id", value = as.character(expense_id))
                    
                    # Show the modal
                    shinyjs::show("edit-expense-modal")
                    
                    # Add JavaScript to ensure modal is visible
                    shinyjs::runjs("showEditModal();")
                  }
                }, error = function(e) {
                  # Log the error
                  cat("Error querying expense data:", e$message, "\n")
                  shinyalert(
                    title = "Error",
                    text = "There was a problem loading the expense data. Please try again.",
                    type = "error"
                  )
                })
              }
            }
          }
        }
      }
    }
  }, ignoreInit = TRUE)
  
  # Delete expense handler
  observeEvent(eventExpr = {
    lapply(names(input)[grepl("^delete_expense_", names(input))], function(x) input[[x]])
  }, {
    # Get all button names that match the pattern
    btn_ids <- names(input)[grepl("^delete_expense_", names(input))]
    
    if (length(btn_ids) > 0) {
      # Create a vector of button values
      btn_values <- sapply(btn_ids, function(x) input[[x]])
      
      # Only proceed if any button has been clicked (value > 0)
      if(any(btn_values > 0)) {
        # Find which button has the maximum value (was clicked)
        max_idx <- which.max(btn_values)
        
        # Make sure we got a valid index
        if (length(max_idx) > 0 && !is.na(max_idx) && max_idx > 0) {
          btn_id <- btn_ids[max_idx]
          
          if (!is.null(btn_id) && !is.na(btn_id)) {
            # Extract the expense ID from the button ID
            id_text <- gsub("delete_expense_", "", btn_id)
            
            # Verify we have a numeric ID
            if (!is.na(id_text) && nchar(id_text) > 0) {
              expense_id <- as.integer(id_text)
              
              if (!is.na(expense_id) && expense_id > 0) {
                # MODIFIED: Use the reactive value instead of global variable
                # Create a unique key for this button click
                click_key <- paste0(btn_id, "_", input[[btn_id]])
                
                # Only show the dialog if we haven't shown it for this click
                if (!(click_key %in% names(expense_tracker$shown_dialogs))) {
                  # Set the button state to 0 immediately to prevent multiple triggers
                  shinyjs::runjs(paste0("Shiny.setInputValue('", btn_id, "', 0, {priority: 'event'});"))
                  
                  # Add this click to shown dialogs
                  temp_dialogs <- expense_tracker$shown_dialogs
                  temp_dialogs[[click_key]] <- TRUE
                  expense_tracker$shown_dialogs <- temp_dialogs
                  
                  # Query the expense data safely
                  tryCatch({
                    query <- paste0("SELECT * FROM expenses WHERE id = ", expense_id, " AND user_id = ", user_data$user_id)
                    expense_data <- dbGetQuery(con, query)
                    
                    if (nrow(expense_data) > 0) {
                      # Format the amount with currency
                      formatted_amount <- format_currency(expense_data$amount, app_settings$currency)
                      formatted_date <- format(as.Date(expense_data$date), "%d %b %Y")
                      
                      # Show confirmation dialog
                      shinyalert(
                        title = "Delete Expense",
                        text = sprintf("Are you sure you want to delete the expense '%s' for %s on %s?", 
                                       expense_data$item_name, formatted_amount, formatted_date),
                        type = "warning",
                        showCancelButton = TRUE,
                        confirmButtonText = "Yes, delete it",
                        cancelButtonText = "Cancel",
                        confirmButtonCol = "#dc3545",
                        callbackR = function(confirmed) {
                          # MODIFIED: Remove from shown dialogs after handling
                          temp_dialogs <- expense_tracker$shown_dialogs
                          temp_dialogs[[click_key]] <- NULL
                          expense_tracker$shown_dialogs <- temp_dialogs
                          
                          if (confirmed) {
                            # User confirmed deletion, delete the expense
                            tryCatch({
                              delete_query <- paste0("DELETE FROM expenses WHERE id = ", expense_id, " AND user_id = ", user_data$user_id)
                              dbExecute(con, delete_query)
                              
                              # Increment the update counter to trigger UI refresh
                              expense_tracker$update_counter <- expense_tracker$update_counter + 1
                              
                              # Show success message
                              shinyalert(
                                title = "Expense Deleted",
                                text = "The expense has been deleted successfully.",
                                type = "success",
                                timer = 2000,
                                showConfirmButton = FALSE
                              )
                            }, error = function(e) {
                              # Error handling
                              cat("Error deleting expense:", e$message, "\n")
                              shinyalert(
                                title = "Error",
                                text = "There was a problem deleting the expense.",
                                type = "error"
                              )
                            })
                          }
                        }
                      )
                    }
                  }, error = function(e) {
                    # Log the error
                    cat("Error querying expense data for deletion:", e$message, "\n")
                    
                    # Remove from shown dialogs on error
                    temp_dialogs <- expense_tracker$shown_dialogs
                    temp_dialogs[[click_key]] <- NULL
                    expense_tracker$shown_dialogs <- temp_dialogs
                  })
                }
              }
            }
          }
        }
      }
    }
  }, ignoreInit = TRUE)
  
  # Reactive function to get expense chart data
  get_expense_chart_data <- reactive({
    req(user_data$user_id)
    
    # Add explicit dependency on expense tracker to ensure updates when expenses change
    expense_tracker$update_counter
    
    # Apply date filters
    date_filter <- ""
    if (!is.null(input$date_range_start) && !is.null(input$date_range_end)) {
      date_filter <- sprintf("AND date BETWEEN '%s' AND '%s'", 
                             input$date_range_start, input$date_range_end)
    }
    
    # Get expenses grouped by category
    expenses <- dbGetQuery(
      con,
      sprintf(
        "SELECT category, SUM(amount) as total FROM expenses 
        WHERE user_id = %d %s 
        GROUP BY category
        ORDER BY total DESC",
        user_data$user_id, date_filter
      )
    )
    
    # Return the data for the chart
    return(expenses)
  })
  
  # Create the donut chart
  output$expense_donut_chart <- renderEcharts4r({
    req(user_data$is_authenticated)
    
    # Get the data
    chart_data <- get_expense_chart_data()
    
    # Check if we have data
    if(nrow(chart_data) == 0) {
      return(NULL)
    }
    
    # Format currency values for the tooltip
    tooltip_formatter <- sprintf("function(params) {
      return params.name + ': %s' + params.value.toFixed(2) + ' (' + params.percent + '%%)'
    }", app_settings$currency_symbol)
    
    # Create the donut chart
    chart <- chart_data |>
      e_charts(category) |>
      e_pie(total, radius = c("40%", "70%")) |>  # Use radius to create a donut chart
      e_tooltip(formatter = htmlwidgets::JS(tooltip_formatter))
    
    # Apply theme based on app settings
    if(app_settings$theme == "dark") {
      chart <- chart |> e_theme("dark")
    }
    
    # Add save as image feature
    chart <- chart |> e_toolbox_feature(feature = "saveAsImage")
    
    return(chart)
  })
  
  # Function to generate expense list content
  expense_list_content <- function() {
    req(user_data$user_id)
    
    # Apply filters
    date_filter <- ""
    if (!is.null(input$date_range_start) && !is.null(input$date_range_end)) {
      date_filter <- sprintf("AND date BETWEEN '%s' AND '%s'", 
                             input$date_range_start, input$date_range_end)
    }
    
    category_filter <- ""
    if (!is.null(input$expense_filter) && length(input$expense_filter) > 0) {
      categories <- paste0("'", input$expense_filter, "'", collapse = ",")
      category_filter <- sprintf("AND category IN (%s)", categories)
    }
    
    # Get expenses
    expenses <- dbGetQuery(
      con,
      sprintf(
        "SELECT id, date, item_name, amount, category FROM expenses 
    WHERE user_id = %d %s %s 
    ORDER BY date DESC, id DESC 
    LIMIT 100",
        user_data$user_id, date_filter, category_filter
      )
    )
    
    if (nrow(expenses) == 0) {
      return(div(
        style = "text-align: center; padding: 20px;",
        "No expenses found for the selected criteria."
      ))
    }
    
    # Calculate total
    total <- sum(expenses$amount)
    
    # Create a simple HTML table
    result <- div(
      style = "overflow-x: auto;",
      tags$table(
        class = "table table-striped",
        tags$thead(
          tags$tr(
            tags$th("Date"),
            tags$th("Description"),
            tags$th("Category"),
            tags$th("Amount"),
            tags$th("Actions", style = "text-align: center;")
          )
        ),
        tags$tbody(
          lapply(1:nrow(expenses), function(i) {
            exp <- expenses[i, ]
            tags$tr(
              tags$td(format(as.Date(exp$date), "%d %b %Y")),
              tags$td(exp$item_name),
              tags$td(exp$category),
              tags$td(sprintf("%s%.2f", app_settings$currency_symbol, exp$amount), style = "text-align: right;"),
              tags$td(
                style = "text-align: center; white-space: nowrap;",
                actionButton(
                  inputId = paste0("edit_expense_", exp$id),
                  label = NULL,
                  icon = icon("edit"),
                  class = "btn-sm btn-primary",
                  style = "margin-right: 5px;"
                ),
                actionButton(
                  inputId = paste0("delete_expense_", exp$id),
                  label = NULL,
                  icon = icon("trash"),
                  class = "btn-sm btn-danger",
                  `data-version` = expense_tracker$update_counter  # Add a version attribute to track UI refreshes
                )
              )
            )
          })
        ),
        tags$tfoot(
          tags$tr(
            tags$th(colspan = 3, "Total", style = "text-align: right;"),
            tags$th(sprintf("%s%.2f", app_settings$currency_symbol, total), style = "text-align: right;")
          )
        )
      )
    )
    
    # Reset button states when refreshing the expense list
    shinyjs::runjs("
      // Reset all edit and delete button states to initial state
      setTimeout(function() {
        var editButtons = document.querySelectorAll('[id^=\"edit_expense_\"]');
        var deleteButtons = document.querySelectorAll('[id^=\"delete_expense_\"]');
        
        editButtons.forEach(function(btn) {
          Shiny.setInputValue(btn.id, 0, {priority: 'event'});
        });
        
        deleteButtons.forEach(function(btn) {
          Shiny.setInputValue(btn.id, 0, {priority: 'event'});
        });
      }, 100);
    ")
    
    # Reset shown_dialogs when refreshing UI
    expense_tracker$shown_dialogs <- list()
    
    return(result)
  }
  
  # Expense list UI output
  output$expense_list <- renderUI({
    req(user_data$user_id)
    
    # Add dependency on the expense tracker to force updates
    expense_tracker$update_counter
    
    result <- expense_list_content()
    
    # Add inline CSS for dark mode table styles if in dark mode
    if(app_settings$theme == "dark") {
      result <- tagList(
        tags$style(HTML("
        #expense_list .table {
          background-color: #1e1e1e !important;
        }
        #expense_list .table tbody tr {
          background-color: #1e1e1e !important;
        }
        #expense_list .table tbody tr:nth-of-type(odd) {
          background-color: #2a2a2a !important;
        }
        #expense_list .table tbody td {
          background-color: inherit !important;
          color: #FFFFFF !important;
        }
        #expense_list .table thead th {
          background-color: #1e1e1e !important;
          color: #FFFFFF !important;
        }
        #expense_list .table tfoot tr {
          background-color: #1e1e1e !important;
        }
        #expense_list .table tfoot th {
          background-color: #1e1e1e !important;
          color: #FFFFFF !important;
        }
      ")),
        result
      )
      
      # Also apply the JavaScript styling after a short delay
      shinyjs::delay(100, {
        shinyjs::runjs('
        if (typeof applyDarkModeToTables === "function") {
          applyDarkModeToTables();
        }
      ')
      })
    }
    
    return(result)
  })
  
  # Reactive function to get filtered expense data for export
  get_filtered_expenses <- reactive({
    req(user_data$user_id)
    
    # Apply filters
    date_filter <- ""
    if (!is.null(input$date_range_start) && !is.null(input$date_range_end)) {
      date_filter <- sprintf("AND date BETWEEN '%s' AND '%s'", 
                             input$date_range_start, input$date_range_end)
    }
    
    category_filter <- ""
    if (!is.null(input$expense_filter) && length(input$expense_filter) > 0) {
      categories <- paste0("'", input$expense_filter, "'", collapse = ",")
      category_filter <- sprintf("AND category IN (%s)", categories)
    }
    
    # Get expenses
    expenses <- dbGetQuery(
      con,
      sprintf(
        "SELECT date, item_name, category, amount FROM expenses 
      WHERE user_id = %d %s %s 
      ORDER BY date DESC, id DESC 
      LIMIT 1000", # Increased limit for exports
        user_data$user_id, date_filter, category_filter
      )
    )
    
    # The formatting will be done by the download handler based on the export format
    return(expenses)
  })
  
  # Download handler for expense exports
  output$download_expenses <- downloadHandler(
    filename = function() {
      format <- input$export_format
      date_range <- format(Sys.Date(), "%Y%m%d")
      
      if (!is.null(input$date_range_start) && !is.null(input$date_range_end)) {
        start_date <- format(as.Date(input$date_range_start), "%Y%m%d")
        end_date <- format(as.Date(input$date_range_end), "%Y%m%d")
        date_range <- paste0(start_date, "-", end_date)
      }
      
      paste0("expenses_", date_range, ".", format)
    },
    
    content = function(file) {
      # Get filtered data
      expenses <- get_filtered_expenses()
      
      # Format data based on export type
      format <- input$export_format
      
      if (format == "csv" || format == "txt") {
        # For CSV and TXT formats, use the same column names as import format
        # and numeric amounts without currency symbols
        
        # Make a copy of the data frame
        export_data <- expenses
        
        # Ensure date is in YYYY-MM-DD format for easy import
        export_data$date <- format(as.Date(export_data$date), "%Y-%m-%d")
        
        # Make sure amount is numeric (no currency symbol)
        export_data$amount <- as.numeric(export_data$amount)
        
        # Ensure column names match import format
        colnames(export_data) <- c("date", "item_name", "category", "amount")
        
        if (format == "csv") {
          # Standard CSV export
          write.csv(export_data, file, row.names = FALSE)
        } else if (format == "txt") {
          # Custom TXT export format (simple CSV in a TXT file)
          # Using write.table with comma separator and no row names
          write.table(export_data, file, sep = ",", row.names = FALSE, 
                      col.names = TRUE, quote = TRUE)
        }
      } else if (format == "xlsx") {
        # For Excel, use more user-friendly column names and formatted amounts
        export_data <- expenses
        
        # Format date more nicely for Excel
        export_data$date <- format(as.Date(export_data$date), "%Y-%m-%d")
        
        # Format amount with currency symbol for Excel
        export_data$amount <- as.numeric(export_data$amount)
        
        # Use more readable column names
        colnames(export_data) <- c("Date", "Description", "Category", "Amount")
        
        writexl::write_xlsx(export_data, file)
      } else if (format == "json") {
        # For JSON, use original column names but clean formatting
        export_data <- expenses
        
        # Format date consistently
        export_data$date <- format(as.Date(export_data$date), "%Y-%m-%d")
        
        # Numeric amount without currency symbol
        export_data$amount <- as.numeric(export_data$amount)
        
        # Convert to JSON
        json_data <- jsonlite::toJSON(export_data, pretty = TRUE)
        writeLines(json_data, file)
      }
    }
  )
  
  # Update UI elements on date range changes
  observeEvent(input$date_range_start, {
    if(user_data$is_authenticated && app_settings$theme == "dark") {
      shinyjs::delay(100, {
        shinyjs::runjs('
          if (document.body.classList.contains("dark-mode")) {
            if (typeof applyDarkModeToTables === "function") {
              applyDarkModeToTables();
            }
          }
        ')
      })
    }
  })
  
  observeEvent(input$date_range_end, {
    if(user_data$is_authenticated && app_settings$theme == "dark") {
      shinyjs::delay(100, {
        shinyjs::runjs('
          if (document.body.classList.contains("dark-mode")) {
            if (typeof applyDarkModeToTables === "function") {
              applyDarkModeToTables();
            }
          }
        ')
      })
    }
  })
  
  # Update UI on tab changes
  observeEvent(input$main_tabs, {
    if(input$main_tabs == "report" && user_data$is_authenticated && app_settings$theme == "dark") {
      shinyjs::delay(100, {
        shinyjs::runjs('
          if (document.body.classList.contains("dark-mode")) {
            if (typeof applyDarkModeToTables === "function") {
              applyDarkModeToTables();
            }
          }
        ')
      })
    }
  })
  
  # Observe expense filter changes
  observeEvent(input$expense_filter, {
    if(user_data$is_authenticated && app_settings$theme == "dark") {
      shinyjs::delay(100, {
        shinyjs::runjs('
          if (document.body.classList.contains("dark-mode")) {
            if (typeof applyDarkModeToTables === "function") {
              applyDarkModeToTables();
            }
          }
        ')
      })
    }
  })
  
  # Update expense list when currency changes
  observeEvent(app_settings$currency, {
    if (user_data$is_authenticated) {
      # This will force the expense list to redraw with the new currency
      output$expense_list <- renderUI({
        expense_list_content()
      })
      
      # If in dark mode, reapply table styling after currency update
      if(app_settings$theme == "dark") {
        shinyjs::delay(100, {
          shinyjs::runjs('
            if (document.body.classList.contains("dark-mode")) {
              if (typeof applyDarkModeToTables === "function") {
                applyDarkModeToTables();
              }
            }
          ')
        })
      }
    }
  })
  
  # Logout handler
  observeEvent(input$logout_btn, {
    # Reset user data
    user_data$user_id <- NULL
    user_data$username <- NULL
    user_data$display_name <- NULL
    user_data$is_authenticated <- FALSE
    
    # Show login, hide panels
    shinyjs::show("login-panel")
    shinyjs::hide("app-content")
    shinyjs::hide("settings-panel")
    
    # Show logout message with shinyalert
    shinyalert(
      title = "Logged Out",
      text = "You have been logged out successfully.",
      type = "info",
      timer = 2000,
      showConfirmButton = FALSE
    )
  })
  
  # File upload preview logic
  output$upload_preview_ui <- renderUI({
    req(input$upload_expenses)
    
    # Try to read the CSV file
    tryCatch({
      # Read the uploaded file
      df <- read.csv(input$upload_expenses$datapath, stringsAsFactors = FALSE)
      
      # Check if required columns exist
      required_cols <- c("date", "item_name", "amount", "category")
      missing_cols <- required_cols[!required_cols %in% colnames(df)]
      
      if (length(missing_cols) > 0) {
        return(div(
          class = "alert alert-danger",
          h5("Error: Missing Columns"),
          p("The following required columns are missing:"),
          tags$ul(
            lapply(missing_cols, function(col) {
              tags$li(col)
            })
          )
        ))
      }
      
      # Validate data types
      validation_errors <- c()
      
      # Check date format
      invalid_dates <- which(!grepl("^\\d{4}-\\d{2}-\\d{2}$", df$date))
      if (length(invalid_dates) > 0) {
        validation_errors <- c(validation_errors, 
                               sprintf("Invalid date format in rows: %s. Use YYYY-MM-DD format.", 
                                       paste(invalid_dates[1:min(length(invalid_dates), 5)], collapse = ", ")))
      }
      
      # Check amount is numeric
      if (!is.numeric(df$amount)) {
        df$amount <- as.numeric(df$amount)
        invalid_amounts <- which(is.na(df$amount))
        if (length(invalid_amounts) > 0) {
          validation_errors <- c(validation_errors, 
                                 sprintf("Invalid amount values in rows: %s. Use numeric values.", 
                                         paste(invalid_amounts[1:min(length(invalid_amounts), 5)], collapse = ", ")))
        }
      }
      
      # Check category is valid
      invalid_categories <- which(!df$category %in% expense_categories)
      if (length(invalid_categories) > 0) {
        validation_errors <- c(validation_errors, 
                               sprintf("Invalid categories in rows: %s. Categories must be one of: %s", 
                                       paste(invalid_categories[1:min(length(invalid_categories), 5)], collapse = ", "),
                                       paste(expense_categories, collapse = ", ")))
      }
      
      # If validation errors exist, show them
      if (length(validation_errors) > 0) {
        return(div(
          class = "alert alert-danger",
          h5("Validation Errors:"),
          tags$ul(
            lapply(validation_errors, function(error) {
              tags$li(error)
            })
          )
        ))
      }
      
      # Show preview table
      preview_rows <- min(nrow(df), 5)  # Preview first 5 rows
      
      # Format the preview data
      preview_df <- df[1:preview_rows, ]
      
      # Make the import button visible
      shinyjs::show("import_button_container")
      
      div(
        class = "upload-preview",
        h5(sprintf("Preview (showing %d of %d rows):", preview_rows, nrow(df))),
        div(style = "overflow-x: auto;",
            tableOutput("upload_preview_table")
        ),
        p(sprintf("Total records to import: %d", nrow(df))),
        div(
          class = "alert alert-success",
          "Data looks good! Click 'Import Expenses' to add these records to your account."
        )
      )
      
    }, error = function(e) {
      # Hide the import button
      shinyjs::hide("import_button_container")
      
      div(
        class = "alert alert-danger",
        h5("Error Reading File"),
        p("There was an error reading your CSV file:"),
        p(e$message),
        p("Please make sure your file is a properly formatted CSV file.")
      )
    })
  })
  
  # Preview table output
  output$upload_preview_table <- renderTable({
    req(input$upload_expenses)
    
    # Try to read the CSV file
    df <- read.csv(input$upload_expenses$datapath, stringsAsFactors = FALSE)
    
    # Preview first 5 rows
    preview_rows <- min(nrow(df), 5)
    preview_df <- df[1:preview_rows, ]
    
    preview_df
  }, striped = TRUE, bordered = TRUE, align = "l")
  
  # Import expenses button handler
  observeEvent(input$import_expenses_btn, {
    req(input$upload_expenses, user_data$user_id)
    
    # Read the uploaded file
    df <- tryCatch({
      read.csv(input$upload_expenses$datapath, stringsAsFactors = FALSE)
    }, error = function(e) {
      shinyalert(
        title = "Import Error",
        text = paste("Error reading CSV file:", e$message),
        type = "error"
      )
      return(NULL)
    })
    
    if (is.null(df)) return()
    
    # Perform validation again (redundant but safer)
    required_cols <- c("date", "item_name", "amount", "category")
    if (!all(required_cols %in% colnames(df))) {
      shinyalert(
        title = "Import Error",
        text = "Required columns are missing from the CSV file.",
        type = "error"
      )
      return()
    }
    
    # Prepare data for import
    import_data <- data.frame(
      user_id = user_data$user_id,
      date = as.character(df$date),
      item_name = as.character(df$item_name),
      amount = as.numeric(df$amount),
      category = as.character(df$category),
      stringsAsFactors = FALSE
    )
    
    # Count successful and failed imports
    success_count <- 0
    failed_count <- 0
    error_messages <- c()
    
    # Process each row
    for (i in 1:nrow(import_data)) {
      tryCatch({
        # Validate this row
        if (is.na(import_data$amount[i]) || 
            !import_data$category[i] %in% expense_categories ||
            !grepl("^\\d{4}-\\d{2}-\\d{2}$", import_data$date[i])) {
          failed_count <- failed_count + 1
          error_messages <- c(error_messages, sprintf("Row %d: Invalid data", i))
          next
        }
        
        # Insert the expense
        query <- sprintf(
          "INSERT INTO expenses (user_id, date, item_name, amount, category) VALUES (%d, '%s', '%s', %.2f, '%s')",
          import_data$user_id[i],
          import_data$date[i],
          gsub("'", "''", import_data$item_name[i]),  # Escape single quotes
          import_data$amount[i],
          import_data$category[i]
        )
        
        dbExecute(con, query)
        success_count <- success_count + 1
        
      }, error = function(e) {
        failed_count <- failed_count + 1
        error_messages <- c(error_messages, sprintf("Row %d: %s", i, e$message))
      })
    }
    
    # Increment the update counter to trigger UI refresh
    expense_tracker$update_counter <- expense_tracker$update_counter + 1
    
    # Show result message
    if (failed_count == 0) {
      shinyalert(
        title = "Import Successful",
        text = sprintf("Successfully imported %d expense records.", success_count),
        type = "success"
      )
    } else {
      error_text <- if (length(error_messages) > 3) {
        paste0(paste(error_messages[1:3], collapse = "\n"), "\n...(more errors)")
      } else {
        paste(error_messages, collapse = "\n")
      }
      
      shinyalert(
        title = "Import Partially Successful",
        text = sprintf("Imported %d records successfully. %d records failed.\n\nErrors:\n%s", 
                       success_count, failed_count, error_text),
        type = "warning"
      )
    }
    
    # Reset the file input
    reset("upload_expenses")
    shinyjs::hide("import_button_container")
  })
  
  # Add these functions to handle text area input
  # Preview text data button handler
  observeEvent(input$preview_text_data_btn, {
    # Get the text area content
    text_data <- input$expense_text_data
    
    # Check if text is empty
    if (is.null(text_data) || text_data == "") {
      output$text_preview_ui <- renderUI({
        div(
          class = "alert alert-warning",
          "Please enter some data in the text area."
        )
      })
      shinyjs::hide("import_text_button_container")
      return()
    }
    
    # Try to parse the text as CSV
    tryCatch({
      # Split the text into lines
      lines <- strsplit(text_data, "\\r?\\n")[[1]]
      lines <- lines[lines != ""] # Remove empty lines
      
      if (length(lines) == 0) {
        output$text_preview_ui <- renderUI({
          div(
            class = "alert alert-warning",
            "No data found. Please enter data in CSV format."
          )
        })
        shinyjs::hide("import_text_button_container")
        return()
      }
      
      # Check if the header row exists and add it if not
      if (!grepl("date", tolower(lines[1]))) {
        # Add header row
        lines <- c("date,item_name,amount,category", lines)
      }
      
      # Combine lines back to text
      csv_text <- paste(lines, collapse = "\n")
      
      # Parse the CSV text
      con <- textConnection(csv_text)
      df <- read.csv(con, stringsAsFactors = FALSE)
      close(con)
      
      # Check if required columns exist
      required_cols <- c("date", "item_name", "amount", "category")
      missing_cols <- required_cols[!required_cols %in% colnames(df)]
      
      if (length(missing_cols) > 0) {
        output$text_preview_ui <- renderUI({
          div(
            class = "alert alert-danger",
            h5("Error: Missing Columns"),
            p("The following required columns are missing:"),
            tags$ul(
              lapply(missing_cols, function(col) {
                tags$li(col)
              })
            ),
            p("Make sure your data has these columns or add a header row.")
          )
        })
        shinyjs::hide("import_text_button_container")
        return()
      }
      
      # Validate data types
      validation_errors <- c()
      
      # Check date format
      invalid_dates <- which(!grepl("^\\d{4}-\\d{2}-\\d{2}$", df$date))
      if (length(invalid_dates) > 0) {
        validation_errors <- c(validation_errors, 
                               sprintf("Invalid date format in rows: %s. Use YYYY-MM-DD format.", 
                                       paste(invalid_dates[1:min(length(invalid_dates), 5)], collapse = ", ")))
      }
      
      # Check amount is numeric
      if (!is.numeric(df$amount)) {
        df$amount <- as.numeric(df$amount)
        invalid_amounts <- which(is.na(df$amount))
        if (length(invalid_amounts) > 0) {
          validation_errors <- c(validation_errors, 
                                 sprintf("Invalid amount values in rows: %s. Use numeric values.", 
                                         paste(invalid_amounts[1:min(length(invalid_amounts), 5)], collapse = ", ")))
        }
      }
      
      # Check category is valid
      invalid_categories <- which(!df$category %in% expense_categories)
      if (length(invalid_categories) > 0) {
        validation_errors <- c(validation_errors, 
                               sprintf("Invalid categories in rows: %s. Categories must be one of: %s", 
                                       paste(invalid_categories[1:min(length(invalid_categories), 5)], collapse = ", "),
                                       paste(expense_categories, collapse = ", ")))
      }
      
      # If validation errors exist, show them
      if (length(validation_errors) > 0) {
        output$text_preview_ui <- renderUI({
          div(
            class = "alert alert-danger",
            h5("Validation Errors:"),
            tags$ul(
              lapply(validation_errors, function(error) {
                tags$li(error)
              })
            )
          )
        })
        shinyjs::hide("import_text_button_container")
        return()
      }
      
      # Store data in a reactive value for later use
      text_import_data <- reactiveVal(df)
      
      # Show preview table
      preview_rows <- min(nrow(df), 5)  # Preview first 5 rows
      
      # Format the preview data
      preview_df <- df[1:preview_rows, ]
      
      # Make the import button visible
      shinyjs::show("import_text_button_container")
      
      output$text_preview_ui <- renderUI({
        div(
          class = "upload-preview",
          h5(sprintf("Preview (showing %d of %d rows):", preview_rows, nrow(df))),
          div(style = "overflow-x: auto;",
              renderTable({
                preview_df
              }, striped = TRUE, bordered = TRUE, align = "l")  # Use "l" instead of "left"
          ),
          p(sprintf("Total records to import: %d", nrow(df))),
          div(
            class = "alert alert-success",
            "Data looks good! Click 'Import Text Data' to add these records to your account."
          )
        )
      })
      
    }, error = function(e) {
      # Hide the import button
      shinyjs::hide("import_text_button_container")
      
      output$text_preview_ui <- renderUI({
        div(
          class = "alert alert-danger",
          h5("Error Parsing Data"),
          p("There was an error parsing your data:"),
          p(e$message),
          p("Please make sure your data is in proper CSV format.")
        )
      })
    })
  })
  
  # Import text data button handler
  observeEvent(input$import_text_expenses_btn, {
    req(user_data$user_id)
    
    # Get the stored parsed data
    df <- parsed_text_data()
    
    # Check if we have data to import
    if (is.null(df) || nrow(df) == 0) {
      # Print debug message
      cat("No data available for import\n")
      
      shinyalert(
        title = "Import Error",
        text = "No data available for import. Please preview your data first.",
        type = "error"
      )
      return()
    }
    
    # Print debug info
    cat("Importing data with", nrow(df), "rows\n")
    print(head(df))
    
    # Prepare data for import
    import_data <- data.frame(
      user_id = user_data$user_id,
      date = as.character(df$date),
      item_name = as.character(df$item_name),
      amount = as.numeric(df$amount),
      category = as.character(df$category),
      stringsAsFactors = FALSE
    )
    
    # Count successful and failed imports
    success_count <- 0
    failed_count <- 0
    error_messages <- c()
    
    # Process each row
    for (i in 1:nrow(import_data)) {
      tryCatch({
        # Debug info
        cat("Processing row", i, ":", 
            import_data$date[i], 
            import_data$item_name[i], 
            import_data$amount[i], 
            import_data$category[i], "\n")
        
        # Validate this row
        if (is.na(import_data$amount[i]) || 
            !import_data$category[i] %in% expense_categories ||
            !grepl("^\\d{4}-\\d{2}-\\d{2}$", import_data$date[i])) {
          failed_count <- failed_count + 1
          error_messages <- c(error_messages, sprintf("Row %d: Invalid data", i))
          cat("Row", i, "validation failed\n")
          next
        }
        
        # Insert the expense
        query <- sprintf(
          "INSERT INTO expenses (user_id, date, item_name, amount, category) VALUES (%d, '%s', '%s', %.2f, '%s')",
          import_data$user_id[i],
          import_data$date[i],
          gsub("'", "''", import_data$item_name[i]),  # Escape single quotes
          import_data$amount[i],
          import_data$category[i]
        )
        
        # Debug info
        cat("Executing query:", query, "\n")
        
        dbExecute(con, query)
        success_count <- success_count + 1
        cat("Row", i, "imported successfully\n")
        
      }, error = function(e) {
        failed_count <- failed_count + 1
        error_messages <- c(error_messages, sprintf("Row %d: %s", i, e$message))
        cat("Error importing row", i, ":", e$message, "\n")
      })
    }
    
    cat("Import complete. Success:", success_count, "Failed:", failed_count, "\n")
    
    # Increment the update counter to trigger UI refresh
    expense_tracker$update_counter <- expense_tracker$update_counter + 1
    
    # Show result message
    if (failed_count == 0 && success_count > 0) {
      shinyalert(
        title = "Import Successful",
        text = sprintf("Successfully imported %d expense records.", success_count),
        type = "success"
      )
    } else if (success_count > 0) {
      error_text <- if (length(error_messages) > 3) {
        paste0(paste(error_messages[1:3], collapse = "\n"), "\n...(more errors)")
      } else {
        paste(error_messages, collapse = "\n")
      }
      
      shinyalert(
        title = "Import Partially Successful",
        text = sprintf("Imported %d records successfully. %d records failed.\n\nErrors:\n%s", 
                       success_count, failed_count, error_text),
        type = "warning"
      )
    } else {
      error_text <- if (length(error_messages) > 3) {
        paste0(paste(error_messages[1:3], collapse = "\n"), "\n...(more errors)")
      } else {
        paste(error_messages, collapse = "\n")
      }
      
      shinyalert(
        title = "Import Failed",
        text = sprintf("Failed to import any records.\n\nErrors:\n%s", error_text),
        type = "error"
      )
    }
    
    # Clear the text area and hide the import button
    updateTextAreaInput(session, "expense_text_data", value = "")
    output$text_preview_ui <- renderUI({})
    shinyjs::hide("import_text_button_container")
    parsed_text_data(NULL)  # Clear stored data
  })
  
  # Add income handler
  observeEvent(input$add_income_btn, {
    req(user_data$user_id, input$income_date, input$income_source, 
        input$income_amount, input$income_category)
    
    # Debug output
    cat("Adding income:", input$income_source, input$income_amount, "\n")
    
    # Insert income with error handling
    tryCatch({
      # Insert income
      dbExecute(
        con,
        sprintf(
          "INSERT INTO income (user_id, date, source_name, amount, category) VALUES (%d, '%s', '%s', %.2f, '%s')",
          user_data$user_id, input$income_date, sql_escape(input$income_source), 
          input$income_amount, sql_escape(input$income_category)
        )
      )
      
      # Force a slightly different update counter value
      new_counter <- expense_tracker$update_counter + 1
      expense_tracker$update_counter <- new_counter
      cat("Updated counter to:", new_counter, "\n")
      
      # Success message and form reset...
    }, error = function(e) {
      cat("Error adding income:", e$message, "\n")
      shinyalert(title = "Error", text = e$message, type = "error")
    })
  })
  
  # Create income list UI
  output$income_list <- renderUI({
    req(user_data$user_id)
    
    # Add explicit dependency on expense tracker to force updates
    expense_tracker$update_counter
    
    # Get income data - use dbSendQuery + dbFetch to avoid caching issues
    query <- dbSendQuery(
      con,
      sprintf(
        "SELECT id, date, source_name, amount, category FROM income 
       WHERE user_id = %d
       ORDER BY date DESC, id DESC 
       LIMIT 100",
        user_data$user_id
      )
    )
    income <- dbFetch(query)
    dbClearResult(query)
    
    # Debug output to console
    cat("Refreshing income list, found", nrow(income), "records\n")
    
    if (nrow(income) == 0) {
      return(div(
        style = "text-align: center; padding: 20px;",
        "No income entries found."
      ))
    }
    
    # Calculate total
    total <- sum(income$amount)
    
    # Create table
    div(
      style = "overflow-x: auto;",
      tags$table(
        class = "table table-striped",
        tags$thead(
          tags$tr(
            tags$th("Date"),
            tags$th("Source"),
            tags$th("Category"),
            tags$th("Amount"),
            tags$th("Actions", style = "text-align: center;")
          )
        ),
        tags$tbody(
          lapply(1:nrow(income), function(i) {
            inc <- income[i, ]
            tags$tr(
              tags$td(format(as.Date(inc$date), "%d %b %Y")),
              tags$td(inc$source_name),
              tags$td(inc$category),
              tags$td(sprintf("%s%.2f", app_settings$currency_symbol, inc$amount), style = "text-align: right;"),
              tags$td(
                style = "text-align: center; white-space: nowrap;",
                # Add edit/delete buttons similar to expenses
                actionButton(
                  inputId = paste0("edit_income_", inc$id),
                  label = NULL,
                  icon = icon("edit"),
                  class = "btn-sm btn-primary",
                  style = "margin-right: 5px;"
                ),
                actionButton(
                  inputId = paste0("delete_income_", inc$id),
                  label = NULL,
                  icon = icon("trash"),
                  class = "btn-sm btn-danger"
                )
              )
            )
          })
        ),
        tags$tfoot(
          tags$tr(
            tags$th(colspan = 3, "Total", style = "text-align: right;"),
            tags$th(sprintf("%s%.2f", app_settings$currency_symbol, total), style = "text-align: right;")
          )
        )
      )
    )
  })
  
  # Unified save transaction handler
  observeEvent(input$save_transaction_btn, {
    req(user_data$user_id, input$transaction_date)
    
    # Handle based on transaction type
    if(input$transaction_type == "expense") {
      req(input$expense_name, input$expense_amount, input$expense_category)
      
      # Escape single quotes
      safe_item_name <- sql_escape(input$expense_name)
      safe_category <- sql_escape(input$expense_category)
      
      # Insert expense
      dbExecute(
        con,
        sprintf(
          "INSERT INTO expenses (user_id, date, item_name, amount, category) VALUES (%d, '%s', '%s', %.2f, '%s')",
          user_data$user_id, input$transaction_date, safe_item_name, 
          input$expense_amount, safe_category
        )
      )
      
      # Show success message
      shinyalert(
        title = "Expense Added",
        text = sprintf("Your expense of %s for %s has been added successfully.",
                       format_currency(input$expense_amount, app_settings$currency), 
                       input$expense_name),
        type = "success",
        timer = 2000,
        showConfirmButton = FALSE
      )
      
    } else if(input$transaction_type == "income") {
      req(input$income_source, input$income_amount, input$income_category)
      
      # Escape single quotes
      safe_source_name <- sql_escape(input$income_source)
      safe_category <- sql_escape(input$income_category)
      
      # Insert income
      dbExecute(
        con,
        sprintf(
          "INSERT INTO income (user_id, date, source_name, amount, category) VALUES (%d, '%s', '%s', %.2f, '%s')",
          user_data$user_id, input$transaction_date, safe_source_name, 
          input$income_amount, safe_category
        )
      )
      
      # Show success message
      shinyalert(
        title = "Income Added",
        text = sprintf("Your income of %s from %s has been added successfully.",
                       format_currency(input$income_amount, app_settings$currency), 
                       input$income_source),
        type = "success",
        timer = 2000,
        showConfirmButton = FALSE
      )
    }
    
    # Increment update counter to trigger UI refresh
    expense_tracker$update_counter <- expense_tracker$update_counter + 1
    
    # Reset form based on transaction type
    if(input$transaction_type == "expense") {
      updateTextInput(session, "expense_name", value = "")
      updateNumericInput(session, "expense_amount", value = 0)
    } else {
      updateTextInput(session, "income_source", value = "")
      updateNumericInput(session, "income_amount", value = 0)
    }
  })
  
  # Create a simple HTML/CSS financial summary
  output$financial_summary <- renderUI({
    req(user_data$is_authenticated)
    
    # Add dependency on expense tracker counter
    expense_tracker$update_counter
    
    # Get financial data
    financial_data <- calculate_net_cash_flow()
    
    # Format currency values
    income_formatted <- format_currency(financial_data$income, app_settings$currency)
    expenses_formatted <- format_currency(financial_data$expenses, app_settings$currency)
    net_formatted <- format_currency(financial_data$net, app_settings$currency)
    
    # Determine net class
    net_class <- if(financial_data$net >= 0) "positive-net" else "negative-net"
    
    # Inline CSS
    tags$div(
      style = "font-family: inherit;",
      tags$style(HTML("
      .financial-summary { width: 100%; margin-bottom: 20px; }
      .financial-card { display: flex; text-align: center; margin-bottom: 15px; }
      .financial-metric { flex: 1; padding: 15px; border-radius: 8px; margin: 0 5px; }
      .income-metric { background-color: rgba(40, 167, 69, 0.2); color: #28a745; }
      .expenses-metric { background-color: rgba(220, 53, 69, 0.2); color: #dc3545; }
      .positive-net { background-color: rgba(40, 167, 69, 0.2); color: #28a745; }
      .negative-net { background-color: rgba(220, 53, 69, 0.2); color: #dc3545; }
      .financial-value { font-size: 24px; font-weight: bold; margin: 10px 0; }
      .financial-label { font-size: 14px; text-transform: uppercase; margin: 0; }
      body.dark-mode .financial-metric { border: 1px solid rgba(255, 255, 255, 0.1); }
    ")),
      
      tags$div(class = "financial-summary",
               
               # Income and Expenses row
               tags$div(class = "financial-card",
                        # Income metric
                        tags$div(class = "financial-metric income-metric",
                                 tags$p(class = "financial-label", "Income"),
                                 tags$p(class = "financial-value", income_formatted)
                        ),
                        
                        # Expenses metric
                        tags$div(class = "financial-metric expenses-metric",
                                 tags$p(class = "financial-label", "Expenses"),
                                 tags$p(class = "financial-value", expenses_formatted)
                        )
               ),
               
               # Net amount row
               tags$div(class = "financial-card",
                        tags$div(class = paste("financial-metric", net_class), style = "width: 100%;",
                                 tags$p(class = "financial-label", "Net Balance"),
                                 tags$p(class = "financial-value", net_formatted)
                        )
               )
      )
    )
  })
  
  # Calculate net cash flow (income - expenses)
  calculate_net_cash_flow <- reactive({
    req(user_data$user_id)
    
    # Get the date range
    end_date <- Sys.Date()
    
    if(user_data$limit_period == "monthly") {
      start_date <- as.Date(format(end_date, "%Y-%m-01"))
    } else if(user_data$limit_period == "weekly") {
      days_since_monday <- as.numeric(format(end_date, "%u")) - 1
      start_date <- end_date - days_since_monday
    } else {
      start_date <- end_date - 30
    }
    
    # Get total expenses
    expenses_query <- dbSendQuery(con, sprintf(
      "SELECT SUM(amount) as total FROM expenses WHERE user_id = %d AND date >= '%s' AND date <= '%s'",
      user_data$user_id, start_date, end_date
    ))
    expenses_result <- dbFetch(expenses_query)
    dbClearResult(expenses_query)
    
    total_expenses <- if(is.null(expenses_result$total) || is.na(expenses_result$total)) 0 else expenses_result$total
    
    # Get total income
    income_query <- dbSendQuery(con, sprintf(
      "SELECT SUM(amount) as total FROM income WHERE user_id = %d AND date >= '%s' AND date <= '%s'",
      user_data$user_id, start_date, end_date
    ))
    income_result <- dbFetch(income_query)
    dbClearResult(income_query)
    
    total_income <- if(is.null(income_result$total) || is.na(income_result$total)) 0 else income_result$total
    
    # Calculate net cash flow
    net_cash_flow <- total_income - total_expenses
    
    return(list(
      expenses = total_expenses,
      income = total_income,
      net = net_cash_flow
    ))
  })
  
  # Observe event to update goal dropdown
  observe({
    req(user_data$user_id)
    
    # Get active goals
    goals <- dbGetQuery(
      con,
      sprintf(
        "SELECT id, name FROM savings_goals WHERE user_id = %d AND is_active = 1 ORDER BY name",
        user_data$user_id
      )
    )
    
    if (nrow(goals) > 0) {
      goal_choices <- setNames(as.character(goals$id), goals$name)
      updateSelectInput(session, "contribution_goal_id", choices = goal_choices)
    } else {
      updateSelectInput(session, "contribution_goal_id", choices = c("No goals available" = ""))
    }
  })
  
  # Add goal handler
  observeEvent(input$add_goal_btn, {
    req(user_data$user_id, input$goal_name, input$goal_amount, input$goal_category)
    
    # Validate inputs
    if (input$goal_amount <= 0) {
      shinyalert(
        title = "Invalid Amount",
        text = "Please enter a positive amount for your goal.",
        type = "error"
      )
      return()
    }
    
    # Format the date or use NULL
    target_date <- if(is.null(input$goal_date) || input$goal_date == "") "NULL" else sprintf("'%s'", input$goal_date)
    
    # Escape single quotes in strings
    safe_name <- gsub("'", "''", input$goal_name)
    safe_category <- gsub("'", "''", input$goal_category)
    
    # Insert goal
    dbExecute(
      con,
      sprintf(
        "INSERT INTO savings_goals (user_id, name, target_amount, target_date, category) VALUES (%d, '%s', %.2f, %s, '%s')",
        user_data$user_id, safe_name, input$goal_amount, target_date, safe_category
      )
    )
    
    # Increment update counter
    expense_tracker$update_counter <- expense_tracker$update_counter + 1
    
    # Reset form
    updateTextInput(session, "goal_name", value = "")
    updateNumericInput(session, "goal_amount", value = 0)
    updateDateInput(session, "goal_date", value = NULL)
    
    # Show success message
    shinyalert(
      title = "Savings Goal Added",
      text = sprintf("Your goal '%s' for %s has been added successfully.",
                     input$goal_name, format_currency(input$goal_amount, app_settings$currency)),
      type = "success",
      timer = 2000,
      showConfirmButton = FALSE
    )
  })
  
  # Add contribution handler
  observeEvent(input$add_contribution_btn, {
    req(user_data$user_id, input$contribution_goal_id, input$contribution_amount, input$contribution_date)
    
    # Validate inputs
    if (input$contribution_amount <= 0) {
      shinyalert(
        title = "Invalid Amount",
        text = "Please enter a positive amount for your contribution.",
        type = "error"
      )
      return()
    }
    
    if (input$contribution_goal_id == "") {
      shinyalert(
        title = "No Goal Selected",
        text = "Please select a goal to contribute to.",
        type = "error"
      )
      return()
    }
    
    # Update the goal's current amount
    dbExecute(
      con,
      sprintf(
        "UPDATE savings_goals SET current_amount = current_amount + %.2f WHERE id = %s AND user_id = %d",
        input$contribution_amount, input$contribution_goal_id, user_data$user_id
      )
    )
    
    # Increment update counter
    expense_tracker$update_counter <- expense_tracker$update_counter + 1
    
    # Reset form
    updateNumericInput(session, "contribution_amount", value = 0)
    
    # Get goal name for the success message
    goal_info <- dbGetQuery(
      con,
      sprintf(
        "SELECT name FROM savings_goals WHERE id = %s AND user_id = %d",
        input$contribution_goal_id, user_data$user_id
      )
    )
    
    # Show success message
    if (nrow(goal_info) > 0) {
      shinyalert(
        title = "Contribution Added",
        text = sprintf("Your contribution of %s to '%s' has been added successfully.",
                       format_currency(input$contribution_amount, app_settings$currency), goal_info$name[1]),
        type = "success",
        timer = 2000,
        showConfirmButton = FALSE
      )
    }
  })
  
  # Goals list UI
  output$goals_list <- renderUI({
    req(user_data$user_id)
    
    # Get goals data
    goals <- dbGetQuery(
      con,
      sprintf(
        "SELECT id, name, target_amount, current_amount, target_date, category, is_active 
       FROM savings_goals 
       WHERE user_id = %d
       ORDER BY is_active DESC, target_date ASC, name ASC",
        user_data$user_id
      )
    )
    
    if (nrow(goals) == 0) {
      return(div(
        style = "text-align: center; padding: 20px;",
        "No savings goals found. Add your first goal above!"
      ))
    }
    
    # Create a list of goal cards
    tagList(
      lapply(1:nrow(goals), function(i) {
        goal <- goals[i, ]
        
        # Calculate percentage complete
        percentage <- min(100, round(goal$current_amount / goal$target_amount * 100))
        
        # Format target date
        target_date_text <- if(is.na(goal$target_date) || goal$target_date == "") "No target date" else format(as.Date(goal$target_date), "%d %b %Y")
        
        # Determine progress bar color
        bar_color <- if(percentage >= 80) "#28a745" else if(percentage >= 50) "#17a2b8" else "#007bff"
        
        # Create a card for each goal
        div(
          class = "goal-card",
          style = paste0("border: 1px solid #ddd; border-radius: 8px; padding: 15px; margin-bottom: 15px; ", 
                         if(!goal$is_active) "opacity: 0.6;" else ""),
          div(
            style = "display: flex; justify-content: space-between; align-items: center;",
            h4(goal$name, style = "margin: 0;"),
            if(goal$is_active) {
              actionButton(
                inputId = paste0("toggle_goal_", goal$id),
                label = "Complete",
                class = "btn-sm btn-outline-success"
              )
            } else {
              actionButton(
                inputId = paste0("toggle_goal_", goal$id),
                label = "Reactivate",
                class = "btn-sm btn-outline-secondary"
              )
            }
          ),
          p(
            style = "margin-top: 10px; color: #666;",
            sprintf("Category: %s", goal$category)
          ),
          p(
            style = "margin-bottom: 5px;",
            sprintf("Progress: %s of %s", 
                    format_currency(goal$current_amount, app_settings$currency),
                    format_currency(goal$target_amount, app_settings$currency))
          ),
          div(
            class = "progress",
            style = "height: 20px; margin-bottom: 10px;",
            div(
              class = "progress-bar",
              role = "progressbar",
              style = sprintf("width: %d%%; background-color: %s;", percentage, bar_color),
              sprintf("%d%%", percentage)
            )
          ),
          div(
            style = "display: flex; justify-content: space-between; font-size: 12px; color: #666;",
            div(sprintf("Target date: %s", target_date_text)),
            div(
              style = "text-align: right;",
              actionButton(
                inputId = paste0("delete_goal_", goal$id),
                label = NULL,
                icon = icon("trash"),
                class = "btn-sm btn-danger",
                style = "margin-left: 10px;"
              )
            )
          )
        )
      })
    )
  })
}