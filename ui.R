# ui.R - User interface definition for Budget Tracker app

ui <- fluidPage(
  title = "Budget Tracker",
  
  # Use shinyjs and shinyalert (force=TRUE per warning message)
  shinyjs::useShinyjs(),
  shinyalert::useShinyalert(force = TRUE),
  
  # Include external CSS and JavaScript files
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$script(src = "scripts.js"),
    tags$style(HTML("
      /* Make tabs equal width and evenly spaced */
      .nav-tabs {
        display: flex;
        width: 100%;
      }
      
      .nav-tabs > li {
        flex: 1;
        text-align: center;
        float: none;
      }
      
      .nav-tabs > li > a {
        margin-right: 0;
        white-space: nowrap;
        text-overflow: ellipsis;
        overflow: hidden;
        padding-left: 5px;
        padding-right: 5px;
      }
      
      /* Improve tab appearance */
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:hover,
      .nav-tabs > li.active > a:focus {
        font-weight: bold;
      }
      
      /* Make sure icons stay aligned with text */
      .nav-tabs > li > a > i {
        margin-right: 5px;
      }
    "))
  ),
  
  # User authentication
  # Login form
  div(
    id = "login-panel",
    div(class = "card",
        h1("Budget Tracker", style = "text-align: center;"),
        
        # Wrap in a form tag to better handle Enter key submissions
        tags$form(
          id = "login-form", 
          onsubmit = "return false;", # Prevent default form submission
          
          # Custom login form
          textInput(inputId = "username", label = "Username", placeholder = "Enter username", width = "100%"),
          passwordInput(inputId = "password", label = "Password", placeholder = "Enter password", width = "100%"),
          actionButton(inputId = "login_btn", label = "Login", class = "btn-primary btn-block"),
        ),
        
        br(),
        actionButton(inputId = "register_btn", label = "Create a New Account", class = "btn-outline-secondary btn-block")
    )
  ),
  
  # Hidden registration form
  shinyjs::hidden(
    div(
      id = "register-panel",
      div(class = "card",
          h1("Create Account", style = "text-align: center;"),
          textInput(inputId = "new_username", label = "Username", placeholder = "Choose a username", width = "100%"),
          passwordInput(inputId = "new_password", label = "Password", placeholder = "Choose a password", width = "100%"),
          textInput(inputId = "new_display_name", label = "Your Name", placeholder = "How should we call you?", width = "100%"),
          actionButton(inputId = "create_account_btn", label = "Create Account", class = "btn-success btn-block"),
          actionButton(inputId = "back_to_login_btn", label = "Back to Login", class = "btn-outline-primary btn-block")
      )
    )
  ),
  
  # Hidden edit expense modal
  shinyjs::hidden(
    div(
      id = "edit-expense-modal",
      class = "modal",
      style = "display: none; position: fixed; z-index: 2000; left: 0; top: 0; width: 100%; height: 100%; overflow: auto; background-color: rgba(0,0,0,0.4);",
      div(
        class = "modal-content card",
        style = "background-color: #fefefe; margin: 15% auto; padding: 20px; border: 1px solid #888; width: 80%; max-width: 500px;",
        div(
          style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;",
          h3("Edit Expense", id = "edit-expense-title"),
          actionButton(inputId = "close_edit_modal_btn", label = NULL, icon = icon("times"), class = "btn-sm")
        ),
        # Hidden expense ID using div with hidden style (Shiny-compatible approach)
        div(style = "display: none;",
            textInput(inputId = "edit_expense_id", label = NULL, value = "")
        ),
        dateInput(inputId = "edit_expense_date", label = "Date", value = Sys.Date(), width = "100%"),
        textInput(inputId = "edit_expense_name", label = "Description", placeholder = "What did you spend on?", width = "100%"),
        numericInput(inputId = "edit_expense_amount", label = "Amount", value = 0, min = 0, max = 99999.99, step = 0.01, width = "100%"),
        selectInput(inputId = "edit_expense_category", label = "Category", choices = expense_categories, width = "100%"),
        div(
          style = "text-align: right; margin-top: 20px;",
          actionButton(inputId = "cancel_edit_btn", label = "Cancel", class = "btn-secondary", style = "margin-right: 10px;"),
          actionButton(inputId = "save_edit_btn", label = "Save Changes", class = "btn-success")
        )
      )
    )
  ),
  
  # Hidden edit income modal UI
  shinyjs::hidden(
    div(
      id = "edit-income-modal",
      class = "modal",
      style = "display: none; position: fixed; z-index: 2000; left: 0; top: 0; width: 100%; height: 100%; overflow: auto; background-color: rgba(0,0,0,0.4);",
      div(
        class = "modal-content card",
        style = "background-color: #fefefe; margin: 15% auto; padding: 20px; border: 1px solid #888; width: 80%; max-width: 500px;",
        div(
          style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;",
          h3("Edit Income", id = "edit-income-title"),
          actionButton(inputId = "close_edit_income_modal_btn", label = NULL, icon = icon("times"), class = "btn-sm")
        ),
        # Hidden income ID using div with hidden style
        div(style = "display: none;",
            textInput(inputId = "edit_income_id", label = NULL, value = "")
        ),
        dateInput(inputId = "edit_income_date", label = "Date", value = Sys.Date(), width = "100%"),
        textInput(inputId = "edit_income_source", label = "Source", placeholder = "Where did the income come from?", width = "100%"),
        numericInput(inputId = "edit_income_amount", label = "Amount", value = 0, min = 0, max = 99999.99, step = 0.01, width = "100%"),
        selectInput(inputId = "edit_income_category", label = "Category", 
                    choices = c("Salary", "Freelance", "Investments", "Gifts", "Refunds", "Other"), width = "100%"),
        div(
          style = "text-align: right; margin-top: 20px;",
          actionButton(inputId = "cancel_edit_income_btn", label = "Cancel", class = "btn-secondary", style = "margin-right: 10px;"),
          actionButton(inputId = "save_edit_income_btn", label = "Save Changes", class = "btn-success")
        )
      )
    )
  ),
  
  # Hidden settings page
  shinyjs::hidden(
    div(
      id = "settings-panel",
      div(class = "navbar",
          h3("Settings"),
          div(class = "nav-buttons",
              actionButton(inputId = "close_settings_btn", label = "Back", icon = icon("arrow-left"), class = "btn-sm btn-secondary")
          )
      ),
      div(class = "container mt-4",
          div(class = "row justify-content-center",
              div(class = "col-md-6 col-sm-12",
                  tabsetPanel(
                    id = "settings_tabs",
                    
                    # User Settings Tab
                    tabPanel(
                      title = "User Settings",
                      value = "user",
                      width = "100%",
                      div(class = "card p-3",
                          h4("Profile Settings"),
                          textInput(inputId = "change_display_name", label = "Display Name", placeholder = "Your name", width = "100%"),
                          textInput(inputId = "view_username", label = "Username", placeholder = "Username", width = "100%"),
                          passwordInput(inputId = "change_password", label = "New Password", placeholder = "Leave blank to keep current", width = "100%"),
                          # Spending Limit Settings
                          h4("Spending Limit", style = "margin-top: 20px;"),
                          numericInput(inputId = "spending_limit", label = "Spending Limit (£)", 
                                       value = 1000, min = 0, step = 10, width = "100%"),
                          selectInput(inputId = "limit_period", label = "Limit Period", 
                                      choices = c("Monthly" = "monthly", "Weekly" = "weekly"), 
                                      selected = "monthly", width = "100%"),
                          hr(),
                          actionButton(inputId = "save_settings_btn", label = "Save Changes", class = "btn-success btn-block mt-3"),
                          
                          # Add Danger Zone with Remove Account button
                          div(class = "danger-zone",
                              h4("Danger Zone"),
                              p("This action cannot be undone. This will permanently delete your account and all associated data."),
                              actionButton(inputId = "remove_account_btn", 
                                           label = "Remove My Account", 
                                           icon = icon("trash"),
                                           class = "btn-danger btn-block mt-3")
                          )
                      )
                    ),
                    
                    # Appearance Tab
                    tabPanel(
                      title = "Appearance",
                      value = "appearance",
                      width = "100%",
                      div(class = "card p-3",
                          h4("Appearance Settings"),
                          selectInput(inputId = "select_theme", label = "Theme", 
                                      choices = c("Light" = "light", "Dark" = "dark"), 
                                      selected = "light", width = "100%"),
                          selectInput(inputId = "select_font", label = "Font", 
                                      choices = fonts,
                                      selected = "Arial", width = "100%"),
                          selectInput(inputId = "select_currency", label = "Currency", 
                                      choices = currencies,
                                      selected = "GBP", width = "100%"),
                          hr(),
                          actionButton(inputId = "save_settings_btn", label = "Save Changes", class = "btn-success btn-block mt-3")
                      )
                    ),
                    
                    # Import Data Tab
                    tabPanel(
                      title = "Import Data",
                      value = "import",
                      width = "100%",
                      div(class = "card p-3",
                          h4("Import Expenses"),
                          p("Upload your expense data in CSV format to bulk import expenses."),
                          
                          # Format instructions
                          div(class = "format-instructions",
                              h5("File Format Requirements:"),
                              p("Your CSV file should have the following columns:"),
                              tags$ul(
                                tags$li(strong("date"), " - Date of expense (YYYY-MM-DD format)"),
                                tags$li(strong("item_name"), " - Description of what you spent on"),
                                tags$li(strong("amount"), " - Amount spent (numeric value only)"),
                                tags$li(strong("category"), " - Category of expense (must match one of the app's categories)")
                              ),
                              p("Example:"),
                              pre(
                                style = "background-color: #f8f9fa; padding: 10px; border-radius: 4px; overflow-x: auto;",
                                "date,item_name,amount,category\n2025-03-20,Groceries,42.50,Supermarket\n2025-03-21,Bus ticket,3.75,Transport"
                              ),
                              div(
                                class = "alert alert-info",
                                style = "margin-top: 10px;",
                                HTML("<strong>Available Categories:</strong> ", paste(expense_categories, collapse = ", "))
                              )
                          ),
                          
                          # File input
                          fileInput("upload_expenses", "Choose CSV File",
                                    accept = c(
                                      "text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv"
                                    )),
                          
                          # Preview and import section for file upload
                          uiOutput("upload_preview_ui"),
                          
                          # Initially hidden import button for file upload
                          div(
                            id = "import_button_container",
                            style = "display: none;",
                            actionButton("import_expenses_btn", "Import Expenses", class = "btn-success btn-block mt-3")
                          ),
                          
                          # Text area import section
                          hr(),
                          h4("Or Paste Data Directly"),
                          p("You can also paste your expense data directly in CSV format below:"),
                          
                          # Example for text input
                          div(class = "format-instructions",
                              p("Each line should contain date, description, amount, and category separated by commas:"),
                              pre(
                                style = "background-color: #f8f9fa; padding: 10px; border-radius: 4px; overflow-x: auto;",
                                "2025-03-20,Groceries,42.50,Supermarket\n2025-03-21,Bus ticket,3.75,Transport"
                              )
                          ),
                          
                          # Text area input
                          tags$textarea(
                            id = "expense_text_data",
                            class = "form-control",
                            rows = 8,
                            placeholder = "Paste your data here in CSV format (date,description,amount,category)...",
                            style = "width: 100%; font-family: monospace;"
                          ),
                          
                          # Preview button for text input
                          div(
                            style = "margin-top: 10px;",
                            actionButton("preview_text_data_btn", "Preview Data", class = "btn-primary")
                          ),
                          
                          # Preview and import section for text input
                          uiOutput("text_preview_ui"),
                          
                          # Initially hidden import button for text data
                          div(
                            id = "import_text_button_container",
                            style = "display: none;",
                            actionButton("import_text_expenses_btn", "Import Text Data", class = "btn-success btn-block mt-3")
                          )
                      )
                    ),
                    
                    # Information Tab
                    tabPanel(
                      title = "Information",
                      value = "info",
                      width = "100%",
                      div(class = "card p-3",
                          h4("Information"),
                          hr(),
                          p("This is a budget tracker that Claude 3.7 Sonnet and I made with R and Shiny.")
                      )
                    ),
                    
                    # Log Tab
                    tabPanel(
                      title = "Log",
                      value = "log",
                      width = "100%",
                      div(class = "card p-3",
                          h4("Log"),
                          hr(),
                          p("28 Mar 25: Fixing Income Tab."),
                          p("27 Mar 25: Adding Report, Income Tracking and Savings."),
                          p("26 Mar 25: Adding importing data feature."),
                          p("25 Mar 25: Adding User's budget limit."), 
                          p("24 Mar 25: Initial commit.")
                      )
                    )
                  )
              )
          )
      )
    )
  ),
  
  # Main application (shown after login)
  shinyjs::hidden(
    div(
      id = "app-content",
      
      # Navigation bar
      div(class = "navbar",
          h3(textOutput("navbar_title")),
          div(class = "nav-buttons",
              actionButton(inputId = "show_settings_btn", label = "Settings", icon = icon("cog"), class = "btn-sm btn-light mr-2"),
              actionButton(inputId = "logout_btn", label = "Logout", icon = icon("sign-out-alt"), class = "btn-sm btn-danger")
          )
      ),
      
      # Tabs
      tabsetPanel(
        id = "main_tabs",
        
        # Home Tab
        tabPanel(
          title = "Home",
          value = "home",
          icon = icon("home"),
          
          div(class = "welcome-message",
              h3(textOutput("welcome_message"))
          ),
          
          div(class = "card",
              h3("What would you like to add?"),
              
              # Action Type Selector
              radioButtons(
                inputId = "action_type", 
                label = "Select action:",
                choices = c(
                  "New Transaction" = "transaction", 
                  "New Savings Goal" = "goal", 
                  "Goal Contribution" = "contribution"
                ),
                selected = "transaction",
                inline = TRUE
              ),
              
              # Transaction section (shown when action_type = "transaction")
              conditionalPanel(
                condition = "input.action_type == 'transaction'",
                hr(),
                h4("Add Transaction"),
                
                # Transaction Type Selector
                radioButtons(
                  inputId = "transaction_type", 
                  label = "Transaction Type:", 
                  choices = c("Expense" = "expense", "Income" = "income"),
                  selected = "expense",
                  inline = TRUE
                ),
                
                # Common Fields
                dateInput(inputId = "transaction_date", label = "Date", value = Sys.Date(), width = "100%"),
                
                # Expense fields
                conditionalPanel(
                  condition = "input.transaction_type == 'expense'",
                  textInput(inputId = "expense_name", label = "Description", placeholder = "What did you spend on?", width = "100%"),
                  numericInput(inputId = "expense_amount", label = "Amount", value = 0, min = 0, max = 99999.99, step = 0.01, width = "100%"),
                  selectInput(inputId = "expense_category", label = "Category", choices = expense_categories, width = "100%")
                ),
                
                # Income fields
                conditionalPanel(
                  condition = "input.transaction_type == 'income'",
                  textInput(inputId = "income_source", label = "Source", placeholder = "Where did this income come from?", width = "100%"),
                  numericInput(inputId = "income_amount", label = "Amount", value = 0, min = 0, max = 99999.99, step = 0.01, width = "100%"),
                  selectInput(inputId = "income_category", label = "Category", 
                              choices = c("Salary", "Freelance", "Investments", "Gifts", "Refunds", "Other"), width = "100%")
                ),
                
                # Save Transaction Button
                actionButton(inputId = "save_transaction_btn", label = "Save Transaction", class = "btn-success btn-block")
              ),
              
              # Savings Goal section (shown when action_type = "goal")
              conditionalPanel(
                condition = "input.action_type == 'goal'",
                hr(),
                h4("Add Savings Goal"),
                textInput(inputId = "goal_name", label = "Goal Name", placeholder = "What are you saving for?", width = "100%"),
                numericInput(inputId = "goal_amount", label = "Target Amount", value = 0, min = 0, step = 10, width = "100%"),
                dateInput(inputId = "goal_date", label = "Target Date (Optional)", value = NULL, width = "100%"),
                selectInput(inputId = "goal_category", label = "Category", 
                            choices = c("Emergency Fund", "Home", "Car", "Vacation", "Education", "Retirement", "Other"), 
                            width = "100%"),
                actionButton(inputId = "add_goal_btn", label = "Save Goal", class = "btn-success btn-block")
              ),
              
              # Contribution section (shown when action_type = "contribution")
              conditionalPanel(
                condition = "input.action_type == 'contribution'",
                hr(),
                h4("Add Contribution to Goal"),
                selectInput(inputId = "contribution_goal_id", label = "Select Goal", choices = NULL, width = "100%"),
                numericInput(inputId = "contribution_amount", label = "Amount", value = 0, min = 0, step = 10, width = "100%"),
                dateInput(inputId = "contribution_date", label = "Date", value = Sys.Date(), width = "100%"),
                actionButton(inputId = "add_contribution_btn", label = "Add Contribution", class = "btn-primary btn-block")
              )
          )
        ),
        
        # Summary Tab
        tabPanel(
          title = "Report",
          value = "report",
          icon = icon("chart-bar"),  # Bar chart icon for reporting
          
          # Financial Summary
          div(class = "card",
              h3("Financial health"),
              uiOutput("financial_summary")
          ),
          
          # Spending Limit
          div(class = "card",
              # Budget progress section
              h3("Spending Limit"),
              uiOutput("budget_progress")
          ),
          
          # Goals List
          div(class = "card",
              h3("Your Savings Goals"),
              uiOutput("goals_list")
          )
        ),
        
        # Expenses Tab
        tabPanel(
          title = "Expenses",
          value = "expenses",
          icon = icon("shopping-cart"),
          div(class = "card",
              h3("Expenses Period"),
              div(
                style = "display: flex; justify-content: space-between;",
                div(style = "width: 48%;", dateInput(inputId = "date_range_start", label = "From", value = Sys.Date() - 30, width = "100%")),
                div(style = "width: 48%;", dateInput(inputId = "date_range_end", label = "To", value = Sys.Date(), width = "100%"))
              ),
          ),
          
          div(class = "card",
              h3("Expense Distribution"),
              # Add a container for the donut chart
              echarts4rOutput(outputId = "expense_donut_chart", height = "400px")
          ),
          
          div(class = "card",
              h3("Recent Expenses"),
              selectInput(inputId = "expense_filter", label = "Filter by category", choices = expense_categories, 
                          multiple = TRUE, selectize = TRUE, width = "100%"),
              
              # Export options
              div(class = "export-container",
                  div(class = "export-format-select",
                      selectInput(
                        inputId = "export_format",
                        label = "Export as:",
                        choices = c(
                          "CSV (.csv)" = "csv",
                          "Excel (.xlsx)" = "xlsx",
                          "Tab Delimited (.txt)" = "txt",
                          "JSON (.json)" = "json"
                        ),
                        selected = "csv",
                        width = "auto"
                      )
                  ),
                  downloadButton(
                    outputId = "download_expenses",
                    label = "Export",
                    class = "btn-primary"
                  )
              ),
              
              uiOutput("expense_list")
          )
        ),
        
        # Income Tab
        tabPanel(
          title = "Income",
          value = "income",
          icon = icon("wallet"),
          
          div(class = "card",
              h3("Income Period"),
              div(
                style = "display: flex; justify-content: space-between;",
                div(style = "width: 48%;", dateInput(inputId = "date_range_start", label = "From", value = Sys.Date() - 30, width = "100%")),
                div(style = "width: 48%;", dateInput(inputId = "date_range_end", label = "To", value = Sys.Date(), width = "100%"))
              ),
          ),
          
          div(class = "card",
              h3("Income Distribution"),
              # Add a container for the donut chart
              echarts4rOutput(outputId = "income_donut_chart", height = "400px")
          ),
          
          div(class = "card",
             h3("Income History"),
             selectInput(inputId = "income_filter", label = "Filter by category", 
                         choices = c("Salary", "Freelance", "Investments", "Gifts", "Refunds", "Other"), 
                         multiple = TRUE, selectize = TRUE, width = "100%"),
             
             # Export options
             div(class = "export-container",
                 div(class = "export-format-select",
                     selectInput(
                       inputId = "export_income_format",
                       label = "Export as:",
                       choices = c(
                         "CSV (.csv)" = "csv",
                         "Excel (.xlsx)" = "xlsx",
                         "Tab Delimited (.txt)" = "txt",
                         "JSON (.json)" = "json"
                       ),
                       selected = "csv",
                       width = "auto"
                     )
                 ),
                 downloadButton(
                   outputId = "download_income",
                   label = "Export",
                   class = "btn-primary"
                 )
             ),
             
             uiOutput("income_list")
          )
        )
      )
    )
  )
)