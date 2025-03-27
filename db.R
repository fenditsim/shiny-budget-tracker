# db.R - Database setup and functions for the Budget Tracker app

# Set up database connection
setup_db <- function() {
  # Create a connection to SQLite database
  con <- dbConnect(RSQLite::SQLite(), "budget_tracker.db")
  
  # Create users table if it doesn't exist
  if (!dbExistsTable(con, "users")) {
    dbExecute(con, "
      CREATE TABLE users (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        username TEXT UNIQUE NOT NULL,
        password TEXT NOT NULL,
        display_name TEXT NOT NULL,
        theme TEXT DEFAULT 'light',
        font TEXT DEFAULT 'Arial',
        currency TEXT DEFAULT 'GBP',
        spending_limit NUMERIC DEFAULT 1000,
        limit_period TEXT DEFAULT 'monthly',
        created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
      )
    ")
    # Add a test user
    dbExecute(con, "
      INSERT INTO users (username, password, display_name)
      VALUES ('test', 'password', 'Test User')
    ")
  } else {
    # Check if columns exist, add them if they don't
    user_columns <- dbListFields(con, "users")
    if (!"theme" %in% user_columns) {
      dbExecute(con, "ALTER TABLE users ADD COLUMN theme TEXT DEFAULT 'light'")
    }
    if (!"font" %in% user_columns) {
      dbExecute(con, "ALTER TABLE users ADD COLUMN font TEXT DEFAULT 'Arial'")
    }
    if (!"currency" %in% user_columns) {
      dbExecute(con, "ALTER TABLE users ADD COLUMN currency TEXT DEFAULT 'GBP'")
    }
    # Add new columns for spending limit feature
    if (!"spending_limit" %in% user_columns) {
      dbExecute(con, "ALTER TABLE users ADD COLUMN spending_limit NUMERIC DEFAULT 1000")
    }
    if (!"limit_period" %in% user_columns) {
      dbExecute(con, "ALTER TABLE users ADD COLUMN limit_period TEXT DEFAULT 'monthly'")
    }
  }
  
  # Create expenses table if it doesn't exist
  if (!dbExistsTable(con, "expenses")) {
    dbExecute(con, "
      CREATE TABLE expenses (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        user_id INTEGER NOT NULL,
        date DATE NOT NULL,
        item_name TEXT NOT NULL,
        amount NUMERIC NOT NULL,
        category TEXT NOT NULL,
        created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        FOREIGN KEY (user_id) REFERENCES users(id)
      )
    ")
  }
  
  # Create income table if it doesn't exist
  if (!dbExistsTable(con, "income")) {
    dbExecute(con, "
    CREATE TABLE income (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      user_id INTEGER NOT NULL,
      date DATE NOT NULL,
      source_name TEXT NOT NULL,
      amount NUMERIC NOT NULL,
      category TEXT NOT NULL,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      FOREIGN KEY (user_id) REFERENCES users(id)
    )
  ")
  }
  
  # Create savings_goals table if it doesn't exist
  if (!dbExistsTable(con, "savings_goals")) {
    dbExecute(con, "
    CREATE TABLE savings_goals (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      user_id INTEGER NOT NULL,
      name TEXT NOT NULL,
      target_amount NUMERIC NOT NULL,
      current_amount NUMERIC DEFAULT 0,
      target_date DATE,
      category TEXT,
      is_active BOOLEAN DEFAULT 1,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      FOREIGN KEY (user_id) REFERENCES users(id)
    )
  ")
  }
  
  return(con)
}