// Budget Tracker App - JavaScript functions

// Dark mode toggle function
function toggleDarkMode(isDark) {
  if (isDark) {
    document.body.classList.add('dark-mode');
    // Force tab styles
    setTimeout(function() {
      $('.nav-tabs .nav-link').css('color', '#48A860');
      $('.nav-tabs .nav-link.active').css({
        'background-color': '#48A860',
        'color': 'white'
      });
    }, 100);
  } else {
    document.body.classList.remove('dark-mode');
  }
}

// Modal control functions
function showEditModal() {
  var modal = document.getElementById('edit-expense-modal');
  if (modal) {
    modal.style.display = 'block';
  }
}

function hideEditModal() {
  var modal = document.getElementById('edit-expense-modal');
  if (modal) {
    modal.style.display = 'none';
  }
}

// Dark mode table styling function
function applyDarkModeToTables() {
  // Target the expense table specifically with highest priority
  var expenseTables = document.querySelectorAll("#expense_list .table, #expense_list table");
  expenseTables.forEach(function(table) {
    table.style.setProperty("background-color", "#1e1e1e", "important");
    
    // Target all table rows directly
    var rows = table.querySelectorAll("tbody tr");
    rows.forEach(function(row, index) {
      row.style.setProperty("background-color", index % 2 === 0 ? "#1e1e1e" : "#2a2a2a", "important");
    });
    
    // Target all table cells directly
    var cells = table.querySelectorAll("td, th");
    cells.forEach(function(cell) {
      cell.style.setProperty("color", "#FFFFFF", "important");
      cell.style.setProperty("background-color", "inherit", "important");
    });
    
    // Also ensure the table footer has dark background
    var tfoot = table.querySelector("tfoot");
    if (tfoot) {
      tfoot.style.setProperty("background-color", "#1e1e1e", "important");
      var tfootRow = tfoot.querySelector("tr");
      if (tfootRow) {
        tfootRow.style.setProperty("background-color", "#1e1e1e", "important");
      }
    }
  });
}

// Document ready function
$(document).ready(function() {
  // Form submission handling
  $('#login-form').on('submit', function(e) {
    e.preventDefault();
    $('#login_btn').click();
    return false;
  });
  
  // Direct keypress handling with specific target selectors
  $('#username, #password').on('keydown', function(e) {
    if (e.which === 13 || e.keyCode === 13) {
      e.preventDefault();
      $('#login_btn').click();
      return false;
    }
  });
  
  // Additional keyup event for redundancy
  $('#username, #password').on('keyup', function(e) {
    if (e.which === 13 || e.keyCode === 13) {
      // Add a small delay to ensure event is processed after input
      setTimeout(function() {
        $('#login_btn').click();
      }, 50);
      e.preventDefault();
      return false;
    }
  });
  
  // Event delegation approach (works even if elements are dynamically added)
  $(document).on('keypress', '#username, #password', function(e) {
    if (e.which === 13 || e.keyCode === 13) {
      e.preventDefault();
      $('#login_btn').click();
      return false;
    }
  });
  
  // Log to console for debugging
  console.log('Enhanced login form Enter key handlers initialized');
});

//Edit Income Modal Control Functions//
// Modal control functions for income editing
function showEditIncomeModal() {
  var modal = document.getElementById("edit-income-modal");
  if (modal) {
    modal.style.display = "block";
  }
}

function hideEditIncomeModal() {
  var modal = document.getElementById("edit-income-modal");
  if (modal) {
    modal.style.display = "none";
  }
}