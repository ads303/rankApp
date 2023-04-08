#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load required libraries
library(shiny)
library(shinydashboard)
library(bslib)
library(shinyWidgets)
library(DBI)
library(pool)

con <- dbConnect(RSQLite::SQLite(), dbname = "rankings.db")

onStop(function() {
  dbDisconnect(con)
})

dbExecute(con, "CREATE TABLE IF NOT EXISTS rankings_data (User TEXT, Category TEXT, Rank INTEGER, Item TEXT)")

ui <- navbarPage(
  title = "Master Debaters: Power Rank Everything",
  theme = bs_theme(bg = "ghostwhite", fg = "black", primary = "darkblue", base_font = "sans-serif"),
  id = "main_navbar",
  
  tabPanel("Rankings",
           fluidRow(
             box(title = "Name", width = 3,
                 textInput("username", "Your Full Name", "")),
             box(title = "What Are You Ranking Today?", width = 3,
                 selectizeInput("category", "Start typing a category.", choices = NULL, options = list(create = TRUE))),
             box(title = "Rankings", width = 3,
                 textAreaInput("rankings", "One item per line, from first to last.", "", rows = 15)),
             box(title = "Submit", width =3,
                 actionButton("submit", "Submit Rankings"),
                 conditionalPanel(
                   condition = "output.submitSuccess",
                   tags$p("Thanks! To view and select someone's rankings of various categories below, please refresh the page.",
                          style = "font-weight: bold; color: green;")
                 )
             ),
             fluidRow(
             box(title = "View How You and Your Friends Rank", width = 4,
                 selectInput("user_filter", "Select User", choices = NULL, selected = NULL),
                 selectInput("category_filter", "Select Category", choices = NULL, selected = NULL)
             ),
             box(title = textOutput("filtered_title"), width = 4,
                 tableOutput("filtered_rankings")))
           )
  )
)

tags$script(HTML("
  $(document).ready(function() {
    $('body').on('dblclick', '.selectize-input input', function() {
      $(this).select();
    });
  });
"))


server <- function(input, output, session) {
  
  rankings_data <- reactive({
    dbGetQuery(con, "SELECT * FROM rankings_data")
  })
  
  observe({
    updateSelectizeInput(session, "category", choices = unique(rankings_data()$Category), options = list(create = TRUE))
    updateSelectInput(session, "user_filter", choices = unique(rankings_data()$User), selected = input$user_filter)
    updateSelectInput(session, "category_filter", choices = unique(rankings_data()$Category), selected = input$category_filter)
  })
  
  submit_success <- reactiveVal(FALSE)
  submit_time <- reactiveVal(NULL)
  
  observeEvent(input$submit, {
    user <- input$username
    if (user == "") {
      user <- "Anonymous"
    }
    category <- tolower(input$category) # Convert the input category to lowercase
    rankings <- unlist(strsplit(input$rankings, split = "\\n"))
    
    if (nrow(rankings_data()) > 0 && any(rankings_data()$User == user & rankings_data()$Category == category)) {
      dbExecute(con, "DELETE FROM rankings_data WHERE User = ? AND Category = ?", params = list(user, category)) # Remove previous entries of the user for the category
    }
    
    new_data <- data.frame(User = rep(user, length(rankings)), Category = rep(category, length(rankings)), Rank = 1:length(rankings), Item = rankings, stringsAsFactors = FALSE)
    dbWriteTable(con, "rankings_data", new_data, append = TRUE, row.names = FALSE)
    
    submit_success(TRUE)
    submit_time(Sys.time()) # Store the submission time
  })
  
  output$filtered_title <- renderText({
    req(input$user_filter, input$category_filter)
    paste0(input$user_filter, "'s ", input$category_filter, " Rankings")
  })
  
  output$filtered_rankings <- renderTable({
    req(input$user_filter, input$category_filter)
    rankings_data <- rankings_data()
    rankings_data[rankings_data$User == input$user_filter & rankings_data$Category == input$category_filter, c("Rank", "Item")]
  }, rownames = FALSE)
  
  output$submitSuccess <- reactive({
    submit_success()
  })
  
  outputOptions(output, "submitSuccess", suspendWhenHidden = FALSE)
  
  observe({
    invalidateLater(1000) # Reactive timer that triggers every second
    if (!is.null(submit_time()) && difftime(Sys.time(), submit_time(), units = "secs") >= 5) {
      submit_success(FALSE)
    }
  })
}

shinyApp(ui = ui, server = server)