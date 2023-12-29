library(shiny)
library(dplyr)

ui <- fluidPage(
  titlePanel("Your Shiny App"),
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(12,
          fileInput("file", "Choose a file"),
          actionButton("show_table", "Show Table", width = "100%"),
          actionButton("show_file_info", "Show File Info", width = "100%"),
          actionButton("quick_preprocess", "Quick Preprocess", width = "100%"),
          actionButton("manage_missed_values", "Manage Missing Values", width = "100%"),
          actionButton("manage_outlier_data", "Manualy Manage Outlier Data", width = "100%"),
          actionButton("manage_outlier_algorithm", "Manage Outlier Data By Algorithm", width = "100%"),
          actionButton("transform_data", "Transform Data", width = "100%"),
          actionButton("dimensionality_reduction", "Dimensionality Reduction", width = "100%"),
          actionButton("feature_selection", "Feature Selection", width = "100%"),
          actionButton("manage_imbalanced_data", "Manage Imbalanced Data", width = "100%")
        )
      )
    ),
    mainPanel(
      textOutput("button_clicked_output"),
      verbatimTextOutput("file_info_output"),
      # Manage Missing Values Section
      uiOutput("manage_missing_values_ui"),
      tableOutput("file_info_table")
    )
  )
)

server <- function(input, output, session) {
  file_data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })

  observeEvent(input$show_table, {
    output$file_info_table <- renderTable({
      head(file_data(), n = 10)  # Display the first 10 rows as an example
    })
  })

  observe({
    # Clear the main area when any button is clicked
    output$file_info_table <- NULL
  })

  observeEvent(input$show_file_info, {
    output$button_clicked_output <- renderText("Show File Info button clicked!")
    output$file_info_output <- renderPrint({
      cat("File Name: ", input$file$name, "\n")
      cat("File Size: ", round(input$file$size / (1024^2), 2), " MB\n")
      cat("File Type: ", input$file$type, "\n")
    })
  })

  observeEvent(input$quick_preprocess, {
    output$button_clicked_output <- renderText("Quick Preprocess button clicked!")
    # Add functionality for Quick Preprocess button
  })

  # Manage Missing Values Section
  observeEvent(input$manage_missed_values, {
    output$button_clicked_output <- renderText("Manage Missing Values button clicked!")
    output$manage_missing_values_ui <- renderUI({
      missing_values_ui(file_data())
    })
  })

  # Repeat the above structure for other buttons

  output$file_info_table <- renderTable({
    head(file_data(), n = 10)  # Display the first 10 rows as an example
  })

  # Dynamic UI for managing missing values
  missing_values_ui <- function(data) {
    column_names <- colnames(data)
    tagList(
      h3("Manage Missing Values"),
      lapply(column_names, function(column) {
        fluidRow(
          column(4, selectInput(paste0("action_", column), label = column, choices = if (sum(is.na(data[[column]])) > 0) c("Select" = "", "Remove", "Most Common") else c("Select" = ""))),
          column(8, uiOutput(paste0("replacement_value_", column)))
        )
      }),
      actionButton("apply_changes", "Apply Changes"),
      textOutput("manage_missing_values_output")
    )
  }

  # Dynamic UI for selecting replacement value based on action
  observe({
    data <- file_data()
    column_names <- colnames(data)
    for (column in column_names) {
      output[[paste0("replacement_value_", column)]] <- renderUI({
        action <- input[[paste0("action_", column)]]
        if (!is.null(action) && !is.na(action) && action == "Most Common" && !is.numeric(data[[column]])) {
          most_common <- names(sort(table(data[[column]], useNA = "always"), decreasing = TRUE))[1]
          selectInput(
            paste0("replacement_value_", column),
            label = "",
            choices = c("Select" = "", most_common),
            selected = most_common
          )
        } else {
          NULL
        }
      })
    }
  })

  observeEvent(input$apply_changes, {
    data <- file_data()
    column_names <- colnames(data)
    for (column in column_names) {
      action <- input[[paste0("action_", column)]]
      replacement_value <- input[[paste0("replacement_value_", column)]]

      # Check if 'action' is not NULL and not missing
      if (!is.null(action) && !is.na(action)) {
        if (action == "Remove") {
          data <- data %>% filter(!is.na(.data[[column]]))
        } else if (action == "Most Common" ) {
          data[[column]] <- ifelse(is.na(data[[column]]), names(sort(table(data[[column]], useNA = "always"), decreasing = TRUE))[1], data[[column]])
        }
      }
    }
    output$manage_missing_values_output <- renderText("Changes applied successfully!")
    output$file_info_table <- renderTable({
      head(data, n = 10)  # Display the first 10 rows as an example
    })
  })
}

shinyApp(ui, server)

