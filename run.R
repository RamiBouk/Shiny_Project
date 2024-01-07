library(shiny)
# Define a custom error handler function
customErrorHandler <- function(message, trace) {
  cat("Custom Error Handler:\n")
  cat(message, "\n")
  cat(trace, "\n")
}

# Define a custom log handler function
customLogHandler <- function(message, timestamp) {
  cat("Custom Log Handler:\n")
  cat(timestamp, " - ", message, "\n")
}

# Set options for custom handlers
options(shiny.app.sanitize.errors = FALSE)
shinyOptions(error = customErrorHandler, log = customLogHandler)

runApp(".")
