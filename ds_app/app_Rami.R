# Installation des packages nécessaires
packages_to_install <- c("shiny", "ggplot2", "dplyr", "randomForest", "caret", "DT", "shinythemes")

# Installation des packages s'ils ne sont pas déjà installés
for (package in packages_to_install) {
  if (!(package %in% installed.packages()[,"Package"])) {
    install.packages(package)
  }
}

# Chargement des libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(randomForest)
library(caret)
library(DT)
library(shinythemes)

# Définition de l'interface utilisateur
ui <- fluidPage(
  theme = shinytheme("cerulean"),  # Thème moderne
  titlePanel("Analyse de Données Avancée", windowTitle = "Analyse de Données"),
  
  sidebarLayout(
  sidebarPanel(
      style = "max-width: 300px;",
      h3("Chargement des Données"),
      fileInput("file1", "Choisir un fichier CSV"),
      actionButton("load", "Charger les Données"),
      hr(),
      h3("Prétraitement"),
      checkboxInput("enablePreprocess", "Activer le Prétraitement", value = FALSE),
      
      # One-Hot Encoding Section
      h4("One-Hot Encoding"),
      uiOutput("onehotOptionsUI"),
      
      # Normalization Section
      h4("Normalisation"),
      uiOutput("normalizeOptionsUI"),
      
      actionButton("preprocess", "Appliquer le Prétraitement"),
      hr(),
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Aperçu des Données",
                 DTOutput("dataTable"),
                                                ),
        tabPanel("Analyse Unidimensionnelle", 
              column( 4,
                plotOutput("unidimPlot"),
                h3("Paramètres d'Analyse"),
                selectizeInput("selectVar1", "Variable Unidimensionnelle", choices = NULL, options = list(placeholder = 'Sélectionnez une variable')),
                selectizeInput("selectVar2", "Variable Bidimensionnelle", choices = NULL, options = list(placeholder = 'Sélectionnez une variable')),
                 )),

        tabPanel("Analyse Bidimensionnelle", 

                  column( 4,
                 plotOutput("bidimPlot"),
                h3("Paramètres d'Analyse"),
                selectizeInput("selectVar3", "Variable Unidimensionnelle", choices = NULL, options = list(placeholder = 'Sélectionnez une variable')),
                selectizeInput("selectVar4", "Variable Bidimensionnelle", choices = NULL, options = list(placeholder = 'Sélectionnez une variable')),
                 )),
        tabPanel("Box Plot", 
                 column(4,
                        plotOutput("boxPlot"),
                        h3("Paramètres d'Analyse"),
                        selectizeInput("selectVar", "Variable Unidimensionnelle", choices = NULL, options = list(placeholder = 'Sélectionnez une variable')),

                 ))  # Ajout du tabPanel pour le Box Plot

      )
    )
  )
)

# Fonction serveur
server <- function(input, output, session) {
  
  # Valeurs réactives pour les données brutes et prétraitées
  rawData <- reactiveVal()
  processedData <- reactiveVal(NULL)
  
  # Chargement et mise à jour de l'UI pour les données
  observeEvent(input$load, {
    inFile <- input$file1
    if (is.null(inFile)) return()
    df <- read.csv(inFile$datapath, stringsAsFactors = FALSE)
    
    # Convert all character columns to factor
    char_cols <- sapply(df, is.character)
    df[char_cols] <- lapply(df[char_cols], as.factor)
    
    rawData(df)
    updateSelectInput(session, "selectVar", choices = names(df))
    updateSelectInput(session, "selectVar2", choices = names(df))
  })
  
  # Génération des options de prétraitement
  output$varOptionsUI <- renderUI({
    if (!input$enablePreprocess) return(NULL)
    df <- rawData()
    if (is.null(df)) return(NULL)
    
    varTypes <- sapply(df, class)
    uiList <- lapply(names(df), function(var) {
      if (varTypes[var] == "factor") {
        checkboxInput(paste0("onehot_", var), paste("One-Hot Encode", var), value = FALSE)
      } else if (varTypes[var] %in% c("integer", "numeric")) {
        checkboxInput(paste0("normalize_", var), paste("Normaliser", var), value = FALSE)
      }
    })
    do.call(tagList, uiList)
  })
  
  # Prétraitement des données
  observeEvent(input$preprocess, {
    if (!input$enablePreprocess || is.null(rawData())) return()
    df <- rawData()
    
    # Logique de prétraitement
    varTypes <- sapply(df, class)
    for (var in names(df)) {
      if (varTypes[var] == "factor" && input[[paste0("onehot_", var)]]) {
        # One-Hot Encoding
        dummyVars <- model.matrix(~ . - 1, data = df[, var, drop = FALSE])
        df <- cbind(df[, !names(df) %in% var, drop = FALSE], dummyVars)
      } else if (varTypes[var] %in% c("integer", "numeric") && input[[paste0("normalize_", var)]]) {
        # Normalisation
        df[[var]] <- scale(df[[var]], center = TRUE, scale = TRUE)
      }
    }
    processedData(df)
  })
  
  # Affichage des données prétraitées ou brutes
  output$dataTable <- renderDT({
    df <- if (input$enablePreprocess && !is.null(processedData())) processedData() else rawData()
    req(df)
    datatable(df)
  })
  
  # Génération des graphiques unidimensionnels
  output$unidimPlot <- renderPlot({
    df <- if (input$enablePreprocess && !is.null(processedData())) processedData() else rawData()
    req(df)
    ggplot(df, aes_string(x = input$selectVar)) + 
      geom_histogram(binwidth = 1) + theme_minimal()
  })
  
  # Génération des graphiques bidimensionnels
  output$bidimPlot <- renderPlot({
    df <- if (input$enablePreprocess && !is.null(processedData())) processedData() else rawData()
    req(df)
    ggplot(df, aes_string(x = input$selectVar, y = input$selectVar2)) + 
      geom_point() + theme_minimal()
  })
  
  # Génération du Box Plot
  output$boxPlot <- renderPlot({
    df <- if (input$enablePreprocess && !is.null(processedData())) processedData() else rawData()
    req(df)
    
    # Utilisez la variable sélectionnée pour le Box Plot
    if (is.numeric(df[[input$selectVar]])) {
      ggplot(df, aes_string(y = input$selectVar)) + 
        geom_boxplot() + 
        theme_minimal()
    } else {
      # Si la variable n'est pas numérique, affichez un message d'erreur ou une visualisation alternative
      plot(NULL, xlim = c(0, 1), ylim = c(0, 1), type = "n", xlab = "", ylab = "")
      text(0.5, 0.5, "Sélectionnez une variable numérique", cex = 1.2, col = "red")
    }
  })
   output$onehotOptionsUI <- renderUI({
    if (!input$enablePreprocess) return(NULL)
    df <- rawData()
    if (is.null(df)) return(NULL)

    varTypes <- sapply(df, class)
    uiList <- lapply(names(df), function(var) {
      if (varTypes[var] %in% c("character", "factor")) {
        checkboxInput(paste0("onehot_", var), paste("One-Hot Encode", var), value = FALSE)
      }
    })
    do.call(tagList, uiList)
  })

  output$normalizeOptionsUI <- renderUI({
    if (!input$enablePreprocess) return(NULL)
    df <- rawData()
    if (is.null(df)) return(NULL)

    varTypes <- sapply(df, class)
    uiList <- lapply(names(df), function(var) {
      if (varTypes[var] %in% c("integer", "numeric")) {
        checkboxInput(paste0("normalize_", var), paste("Normaliser", var), value = FALSE)
      }
    })
    do.call(tagList, uiList)
  })
}

# Lancement de l'application
shinyApp(ui = ui, server = server)

