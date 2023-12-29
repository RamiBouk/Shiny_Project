# Installation des packages nécessaires
packages_to_install <- c("shiny","shinyjs", "ggplot2", "dplyr", "randomForest", "caret", "DT", "shinythemes", "ggcorrplot")

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
library(shinyjs)
library(shinythemes)
library(ggcorrplot)

# Définition de l'interface utilisateur
ui <- navbarPage(
  theme = shinytheme("cerulean"),
  shinyjs::useShinyjs(),  # Initialize shinyjs
  title = "Analyse de Données Avancée",
  
  # Catégorie Principale: Data Loading & Processing
  tabPanel("Data Loading & Processing",
           sidebarLayout(
             sidebarPanel(
               h3("Chargement des Données"),
               fileInput("file1", "Choisir un fichier CSV"),
               actionButton("load", "Charger les Données"),
               hr(),
               conditionalPanel(
               condition = "output.dataLoaded",
                tabPanel("Preprocessing",
               # Sub-tab panel for preprocessing options
               tabsetPanel(
                 tabPanel("Variables Options",
                          h3("Prétraitement"),
                          uiOutput("varOptionsUI")
                 ),
                 tabPanel("Variables Qualitatives",
                          h4("Traitement des Variables Qualitatives"),
                          checkboxInput("manageQualitative", "Gérer les Variables Qualitatives", value = FALSE)
                 ),
                 tabPanel("Gestion des Outliers",
                          h4("Gestion des Outliers"),
                          checkboxInput("manageOutliers", "Identifier et Gérer les Outliers", value = FALSE),
                          numericInput("outlierThreshold", "Seuil pour Outliers (IQR)", 1.5, min = 1, max = 5, step = 0.1)
                 ),
                 tabPanel("Valeurs Manquantes",
                          h4("Valeurs Manquantes"),
                          radioButtons("missingValues", "Gestion des Valeurs Manquantes",
                                       choices = c("Aucune", "Imputation", "Suppression"),
                                       selected = "Aucune"),
                 )
               )
             ),
                          actionButton("preprocess", "Appliquer le Prétraitement")


                                )),
             mainPanel(
               h3("Résumé des Données"),
               verbatimTextOutput("dataSummary")  # Ajout pour afficher le résumé des données
             )
           )
  ),
  
  # Catégorie Principale: Data Overview
  tabPanel("Data Overview",
           mainPanel(
             tabPanel("Aperçu des Données", DTOutput("dataTable"))
           )
  ),
  
  # Catégorie Principale: Analyse EDA
  tabPanel("Analyse EDA",
           sidebarLayout(
             sidebarPanel(
               h3("Paramètres d'Analyse"),
               selectizeInput("selectVar", "Variable Unidimensionnelle", choices = NULL),
               selectizeInput("selectVar2", "Variable Bidimensionnelle", choices = NULL)
             ),
             mainPanel(
               navlistPanel(
                 tabPanel("Analyse Unidimensionnelle", plotOutput("unidimPlot"), plotOutput("boxPlot")),
                 tabPanel("Analyse Bidimensionnelle", plotOutput("bidimPlot")),
                 tabPanel("Corrélation", plotOutput("corrPlot"))
               )
             )
           )
  ),
  
  # Catégorie Principale: Entraînement et évaluation des modèles ML
  tabPanel("Entraînement et évaluation des modèles ML"
           # Contenu pour l'entraînement et l'évaluation des modèles ML
  )
)

# Fonction serveur
server <- function(input, output, session) {
  
  # Valeurs réactives pour les données brutes et prétraitées
  rawData <- reactiveVal()
  processedData <- reactiveVal(NULL)
  observe({
    if (is.null(input$file1)) {
      shinyjs::disable("load")
    }else{
      shinyjs::enable("load")
    }
  })
  
  output$dataLoaded<-reactive({
    !is.null(rawData()) 
  })
  outputOptions(output,"dataLoaded",suspendWhenHidden=FALSE)
  # Chargement et mise à jour de l'UI pour les données
  observeEvent(input$load, {
    inFile <- input$file1
    if (is.null(inFile)) return()
    df <- read.csv(inFile$datapath, stringsAsFactors = FALSE)
    rawData(df)
    updateSelectInput(session, "selectVar", choices = names(df))
    updateSelectInput(session, "selectVar2", choices = names(df))
  })
  
  # Affichage du résumé des données chargées
  output$dataSummary <- renderPrint({
    # Utiliser la variable réactive contenant les données chargées
    if (!is.null(rawData())) {
      print("Raw Data")
      print(summary(rawData()))
      if(!is.null(processedData())){
        print("Processed Data")
        print(summary(processedData()))
    }}
    else {
      "Aucune donnée chargée."
    }
  })
  
  # Génération des options de prétraitement
  output$varOptionsUI <- renderUI({
    if (is.null(df)) return(NULL)
    df <- rawData()
    varTypes <- sapply(df, class)
    uiList <- lapply(names(df), function(var) {
      if(varTypes[var] == "factor") {
        checkboxInput(paste0("dummy_", var), paste("Dummifier", var), value = FALSE)
      } else if(varTypes[var] %in% c("integer", "numeric")) {
        checkboxInput(paste0("normalize_", var), paste("Normaliser", var), value = FALSE)
      }
    })
    do.call(tagList, uiList)
  })
  
  # Prétraitement des données
  observeEvent(input$preprocess, {
    df <- rawData()
    if (is.null(df)) return()
    
    # Gestion des outliers
    if (input$manageOutliers) {
      for (col in names(df)) {
        if (is.numeric(df[[col]])) {
          iqr_val <- IQR(df[[col]], na.rm = TRUE)
          upper <- quantile(df[[col]], 0.75, na.rm = TRUE) + input$outlierThreshold * iqr_val
          lower <- quantile(df[[col]], 0.25, na.rm = TRUE) - input$outlierThreshold * iqr_val
          df <- df[df[[col]] <= upper & df[[col]] >= lower,]
        }
      }
    }
    
    # Gestion des valeurs manquantes
    if (input$missingValues == "Imputation") {
      for (col in names(df)) {
        if (is.numeric(df[[col]])) {
          df[[col]][is.na(df[[col]])] <- mean(df[[col]], na.rm = TRUE)
        }
      }
    } else if (input$missingValues == "Suppression") {
      df <- na.omit(df)
    }
    
    # Normalisation des données
    varTypes <- sapply(df, class)
    for(var in names(df)) {
      if(varTypes[var] %in% c("integer", "numeric") && input[[paste0("normalize_", var)]]) {
        df[[var]] <- scale(df[[var]], center = TRUE, scale = TRUE)
      }
    }
    
    processedData(df)
  })
  
  # Affichage des données prétraitées ou brutes
  output$dataTable <- renderDT({
    df <- if (!is.null(processedData())) processedData() else rawData()
    req(df)
    datatable(df)
  })
  
  # Génération des graphiques unidimensionnels
  output$unidimPlot <- renderPlot({
    df <- if ( !is.null(processedData())) processedData() else rawData()
    req(df)
    ggplot(df, aes_string(x = input$selectVar)) + 
      geom_histogram(binwidth = 1) + theme_minimal() + ggtitle(paste("Histogramme :", input$selectVar)) + 
      theme(plot.title = element_text(face = "bold", color = "#2E8B57", size = 14, hjust = 0.5))
  })
  
  # Génération des graphiques bidimensionnels avec indicateur de corrélation
  output$bidimPlot <- renderPlot({
    df <- if ( !is.null(processedData())) processedData() else rawData()
    req(df)
    
    if (is.numeric(df[[input$selectVar]]) && is.numeric(df[[input$selectVar2]])) {
      cor_value <- cor(df[[input$selectVar]], df[[input$selectVar2]], use = "complete.obs")
      ggplot(df, aes_string(x = input$selectVar, y = input$selectVar2)) + 
        geom_point() + 
        theme_minimal() +
        ggtitle(paste("Scatter Plot :", input$selectVar, "&", input$selectVar2)) + 
        theme(plot.title = element_text(face = "bold", color = "#2E8B57", size = 14, hjust = 0.5)) +
        annotate("text", x = Inf, y = Inf, label = paste("Corrélation:", round(cor_value, 2)), 
                 hjust = 1.1, vjust = 1.1, color = "red", size = 5, fontface = "bold")
    } else {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Sélectionnez deux variables numériques", color = "red", size = 5, fontface = "bold")
    }
  })
  
  # Génération du Box Plot
  output$boxPlot <- renderPlot({
    df <- if ( !is.null(processedData())) processedData() else rawData()
    req(df)
    
    if (is.numeric(df[[input$selectVar]])) {
      ggplot(df, aes_string(y = input$selectVar)) + 
        geom_boxplot() + 
        theme_minimal() +
        ggtitle(paste("Box Plot :", input$selectVar)) + 
        theme(plot.title = element_text(face = "bold", color = "#2E8B57", size = 14, hjust = 0.5))
    } else {
      plot(NULL, xlim = c(0, 1), ylim = c(0, 1), type = "n", xlab = "", ylab = "")
      text(0.5, 0.5, "Sélectionnez une variable numérique", cex = 1.2, col = "red")
    }
  })
  
    # Génération du graphique de matrice de corrélation
  output$corrPlot <- renderPlot({
    df <- if (input$enablePreprocess && !is.null(processedData())) processedData() else rawData()
    req(df)

    # Sélectionner uniquement les colonnes numériques
    num_df <- df[sapply(df, is.numeric)]

    # Calcul de la matrice de corrélation
    corr_matrix <- cor(num_df, use = "complete.obs")

    # Utilisation de ggcorrplot pour une visualisation esthétique
    ggcorrplot(corr_matrix, method = "circle", hc.order = TRUE, type = "lower",
               lab = TRUE, lab_size = 3, outline.color = "white")
  })
}
# Lancement de l'application
shinyApp(ui = ui, server = server)
