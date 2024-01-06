# Installation des packages nécessaires
packages_to_install <- c("shiny","shinyjs", "ggplot2","editData", "dplyr", "randomForest", "caret", "DT", "shinythemes", "ggcorrplot", "e1071", "caret", "randomForest", "rlang", "pROC", "Rtsne", "xgboost", "gbm")

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
library(editData)
library(randomForest)
library(caret)
library(DT)
library(shinyjs)
library(shinythemes)
library(ggcorrplot)
library(e1071)
library(caret)
library(randomForest)
library(pROC)
library(rlang)
library(Rtsne)
library(xgboost)
library(gbm)



# Définition de l'interface utilisateur
ui <- navbarPage(
  theme = shinytheme("cerulean"),
  shinyjs::useShinyjs(),
  title = "Analyse de Données Avancée",
  
  # Injection de CSS personnalisé
  withTags(
    header(
      script(HTML(
        "document.documentElement.style.setProperty('--dt-row-selected', '#a6b4cd');
         document.documentElement.style.setProperty('--dt-row-selected-text', '#333');"
      ))  
    )
  ),
  
  # Catégorie Principale: Chargement et vue d'ensemble des données
  tabPanel("Data Loading & Overview",
           sidebarLayout(
             sidebarPanel(
               h3("Chargement des Données"),
               fileInput("file1", "Choisir un fichier CSV"),
               actionButton("load", "Charger les Données")
             ),
             mainPanel(
               conditionalPanel(
                 condition = "output.dataLoaded",
                 tabsetPanel(
                   tabPanel("Data Table",
                            h6("Click to edit Table"),
                            DTOutput("dataTable")
                   ),
                   tabPanel("Data Summary",
                            verbatimTextOutput("dataSummary")
                   )
                 )
               )
             )
           )
  ),
  
  # Catégorie Principale: Prétraitement des données
  tabPanel("Data Preprocessing",
     sidebarLayout(
       sidebarPanel(
         selectizeInput("selectVar", "Variable Unidimensionnelle", choices = NULL),
         selectizeInput("selectVar2", "Variable Bidimensionnelle", choices = NULL),
         tabsetPanel(
           tabPanel("Unidimensionnelle", plotOutput("unidimPlot"), plotOutput("boxPlot")),
           tabPanel("Bidimensionnelle", plotOutput("bidimPlot"))
         )
       ),
       mainPanel(
         conditionalPanel(
           condition = "output.dataLoaded",
           tabsetPanel(
             tabPanel("Normalization",
                      h3("Select Columns"),
                      uiOutput("varOptionsUI")
             ),
             tabPanel("Dummification",
                      h4("Traitement des Variables Qualitatives"),
                      checkboxInput("manageQualitative", "Gérer les Variables Qualitatives", value = FALSE)
             ),
             tabPanel("Outliers",
                      h4("Gestion des Outliers"),
                      checkboxInput("manageOutliers", "Identifier et Gérer les Outliers", value = FALSE),
                      numericInput("outlierThreshold", "Seuil pour Outliers (IQR)", 1.5, min = 1, max = 5, step = 0.1)
             ),
             tabPanel("Missing Values",
                      h4("Valeurs Manquantes"),
                      radioButtons("missingValues", "Gestion des Valeurs Manquantes", choices = c("Aucune", "Imputation", "Suppression"), selected = "Aucune")
             )
           ),
           actionButton("preprocess", "Appliquer")
         )
       )
     )
  ),
  
  # Catégorie Principale: Gestion de déséquilibre
  tabPanel("Gestion de déséquilibre",
     sidebarLayout(
       sidebarPanel(
         uiOutput("imbalanceTargetSelectorUI"),  # Dropdown for selecting the target variable
         actionButton("showClassDistribution", "Afficher la Distribution des Classes"),
         br(),
         actionButton("applySMOTE", "Appliquer SMOTE pour rééquilibrer")
       ),
       mainPanel(
         plotOutput("classDistributionPlot"),  # Plot to show class distribution
         DTOutput("dataPreview")  # Data preview after applying SMOTE
       )
     )
  ),
  
  
  
  # Catégorie Principale: Entraînement et évaluation des modèles ML
  tabPanel("Entraînement et évaluation des modèles ML",
     sidebarLayout(
       sidebarPanel(
         uiOutput("mlFeatureSelectorUI"),
         #checkboxGroupInput("selectedFeatures", "Choisir les Features", choices = NULL),
         uiOutput("targetSelectorUI"),
         selectInput("modelType", "Choisir un modèle",
                     choices = c("Logistic Regression", "Forêts Aléatoires", "k-Nearest Neighbors", "Decision Trees", "Gradient Boosting Machines", "XGBoost")),
         conditionalPanel(
           condition = "input.modelType == 'k-Nearest Neighbors'",
           numericInput("numNeighbors", "Nombre de voisins (k):", value = 5, min = 1)
         ),
         checkboxInput("crossValidation", "Appliquer la validation croisée", value = FALSE),
         conditionalPanel(
           condition = "input.crossValidation == true",
           numericInput("numFolds", "Nombre de plis pour la validation croisée", value = 10, min = 2)
         ),
         actionButton("trainModel", "Entraîner le Modèle")
       ),
       mainPanel(
         tabsetPanel(
           tabPanel("Résultats d'Évaluation",
                    verbatimTextOutput("modelMetrics"),
                    plotOutput("rocPlot")),  # Ajout d'un plot pour la courbe ROC
           tabPanel("Importance des Features",
                    plotOutput("featureImportancePlot"))
         )
       )
     )
  ),
  
  
  tabPanel("Réduction de la dimensionalité",
     sidebarLayout(
       sidebarPanel(
         h4("Réduction de la Dimension"),
         selectInput("dimReductionMethod", "Méthode:",
                     choices = c("PCA", "t-SNE")),
         numericInput("numComponents", "Nombre de composants:", value = 2, min = 1),
         checkboxGroupInput("selectedFeatures", "Choisir les Features", choices = NULL),
         #uiOutput("dimRedFeatureSelectorUI"),
         actionButton("runDimReduction", "Exécuter la Réduction de Dimension")
       ),
       mainPanel(
         tabsetPanel(
           tabPanel("Plot", plotOutput("dimRedPlot")),
           tabPanel("Summary", verbatimTextOutput("dimRedSummary"))
         )
       )
     )
  )
  
  
)


# Fonction serveur
server <- function(input, output, session) {
  
  # Initialisation des valeurs réactives
  rawData <- reactiveVal()
  processedData <- reactiveVal(NULL)
  
  # Activation/désactivation du bouton de chargement
  observe({
    if (is.null(input$file1)) {
      shinyjs::disable("load")
    } else {
      shinyjs::enable("load")
    }
  })
  
  # Réactivité pour la charge des données
  output$dataLoaded <- reactive({
    !is.null(rawData()) 
  })
  outputOptions(output, "dataLoaded", suspendWhenHidden = FALSE)
  
  # Chargement des données
  observeEvent(input$load, {
    inFile <- input$file1
    if (is.null(inFile)) return()
    df <- read.csv(inFile$datapath, stringsAsFactors = FALSE)
    rawData(df)
    updateSelectInput(session, "selectVar", choices = names(df))
    updateSelectInput(session, "selectVar2", choices = names(df))
  })
  
  # Résumé des données
  output$dataSummary <- renderPrint({
    if (!is.null(rawData())) {
      print("Raw Data")
      print(summary(rawData()))
      if (!is.null(processedData())) {
        print("Processed Data")
        print(summary(processedData()))
      }
    } else {
      "Aucune donnée chargée."
    }
  })
  
  # Options de prétraitement
  output$varOptionsUI <- renderUI({
    df <- rawData()
    if (is.null(df)) return(NULL)
    varTypes <- sapply(df, class)
    uiList <- lapply(names(df), function(var) {
      if (varTypes[var] == "factor") {
        checkboxInput(paste0("dummy_", var), paste("Dummifier", var), value = FALSE)
      } else if (varTypes[var] %in% c("integer", "numeric")) {
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
    if (!is.null(processedData())) 
    {
      df<- processedData()}
    else if (!is.null(rawData())){
      df<-rawData()
    }
    else 
      return() 
    req(df)

    datatable(df,editable="cell",
              options = list(scrollY = 650,
                               scrollX = 500,
                               deferRender = TRUE,
                               scroller = TRUE,
                               buttons = list('excel',
                                              list(extend = 'colvis', targets = 0, visible = FALSE)),
                               fixedColumns = TRUE),selection='single')
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
  
  
  # MACHINE LEARNING :
  
  # Génération des sélecteurs de features et target
  output$mlFeatureSelectorUI <- renderUI({
    df <- if (!is.null(processedData())) processedData() else rawData()
    req(df)
    checkboxGroupInput("mlSelectedFeatures", "Choisir les Features", choices = names(df))
  })
  
  output$targetSelectorUI <- renderUI({
    df <- if (!is.null(processedData())) processedData() else rawData()
    req(df)
    selectInput("selectedTarget", "Choisir la Target", choices = names(df))
  })
  
  # Entraînement et évaluation des modèles
  observeEvent(input$trainModel, {
    df <- if (!is.null(processedData())) processedData() else rawData()
    print("Hello")
    req(df, input$mlSelectedFeatures, input$selectedTarget)
    
    # Séparation des features et de la target
    targetName <- input$selectedTarget
    target <- as.factor(df[[targetName]]) 
    levels(target) <- make.names(levels(target))
    features <- df[, input$mlSelectedFeatures, drop = FALSE]
    
    # Division des données en ensembles d'entraînement et de test
    set.seed(123) # Pour la reproductibilité
    partitions <- createDataPartition(target, p = .8, list = TRUE)
    trainIndex <- partitions[[1]]  # Extrait correctement les indices
    trainData <- features[trainIndex, ]
    trainTarget <- target[trainIndex]
    testData <- features[-trainIndex, ]
    testTarget <- target[-trainIndex]
    
    # Configuration du contrôle d'entraînement avec ou sans validation croisée
    control <- trainControl(method = if (input$crossValidation) "cv" else "none",
                            number = if (input$crossValidation) input$numFolds else 1,
                            summaryFunction = twoClassSummary,
                            classProbs = TRUE,
                            savePredictions = TRUE)
    
    # Entraînement du modèle
    model <- NULL
    if (input$modelType == "Logistic Regression") {
      model <- train(x = trainData, y = trainTarget, method = "glm", 
                     family = "binomial",
                     trControl = control)
      
    } else if (input$modelType == "Forêts Aléatoires") {
      model <- train(x = trainData, y = trainTarget, method = "rf", 
                     trControl = control, 
                     metric = "Accuracy",
                     importance = TRUE)
    } else if (input$modelType == "k-Nearest Neighbors") {
      tuneGrid <- expand.grid(k = input$numNeighbors)
      model <- train(x = trainData, y = trainTarget, method = "knn", 
                     trControl = control,
                     tuneGrid = tuneGrid)
    } else if (input$modelType == "Decision Trees") {
      model <- train(x = trainData, y = trainTarget, method = "rpart", 
                     trControl = control)
    } else if (input$modelType == "Gradient Boosting Machines") {
      model <- train(x = trainData, y = trainTarget, method = "gbm", 
                     trControl = control,
                     verbose = FALSE)
    }else if (input$modelType == "XGBoost") {
      tuneGridXGB <- expand.grid(
        nrounds = 100, 
        max_depth = 3, 
        eta = 0.3, 
        gamma = 0,
        colsample_bytree = 1,
        min_child_weight = 1,
        subsample = 1
      )
      
      model <- train(x = trainData, y = trainTarget, method = "xgbTree", 
                     trControl = control,
                     tuneGrid = tuneGridXGB,
                     verbose = FALSE) 
    }
    
    # Prédiction et évaluation
    if (!is.null(model)) {
      predictions <- predict(model, newdata = testData)
      confusionMatrix <- confusionMatrix(predictions, testTarget)
      
      output$modelMetrics <- renderPrint({
        # Affichage des métriques de performance
        metrics <- confusionMatrix$overall
        cat("Accuracy:", metrics["Accuracy"], "\n")
        
        # Calcul et affichage de précision, rappel et F-score
        precision <- posPredValue(predictions, testTarget)
        sensitivity <- sensitivity(predictions, testTarget)
        specificity <- specificity(predictions, testTarget)
        fscore <- (2 * precision * sensitivity) / (precision + sensitivity)
        
        cat("Precision:", precision, "\n",
            "Recall (Sensitivity):", sensitivity, "\n",
            "Specificity:", specificity, "\n",
            "F-score:", fscore, "\n")
      })
      
      # Courbe ROC et AUC
      output$rocPlot <- renderPlot({
        probPred <- predict(model, newdata = testData, type = "prob")
        rocCurve <- roc(response = testTarget, predictor = probPred[,2])
        if (input$modelType != "Decision Trees"){
          # Ajouter une interpolation
          plot(smooth(rocCurve), main = "ROC Curve")
          # Afficher l'AUC dans le titre ou comme une légende
          aucValue <- auc(rocCurve)
          legend("bottomright", legend = paste("AUC:", format(aucValue, digits = 4)))
        }
      })
      
      # Feature Importance Plot
      output$featureImportancePlot <- renderPlot({
        if (input$modelType == "Forêts Aléatoires") {
          varImpPlot <- varImp(model, scale = FALSE)
          plot(varImpPlot)
        } else if (input$modelType == "Logistic Regression" && !is.null(model)) {
          # Extract coefficients and convert them to absolute values
          coef_data <- as.data.frame(abs(coef(model$finalModel)[-1])) # Excluding intercept
          names(coef_data) <- c("Importance")
          coef_data$Feature <- rownames(coef_data)
          ggplot(coef_data, aes(x = reorder(Feature, Importance), y = Importance)) +
            geom_bar(stat = "identity") +
            coord_flip() +
            xlab("Feature") +
            ylab("Absolute Coefficient") +
            ggtitle("Feature Importance for Logistic Regression (Absolute Coefficients)")
        } else if (input$modelType == "k-Nearest Neighbors") {
          plot.new()
          text(0.5, 0.5, "Feature importance is not applicable for k-Nearest Neighbors",
               cex = 1.5)
        }else if (input$modelType == "Gradient Boosting Machines" && !is.null(model)) {
          varImpPlot <- varImp(model, scale = FALSE)
          plot(varImpPlot)
        } else if (input$modelType == "XGBoost" && !is.null(model)) {
          xgbImp <- xgb.importance(feature_names = model$xNames, model = model$finalModel)
          xgb.plot.importance(importance_matrix = xgbImp)
        } else {
          plot.new()
          text(0.5, 0.5, "Feature importance not available for the selected model",
               cex = 1.5)
        }
      })
      
    }
    
    
  })
  
  
  # Dimensionality Reduction :
  
  # Dynamically update the choices for 'selectedFeatures'
  observe({
    df <- if (!is.null(processedData())) processedData() else rawData()
    if (!is.null(df)) {
      updateCheckboxGroupInput(session, "selectedFeatures", choices = names(df))
    }
  })
  
  observeEvent(input$runDimReduction, {
    df <- if (!is.null(processedData())) processedData() else rawData()
    req(df)
    
    # Validate selected features
    if (!is.null(input$selectedFeatures) && length(input$selectedFeatures) > 0) {
      selectedData <- df[, input$selectedFeatures, drop = FALSE]
    } else {
      output$dimRedPlot <- renderText("No features selected for dimensionality reduction.")
      output$dimRedSummary <- renderText("No summary available.")
      return()
    }
    
    # Dimensionality Reduction
    result <- NULL
    if (input$dimReductionMethod == "PCA") {
      #pcaResult <- prcomp(selectedData, scale. = TRUE, rank. = input$numComponents)
      pcaResult <- prcomp(selectedData, scale. = TRUE)
      result <- as.data.frame(pcaResult$x)
    } else if (input$dimReductionMethod == "t-SNE") {
      set.seed(123)  # For reproducibility
      tsneResult <- Rtsne(selectedData, dims = input$numComponents, check_duplicates = FALSE)$Y
      result <- as.data.frame(tsneResult)
    }
    
    # Output: Plot
    output$dimRedPlot <- renderPlot({
      # Check if the result has data and use the actual column names from the result
      if (!is.null(result) && ncol(result) >= 2) {
        # Using actual column names
        colnames <- names(result)
        ggplot(result, aes(x = .data[[colnames[1]]], y = .data[[colnames[2]]])) +
          geom_point() +
          theme_minimal() +
          labs(title = "Visualization of Dimensionality Reduction")
      } else {
        ggplot() + 
          geom_blank() +
          labs(title = "No data available for visualization",
               subtitle = "Please check if the selected features are numeric and try again.")
      }
    })
    
    
    # Output: Summary
    output$dimRedSummary <- renderPrint({
      if (input$dimReductionMethod == "PCA" && !is.null(pcaResult)) {
        sv <- pcaResult$sdev  # Singular values
        variance <- sv^2 / sum(sv^2)  # Proportion of variance
        cumvar <- cumsum(variance)  # Cumulative variance
        
        dfSummary <- data.frame(
          #SingularValue = sv[1:input$numComponents],
          SingularValue = sv,
          #Variance = variance[1:input$numComponents],
          Variance = variance,
          #CumulativeVariance = cumvar[1:input$numComponents]
          CumulativeVariance = cumvar
        )
        print(dfSummary)
      } else if (input$dimReductionMethod == "t-SNE" && !is.null(tsneResult)) {
        cat("t-SNE does not provide a summary of variance explained as PCA does.\n")
        cat("t-SNE is mainly used for visualization to see the clustering of high-dimensional data.")
      } else {
        cat("No result available.")
      }
    })
    
  })
  
  
  
  # SMOTE :
  
  # UI for selecting the target variable for imbalance management
  output$imbalanceTargetSelectorUI <- renderUI({
    df <- if (!is.null(processedData())) processedData() else rawData()
    req(df)
    selectInput("imbalanceTarget", "Choisir la Target pour Gestion de Déséquilibre", 
                choices = names(df))
  })
    
  # Show class distribution for the selected target variable
  observeEvent(input$showClassDistribution, {
    df <- if (!is.null(processedData())) processedData() else rawData()
    req(df)
    targetVar <- input$imbalanceTarget
    output$classDistributionPlot <- renderPlot({
      class_counts <- table(df[[targetVar]])
      barplot(class_counts, 
              main = "Distribution des Classes", 
              xlab = "Classes", 
              ylab = "Fréquence",
              col = "skyblue",  # You can choose different colors
              names.arg = names(class_counts))
    })
  })
    
    
  # Apply SMOTE and show the updated data using the 'grt' package
  observeEvent(input$applySMOTE, {

    print('Cant find the right library for SMOTE')
    
  })
  
    
    
  }
  # Lancement de l'application
  shinyApp(ui = ui, server = server)
