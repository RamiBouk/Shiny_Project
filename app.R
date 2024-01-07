## TODO: 
## - fix the the columns disabpearing when  nromalizaotin
## - fix normalize constant columns

# Installation des packages nécessaires

packages_to_install <- c("shiny","shinyjs", "ggplot2","editData", "dplyr", "randomForest", "caret", "DT", "shinythemes", "ggcorrplot", "e1071", "caret", "randomForest", "rlang", "pROC", "Rtsne", "xgboost", "gbm","virdis","ROSE","caret")

# Installation des packages s'ils ne sont pas déjà installés
for (package in packages_to_install) {
  if (!(package %in% installed.packages()[,"Package"])) {
    install.packages(package)
  }
}
NO_DATA="No Data Loaded"

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
library(gridExtra)
library(viridis)
library(xgboost)
library(gbm)
library(ROSE)
library(caret)



# Définition de l'interface utilisateur
NoDataMessage <- conditionalPanel(
                 condition = "! output.dataLoaded",
                 h2(NO_DATA)
               )
ui <- shinyUI(

  navbarPage( id="navbarPage",
  theme = shinytheme("cerulean"),
  shinyjs::useShinyjs(),

  title = "Analyse de Données Avancée",
  
  # Injection de CSS personnalisé
  
  # Catégorie Principale: Chargement et vue d'ensemble des données
  withTags(
    header(
      script(HTML(
        "document.documentElement.style.setProperty('--dt-row-selected', '#a6b4cd');
         document.documentElement.style.setProperty('--dt-row-selected-text', '#333');"
      ))  
    )
  ),
  tabPanel("Data Loading & Overview",
           sidebarLayout(
             sidebarPanel(
               h3("Chargement des Données"),
               fileInput("dataFile", "Choose .data file"),
               fileInput("namesFile", "Choose .names file"),

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
                              DTOutput("summaryTable"),
                   )
                 )
               ),NoDataMessage
             )
           )
  ),
  
  # Catégorie Principale: Prétraitement des données

  tabPanel("Data Preprocessing",
    conditionalPanel(
    condition = "output.dataLoaded",
     sidebarLayout(
       sidebarPanel(
           tabsetPanel(
             tabPanel("Types",
                      h4("Convert to categorical"),
                       uiOutput("typeOptionsUI")

             ),
             tabPanel("Missing Values",
                      h4("Valeurs Manquantes"),
                      radioButtons("missingValues", 
                                   "Gestion des Valeurs Manquantes", 
                                   choices = c("Suppression", 
                                               "Replace with Mode", 
                                               "Replace with Mean",
                                               "Replace with Median"
                                               ), 
                                   selected = "Suppression"),
                       uiOutput("missingOptionsUI")

             ),
             tabPanel("Normalization",
                      h4("Select Columns"),
                      uiOutput("varOptionsUI")
             ),
             tabPanel("Dummification",
                      h4("Turn to one hot encoding:"), 
                      uiOutput("catOptionsUI")), 
             tabPanel("Outliers", 
                      h4("Remove Outliers"), 
                      uiOutput("outlierOptionsUI"),
                      numericInput("outlierThreshold", "Seuil pour Outliers (IQR)", 1.5, min = 1, max = 5, step = 0.1)),
             tabPanel("Doubles", 
                      h4("Handle doubles"), 
                      h5("Method"), 
                      radioButtons("duplicatsMethod", 
                                   "Method", 
                                   choices = c("Automatic" 
                                               #"Manual"
                                               )),
                      h5("Columns"), 
                      uiOutput("doubleOptionsUI"),
           ),
           ),
           actionButton("preprocess", "Appliquer")
       ),
       mainPanel(
         tabsetPanel(
           tabPanel("Data Table", 
                              DTOutput("dataTablePre")
                    ),
           tabPanel("Unidimensionnelle Visualization", 
                    selectizeInput("selectVar", "Variable", choices = NULL),
                    uiOutput("unidimPlot"), 
                    ),
           tabPanel("Bidimensionnelle Visualization", 
         selectizeInput("selectVar1", "Variable X", choices = NULL),
         selectizeInput("selectVar2", "Variable Y", choices = NULL),
                    plotOutput("bidimPlot"))
         ),
         )
       )
     ),NoDataMessage
  ),
  
  # Catégorie Principale: Gestion de déséquilibre
  tabPanel("Imbalance Management",
     sidebarLayout(
       sidebarPanel(
         uiOutput("imbalanceTargetSelectorUI"),  # Dropdown for selecting the target variable
         br(),
         actionButton("applySMOTE", "Apply Over Sampling(SMOTE)"),
         actionButton("applyUnder", "Apply Under Sampling")
       ),
       mainPanel(
         plotOutput("classDistributionPlot"),  # Plot to show class distribution
         DTOutput("dataPreview")  # Data preview after applying SMOTE
       )
     )
  ),
  
  
  
  # Catégorie Principale: Entraînement et évaluation des modèles ML
  tabPanel("Machine Learning",id ="tab3",
   conditionalPanel(
    condition = "output.dataLoaded",
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
         numericInput("numFolds", "Nombre de plis pour la validation croisée", value = 10, min = 2),
         
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
     )),NoDataMessage
  ),
  
  # PCA 
  tabPanel("Dimensionality Reduction",
     sidebarLayout(
       sidebarPanel(
         h4("Réduction de la Dimension"),
         selectInput("dimReductionMethod", "Méthode:",
                     choices = c("PCA", "t-SNE")),
         numericInput("numComponents", "Nombre de composants:", value = 2, min = 1),
         checkboxGroupInput("selectedFeatures", "Choisir les Features", choices = NULL),
         #uiOutput("dimRedFeatureSelectorUI"),
         actionButton("runDimReduction", "Exécuter la Réduction de Diension")
       ),
       mainPanel(
         tabsetPanel(
           tabPanel("Plot", plotOutput("dimRedPlot")),
           tabPanel("Summary", verbatimTextOutput("dimRedSummary"))
         )
       )
     )
  )
  
  
))


# Fonction serveur
server <- function(input, output, session) {
  # Initialisation des valeurs réactives
  rawData <- reactiveVal()
  processedData <- reactiveVal(NULL)

  # ReactiveValues to track changes
  editedData <- reactiveValues(data = NULL)

  # Activation/désactivation du bouton de chargement
  observe({
    if (is.null(input$dataFile)) {
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
                 # Read .data file
                 df <- read.csv(input$dataFile$datapath, header = FALSE,na.strings = c("", "NA", "?"))

                 # Read .names file
                 names_file <- input$namesFile$datapath
                 if(!is.null(names_file)){
                 attribute_info <- read_attributes_names(names_file)

                 # Set column names
                 colnames(df) <- attribute_info
                 }

                 char_columns <- names(df)[sapply(df, function(x) is.character(x))]
                 for(char_column in char_columns)
                   df[, char_column] <- ifelse(df[, char_column] == "", NA, df[, char_column])
                 rawData(df)
                 processedData(df)
                 # Assuming 'df' is your data frame
                 numeric_columns <- names(df)[sapply(df, function(x) is.numeric(x) | is.integer(x))]
                 category_columns <- names(df)[sapply(df, function(x) !is.numeric(x) && length(unique(x))<30)]
                 column=c(numeric_columns,category_columns)
                 updateSelectInput(session, "selectVar", choices =column) 
                 updateSelectInput(session, "selectVar1", choices = column)
                 updateSelectInput(session, "selectVar2", choices = column)
  })

  # Résumé des données
  output$dataSummary <- renderPrint({
    if (!is.null(rawData())) {
      print("Data Summary")
      print(summary(processedData()))}
    else if (!is.null(processedData())) {
        print("Data Summary")
        print(summary(processedData()))
      }
     else {
      "Aucune donnée chargée."
    }
  })

  observeEvent(input$dataTable_cell_edit, {
                 info <- input$dataTable_cell_edit
                 str(info)

                 if (is.null(rawData())) {
                       print(h6("no Data Loaded"))
                 }
                 # Ensure the editedData$data has the same dimensions as the original data
                 if (is.null(editedData$data)) {
                   editedData$data <- rawData()
                 }

                 # Update the reactiveValues with the edited data
                 editedData$data[info$row, info$col] <- info$value

                 # Apply changes to df
                 processedData(editedData$data)

  })
  observeEvent(input$dataTablePre_cell_edit, {
                 info <- input$dataTablePre_cell_edit
                 str(info)

                 if (is.null(rawData())) {
                       print(h6("no Data Loaded"))
                 }
                 # Ensure the editedData$data has the same dimensions as the original data
                 if (is.null(editedData$data)) {
                   editedData$data <- rawData()
                 }

                 # Update the reactiveValues with the edited data
                 editedData$data[info$row, info$col] <- info$value

                 # Apply changes to df
                 processedData(editedData$data)

  })
  output$missingOptionsUI <- renderUI({
    if (!is.null(processedData())) df<-processedData() else df<-rawData() 
    varTypes <- sapply(df, class)
    uiList <- lapply(names(df), function(var) {
                       if (any(is.na(df[[var]]))) {
                        checkboxInput(paste0("missing_", var),  paste(var,"(",sum(is.na(df[[var]])),")"), value = FALSE)
                       }
  })
    do.call(tagList, uiList)
  })
  output$typeOptionsUI <- renderUI({
    if (!is.null(processedData())) df<-processedData() else df<-rawData() 
    varTypes <- sapply(df, class)
    category_columns <- names(df)[sapply(df, function(x) is.numeric(x) && length(unique(x))<20)]
    uiList <- lapply(names(df), function(var) {
                       if (var %in% category_columns) {
                        checkboxInput(paste0("cat_",var) ,  paste(var,"(",length(unique(df[[var]])),"unique values)"), value = FALSE)
                       }
  })
    do.call(tagList, uiList)
  })


  # Options de prétraitement
  output$varOptionsUI <- renderUI({
    if (!is.null(processedData())) df<-processedData() else df<-rawData() 
    varTypes <- sapply(df, class)
    uiList <- lapply(names(df), function(var) {
                       if (varTypes[var] %in% c("integer", "numeric") && 
                           !all(unique(df[[var]]) %in% c(0, 1))) {
                        checkboxInput(paste0("normalize_", var), paste( var,
                                                                       "(mean=",round(mean(df[, var], na.rm = TRUE),digits=2),
                                                                       ",sd=",round(sd(df[, var], na.rm = TRUE),digits=2),")"), value = FALSE)
                       }
  })
    do.call(tagList, uiList)
  })

  output$catOptionsUI <- renderUI({
    df <- processedData()
    if (is.null(df)) return(NULL)
    varTypes <- sapply(df, class)
    uiList <- lapply(names(df), function(var) {
                       if (length(unique(df[[var]])) < 24 && !all(unique(df[[var]]) %in% c(0, 1)) ) { ### JUMP
                         checkboxInput(paste0("dummy_", var),paste(var,"(",length(unique(df[[var]])),"unique values)"), value = FALSE)
  }})
    do.call(tagList, uiList)
  })
  output$outlierOptionsUI <- renderUI({
    df <- processedData()
    if (is.null(df)) return(NULL)
    varTypes <- sapply(df, class)
    uiList <- lapply(names(df), function(var) {
                       if (varTypes[var] %in% c("integer", "numeric")&& !all(unique(df[[var]]) %in% c(0, 1)) ) { ### JUMP
                         checkboxInput(paste0("remove_", var), var, value = FALSE)
  }})
    do.call(tagList, uiList)
  })
  output$doubleOptionsUI <- renderUI({
    df <- processedData()
    if (is.null(df)) return(NULL)
    varTypes <- sapply(df, class)
    uiList <- lapply(names(df), function(var) {
                       if (any(duplicated(df[[var]]))) { ### JUMP
                         checkboxInput(paste0("doubleSelect_", var),paste(var,"(",sum(duplicated(df[[var]])),")"), value = FALSE)
  }})
    do.call(tagList, uiList)
  })

  # Prétraitement des données
  observeEvent(input$preprocess, {
                 if(!is.null(processedData())){
                    df <- processedData()
                  } else{
                    df <- rawData()
                 }
                 if (is.null(df)) return()
                 selected_var <- input$selectVar
                 selected_var1 <- input$selectVar1
                 selected_var2 <- input$selectVar2

                 # Gestion des outliers
                 varTypes <- sapply(df, class)
                 for(var in names(df)) {
                   if(!is.null(input[[paste0("remove_", var)]]) &&
                      varTypes[var] %in% c("integer", "numeric") && 
                      input[[paste0("remove_", var)]]) {
                       iqr_val <- IQR(df[[var]], na.rm = TRUE)
                       upper <- quantile(df[[var]], 0.75, na.rm = TRUE) + input$outlierThreshold * iqr_val
                       lower <- quantile(df[[var]], 0.25, na.rm = TRUE) - input$outlierThreshold * iqr_val
                       df <- df[df[[var]] <= upper & df[[var]] >= lower,]
                   }
                 }
                 if (input$duplicatsMethod == "Automatic") {
                  for(var in names(df)) {
                   if(!is.null(input[[paste0("doubleSelect_", var)]]) &&
                      input[[paste0("doubleSelect_", var)]]) {
                     df <- df %>% distinct(!!sym(var), .keep_all = TRUE)

                   }
                 }
                 }

                 # Gestion des valeurs manquantes
                 if (input$missingValues == "Suppression") {
                  for(var in names(df)) {
                   if(!is.null(input[[paste0("missing_", var)]]) &&
                      input[[paste0("missing_", var)]]) {
                     df <- df[complete.cases(df[, var]), ]
                   }
                 }

                 } else if (input$missingValues == "Replace with Mean") {
                 for(var in names(df)) {
                   if(!is.null(input[[paste0("missing_", var)]]) &&
                      varTypes[var] %in% c("integer", "numeric") && 
                      input[[paste0("missing_", var)]]) {
                        # Calculate the mean of the column
                        mean_value <- mean(df[, var], na.rm = TRUE)

                        # Fill missing values in the specified column with the mean
                        df[, var][is.na(df[, var])] <- mean_value
                   }
                 }

                 } else if (input$missingValues == "Replace with Median") {
                 for(var in names(df)) {
                   if(!is.null(input[[paste0("missing_", var)]]) &&
                      varTypes[var] %in% c("integer", "numeric") && 
                      input[[paste0("missing_", var)]]) {
                     # Calculate the median of the column
                     median_value <- median(df[, var], na.rm = TRUE)

                     # Fill missing values in the specified column with the median
                     df[, var][is.na(df[, var])] <- median_value
                   }
                 }

                 } else if (input$missingValues == "Replace with Mode") {
                 for(var in names(df)) {
                   if(!is.null(input[[paste0("missing_", var)]]) &&
                      input[[paste0("missing_", var)]]) {
                     # Calculate the mode of the column
                     mode_value <- as.numeric(names(which.max(table(df[, var]))))

                     # Fill missing values in the specified column with the mode
                     df[, var][is.na(df[, var])] <- mode_value

                   }
                 }

                 }



                 # Normalisation des données
                 varTypes <- sapply(df, class)
                 for(var in names(df)) {
                   if(!is.null(input[[paste0("normalize_", var)]]) &&
                      varTypes[var] %in% c("integer", "numeric") && 
                      input[[paste0("normalize_", var)]]) {

                     df[[var]] <- as.double(scale(df[[var]]))
                   }
                 }
                 # Dummification des données
                for(var in names(df)) {
                if(!is.null(input[[paste0("dummy_", var)]]) && 
                   input[[paste0("dummy_", var)]]) {
                     # Check for missing values and replace them with a placeholder
                     if(is.factor(df[[var]])){
                       df[[var]]=as.character(df[[var]])

                     }

                      missing_values <- is.na(df[[var]])
                      df[[var]][missing_values] <- "missing"
                   

                     # Create dummy variables
                     dummies <- as.data.frame(model.matrix(~as.factor(df[[var]]) - 1))
                     unique_values <- levels(factor(df[[var]]))
                     colnames(dummies) <- paste0(var, "_", unique_values)

                     # Ensure that the number of rows in df and dummies match
                     n_rows <- max(nrow(df), nrow(dummies))
                     df <- df[1:n_rows, ]
                     dummies <- dummies[1:n_rows, ]

                     df <- cbind(df, dummies)
                     df[[var]] <- NULL  # Remove the original column
                     }
                     
                   
                 }

                for(var in names(df)) {
                if(!is.null(input[[paste0("cat_", var)]]) && 
                   input[[paste0("cat_", var)]]) {
                    df[[var]]=as.factor(df[[var]])
                   }
                }
                 
                 numeric_columns <- names(df)[sapply(df, function(x) is.numeric(x) | is.integer(x))]
                 category_columns <- names(df)[sapply(df, function(x) !is.numeric(x) && length(unique(x))<20)]
                 column=c(numeric_columns,category_columns)
                 if(selected_var %in% names(df))  
                 updateSelectInput(session, "selectVar", choices = column,selected=selected_var)
                else
                 updateSelectInput(session, "selectVar", choices = column)
                 if(selected_var1 %in% names(df))  
                 updateSelectInput(session, "selectVar1", choices = column,selected=selected_var1)
               else
                 updateSelectInput(session, "selectVar1", choices = column)
                 if(selected_var2 %in% names(df))  
                 updateSelectInput(session, "selectVar2", choices =column,selected=selected_var2)
               else
                 updateSelectInput(session, "selectVar2", choices = column)

                 processedData(df)
  })

  # SUmmary table
  output$summaryTable <- renderDT({
    if (!is.null(processedData())) 
    {
      df<- processedData()}
    else if (!is.null(rawData())){
      df<-rawData()
    }
    if(!is.null(df)){  
      temp=processedData()
  number_of_columns <- ncol(temp)

  number_of_rows <- nrow(temp)

  y <- do.call(rbind, lapply(names(temp), FUN=function(x) {
    mean <- ifelse(is.numeric(temp[[x]]), round(mean(temp[[x]], na.rm = TRUE), digits = 2), NA)
    median <- ifelse(is.numeric(temp[[x]]), round(stats::median(temp[[x]], na.rm = TRUE), digits = 2), NA)
    sd <- ifelse(is.numeric(temp[[x]]), round(stats::sd(temp[[x]], na.rm = TRUE), digits = 2), NA)
    variance <- ifelse(is.numeric(temp[[x]]), round(stats::var(temp[[x]], na.rm = TRUE), digits = 2), NA)
    max <- ifelse(is.numeric(temp[[x]]), max(temp[[x]], na.rm = TRUE), NA)
    min <- ifelse(is.numeric(temp[[x]]), min(temp[[x]], na.rm = TRUE), NA)
    IQR <- ifelse(is.numeric(temp[[x]]), IQR(temp[[x]], na.rm =TRUE), NA)
    levels <- length(unique(temp[[x]]))
    c(Column=x,
      Class=class(temp[[x]]),
      Missing = sum(is.na(temp[[x]])),
      Min = min,
      Median = median,
      Max = max,
      Mean = mean,
      SD = sd,
      Variance = variance,
      Unique_Values = as.integer(levels))

  }))
      x= list(Rows = number_of_rows, Columns = number_of_columns, Types = y)
      DT::datatable(x$Types,
                class = 'cell-border stripe',
                selection=list(mode="multiple", target="row"),
                rownames=FALSE,
                options = list(scrollX = TRUE, autoWidth = FALSE)
                )
    }
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
              options = list(scrollY = 450,
                             scrollX = 500,
                             deferRender = TRUE,
                             scroller = TRUE,
                             buttons = list('excel',
                                            list(extend = 'colvis', targets = 0, visible = FALSE)),
                             fixedColumns = TRUE),selection='single')
  })
  output$dataTablePre <- renderDT({
    if (!is.null(processedData())) 
    {
      df<- processedData()
    }
    else if (!is.null(rawData())){
      df<-rawData()
    }
    else 
      return() 

    datatable(df,editable="cell",
              options = list(scrollY = 450,
                             scrollX = 500,
                             deferRender = TRUE,
                             scroller = TRUE,
                             buttons = list('excel',
                                            list(extend = 'colvis', targets = 0, visible = FALSE)),
                             fixedColumns = TRUE),selection='single')
  })

  # Génération des graphiques unidimensionnels

  output$unidimPlot <- renderUI({
    df <- if ( !is.null(processedData())) processedData() else rawData()
    varTypes=sapply(df,class)
    print(input$selectVar)
    print(sum(unique(df[[input$selectVar]])))
    if (varTypes[input$selectVar] %in% c('integer','numeric' ) 
        && !is.na(sum(unique(df[[input$selectVar]])))
        && sum(unique(df[[input$selectVar]]))>5
        ) {

      scatter_plt<-ggplot(df, aes_string(x = input$selectVar)) + 
      geom_histogram(binwidth = (max(df[[input$selectVar]],na.rm=TRUE) - min(df[[input$selectVar]],na.rm=TRUE)) / 20) + 
      theme_minimal() + 
      ggtitle(paste("Histogramme :", input$selectVar)) + 
      theme(plot.title = element_text(face = "bold", color = "#2E8B57", size = 14, hjust = 0.5))

      box_plt <-ggplot(df,aes_string(y=input$selectVar))+ 
      geom_boxplot(fill = "skyblue", color = "black") +
        theme_minimal() +
        ggtitle(paste("Box Plot :", input$selectVar)) + 
        theme(plot.title = element_text(face = "bold", color = "#2E8B57", size = 14, hjust = 0.5))

    fluidRow(
                    column(6,renderPlot({scatter_plt})), 
                    column(6,renderPlot({box_plt})))
    }
    else if (length(unique(df[[input$selectVar]]))<20){
           your_data_factor <- within(df, {
        selected_factor <- factor(df[[input$selectVar]])
      })
      
       bar_plt=  
          ggplot(df, aes(x = factor(df[[input$selectVar]]), fill = factor(df[[input$selectVar]]))) +
         geom_bar() +
          labs(title = "Categorical Columns Stacked Bar Plot",
            x = input$selectVar,
            y = "Count",
            fill=input$selectVar
          ) 

        fluidRow(
             column(12,renderPlot({bar_plt}))
             )
  }
  })

  # Génération des graphiques bidimensionnels avec indicateur de corrélation
  output$bidimPlot <- renderPlot({
    df <- if ( !is.null(processedData())) processedData() else rawData()
    req(df)
    numeric_columns <- names(df)[sapply(df, function(x) is.numeric(x) | is.integer(x))]
    category_columns <- names(df)[sapply(df, function(x) length(unique(x))<20)]
    print(category_columns)
     if (input$selectVar1 %in% category_columns || input$selectVar2 %in% category_columns) {
       if(input$selectVar1 %in% category_columns && input$selectVar2 %in% category_columns){
            col1 <- factor(df[[input$selectVar1]])
            col2 <- factor(df[[input$selectVar2]])
          ggplot(df, aes(x = col1, fill = col2)) +
          geom_bar() +
          labs(title = "Categorical Columns Stacked Bar Plot",
            x = input$selectVar1,
            y = "Count",
            fill = input$selectVar2
          ) 

       }
       else{
       if(input$selectVar1 %in% category_columns){
        selected_factor <- input$selectVar1
        selected_num <- input$selectVar2
       }else{
        selected_factor <- input$selectVar2
        selected_num <- input$selectVar1
       }
      selected_factor_val <- factor(df[[selected_factor]])
      selected_num_val <- df[[selected_num]]

      ggplot(df, aes(x =selected_factor_val, y =selected_num_val)) +
      geom_boxplot(fill = "skyblue", color = "black") +
      labs(title = paste("Boxplot of ", selected_num, "by Category ",selected_factor), x =selected_factor, y = selected_num)
       }
    } else if (input$selectVar1 %in% numeric_columns && input$selectVar2 %in% numeric_columns) {
      cor_value <- cor(df[[input$selectVar1]], df[[input$selectVar2]], use = "complete.obs")
      ggplot(df, aes_string(x = input$selectVar1, y = input$selectVar2)) + 
        geom_point() + 
        theme_minimal() +
        ggtitle(paste("Scatter Plot :", input$selectVar1, "&", input$selectVar2)) + 
        theme(plot.title = element_text(face = "bold", color = "#2E8B57", size = 14, hjust = 0.5)) +
        annotate("text", x = Inf, y = Inf, label = paste("Corrélation:", round(cor_value, 2)), 
                 hjust = 1.1, vjust = 1.1, color = "red", size = 5, fontface = "bold")
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
                     pirint(dfSummary)
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

    category_columns <- names(df)[sapply(df, function(x) is.factor(x) || length(unique(x))<20  )]
    selectInput("imbalanceTarget", "Select the class variable", 
                choices = category_columns)
  })
    
  # Show class distribution for the selected target variable
  output$classDistributionPlot <- renderPlot({
      df <- if (!is.null(processedData())) processedData() else rawData()
      targetVar <- input$imbalanceTarget
      class_counts <- table(df[[input$imbalanceTarget]])
      barplot(class_counts, 
              main = "Distribution des Classes", 
              xlab = "Classes", 
              ylab = "Fréquence",
              col = "skyblue",  # You can choose different colors
              names.arg = names(class_counts))
    })
    
    
  # Apply SMOTE and show the updated data using the 'grt' package
  observeEvent(input$applySMOTE, {
      df <- if (!is.null(processedData())) processedData() else rawData()
      # Create a formula dynamically
      print(2)
      formula <- as.formula(paste(input$imbalanceTarget, "~ ."))
      df <- ROSE(formula, data = df, seed =123)$data
      processedData(df)
  })
  observeEvent(input$applyUnder, {
        df <- if (!is.null(processedData())) processedData() else rawData()
        print(input)
        print("<<<<<<<<<<<<<<<")
        class_variable_name <- input$imbalanceTarget
        class_variable_name <- input$imbalanceTarget
        df[[class_variable_name]]=as.factor(df[[class_variable_name]])

# Perform undersampling using caret package
        df <- downSample(x = df[, names(df) != class_variable_name], 
                                y = df[[class_variable_name]],
                                yname = class_variable_name)
 
       processedData(df)
  })
  

    
    
  }
read_attributes_names <- function(file_path) {
  lines <- readLines(file_path)
  
  # Find the line containing "7. Attribute information:"
  attribute_info_line <- grep("^7\\.", lines)
  
  if (length(attribute_info_line) == 0) {
    stop("Attribute information not found in the file.")
  }
  
  # Extract the lines containing attribute information
  attribute_lines <- lines[(attribute_info_line + 1):(length(lines))]
  
  # Filter out lines that don't start with a number
  attribute_lines <- attribute_lines[grepl("^\\s*\\d", attribute_lines)]
  
  # Stop when reaching an empty line or a line starting with a digit
  stop_line <- min(which(attribute_lines == " " | grepl("^\\d", attribute_lines))) - 1
  
  if (length(stop_line) == 0) {
    stop_line <- length(attribute_lines)
  }
  
  # Extract the attribute information
  attribute_information <- attribute_lines[1:stop_line]
  
  # Extract attribute names without numbers and spaces
  attribute_names <- gsub("^\\s*\\d+[:\\.]\\s*([^:]+).*", "\\1", attribute_information)

  # Remove elements that don't start with a letter
  clean_attribute_names <- attribute_names[sapply(strsplit(attribute_names, "\\s+"), function(x) any(grepl("^[[:alpha:]]", x)))]
  clean_attribute_names <- gsub("\\s+", "_", clean_attribute_names)

  
  return(clean_attribute_names)
}


  # Lancement de l'application
  shinyApp(ui = ui, server = server)



