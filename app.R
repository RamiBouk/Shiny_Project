## TODO: 
## - fix the the columns disabpearing when  nromalizaotin
## - fix normalize constant columns

# Installing required packages 


options(repos = c(CRAN = "https://cran.r-project.org"))
options(shiny.maxRequestSize=200*1024^2)

packages_to_install <- c("shiny","shinyjs", "ggplot2","editData", 
                         "dplyr", "randomForest", "caret", "DT", 
                         "shinythemes", "ggcorrplot", "e1071", "caret", 
                         "randomForest", "rlang", "pROC", "Rtsne", 
                         "xgboost", "gbm","virdis","ROSE","caret","bslib",
                         "plotly"
                        )

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
library(gridExtra)
library(viridis)
library(xgboost)
library(gbm)
library(ROSE)
library(caret)
library(bslib)
library(plotly)




NO_DATA="No Data Loaded"



# Définition de l'interface utilisateur
NoDataMessage <- conditionalPanel(
                 condition = "! output.dataLoaded",
                 h2(NO_DATA)
               )
all_features_selected=FALSE
ui <- shinyUI(

  navbarPage( id="navbarPage",
  theme = shinytheme("cerulean"),
  shinyjs::useShinyjs(),

  title = "Machine Learning Platform",
  
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
               h3("Select Data"),
               fileInput("dataFile", "Choose .csv file"),
               actionButton("load", "Load")
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
               checkboxGroupInput("_", "Convert To Categorical", choices = NULL),
                  card(
                  height = 400,
                    card_body(
                       uiOutput("typeOptionsUI")
                       ))

             ),
             tabPanel("Missing Values",
                      radioButtons("missingValues", 
                                   "Method", 
                                   choices = c("Suppression", 
                                               "Replace with Mode", 
                                               "Replace with Mean",
                                               "Replace with Median"
                                               ), 
                                   selected = "Suppression"),
                checkboxGroupInput("_", "Select The Features", choices = NULL),
                  card(
                  height = 380,
                    card_body(
                       uiOutput("missingOptionsUI")
                       ))

             ),
             tabPanel("Normalization",
                checkboxGroupInput("_", "Select The Columns", choices = NULL),
                  card(
                  height = 500,
                    card_body(
                      uiOutput("varOptionsUI")
                      ))
             ),
             tabPanel("Dummification",
                checkboxGroupInput("_", "Turn to one hot encoding", choices = NULL),
                  card(
                  height = 500,
                    card_body(
                      uiOutput("catOptionsUI")), 
                       )),
             tabPanel("Outliers", 
                checkboxGroupInput("_", "Remove Outliers", choices = NULL),
                  card(
                  height = 400,
                    card_body(
                      uiOutput("outlierOptionsUI"),
                      )),
                      numericInput("outlierThreshold", "IQR", 1.5, min = 1, max = 5, step = 0.1)),
             tabPanel("Duplicates", 
                checkboxGroupInput("_", "Remove Duplicates", choices = NULL),
                  card(
                  height = 400,
                    card_body(
                      uiOutput("doubleOptionsUI"),
                      )),
                      
           ),
           ),
           actionButton("preprocess", "Apply")
       ),
       mainPanel(
         tabsetPanel(
           tabPanel("Data Table", 
                              DTOutput("dataTablePre")
                    ),
           tabPanel("Correlation", 
                    uiOutput("correlationPlot"), 
                    ),
            tabPanel("2D Visualization", 

                              fluidRow(
                                       column(3, 
                                              selectizeInput("selectVar1", "Variable X", choices = NULL),
                                              ),
                                       column(9, 
                                              selectizeInput("selectVar2", "Variable Y", choices = NULL),
                                       ),
                                       column(12, 
                                              selectizeInput("selectVarT", "target", choices = NULL),
                                       )
                                       ),
                              uiOutput("bidimPlot")),
           tabPanel("1D Visualization", 
                    selectizeInput("selectVar", "Variable", choices = NULL),
                    uiOutput("unidimPlot"), 
                    ),
         ),
         )
       )
     ),NoDataMessage
  ),
  
  # Catégorie Principale: Gestion de déséquilibre
  tabPanel("Imbalance Management",
   conditionalPanel(
    condition = "output.dataLoaded",
     sidebarLayout(
       sidebarPanel(
         uiOutput("imbalanceTargetSelectorUI"),  # Dropdown for selecting the target variable
         br(),
         actionButton("applySMOTE", "Apply Over Sampling"),
         actionButton("applyUnder", "Apply Under Sampling")
       ),
       mainPanel(
         uiOutput("classDistributionPlot"),  # Plot to show class distribution
         DTOutput("dataPreview")  # Data preview after applying SMOTE
       )
     )
    ),NoDataMessage

  ),
  
  
  
  # Catégorie Principale: Entraînement et évaluation des modèles ML
  tabPanel("Machine Learning",id ="tab3",
   conditionalPanel(
    condition = "output.dataLoaded",
     sidebarLayout(
       sidebarPanel(
         uiOutput("mltargetSelectorUI"),
         uiOutput("modelSelectorUI"),
         uiOutput("mlFeatureSelectorUI"),
         #checkboxGroupInput("selectedFeatures", "Choose The Features", choices = NULL),
         conditionalPanel(
           condition = "input.modelType == 'k-Nearest Neighbors'",
           numericInput("numNeighbors", "Number of Neighbors (k):", value = 5, min = 1)
         ),
         checkboxInput("crossValidation", "Apply Cross Validation", value = FALSE),
         checkboxInput("gridSearch", "Apply Grid Search", value = FALSE),
         numericInput("numFolds", "Number of folds", value = 10, min = 2),
         
         actionButton("trainModel", "Train The model")
       ),
       mainPanel(
         tabsetPanel(
           tabPanel("Evaluatoin Results",
                  fluidRow(
                          column(6, 
                    plotOutput("rocPlot")),
                          column(6, 
                    verbatimTextOutput("modelMetrics")),
                          )),  # Ajout d'un plot pour la courbe ROC
           tabPanel("Feature Importance",
                    plotOutput("featureImportancePlot")
           )
         )
       )
     )),NoDataMessage
  ),
  
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
  # Read .data file
  df <- read.csv("./hepatitis/hepatitis.data", header = FALSE,na.strings = c("", "NA", "?"))

  # Read .names file
  names_file <- "./hepatitis/hepatitis.names"
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
  updateSelectInput(session, "selectVarT", choices = column)
  observeEvent(input$load, {
                 # Read .data file
                 df <- read.csv(input$dataFile$datapath,na.strings = c("", "NA", "?"))

                 rawData(df)
                 processedData(df)
                 # Assuming 'df' is your data frame
                 numeric_columns <- names(df)[sapply(df, function(x) is.numeric(x) | is.integer(x))]
                 category_columns <- names(df)[sapply(df, function(x) !is.numeric(x) && length(unique(x))<30)]
                 column=c(numeric_columns,category_columns)
                 updateSelectInput(session, "selectVar", choices =column) 
                 updateSelectInput(session, "selectVar1", choices = column)
                 updateSelectInput(session, "selectVar2", choices = column)
                 updateSelectInput(session, "selectVarT", choices = column)
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
                 selected_varT <- input$selectVarT

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
                 
                  for(var in names(df)) {
                   if(!is.null(input[[paste0("doubleSelect_", var)]]) &&
                      input[[paste0("doubleSelect_", var)]]) {
                     df <- df %>% distinct(!!sym(var), .keep_all = TRUE)

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
                     for(colname in colnames(dummies)){
                           df[[colname]]=as.factor(df[[colname]])
                      }
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
    if (varTypes[input$selectVar] %in% c('integer','numeric' ) 
       # && !is.na(sum(unique(df[[input$selectVar]])))
       &&n_distinct(df[[input$selectVar]], na.rm = FALSE) >5
        ) {

      scatter_plt<-ggplot(df, aes_string(x = input$selectVar)) + 
      geom_histogram(binwidth = (max(df[[input$selectVar]],na.rm=TRUE) - min(df[[input$selectVar]],na.rm=TRUE)) / 20,
                     alpha=0.9,
                     color="black",
                     fill = "skyblue"
                     ) + 
      ggtitle(paste("Histogramme :", input$selectVar)) + 
      theme(plot.title = element_text(face = "bold", color = "#2E8B57", size = 14, hjust = 0.5))


      box_plt <-ggplot(df,aes_string(y=input$selectVar))+ 
      geom_boxplot(fill = "skyblue", color = "black",alpha=0.8) +
      stat_boxplot(geom = "errorbar",width = 0.15) +
      ggtitle(paste("Box Plot :", input$selectVar)) + 
      theme(plot.title = element_text(face = "bold", color = "#2E8B57", size = 14, hjust = 0.5))

    fluidRow(
                    column(6,renderPlotly({scatter_plt})), 
                    column(6,renderPlotly({box_plt})))
    }
    else if (length(unique(df[[input$selectVar]]))<20){
           your_data_factor <- within(df, {
        selected_factor <- factor(df[[input$selectVar]])
      })
      
       variable=factor(df[[input$selectVar]])
       bar_plt=  
          ggplot(df, aes(x = variable)) +
         geom_bar(fill = "skyblue", color = "black") +
          labs(title = "Categorical Columns Stacked Bar Plot",
            x = input$selectVar,
            y = "Count",
          ) 

        fluidRow(
             column(12,renderPlotly({bar_plt}))
             )
  }
  })

  # Génération des graphiques bidimensionnels avec indicateur de corrélation
  output$bidimPlot <- renderUI({
    df <- if ( !is.null(processedData())) processedData() else rawData()
    req(df)
    numeric_columns <- names(df)[sapply(df, function(x) is.numeric(x) | is.integer(x))]
    category_columns <- names(df)[sapply(df, function(x) length(unique(x))<20)]
     if (input$selectVar1 %in% category_columns || input$selectVar2 %in% category_columns) {
       if(input$selectVar1 %in% category_columns && input$selectVar2 %in% category_columns){
            X <- factor(df[[input$selectVar1]])
            Y <- factor(df[[input$selectVar2]])
          renderPlotly({ggplot(df, aes(x = X, fill = Y)) +
          geom_bar() +
          labs(title = "Categorical Columns Stacked Bar Plot",
            x = input$selectVar1,
            y =input$selectVar2,
            fill = input$selectVar2
          ) 
             })

       }
       else{
       if(input$selectVar1 %in% category_columns){
        selected_factor <- input$selectVar1
        selected_num <- input$selectVar2
       }else{
        selected_factor <- input$selectVar2
        selected_num <- input$selectVar1
       }
      X <- factor(df[[selected_factor]])
      Y <- df[[selected_num]]


      renderPlotly({
        ggplot(df, aes(x =X, y =Y)) +
      geom_violin(fill="skyblue",alpha=0.2)+
      geom_boxplot(fill = "skyblue", color = "black",alpha=0.9) +
      stat_boxplot(geom = "errorbar",
               width = 0.15) +
      labs(title = paste("Boxplot of ", selected_num, "by Category ",selected_factor), x =selected_factor, y = selected_num)
       })
       }

    } else if (input$selectVar1 %in% numeric_columns && input$selectVar2 %in% numeric_columns) {
      cor_value <- cor(df[[input$selectVar1]], df[[input$selectVar2]], use = "complete.obs")
      p <- ggplot(df, aes_string(x = input$selectVar1, y = input$selectVar2)) + 
        geom_point() + 
        theme_minimal() +
        geom_smooth()+
        ggtitle(paste("Scatter Plot :", input$selectVar1, "&", input$selectVar2)) + 
        theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5)) +
        annotate("text", x = Inf, y = Inf, label = paste("Correlatoin:", round(cor_value, 2)), 
                 hjust = 1.1, vjust = 1.1, size = 5, fontface = "bold")
      margin_type <- "histogram"
      #:widthp <- ggExtra::ggMarginal(p, type = margin_type, margins = "both",
      #:width  size = 8)

      renderPlotly({p})
    }
  })
output$correlationPlot <- renderUI({
    df <- if (!is.null(processedData())) processedData() else rawData()

    # Select only numeric variables from the dataset
    numeric_vars <- df %>% select_if(is.numeric)

    # Ensure there are at least two numeric columns to compute correlation
    if (ncol(numeric_vars) > 1) {
        # Calculate the correlation matrix
        correlation_matrix <- cor(numeric_vars, use = "complete.obs")

        # Melt the correlation matrix into long format for ggplot
        corr_melted <- reshape2::melt(correlation_matrix)

        # Create the correlation matrix heatmap
        corr_plot <- ggplot(corr_melted, aes(Var1, Var2, fill = value)) +
            geom_tile(color = "white") +
            scale_fill_gradient2(low = "red", high = "blue", mid = "white",
                                 midpoint = 0, limit = c(-1, 1), space = "Lab",
                                 name = "Correlation") +
            #theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                             size = 12, hjust = 1),
                  axis.text.y = element_text(size = 12)) +
            coord_fixed() +
            ggtitle("Correlation Matrix") +
            theme(plot.title = element_text(face = "bold", color = "#2E8B57", size = 14, hjust = 0.5))

        # Render the plot
        fluidRow(
            column(12, renderPlotly({ggplotly(corr_plot) %>% layout(height = 700)})) 
        )
    } else {
        # If there are no or only one numeric column, show a message
        fluidRow(
            column(12, "Not enough numeric variables to compute a correlation matrix.")
        )
    }
})

  # Génération du Box Plot
  output$boxPlot <- renderUI({
    df <- if ( !is.null(processedData())) processedData() else rawData()
    req(df)
    a=2

    if (is.numeric(df[[input$selectVar]])) {
      renderPlotly({ggplot(df, aes_string(y = input$selectVar)) + 
        geom_boxplot() + 
        theme_minimal() +
        ggtitle(paste("Box Plot :", input$selectVar)) + 
        theme(plot.title = element_text(face = "bold", color = "#2E8B57", size = 14, hjust = 0.5))
        })
    } else {
      renderPlotly({plot(NULL, xlim = c(0, 1), ylim = c(0, 1), type = "n", xlab = "", ylab = "")
      text(0.5, 0.5, "Sélectionnez une variable numérique", cex = 1.2, col = "red")
        })
    }
  })

  # Génération du graphique de matrice de corrélation
  output$corrPlot <- renderPlotly({
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

  ## intialize the plot
  output$rocPlot <- renderPlot({
      text(0.5, 0.5, "No Results",cex = 1.5)
      })
  output$featureImportancePlot <- renderPlot({
      text(0.5, 0.5, "No Results",cex = 1.5)
      })
  # UI for feature selection based on column type
  output$mlFeatureSelectorUI <- renderUI({
      df <- if (!is.null(processedData())) processedData() else rawData()
      req(df)
      req(input$modelType)
  
      # Check if trainModel starts with "SVM"
      if (startsWith(input$modelType, "SVM")) {
          # Filter numeric columns only
          availableFeatures <- names(df)[sapply(df, is.numeric)]
      } else {
          # Use all column names if not SVM
          availableFeatures <- names(df)
      }
  
      # UI for selecting features with a "Select All" button
      card(
          card_body(
              checkboxGroupInput("_", "Choose at least 2 Features", choices = NULL),
              card(
                  height = 300,
                  card_body(
                      checkboxGroupInput("mlSelectedFeatures", NULL, 
                                         choices = availableFeatures[availableFeatures != input$mlselectedTarget])
                  )
              ),
              # Adding a "Select All" button
              actionButton("selectAllBtn", "Select All / Unselect All")
          )
      )
  })
  
  # Variable to track whether features are selected or not
  selectedState <- reactiveVal(FALSE)
  
  # Observer to handle "Select All / Unselect All" button click
  observeEvent(input$selectAllBtn, {
      df <- if (!is.null(processedData())) processedData() else rawData()
      
      # Check if trainModel starts with "SVM"
      availableFeatures <- if (startsWith(input$modelType, "SVM")) {
          names(df)[sapply(df, is.numeric)]
      } else {
          names(df)
      }
      
      # Toggle logic for selecting/unselecting features
      if (selectedState()) {
          # Unselect everything
          updateCheckboxGroupInput(session, "mlSelectedFeatures", selected = character(0))
          selectedState(FALSE)  # Set state to unselected
      } else {
          # Select all features except the target
          selectedFeatures <- availableFeatures[availableFeatures != input$mlselectedTarget]
          updateCheckboxGroupInput(session, "mlSelectedFeatures", selected = selectedFeatures)
          selectedState(TRUE)  # Set state to selected
      }
  })
  output$modelSelectorUI <- renderUI({
    df <- if (!is.null(processedData())) processedData() else rawData()
    req(df)
    varTypes <- sapply(df, class)
    selectInput("modelType", "Choose the model",
                     choices = c( "Random Forest",
                                 "k-Nearest Neighbors", "Decision Tree", 
                                 "Gradient Boosting Machines",
                                 "XGBoost",
                                 "Logistic Regression",
                                 "SVM (Lineare)",
                                 "SVM (RBF Kernel)"))


  })
  observeEvent(input$mlselectedTarget,{
    df <- if (!is.null(processedData())) processedData() else rawData()
    req(df)
                models=c( "Decision Tree","Random Forest", 
                              "k-Nearest Neighbors",  
                              "Gradient Boosting Machines", 
                              "XGBoost")

                if(n_distinct(df[[input$mlselectedTarget]], na.rm = FALSE) <3){
                  updateSelectInput(session,"modelType",choices=c(models,"Logistic Regression",
                              "SVM (Lineare)",
                              "SVM (RBF Kernel)"))
                }
                else{
                  updateSelectInput(session,"modelType",choices=models)
                }
                updateCheckboxGroupInput(session,"mlSelectedFeatures",NULL,names(df)[names(df)!=input$mlselectedTarget])

  })
#  observeEvent(input$modelType,{
#                 print("changed target")
#    df <- if (!is.null(processedData())) processedData() else rawData()
#    req(df)
#                models=c( "Random Forest", 
#                              "k-Nearest Neighbors", "Decision Tree", 
#                              "Gradient Boosting Machines", "XGBoost")
#                if(n_distinct(df[[input$mlselectedTarget]], na.rm = FALSE) <3){
#                  updateSelectInput(session,"modelType",choices=c(models,"Logistic Regression"))
#                }
#                else{
#                  updateSelectInput(session,"modelType",choices=models)
#                }
#
#  })
output$modelSummary <- renderPrint({
        print(model)
      })

  output$modelMetrics <- renderPrint({
       print("No Results") 
      })

  output$mltargetSelectorUI <- renderUI({
    df <- if (!is.null(processedData())) processedData() else rawData()
    req(df)
    selectInput("mlselectedTarget", "Choose the Target", choices = names(df))
  })

  # Entraînement et évaluation des modèles
  observeEvent(input$trainModel, {
  
      df <- if (!is.null(processedData())) processedData() else rawData()
      if(length(input$mlSelectedFeatures)<2){
        output$rocPlot <- renderPlot({
          text(0.5, 0.5, "Select at least 2 features", cex = 1.5)
        })
      } else {
        # Settings
        used_cv <- input$crossValidation
        grid_search <-input$gridSearch
        # remove NA 
        df_comp <- na.omit(df[c(input$mlselectedTarget, input$mlSelectedFeatures)])
      
        # Separate features and target
        targetName <- input$mlselectedTarget
        target <- as.factor(df_comp[[targetName]])
        levels(target) <- make.names(levels(target))
        features <- df_comp[, input$mlSelectedFeatures]
      
        # Split data into training and testing sets
        partitions <- createDataPartition(target, p = .8, list = TRUE)
        trainIndex <- partitions[[1]] 
        trainData <- features[trainIndex,]
        trainTarget <- target[trainIndex]
        testData <- features[-trainIndex,]
        testTarget <- target[-trainIndex]

        # Handle factor columns: Convert factors to dummy variables
        dummies <- dummyVars(~ ., data = features) 
        features <- predict(dummies, newdata = features)  

        column_types <- sapply(features, class)
      
        # Train control configuration with or without cross-validation
        applyCrossValidation<-input$crossValidation||input$gridSearch
        print(applyCrossValidation)
        control <- trainControl(
          method = if (applyCrossValidation) "cv" else "none",
          number = if (applyCrossValidation) input$numFolds else 1,
          summaryFunction = twoClassSummary,
          classProbs = TRUE,
          savePredictions = TRUE
        )
      
        # Train model
        model <- NULL
        if (input$modelType == "Logistic Regression") {
          model <- train(x = trainData, y = trainTarget, method = "glm", 
                         family = "binomial", trControl = control)
        
        } else if (input$modelType == "Random Forest") {
          if (grid_search) {
            print(">##############################")
            tuneGridRF <- expand.grid(mtry = c(2, 4, 6))  # Example: Tuning 'mtry' parameter
            print("<##############################")
            model <- train(x = trainData, y = trainTarget, method = "rf", 
                           trControl = control, tuneGrid = tuneGridRF, metric = "Accuracy", importance = TRUE)
            print("<>##############################")
          } else {
            model <- train(x = trainData, y = trainTarget, method = "rf", 
                           trControl = control, metric = "Accuracy", importance = TRUE)
          }
        
        } else if (input$modelType == "k-Nearest Neighbors") {
          if (grid_search) {
            tuneGridKNN <- expand.grid(k = seq(1, 20, by = 2))  # Example: Tuning 'k' parameter
            model <- train(x = trainData, y = trainTarget, method = "knn", 
                           trControl = control, tuneGrid = tuneGridKNN)
          } else {
            tuneGridKNN <- expand.grid(k = input$numNeighbors)  # Using default k from user input
            model <- train(x = trainData, y = trainTarget, method = "knn", 
                           trControl = control, tuneGrid = tuneGridKNN)
          }
        
        } else if (input$modelType == "Decision Tree") {
          if (grid_search) {
            tuneGridDT <- expand.grid(cp = seq(0.01, 0.1, by = 0.01))  # Tuning complexity parameter 'cp'
            model <- train(x = trainData, y = trainTarget, method = "rpart", 
                           trControl = control, tuneGrid = tuneGridDT)
          } else {
            model <- train(x = trainData, y = trainTarget, method = "rpart", 
                           trControl = control)
          }
        
        } else if (input$modelType == "Gradient Boosting Machines") {
          if (grid_search) {
            tuneGridGBM <- expand.grid(interaction.depth = c(1, 3, 5), 
                                       n.trees = c(50, 100, 150), 
                                       shrinkage = c(0.01, 0.1), 
                                       n.minobsinnode = c(10, 20))
            model <- train(x = trainData, y = trainTarget, method = "gbm", 
                           trControl = control, tuneGrid = tuneGridGBM, verbose = FALSE)
          } else {
            model <- train(x = trainData, y = trainTarget, method = "gbm", 
                           trControl = control, verbose = FALSE)
          }
        
        } else if (input$modelType == "XGBoost") {
          if (grid_search) {
            tuneGridXGB <- expand.grid(
              nrounds = c(100, 200), 
              max_depth = c(3, 6, 9), 
              eta = c(0.01, 0.1, 0.3), 
              gamma = c(0, 1), 
              colsample_bytree = c(0.8, 1), 
              min_child_weight = c(1, 3), 
              subsample = c(0.8, 1)
            )
          } else {
            tuneGridXGB <- expand.grid(
              nrounds = 100, 
              max_depth = 3, 
              eta = 0.3, 
              gamma = 0, 
              colsample_bytree = 1, 
              min_child_weight = 1, 
              subsample = 1
            )
          }
          model <- train(x = trainData, y = trainTarget, method = "xgbTree", 
                         trControl = control, tuneGrid = tuneGridXGB, verbose = FALSE)
        
        } else if (input$modelType == "SVM (Lineare)") {
          model <- train(x = trainData %>% select(where(is.numeric)), y = trainTarget, method = "svmLinear", 
                         trControl = control)
        
        } else if (input$modelType == "SVM (RBF Kernel)") {
          if (grid_search) {
            tuneGridSVM <- expand.grid(sigma = c(0.01, 0.1, 1), C = c(0.1, 1, 10))  # Example tuning 'sigma' and 'C'
          } else {
            tuneGridSVM <- expand.grid(sigma = 0.1, C = 1)  # Default values if grid search is disabled
          }
          model <- train(x = trainData %>% select(where(is.numeric)), y = trainTarget, method = "svmRadial", 
                         trControl = control, tuneGrid = tuneGridSVM)
        }

        print('training done')
        print('evaluating...')
        # Prediction and evaluation
        if (!is.null(model)) {
          if(input$modelType =="SVM (Lineare)" || input$modelType =="SVM (RBF Kernel)"){
            predictions <- predict(model, newdata = testData %>% select(where(is.numeric)))
          } else {
            predictions <- predict(model, newdata = testData )}
          print('________________________________prediction')
          cm <- confusionMatrix(predictions, testTarget)
        
          output$modelMetrics <- renderPrint({
            if (used_cv||grid_search) {
              overall_confusion_matrix <- confusionMatrix(model$pred$pred, model$pred$obs)
              cat("Results in cross validation\n")
              print(overall_confusion_matrix)
            } else {
              cat("Results on testing set (20%)\n")
              print(cm)
            }
          })
          model_type <- input$modelType
        
          # ROC Curve and AUC
          output$rocPlot <- renderPlot({
            probPred <- predict(model, newdata = testData, type = "prob")
            rocCurve <- roc(response = testTarget, predictor = probPred[,2])
            if (model_type != "Decision Tree" ||model_type !="SVM (Lineare)"  || model_type !="SVM (RBF Kernel)") {
              plot(rocCurve, main = "ROC Curve")
              aucValue <- auc(rocCurve)
              legend("bottomright", legend = paste("AUC:", format(aucValue, digits = 4)))
            } else {
              plot.new()
              text(0.5, 0.5, "ROC not available for Decision Trees", cex = 1.5)
            }
          })
          print('evaluation done')
          print('plotting...')
        
          # Feature Importance Plot
          output$featureImportancePlot <- renderPlot({
            if (model_type == "Random Forest") {
              varImpPlot <- varImp(model)
              plot(varImpPlot)
            } else if (model_type == "Logistic Regression" && !is.null(model)) {
              coef_data <- as.data.frame(abs(coef(model$finalModel)[-1])) 
              names(coef_data) <- c("Importance")
              coef_data$Feature <- rownames(coef_data)
              ggplot(coef_data, aes(x = reorder(Feature, Importance), y = Importance)) +
                geom_bar(stat = "identity") +
                coord_flip() +
                xlab("Feature") +
                ylab("Absolute Coefficient") +
                ggtitle("Feature Importance for Logistic Regression (Absolute Coefficients)")
            } else if (model_type == "k-Nearest Neighbors") {
              plot.new()
              text(0.5, 0.5, "Feature importance is not applicable for k-Nearest Neighbors", cex = 1.5)
            } else if (model_type == "Gradient Boosting Machines" && !is.null(model)) {
              varImpPlot <- varImp(model, scale = FALSE)
              plot(varImpPlot)
            } else if (model_type == "XGBoost" && !is.null(model)) {
              xgbImp <- xgb.importance(feature_names = model$xNames, model = model$finalModel)
              xgb.plot.importance(importance_matrix = xgbImp)
            } else {
              plot.new()
              text(0.5, 0.5, "Feature importance not available for the selected model", cex = 1.5)
            }
          })
          print('plotting done')
        }
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
                   tsneResult <- Rtsne(selectedData, dims = input$numComponents, check_duplicates = FALSE)$Y
                   result <- as.data.frame(tsneResult)
                 }

                 # Output: Plot
                 output$dimRedPlot <- renderPlotly({
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
  output$classDistributionPlot <- renderUI({
      df <- if (!is.null(processedData())) processedData() else rawData()
      targetVar <- input$imbalanceTarget

      req(input$imbalanceTarget)
      class_counts <- table(df[[input$imbalanceTarget]])

      
       renderPlot({ 
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
      df <- if (!is.null(processedData())) processedData() else rawData()
      # Create a formula dynamically
     ##123 
      char_columns <- names(df)[sapply(df, function(x) is.character(x))]
      for(char_column in char_columns)
        df[, char_column] <- as.factor(df[, char_column])
      formula <- as.formula(paste(input$imbalanceTarget, "~ ."))
      df <- ROSE(formula, data = df, seed =123)$data
     # df <- df[, c(ncol(df), 1:(ncol(df)-1))]
      processedData(df)
  })
  observeEvent(input$applyUnder, {
        df <- if (!is.null(processedData())) processedData() else rawData()
        class_variable_name <- input$imbalanceTarget
        class_variable_name <- input$imbalanceTarget
        df[[class_variable_name]]=as.factor(df[[class_variable_name]])

# Perform undersampling using caret package
        df <- downSample(x = df[, names(df) != class_variable_name], 
                                y = df[[class_variable_name]],
                                yname = class_variable_name)
 
       df <- df[, c(ncol(df), 1:(ncol(df)-1))]
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



