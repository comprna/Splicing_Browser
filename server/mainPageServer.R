
"  
  Author: Hèctor García Guillén
  Name: loadFiles.R 
  Description:
  Date: 17/06/07
  Version: 1.1
"

######################################################################################

# Load required general functions
source(file.path("server", "scripts", "generalFunctions.R"),  local = TRUE)$value

# Load required functions to run browser
source(file.path("server", "scripts", "browserFunctions.R"),  local = TRUE)$value

# Load required functions to run ggplot + plotly functions
source(file.path("server", "scripts", "ggplotFunctions.R"),  local = TRUE)$value

######################################################################################

# Initialize all reactive values to NULL
v <- reactiveValues(data = NULL)
v <- reactiveValues(dataAux = NULL)
v <- reactiveValues(preparedData = NULL)
v <- reactiveValues(loc = NULL)
v <- reactiveValues(clinDataFile = NULL)
v <- reactiveValues(patDataFile = NULL)
v <- reactiveValues(gtfDataFile = NULL)
v <- reactiveValues(clinData = NULL)
v <- reactiveValues(patData = NULL)
v <- reactiveValues(gtfData = NULL)
v <- reactiveValues(plotFilters = NULL)
v <- reactiveValues(auxPlotFilters = NULL)
v <- reactiveValues(parameters = NULL)
v <- reactiveValues(parametersAux = NULL)
v <- reactiveValues(parameterListAux = NULL)
v <- reactiveValues(parameterList = list())
v <- reactiveValues(errors = NULL)
v <- reactiveValues(aux = NULL)
v <- reactiveValues(auxSelect = NULL)

######################################################################################

v$parameters <- matrix(ncol = 2, nrow = 0)
v$clinData <- info_clin_all
v$patData <- events_filter_all
v$gtfData <- allGtf
v$plotFilters <- plotFilters
v$auxPlotFilters <- plotFilters 
v$auxSelect <- plotFilters

######################################################################################

# Observe if "goButton" is clicked
observeEvent(input$goButton, {
  # If it's clicked get value from "inputSearch"
  gene <- input$inputSearch
  # Validate if "inputSearch" is inicialized, if not return an error message
  validate(
    need(gene!="", "You should to imput a gene name or gene id to check it")
  )
  # Check if input data refers Gene.stable.ID or Gene.name
  gene <-paste(getGeneStableId(gene), collapse = '|')
  # Check if exist any event related to Gene.stable.ID, if not return an error message
  validate(
    need(
      nrow(v$patData[nrow(v$patData[grep(paste("\\",gene,"\\b",sep=""), rownames(v$patData)),]),])>0, 
      "Error: no events with that gene"
    )
  )
  # Print location into aux variable, it's showed into verbatimTextOutput("loc")
  v$location <- getGeneLoc(gene)
  # Initialize reactive variable v$preparedData
  #v$preparedData <- v$dataAux
})

######################################################################################

observeEvent(input$goPlot, {
  if(is.null(v$parameters)) return()
  v$parametersAux <- v$parameters
  v$aux <- v$parameterListAux
  v$preparedData <- v$dataAux
})

######################################################################################

observeEvent(c(v$parameters, v$parameterListAux, v$dataAux, input$resetParameters, input$addParameter), {
  if(is.null(v$parameters) || is.null(v$parameterListAux) || is.null(v$dataAux) || is.null(v$dataAux)) {
    
  } else {
    shinyjs::enable("goPlot") 
  }
})

######################################################################################

# "loc" output
output$loc <- renderText(
  if(is.null(v$location)){
    # If it's null put default message
    return("input some data")
  } else {
    # Else, print v$location value
    return(v$location)
  }
)

######################################################################################

# Observe if "v$location" it's updated
observeEvent(v$location, {
  if(is.null(v$location))
    return()
  # If it's not null update "location_str" with it's value
  updateTextInput(session, "location_str", value=as.character(v$location))
})

######################################################################################

# Observe if "inputSearch" is updated
observeEvent(input$inputSearch, {
  #If it's null retun's nothing
  if(is.null(input$inputSearch))
    return()
  # Else, get "input$inputSearch" value
  gene <- input$inputSearch
  # Validate if inputdata is inicialized
  validate(
    need(gene!="", "You should to imput a gene name or gene id to check it")
  )
  # Check if input data refers Gene.stable.ID, Gene.name or location
  gene <-paste(getGeneStableId(gene), collapse = '|')
  # Check if exist any event related to Gene.stable.ID
  validate(
    need(
      nrow(v$patData[nrow(v$patData[grep(paste("\\",gene,"\\b",sep=""), rownames(v$patData)),]),])>0, 
      "Error: no events with that gene"
    )
  )
  # Initialize reactive variable v$data
  v$data <- prepareDataToPlot(v$patData, v$clinData, gene)
})

######################################################################################

# Observe if "events"
observeEvent(input$events, {
  if(is.null(v$data))
    return()
  v$dataAux<-v$data[v$data$Event %in% input$events, ]
})

######################################################################################

# Disable goButton when input$inputSearch is empty
observeEvent(input$inputSearch, {
  if(!is.na(input$inputSearch)){
    shinyjs::disable("goButton")
  }
})

######################################################################################

# eventList output
output$eventList <- renderUI({
  if(is.null(input$inputSearch))
    return()
  gene <- input$inputSearch
  # Validate if inputdata is inicialized
  validate(
    need(
      expr = gene!="", 
      message = "You should to imput a gene name or gene id to check it")
  )
  # Check if input data refers Gene.stable.ID or Gene.name
  gene <-paste(getGeneStableId(gene), collapse = '|')
  # Check if exist any event related to Gene.stable.ID
  validate(
    need(
      nrow(v$patData[nrow(v$patData[grep(paste("\\",gene,"\\b",sep=""), rownames(v$patData)),]),])>0, 
      "Error: no events with that gene eventList output"
    )
  )
  if(!is.null(input$inputSearch)) shinyjs::enable("goButton") 
  box(
    id="chooseEvens",
    title = "Choose events",
    status = "primary",
    solidHeader = F,
    collapsible = T,
    collapsed = F,
    width = 14,
    # Create the checkboxes and select them all by default
    checkboxGroupInput(
      inputId = "events", 
      label = NULL,
      choices  = levels(v$data$Event)
    ),
    checkboxInput(
      inputId = "selectAll",
      label = "Select all events"#,
    )
  )
})

observeEvent(c(input$selectAll), {
  if (input$selectAll == F) {
    updateCheckboxGroupInput(session,"events","Select all events:",choices=levels(v$data$Event))
    updateCheckboxGroupInput(session,"selectAll","Select all events")
  }
  else {
    updateCheckboxGroupInput(session,"events","Unselect all events:",choices=levels(v$data$Event), selected=levels(v$data$Event))
    updateCheckboxGroupInput(session,"selectAll","Unselect all events")
  }
})

######################################################################################

# OUTPUT MULTIPLOT
output$multiPlot <- renderPlotly({
  if(is.null(v$preparedData))
    return()
  ggplotly(generateMultiPlot(v$preparedData, v$clinData, v$parametersAux, v$aux)) # input$dataType, input$patientType
})

######################################################################################

# OUTPUT PLOT DATA
output$dataResults <- renderDataTable({
  v$dataAux
})

######################################################################################

# GET LOCATION 
location <- reactive({
  if(is.null(input$location_str)) return ()
  GRanges(input$location_str, seqinfo=seqinfo(genome))
})

######################################################################################

# Clear all data
observeEvent(input$clear, {
  updateTextInput(session, "inputSearch", value="")
  updateTextInput(session, "location_str", value="")
  v$preparedData <- NULL
  v$location <- NULL
  shinyjs::disable("go_left") 
  shinyjs::disable("go_right") 
  shinyjs::disable("zoom_in") 
  shinyjs::disable("zoom_out")
})

######################################################################################

# Modal window "listGenes" 
# observe if "list" button is clicked
observeEvent(input$list, {
  # If it's clicked open "geneList" modal window
  toggleModal(session, "geneList", "open")
})
# Render modal output for "geneList" modal window
output$geneList <- renderUI({
  bsModal(
    id = "geneList", 
    title = NULL, 
    footer = NULL,
    trigger = "", 
    size = "large",
    #tags$p(
    #  "Click on one gene to select it"
    #),
    column(
      12,                   
      DT::dataTableOutput("genes")
    ),
    
    bsButton(
      inputId = "closeGeneList", 
      label = "Close"
    )
    
    
    
  )
})


observeEvent(input$closeGeneList, {
  toggleModal(session, "geneList", "close")
})


# observe if any row into "geneList" is clicked
observeEvent(input$genes_rows_selected, {
  # Get the Gene.Stable.ID of clicked line
  gene <- ENSEMBLEgenes_df$gene[input$genes_rows_selected]
  # Update "inputSearch" with selected Gene.Stable.ID
  updateTextInput(session, "inputSearch", value=as.character(gene))
  # Close "geneList" modal window
  toggleModal(session, "geneList", "close")
})

######################################################################################

# Modal window "uploadFiles" 
# Observe if "upload" button is clicked
observeEvent(input$upload, {
  # If it's clicked modal window "uploadFil" opens
  toggleModal(session, "uploadFil", "open")
})

######################################################################################

observeEvent(input$closeUpload, {
  toggleModal(session, "uploadFil", "close")
})

######################################################################################

# uploadFiles output
output$uploadFiles <- renderUI({
  bsModal(
    id = "uploadFil", 
    title = NULL, 
    trigger = "", 
    size = "large",
    #tags$p(
    #  "Tou should to imput new files..."
    #),
    div (
      column(
        width = 4,
        fileInput(
          inputId = 'clinDataFile', 
          label="Clinical data", 
          buttonLabel = "Browse", 
          placeholder = "No file selected"
          #accept = c()
        ),
        tableOutput('clinDataFile')
      ),
      column(
        width = 4,
        fileInput(
          inputId = 'patDataFile', 
          label="Patient data", 
          buttonLabel = "Browse...", 
          placeholder = "No file selected"
          #accept = c()
        ),
        tableOutput('patDataFile')
      )
    ),
    div (
      column(
        width = 4,
        fileInput(
          inputId = 'gtfFileData', 
          label="GTF data", 
          buttonLabel = "Browse", 
          placeholder = "No file selected"
        ),
        tableOutput('gtfFile')
      )
    ),
    div(
      id = "textErrors",
      textOutput('errors')
    ),
    bsButton(
      inputId = "uploadButton", 
      label = "Update Files",
      disabled = T
    ),
    bsButton(
      inputId = "uploadGtfButton", 
      label = "Update GTF",
      disabled = T
    ),
    bsButton(
      inputId = "closeUpload", 
      label = "Close"
    )
  )
})

######################################################################################

# Modal window "removeTracks" 
# Observe if "add" button is clicked
observeEvent(input$remove, {
  # If it's clicked modal window "removeTracks" opens
  toggleModal(session, "removeTracks", "open")
})
# removeTracks output
output$removeTracks <- renderUI({
  bsModal(
    id = "removeTracks", 
    title = "Remove Tracks", 
    trigger = "", 
    selectInput(
      inputId = "rTrackItem",
      label = "Select one to delete",
      choices = as.character(names(v$trackList))
    ),
    bsButton(
      inputId = "removeT", 
      label = "Remove Track"
    )
  )
})

# Observe if "removeT" button is clicked
observeEvent(input$removeT, {
  if(input$rTrackItem %in% names(v$trackList)){
    v$trackList <- removeTrack(v$trackList, "ucsc")
    toggleModal(session, "removeTracks", "close")
  }
})

######################################################################################

# Modal window "addTracks" 
observeEvent(input$add, {
  # If it's clicked modal window "addTracks" opens
  toggleModal(session, "addTracks", "open")
})
# removeTracks output
output$addTracks <- renderUI({
  bsModal(
    id = "addTracks", 
    title = "Add Tracks", 
    trigger = "", 
    selectInput(
      inputId = "aTrackItem",
      label = "Select one to add",
      choices = c("ucsc","ensemble")
    ),
    bsButton(
      inputId = "addT", 
      label = "Add Track"
    )
  )
})

# Observe if "addT" button is clicked
observeEvent(input$addT, {
  if(input$aTrackItem %nin% names(v$trackList)){
    v$trackList <- addTrack(v$trackList, input$aTrackItem)
    toggleModal(session, "addTracks", "close")
  }
})

######################################################################################

# Modal window "change" 
# Observe if "add" button is clicked
observeEvent(input$change, {
  toggleModal(session, "changeFilters", "open")
})

observeEvent(input$closeChange, {
  toggleModal(session, "changeFilters", "close")
})

######################################################################################

# removeTracks output
output$changeFilters <- renderUI({
  bsModal(
    id = "changeFilters", 
    title = "Select filters",
    trigger = "", 
    size = "small",
    checkboxGroupInput(
      inputId = "rChange",
      label = NULL,
      choices = colnames(v$clinData)[2:ncol(v$clinData)],
      selected = rownames(v$plotFilters)
    ),
    bsButton(
      inputId = "changeF", 
      label = "Change filters"
    ),
    bsButton(
      inputId = "closeChange", 
      label = "Close"
    )
  )
})

observeEvent(input$changeF, {
  v$preparedData <- NULL
  v$parameters <- NULL
  v$parameters <- as.matrix(data.frame(rownames(v$plotFilters)[1:nrow(v$plotFilters)], rep("", nrow(v$plotFilters))))
  v$parameterList <- list()
  v$parameterListAux <- NULL
  #v$auxPlotFilters <- 
  
  
  v$auxSelect <- as.data.frame(v$plotFilters[rownames(v$plotFilters)%in% input$rChange,])
  
  
  updateSelectInput(
    session = session,
    inputId = "newParam",
    choices = rownames(v$auxSelect)
  )
  toggleModal(session, "changeFilters", "close")
})

######################################################################################

# Observe if "go_left" button is clicked
observeEvent(input$go_left, {
  amount <- max(1, width(location()) %/% 4)
  new_location <- shift(location(), -amount)
  new_location <- trim(new_location)
  updateTextInput(session, inputId = "location_str", value=as.character(new_location))
  v$location <- as.character(new_location)
})

######################################################################################

# Observe if "go_right" button is clicked
observeEvent(input$go_right, {
  amount <- max(1, width(location()) %/% 4)
  new_location <- shift(location(), amount)
  new_location <- trim(new_location)
  updateTextInput(session, "location_str", value=as.character(new_location))
  v$location <- as.character(new_location)
})

######################################################################################

# Observe if "zoom_in" button is clicked
observeEvent(input$zoom_in, {
  amount <- width(location()) %/% 4
  new_location <- location()
  start(new_location) <- start(new_location) + amount
  end(new_location) <- end(new_location) - amount
  updateTextInput(session, "location_str", value=as.character(new_location))
  v$location <- as.character(new_location)
})

######################################################################################

# Observe if "zoom_out" button is clicked
observeEvent(input$zoom_out, {
  amount <- width(location()) %/% 2
  new_location <- location()
  start(new_location) <- start(new_location) - amount
  end(new_location) <- end(new_location) + amount
  new_location <- trim(new_location)
  updateTextInput(session, "location_str", value=as.character(new_location))
  v$location <- as.character(new_location)
})

######################################################################################

# "genome_plot" output
output$genome_plot <- renderPlot({
  # If goButton is unclicked returns nothing
  if (is.null(v$location)) return()
  # Else return genome plot and enable browser buttons
  plot_genome(v$trackList, GRanges(v$location, seqinfo=seqinfo(genome)), v$gtfData)
  shinyjs::enable("go_left") 
  shinyjs::enable("go_right") 
  shinyjs::enable("zoom_in") 
  shinyjs::enable("zoom_out") 
})

######################################################################################

# "genes" output, render data table with all gene data (Id and location)
output$genes <- DT::renderDataTable(
  server = TRUE,
  selection = "single",
  options = list(pageLength=10), {
    ENSEMBLEgenes_df
  }
)

######################################################################################

library(tools)
output$clinDataFile <- renderText({
  file <- input$clinDataFile
  #catches null exception
  if (is.null(file))
    return(NULL)
  validate(
    need(file_ext(input$clinDataFile$name) %in% c(
      'text/txt',
      'text/comma-separated-values',
      'text/tab-separated-values',
      'text/plain',
      'txt'
    ), "Wrong format!")
  )
  v$clinDataFile <- read.table(file$datapath, header = TRUE, sep = "\t", dec = ",")
  if(!is.null(v$clinDataFile)){
    return(paste("OK: ", paste(dim(v$clinDataFile), collapse = "x"), collapse = " "))
  }
})

######################################################################################

output$patDataFile <- renderText({
  #catches null exception
  file <- input$patDataFile
  if (is.null(file))
    return(NULL)
  validate(
    need(file_ext(input$patDataFile$name) %in% c(
      'text/psi',
      'text/comma-separated-values',
      'text/tab-separated-values',
      'text/plain',
      'psi'
    ), "Wrong format!")
  )
  v$patDataFile <- read.table(file$datapath, header = T, sep = "\t", dec = ",")
  if(!is.null(v$patDataFile)){
    return(paste("OK: ", paste(dim(v$patDataFile), collapse = "x"), collapse = " "))
  }
})

######################################################################################

output$gtfFile <- renderText({
  file <- input$gtfFileData
  if (is.null(file))
    return(NULL)
  #validate(
  #  need(file_ext(input$patDataFile$name) %in% c(
  #    'text/gtf',
  #    'text/comma-separated-values',
  #    'text/tab-separated-values',
  #    'text/plain',
  #    'gtf'
  #  ), "Wrong format!")
  #)
  v$gtfDataFile <- as.data.frame(readGFF(file$datapath))
  if(!is.null(v$gtfDataFile)){
    return(paste("OK: ", paste(dim(v$gtfDataFile), collapse = "x"), collapse = " "))
  }
})

######################################################################################

output$errors <- renderText({
  if(!is.null(v$errors)){
    print(v$errors)
  }
})

######################################################################################

observeEvent(c(v$clinDataFile,v$patDataFile), {
  if(!is.null(v$patDataFile) && !is.null(v$clinDataFile)){
    v$errors <- ""
    if(nrow(v$patDataFile) < 1) {
      v$errors <- c(v$errors, paste0("Number of events in Patient file must to be upper than 1"))
    }
    if(ncol(v$patDataFile) < 1) {
      v$errors <- c(v$errors, "Number of patients in Patient file must to be upper than 1")
    }
    if(nrow(v$clinDataFile) < 1) {
      v$errors <- c(v$errors, "Number of patients in Sample file must to be upper than 1")
    }
    if(ncol(v$clinDataFile) < 1) {
      v$errors <- c(v$errors, "Number of parameters in Sample file must to be upper than 1")
    }
    
    file1 <- length(v$clinDataFile[v$clinDataFile[,1] %in% colnames(v$patDataFile),1])
    file2 <- length(colnames(v$patDataFile)[colnames(v$patDataFile) %in% v$clinDataFile[,1]])
    if(file1 < 1 || file2 < 1){
      v$errors <- c(v$errors, "There are no matching samples between both files")
    }
    if(length(v$errors)<2){
      shinyjs::enable("uploadButton")
    }
  }
})

######################################################################################

observeEvent(input$uploadButton, {
  v$patData <- v$patDataFile
  v$clinData <- v$clinDataFile
  colnames(v$clinData) <- tolower(gsub(".", "_", colnames(v$clinData), fixed = TRUE))
  v$plotFilters <- as.matrix(as.data.frame(t(as.data.frame(lapply(v$clinData, class))))[2:nrow(as.data.frame(t(as.data.frame(lapply(v$clinData, class))))),])
  plotFilters <- as.matrix(as.data.frame(t(as.data.frame(lapply(v$clinData, class))))[2:nrow(as.data.frame(t(as.data.frame(lapply(v$clinData, class))))),])
  for (i in 1:ncol(v$clinData)){
    v$clinData[,i] <- gsub(" ", "", v$clinData[,i], fixed = TRUE)
  }
  v$patData[v$patData==-1]<-NA
  v$clinData[v$clinData==-1]<-NA
  v$parameters <- as.matrix(data.frame(rownames(v$plotFilters)[1:nrow(v$plotFilters)], rep("", nrow(v$plotFilters))))
  # Close modal window when user click on "uploadButton"
  toggleModal(session, "uploadFil", toggle = "toggle")
  updateTextInput(session, "inputSearch", value="")
  updateTextInput(session, "location_str", value="")
  v$preparedData <- NULL
  v$location <- NULL
  shinyjs::disable("go_left") 
  shinyjs::disable("go_right") 
  shinyjs::disable("zoom_in") 
  shinyjs::disable("zoom_out")
  v$parameterList <- list()
  v$parameterListAux <- NULL
  v$auxPlotFilters <- v$plotFilters
})

######################################################################################

observeEvent(v$gtfDataFile, {
  
  if(!is.null(v$gtfDataFile)){
    v$errors <- ""
    if(nrow(v$gtfDataFile) < 1) {
      v$errors <- c(v$errors, "Number of columns must to be upper than 1")
    }
    if(ncol(v$gtfDataFile) < 10) {
      v$errors <- c(v$errors, "The file must have at least 10 columns")
    }
    
    columnNames <- c("seqid", "source", "feature", "start", "end", "score", "strand", "frame", "transcript_id", "gene_id")
    number <- length(colnames(v$gtfDataFile)[colnames(v$gtfDataFile) %in% columnNames])
    
    if(number < 10){
      v$errors <- c(v$errors, "The columns must have the following names: seqid, source, feature, start, end, score, strand, frame, transcript_id, gene_id")
    }
    if(length(v$errors)<2){
      shinyjs::enable("uploadButton")
    }
  }
  
  
  if(!is.null(v$gtfDataFile)){
    v$errors <- ""
    if(length(v$errors)<2){
      #v$gtfDataFile <- v$gtfDataFile[!is.na(v$gtfDataFile$transcript_id),]
      v$errors <- c(v$errors, "\nActualizado")
      shinyjs::enable("uploadGtfButton")
    }
  }
})

######################################################################################

observeEvent(input$uploadGtfButton, {
  v$gtfData <- v$gtfDataFile
  v$gtfData <- v$gtfData[!is.na(v$gtfData$transcript_id),]
  # Close modal window when user click on "uploadButton"
  updateTextInput(session, "inputSearch", value="")
  updateTextInput(session, "location_str", value="")
  v$preparedData <- NULL
  v$location <- NULL
  shinyjs::disable("go_left") 
  shinyjs::disable("go_right") 
  shinyjs::disable("zoom_in") 
  shinyjs::disable("zoom_out")
  v$parameterList <- list()
  v$parameterListAux <- NULL
  v$auxPlotFilters <- v$plotFilters
  toggleModal(session, "uploadFil", "close")
  
})

######################################################################################

observeEvent(input$addParameter, {
  if(nrow(v$auxPlotFilters) <= 0 || nrow(v$auxSelect) == length(v$parameterList)+1) {
    shinyjs::disable("addParameter")
    return(alert("No more parameters to add"))
  }
    
  i <- length(v$parameterList) +1
  v$parameterListAux <- c(v$parameterListAux, input$newParam)
  v$auxPlotFilters <- as.data.frame(v$plotFilters[rownames(v$plotFilters)%nin% v$parameterListAux,])
  if(class(v$clinData[input$newParam]) == "integer") {
    a <- c(c("0","1"))
  } 
  else {
    a <- c(as.character(unique(v$clinData[input$newParam])[,1]))
  }  
  v$parameterList[[i]] <- div(
    div (
      class="otherParametersData",
      checkboxGroupInput(
        inputId = paste0("col",which(colnames(v$clinData)==input$newParam)),
        label = toupper(input$newParam),
        choices = a[a %nin% c("","NA")],
        selected =a[a %nin% c("","NA")]
      )
    )
  )
  if(i!=1){
    if(length(v$parameterListAux)>1) {
      for(j in 1:nrow(v$parameters)){
        param <- v$parameterListAux[j]
        n <- which(colnames(v$clinData)==param)
        values <- input[[paste0("col",n)]]
        if(class(values) == "NULL") {
          values <- c("a")
        }
        if(j!=length(v$parameterListAux)){
          updateCheckboxGroupInput(
            session = session,
            inputId = paste0("col",n),
            selected = values
          )
        }
      }
    }
  }
})

######################################################################################

output$otherParameters <- renderUI({
  v$parameterList
})

######################################################################################

observeEvent(input$removeParameter, {
  if(nrow(v$auxPlotFilters) == nrow(v$plotFilters)){
    shinyjs::disable("removeParameter")
    return(alert("No more parameters to remove"))
  }
  if(nrow(v$auxPlotFilters) == nrow(v$plotFilters)){
    
    v$preparedData <- NULL
    v$parameters <- NULL
    v$parameters <- as.matrix(data.frame(rownames(v$plotFilters)[1:nrow(v$plotFilters)], rep("", nrow(v$plotFilters))))
    v$parameterList <- list()
    v$parameterListAux <- NULL
    v$auxPlotFilters <- v$plotFilters

    } else {
    if(length(v$parameterListAux)>1) {
      for(j in 1:nrow(v$parameters)){
        param <- v$parameterListAux[j]
        n <- which(colnames(v$clinData)==param)
        values <- input[[paste0("col",n)]]
        if(class(values) == "NULL") {
          values <- c("a")
        }
        if(j!=length(v$parameterListAux)){
          updateCheckboxGroupInput(
            session = session,
            inputId = paste0("col",n),
            selected = values
          )
        }
      }
    }
    for(j in 1:nrow(v$parameters)){
      if(v$parameterListAux[length(v$parameterListAux)]==v$parameters[j,1]){
        v$parameters[j,2] <- ""
      } 
    }
    v$parameterListAux <- v$parameterListAux[1:length(v$parameterListAux)-1]
    v$auxPlotFilters <- as.data.frame(v$plotFilters[rownames(v$plotFilters)%nin% v$parameterListAux,])
    v$parameterList <- v$parameterList[1:length(v$parameterList)-1]
  }
  #print(v$parameters)
})

######################################################################################

output$todecide <- renderUI({
  if(nrow(v$auxPlotFilters)<=0) {
    choices = c("No more params")
  } else {
    choices = rownames(v$auxPlotFilters)[rownames(v$auxPlotFilters) %in% input$rChange]
  }
  div(
    id = "block",
    selectInput(
      inputId = "newParam",
      label = NULL,
      width = "40%",
      choices = choices
    ),
    bsButton("addParameter", label=NULL, icon = icon("glyphicon glyphicon-plus", lib = "glyphicon")),
    bsButton("removeParameter", label=NULL, icon = icon("glyphicon glyphicon-minus", lib = "glyphicon")),
    bsButton("resetParameters", label=NULL, icon = icon("glyphicon glyphicon-trash", lib = "glyphicon")),
    bsButton("change", "Change Params"),
    bsButton("goPlot", "Go!", disabled = T)
  )
})

######################################################################################

observeEvent(input$resetParameters, {
  v$preparedData <- NULL
  v$parameters <- NULL
  v$parameters <- as.matrix(data.frame(rownames(v$plotFilters)[1:nrow(v$plotFilters)], rep("", nrow(v$plotFilters))))
  v$parameterList <- list()
  v$parameterListAux <- NULL
  v$auxPlotFilters <- v$plotFilters
  #print(v$parameters)
})

######################################################################################

lapply(
  X = 1:nrow(plotFilters)+1,
  FUN = function(i){
    observeEvent(input[[paste0("col",i)]], {
      param <- colnames(v$clinData)[i]
      value <- paste(input[[paste0("col",i)]],collapse=" ")
      if(i==1){
        v$parameters <- as.matrix(rbind(v$parameters, data.frame(param,value)))
      } else {
        if(param %in% v$parameters[,1]){
          for(j in 1:nrow(v$parameters)){
            if(param==v$parameters[j,1]){
              v$parameters[j,2] <- value
            } 
          }
        } else {
          if(is.null(v$parameters)){
            v$parameters <- as.matrix(data.frame(rownames(v$plotFilters)[1:nrow(v$plotFilters)], rep("", nrow(v$plotFilters))))
          } else {
            v$parameters <- as.matrix(rbind(v$parameters, data.frame(param,value)))
          }
        }
      }
      #colnames(v$parameters) <- c("param", "value")
      #print(v$parameters)
    }, ignoreNULL = F)
  }
)

######################################################################################
