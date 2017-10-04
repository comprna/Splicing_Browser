
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
v <- reactiveValues(gene = NULL)
v <- reactiveValues(auxGene = NULL)
v <- reactiveValues(data = NULL)
v <- reactiveValues(dataAux = NULL)
v <- reactiveValues(preparedData = NULL)
v <- reactiveValues(flag = NULL)
v <- reactiveValues(loc = NULL)
v <- reactiveValues(location = NULL)
v <- reactiveValues(events = NULL)
v <- reactiveValues(trackList = tracks)

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
      nrow(events_filter_all[nrow(events_filter_all[grep(paste("\\",gene,"\\b",sep=""), rownames(events_filter_0.1)),]),])>0, 
      "Error: no events with that gene"
    )
  )
  # Print location into aux variable, it's showed into verbatimTextOutput("loc")
  v$loc <- getGeneLoc(gene)
  # Initialize reactive variable v$preparedData
  v$preparedData <- v$dataAux
})

######################################################################################

# "loc" output
output$loc <- renderText(
  if(is.null(v$loc)){
    # If it's null put default message
    return("input some data")
  } else {
    # Else, print v$loc value
    return(v$loc)
  }
)

######################################################################################

# Observe if "v$loc" it's updated
observeEvent(v$loc, {
  if(is.null(v$loc))
    return()
  # If it's not null update "location_str" with it's value
  updateTextInput(session, "location_str", value=as.character(v$loc))
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
      nrow(events_filter_all[nrow(events_filter_all[grep(paste("\\",gene,"\\b",sep=""), rownames(events_filter_0.1)),]),])>0, 
      "Error: no events with that gene"
    )
  )
  # Initialize reactive variable v$data
  v$data <- prepareDataToPlot(gene)
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
      nrow(events_filter_all[nrow(events_filter_all[grep(paste("\\",gene,"\\b",sep=""), rownames(events_filter_0.1)),]),])>0, 
      "Error: no events with that gene"
    )
  )
  # Get the data set with the appropriate name
  levels <-levels(v$data$Event)

  if(!is.null(input$inputSearch)) shinyjs::enable("goButton") 
  
  flag <- T
  if(is.null(v$events)) flag <- F
  
  box(
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
      choices  = levels,
      selected = v$events
    ),
    footer = checkboxInput(
      inputId = "selectAll",
      label = "Select all events",
      value = flag
    )
  )
})

observeEvent(input$selectAll, {
  if(input$selectAll) v$events <- levels(v$data$Event)
  else v$events <- NULL
})

######################################################################################

# OUTPUT MULTIPLOT
output$multiPlot <- renderPlotly({
  if(is.null(v$preparedData))
    return()
  ggplotly(generateMultiPlot(v$preparedData, input$dataType, input$patientType))
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
  v$loc <- NULL
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
    title = "Select gene", 
    trigger = "", 
    size = "large",
    tags$p(
      "Click on one gene to select it"
    ),
    column(
      12,                   
      DT::dataTableOutput("genes")
    )
  )
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
# uploadFiles output
output$uploadFiles <- renderUI({
  bsModal(
    id = "uploadFil", 
    title = "Upload files", 
    trigger = "", 
    size = "large",
    tags$p(
      "Tou should to imput new files..."
    ),
    column(
      width = 6,
      fileInput(
        inputId = 'clinData', 
        label="Clinical data", 
        buttonLabel = "Browse", 
        placeholder = "No file selected"
      )
    ),
    column(
      width = 6,
      fileInput(
        inputId = 'patData', 
        label="Patient data", 
        buttonLabel = "Browse...", 
        placeholder = "No file selected"
      )
    ),
    bsButton(
      inputId = "changeFiles", 
      label = "Udate"
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
  } else {
    cat("EXISTE")
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
  } else {
    cat("YA EXISTE")
  }
})

######################################################################################

# Observe if "go_left" button is clicked
observeEvent(input$go_left, {
  amount <- max(1, width(location()) %/% 4)
  new_location <- shift(location(), -amount)
  new_location <- trim(new_location)
  updateTextInput(session, inputId = "location_str", value=as.character(new_location))
  v$loc <- as.character(new_location)
})

######################################################################################

# Observe if "go_right" button is clicked
observeEvent(input$go_right, {
  amount <- max(1, width(location()) %/% 4)
  new_location <- shift(location(), amount)
  new_location <- trim(new_location)
  updateTextInput(session, "location_str", value=as.character(new_location))
  v$loc <- as.character(new_location)
})

######################################################################################

# Observe if "zoom_in" button is clicked
observeEvent(input$zoom_in, {
  amount <- width(location()) %/% 4
  new_location <- location()
  start(new_location) <- start(new_location) + amount
  end(new_location) <- end(new_location) - amount
  updateTextInput(session, "location_str", value=as.character(new_location))
  v$loc <- as.character(new_location)
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
  v$loc <- as.character(new_location)
})

######################################################################################

# "genome_plot" output
output$genome_plot <- renderPlot({
  # If goButton is unclicked returns nothing
  if (is.null(v$loc)) return()
  # Else return genome plot and enable browser buttons
  plot_genome(v$trackList, GRanges(v$loc, seqinfo=seqinfo(genome)))
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

