
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
v <- reactiveValues(clinDataFile = NULL)
v <- reactiveValues(patDataFile = NULL)
v <- reactiveValues(clinData = NULL)
v <- reactiveValues(patData = NULL)
v <- reactiveValues(plotFilters = NULL)
v <- reactiveValues(listFilters = NULL)
v <- reactiveValues(parameter1 = NULL)
v <- reactiveValues(aux = NULL)
v <- reactiveValues(a = NULL)
v <- reactiveValues(trackList = tracks)

v$a <- 5
v$listFilters <- c()
v$clinData <- info_clin_all
v$patData <- events_filter_all
v$plotFilters <- plotFilters

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
  #write.table(v$patData)
  validate(
    need(
      nrow(v$patData[nrow(v$patData[grep(paste("\\",gene,"\\b",sep=""), rownames(v$patData)),]),])>0, 
      "Error: no events with that gene eventList output"
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
  ggplotly(generateMultiPlot(v$preparedData, v$parameter1, input$parameter1)) # input$dataType, input$patientType
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
    footer = NULL, 
    close.button = F,
    easyClose = F,
    tags$p(
      "Tou should to imput new files..."
    ),
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
    ),
    column(
      width = 4,
      fileInput(
        inputId = 'gtfDataFile', 
        label="GTF data", 
        buttonLabel = "Browse", 
        placeholder = "No file selected",
        accept = c(
          'text/csv',
          'text/comma-separated-values',
          'text/tab-separated-values',
          'text/plain',
          'csv',
          'tsv'
        )
      ),
      tableOutput('gtfFile')
    ),
    #shiny::tags$button(id = "uploadButton", disabled = T, type = "button", class = "btn btn-default", "data-dismiss" = "modal", "Update")
    bsButton(
      inputId = "uploadButton", 
      label = "Update",
      disabled = T
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

# Modal window "change" 
# Observe if "add" button is clicked
observeEvent(input$change, {
  # If it's clicked modal window "removeTracks" opens
  toggleModal(session, "changeFilters", "open")
  cat("Aprieta boton")
})
# removeTracks output
output$changeFilters <- renderUI({
  #if(is.null(v$plotFilters))
  #  return(
  #    bsModal(
  #      id = "changeFilters", 
  #      trigger = "", 
  #      tags$p(
  #        "You should work with any data, please load data files previously"
  #      )
  #    )
  #  )
  
  bsModal(
    id = "changeFilters", 
    title = "Select filters",
    trigger = "", 
    checkboxGroupInput(
      inputId = "rChange",
      label = NULL,
      choices = rownames(v$plotFilters)
    ),
    bsButton(
      inputId = "changeF", 
      label = "Change filters"
    )
  )
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
    ), "Wrong File Format try again!")
  )
  v$clinDataFile <- read.table(file$datapath, header = TRUE, sep = "\t", dec = ",")
  cat(class(v$clinDataFile))
  cat(dim(v$clinDataFile))
  if(!is.null(v$clinDataFile)){
    return(paste (c("File",file$name, "has been uploaded correctly"), sep = " ", collapse = NULL))
  }
})

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
    ), "Wrong File Format try again!")
  )
  v$patDataFile <- read.table(file$datapath, header = T, sep = "\t", dec = ",")
  cat(class(v$patDataFile))
  cat(dim(v$patDataFile))
  if(!is.null(v$patDataFile)){
    return(paste (c("File",file$name, "has been uploaded correctly"), sep = " ", collapse = NULL))
  }
})

# Comprobar que el archivo subido es de la extension deseada
# - En caso contrario indicar la extension y mostrar mensaje de error
# Comprobar el numero y tipo de columnas que contiene el archivo
# - En caso de no contener filas y columnas indicar error
# - En caso de no contener almenos una variable numerica indicar error (debe contenerla para generar al menos un grafico)
# - En caso de no contener almenos una variable no numerica indicar error (debe contenerla para mostrar los filtros)
# Repetir el proceso con el segundo archivo
# Una vez validados se debe confirmar que el numero de filas del archivo 1 es igual al numero de columnas del archivo 2
# - En caso contrario mostrar error

######################################################################################
" COMPROBAR COMPATIBILIDAD ENTRE AMBOS ARCHIVOS "
" Comprobar numero y tipo de columnas para generar filtro dinámico"
# eventList output
output$parameter1Type <- renderUI({
  if(is.null(v$plotFilters))
    return()
  selectInput(
    inputId = "parameter1",
    label = "Parameter 1",
    choices = rownames(v$plotFilters),
    width = 400
  )
})

output$parameter1Choices <- renderUI({
  if(is.null(input$parameter1))
    return()
  if(v$plotFilters[input$parameter1,] == "integer") {
    choices <- c("All", c("1","2"))
    tabPanel(
      id = paste0("tabPanel", i),
      title=rownames(v$plotFilters)[i],
      radioButtons(
        inputId = "parameter1Data", 
        label = "",
        choices = choices,
        selected = "All"
      )
    )
  } 
  else {
    choices <- c("All", as.character(levels(v$clinData[,input$parameter1])))
    tabPanel(
      title=input$parameter1,
      radioButtons(
        inputId = "parameter1Data", 
        label = "",
        choices = choices,
        selected = "All"
      )
    )
  }
})

observeEvent(c(input$parameter1, input$parameter1Data), {
  v$parameter1 <- c(input$parameter1, input$parameter1Data)
})



observeEvent(input$addParameter, {
  v$aux <- c(v$aux, selectInput(
    inputId = paste0("otherParameters", length(v$aux) +2),
    label = paste0("Parameter ", length(v$aux) +2),
    choices = rownames(v$plotFilters)
  ))
})

output$otherParameters <- renderUI({

    v$aux

  
})


observeEvent(input$removeParameter, {
  v$aux <- head(v$aux, length(v$aux) -3)
})


observeEvent(input$parameter1, {
  
  # Cada vez que se seleccione una opción quitarla del auxiliar
  
})


output$otherParametersChoices <- renderUI({
  
  aux <- v$plotFilters
  
  aux <- as.data.frame("X_gender", "male")
  
  
  if(is.null(aux))
    return()
  #### GENERAR TANTOS PANELES COMO COLUMNAS ####
  do.call(tabsetPanel, c(id='t',lapply(1:nrow(aux) , function(i) {
    # Paste option "All"
    id = paste0("tab", i)
    widtith = 6
    if(aux[i] == "integer") {
      choices <- c("All", c("1","2"))
      tabPanel(
        id = paste0("tabPanel", i),
        #title=paste0(rownames(v$plotFilters)[i], i), 
        title=rownames(aux)[i],
        radioButtons(
          inputId = rownames(aux)[i], 
          label = "",
          choices = choices,
          selected = "All"
        )
      )
    } else {
      choices <- c("All", as.character(levels(v$clinData[,i+1])))
      tabPanel(
        title=rownames(aux)[i],
        radioButtons(
          inputId = rownames(aux)[i], 
          label = "",
          choices = choices,
          selected = "All"
        )
      )
    }
  })))
})

lapply(
  X = 1:50,
  FUN = function(i){
    observeEvent(input[[paste0("otherParameters", i)]], {
      cat(paste0("\ninput$", paste0("otherParameters", i,"\n")))
      cat(input[[paste0("otherParameters", i)]])
      
      # Cada vez que se seleccione una opción quitarla del auxiliar
      
      
    })
  }
)



observeEvent(v$plotFilters, {
  v$listFilters <- c()
  for (i in 1:nrow(v$plotFilters)){
    if(v$plotFilters[i] == "integer") {
      #v$listFilters <- paste0(v$listFilters, ", ", paste0("input$" ,rownames(v$plotFilters)[i]))
      v$listFilters <- c(v$listFilters, paste0("input$" ,rownames(v$plotFilters)[i]))
    } else {
      #v$listFilters <- paste0(v$listFilters, ", ", paste0("input$" ,rownames(v$plotFilters)[i]))
      v$listFilters <- c(v$listFilters, paste0("input$" ,rownames(v$plotFilters)[i]))
    }
  }
  #v$listFilters <- substr(v$listFilters, 3, nchar(v$listFilters))
  #v$listFilters <- as.formula(v$listFilters)
  #cat(class(v$listFilters[i]))
  #cat(length(v$listFilters[i]))
  #cat(v$listFilters[2])
})
######################################################################################
observeEvent(input$goButton, { #cat(v$listFilters)
  #cat("PLOT FILTERS\n")
  #cat(class(input$PD.R.GO))
  #  filterDF <- DataFrame()
  #  for (i in 2:nrow(v$plotFilters)){
  #    if(v$plotFilters[i] == "integer") {
  #      filterDF[i-1,1] <- rownames(v$plotFilters)[i]
  #      filterDF[i-1,2] <- v$plotFilters[i]
  #      filterDF[i-1,3] <- input[[rownames(v$plotFilters)[i]]]
  #    } else {
  #      filterDF[i-1,1] <- rownames(v$plotFilters)[i]
  #      filterDF[i-1,2] <- v$plotFilters[i]
  #      filterDF[i-1,3] <- input[[rownames(v$plotFilters)[i]]]
  #      
  #    }
  #  }
  
  #colnames(filterDF) <- c("ID", "CLASS", "INPUT")
  #write.table(filterDF)
})
######################################################################################
" ACTIVAR BOTON UPLOAD Y SUBIR FICHEROS"
" Cambiar todo para funcionar con variables dinamicas "
observeEvent(c(v$clinDataFile,v$patDataFile), {
  if(!is.null(v$patDataFile) && !is.null(v$clinDataFile)){
    shinyjs::enable("uploadButton")
  }
})
observeEvent(input$uploadButton, {
  v$patData <- v$patDataFile
  v$clinData <- v$clinDataFile
  #cat(rownames(v$clinData))
  v$plotFilters <- t(as.data.frame(lapply(v$clinData, class)))
  # Close modal window when user click on "uploadButton"
  toggleModal(session, "uploadFil", toggle = "toggle")
})
######################################################################################

#input[[rownames(v$plotFilters)[2]]]


vals <- reactiveValues()

lapply(
  X = 1:50,
  FUN = function(i){
    observeEvent(input[[rownames(v$plotFilters)[i]]], {
      #vals[[paste0("val", i)]] <- input[[rownames(v$plotFilters)[i]]]
      #cat(paste0("input$", rownames(v$plotFilters)[i]))
    })
  }
)

lapply(
  X = 1:50,
  FUN = function(i){
    observeEvent(input[[paste0("tab", i)]], {
      cat(paste0("input$", paste0("tab", i)))
    })
  }
)



observeEvent(input$t, {
  cat("\nEntra\n")
  cat(input$t)
})





