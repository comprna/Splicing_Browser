"  
  Author: Hèctor Garcia Guillen
  Name: ggplotFunctions.R 
  Description: contain all functions required to run multiplot function
  Date: 17/06/07
  Version: 1.1
"

######################################################################################

"
  Name: prepareDataToPlot
  Description: prepare data prevously run function generateMultiPlot. 
               Always prepare data to GENE.STABLE.ID
  Date: 17/06/07
  Version: 1.1
  Param: gene, GENE.STABLE.ID to execute multiplot.
  Return: df.outPut, contain all neccesary data to generate multiplot with ggplot or plotly
"
prepareDataToPlot <- function (gene) {
  # Search event in every row and put into data frame
  prueba <- events_filter_all[grep(paste("\\",gene,"\\b",sep=""), rownames(events_filter_all)),]
  # Search all patients
  info.vh <- info_clin_all
  # Search columns into info.vh
  info.vh <- info.vh[info.vh$Sample.ID %in% colnames(prueba),]
  # Files must to be the same lenght and the same order: 
  s.prueba <- prueba[,order(colnames(prueba))]
  s.info.vh <- info.vh[order(info.vh$Sample.ID),]
  # Create DataFrames to obtain all data
  df.outPut<-NULL;
  for (i in 1:nrow(s.prueba)) {
    Patient <- colnames(s.prueba)
    Outcome <- as.character(s.info.vh$PD.R.GO)
    OutcomePlus <- as.character(s.info.vh$PD.R.GO)
    MICROSTAGING_AB <- as.numeric(s.info.vh$MICROSTAGING_AB)
    MICROPAPILARYP_ATTERN <- as.numeric(s.info.vh$MICROPAPILARYP_ATTERN)
    Event <- rep(rownames(s.prueba)[i],ncol(s.prueba))
    PSI <- as.numeric(as.matrix(s.prueba[i,]))
    PATIENT_TYPE <- substr(colnames(s.prueba),1,2)
    df.outPut <-rbind(df.outPut, t(rbind(Patient, Outcome, MICROSTAGING_AB, MICROPAPILARYP_ATTERN, Event, PSI, PATIENT_TYPE)))
  }
  # Convert matrix to dataframe
  df.outPut <- as.data.frame(df.outPut)
  # Sort Outcome
  df.outPut$Outcome <- factor(df.outPut$Outcome, levels = c("GO","R","PD"))
  return(df.outPut)
}

######################################################################################

"
  Name: generateMultiPlot
  Description: Generate multiplot with ggplot or plotly. Uncomment to try other
               The main variable on which the graph is made is the PSI
               It allows to filter data by:
                  - Patient type
                      - VH
                      - MP
                  - Data type
                      - Outcome
                      - MICROSTAGING_AB
                      - MICROPAPILARYP_ATTERN
  Date: 17/06/07
  Version: 1.1
  Param: data, contain all data
         dataType, contain the search filters to data type
         patientType, contain the search filters to type of patient
  Return: p, ggplot or plotly graph
"

generateMultiPlot <- function (data, dataType, patientType) {
  data<-droplevels(data)
  # PatientType filter
  patientType <- toupper(patientType)
  if(!is.null(patientType)){
    if(patientType=="VH"){
      data <- data[grep("vh",data$PATIENT_TYPE),]
    } else if (patientType=="MP"){
      data <- data[grep("MP",data$PATIENT_TYPE),]
    } 
  }
  
  # DataType filters
  dataType <- toupper(dataType)
  if(!is.null(dataType)){
    if(dataType=="OUTCOME"){
      aes <- aes(colour=Outcome,x=factor(Event), y=as.numeric(as.character(PSI)))
      splitData <- data$Outcome
    } else if (dataType=="MICROSTAGING_AB"){
      aes <- aes(colour=MICROSTAGING_AB,x=factor(Event), y=as.numeric(as.character(PSI)))
      splitData <- data$MICROSTAGING_AB
    } else if (dataType=="MICROPAPILARYP_ATTERN"){
      aes <- aes(colour=MICROPAPILARYP_ATTERN,x=factor(Event), y=as.numeric(as.character(PSI)))
      splitData <- data$MICROPAPILARYP_ATTERN
    } else {
      aes <- aes(colour=Outcome,x=factor(Event), y=as.numeric(as.character(PSI)))
      splitData <- data$Outcome
    }
  }
  head(iris)
  p <- plot_ly(data, 
               x=data$Event, 
               y=as.numeric(as.character(data$PSI)),  
               split = splitData,
               type = "box",
               boxpoints = "all",
               marker=list( size=5 , opacity=0.5), 
               color = splitData,
               colors = c("#298A08","#2E64FE","#DF0101")
  )
  p <- layout(p = p,
              boxmode = "group",
              hovermode= data$PSI,
              #height = "1200px",
              xaxis = list(
                title = "Event",
                autotick = FALSE,
                #dtick = 6,
                tickcolor = toRGB("black")
              ), 
              yaxis = list(
                title = "PSI",
                autotick = FALSE,
                ticks = "outside",
                tick0 = 0,
                dtick = 0.2,
                ticklen = 0.5,
                tickwidth = 0.2,
                tickcolor = toRGB("black")
              )
  )
  return(p)
}

######################################################################################
