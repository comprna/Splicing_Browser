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
prepareDataToPlot <- function (patientData, clinicalData, gene) {
  beginning <- Sys.time()
  aux <- NULL
  # Search event in every row and put into data frame
  patientData <- patientData[grep(paste("\\",gene,"\\b",sep=""), rownames(patientData)),]
  # Search columns into info.vh
  clinicalData <- clinicalData[clinicalData$sample_id %in% colnames(patientData),]
  patientData <- patientData[, colnames(patientData) %in% clinicalData$sample_id]
  # Files must to be the same lenght and the same order:
  patientData <- patientData[,order(colnames(patientData))]
  clinicalData <- clinicalData[order(clinicalData$sample_id),]
  # Create DataFrames to obtain all data
  df.outPut<-NULL;
  for (i in 1:nrow(patientData)) {
    Patient <- colnames(patientData)
    Event <- rep(rownames(patientData)[i],ncol(patientData))
    PSI <- as.numeric(as.matrix(patientData[i,]))
    df.outPut <-rbind(df.outPut, t(rbind(Patient, Event, PSI)))
  }
  clinicalData <- do.call("rbind", replicate(nrow(patientData), clinicalData, simplify = FALSE))
  clinicalData <- clinicalData[order(clinicalData$sample_id),]
  aux <- cbind(as.data.frame(df.outPut), clinicalData)
  end <- Sys.time()
  print(paste0("Prepare data: ",end - beginning, " sec"))
  return(aux)
}

######################################################################################

"
  Name: generateMultiPlot
  Description: Generate multiplot with ggplot or plotly. Uncomment to try other
               The main variable on which the graph is made is the PSI
               It allows to filter data 
  Date: 17/06/07
  Version: 1.1
  Param: data, contain all data
         dataType, contain the search filters to data type
         patientType, contain the search filters to type of patient
  Return: p, ggplot or plotly graph
"

generateMultiPlot <- function (data, clinData, parameters, parameterListAux) {
  beginning <- Sys.time()
  
  n <- which(colnames(clinData)==parameterListAux[1])
  firstParam <- parameters[n-1,1]
  if(is.null(firstParam)) return()
  
  for(i in 1:nrow(parameters)){
    if(!is.na(as.character(parameters[i,2])) && parameters[i,2] != ""){
      param <- parameters[i,1]
      values <- unlist(strsplit(parameters[i,2]," "))
      data <- data[data[[param]] %in% values,]
      #cat(paste0("\nnum points", nrow(data)))
    }
  }
  #cat(paste0("\nfirstParam = ", firstParam))
  v$preparedData <- data
  a <- factor(data[[firstParam]])
  p <- plot_ly(
    data = data, 
    x=data$Event, 
    y=as.numeric(as.character(data$PSI)),
    split = a,
    type = "box",
    boxpoints = "all",
    marker=list( size=2.5 , opacity=1),
    color = a,
    colors = c("#298A08","#2E64FE","#DF0101")
  ) 
  p <- layout(
    p = p,
    boxmode = "group",
    showlegend = T,
    hovermode= data$PSI,
    xaxis = list(
      title = "Event",
      autotick = T,
      ticks = "",
      tickcolor = toRGB("black")
    ), 
    yaxis = list(
      title = "PSI",
      range = c("0","1"),
      autotick = T,
      ticks = "outside",
      tickcolor = toRGB("black")
    )
  )
  end <- Sys.time()
  print(paste0("Plot psi: ",end - beginning, " sec   ---   ", paste0("num points = ", nrow(data))))
  return(p)
}

######################################################################################
