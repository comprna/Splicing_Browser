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
  aux <- NULL
  # Search event in every row and put into data frame
  patientData <- patientData[grep(paste("\\",gene,"\\b",sep=""), rownames(patientData)),]
  # Search columns into info.vh
  clinicalData <- clinicalData[clinicalData$Sample.ID %in% colnames(patientData),]
  patientData <- patientData[, colnames(patientData) %in% clinicalData$Sample.ID]
  # Files must to be the same lenght and the same order:
  patientData <- patientData[,order(colnames(patientData))]
  clinicalData <- clinicalData[order(clinicalData$Sample.ID),]
  # Create DataFrames to obtain all data
  df.outPut<-NULL;
  for (i in 1:nrow(patientData)) {
    Patient <- colnames(patientData)
    Event <- rep(rownames(patientData)[i],ncol(patientData))
    PSI <- as.numeric(as.matrix(patientData[i,]))
    df.outPut <-rbind(df.outPut, t(rbind(Patient, Event, PSI)))
  }
  clinicalData <- do.call("rbind", replicate(nrow(patientData), clinicalData, simplify = FALSE))
  clinicalData <- clinicalData[order(clinicalData$Sample.ID),]
  aux <- cbind(as.data.frame(df.outPut), clinicalData)
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

generateMultiPlot <- function (data, parameter1, otherParameters) {

  if(parameter1[2]!="All"){
    data <- data[parameter1[2] == data[[parameter1[1]]],]
    cat(data[[parameter1[1]]])
  }
  
  #if(!is.null(otherParameters)){
  #  for(i in 1:nrow(otherParameters)){
  #    
  #  }
  #  
  #}
  
  #if(is.null(type)) return()
  p <- plot_ly(
    data = data, 
    x=data$Event, 
    y=as.numeric(as.character(data$PSI)),  
    split = factor(data[[parameter1[1]]]),
    type = "box",
    boxpoints = "all",
    marker=list( size=5 , opacity=0.5), 
    color = factor(data[[parameter1[1]]]),
    colors = c("#298A08","#2E64FE","#DF0101")
  )
  p <- layout(
    p = p,
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
