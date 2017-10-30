
gene <- "ENSG00000000460"
Gene.stable.ID <- "ENSG00000000460"

########################################################################################

prepareDataToPlot <- function (gene) {
  # Search event in every row and put into data frame
  prueba <- events_filter_all[grep(paste("\\",gene,"\\b",sep=""), rownames(events_filter_all)),]
  # Search all patients
  info.vh <- info_clin_all
  # Search columns into info.vh
  info.vh <- info.vh[info.vh$Sample.ID %in% colnames(prueba),]
  #Filest must to be the same lenght and the same order: 
  s.prueba <- prueba[,order(colnames(prueba))]
  s.info.vh <- info.vh[order(info.vh$Sample.ID),]
  s.info.vh$PD.R.GO
  aux <- aux[s.info.vh$PD.R.GO=="R",]
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
  # Assign name for each column
  df.outPut <- as.data.frame(df.outPut)
  return(df.outPut)
}

########################################################################################

generateMultiPlot <- function (data, dataType, patientType) {
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
    } else if (dataType=="OUTCOME+"){
      aes <- aes(colour=Outcome,x=factor(Event), y=as.numeric(as.character(PSI)))
    } else if (dataType=="MICROSTAGING_AB"){
      aes <- aes(colour=MICROSTAGING_AB,x=factor(Event), y=as.numeric(as.character(PSI)))
    } else if (dataType=="MICROPAPILARYP_ATTERN"){
      aes <- aes(colour=MICROPAPILARYP_ATTERN,x=factor(Event), y=as.numeric(as.character(PSI)))
    } else {
      aes <- aes(colour=Outcome,x=factor(Event), y=as.numeric(as.character(PSI)))
    }
  }
  return(
    ggplot(
      data = as.data.frame(data), 
      aes
    ) + xlab(label = "Events") +
      ylab(label = "PSI") +
      geom_boxplot(outlier.shape=NA) + 
      geom_point(position=position_jitterdodge()) + 
      facet_wrap( ~ Event, scales="free") +
      coord_cartesian(ylim = c(0, 1)) + 
      theme(
        axis.text.x=element_blank(), 
        axis.line = element_line(colour = "black", size = 0.1, linetype = "solid")
      )
  )
}

ggplotly(generateMultiPlot(prepareDataToPlot(gene) , "OUTCOME", "All"))
ggplotly(p)

#######################################################################################


