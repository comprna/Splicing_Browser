patientData <- events_filter_all
clinicalData <- info_clin_all

#colnames(clinicalData)[1] <- "Sample.ID"
#rownames(patientData) <- patientData[,1]
#patientData[,1] <- NULL
#patientData <- patientData[ , order(names(patientData))]
#clinicalData <- clinicalData[order(clinicalData$Sample.ID),]

nrow(clinicalData)
ncol(patientData)

#clinicalData <- head(clinicalData,7863)
#colnames(patientData) <- clinicalData$Sample.ID

class(colnames(patientData))
class(clinicalData$Sample.ID)
a <- data.frame(lapply(clinicalData, as.character), stringsAsFactors=FALSE)

clinicalData$Sample.ID <- as.character(clinicalData$Sample.ID)

# Search event in every row and put into data frame
aux <- events_filter_all[grep(paste("\\","ENSG00000000003","\\b",sep=""), rownames(events_filter_all)),]
# Search columns into info.vh

drop.levels(factor(colnames(patientData)))

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
  df.outPut <-rbind(
    df.outPut, 
    t(rbind(
      Patient, 
      Event, 
      PSI
    ))
  )
}

clinicalData <- do.call("rbind", replicate(nrow(patientData), clinicalData, simplify = FALSE))
clinicalData <- clinicalData[order(clinicalData$Sample.ID),] 
aux <- cbind(as.data.frame(df.outPut), clinicalData)

# Convert matrix to dataframe
df.outPut <- as.data.frame(df.outPut)



aux <- as.data.frame(plotFilters)

parameter1="X_gender"




aux[parameter1,]


if(plotFilters[parameter1] == "integer") {
  choices <- c("All", c("1","2"))
  tabPanel(
    id = paste0("tabPanel", i),
    #title=paste0(rownames(v$plotFilters)[i], i), 
    title=rownames(v$plotFilters)[i],
    radioButtons(
      inputId = rownames(v$plotFilters)[i], 
      label = "",
      choices = choices,
      selected = "All"
    )
  )
} 


parameter1[1] <- "X_gender"
parameter1[2] <- "male"


aux1 <- aux[parameter1[2] == aux[[parameter1[1]]],]
cat(data[[parameter1[1]]])



aux$X_gender == "male"



########################################################################################


# The object was created from the downloaded Ensembl file as follows
## Not run:
source("https://bioconductor.org/biocLite.R")
biocLite("GenomicFeatures")
library(GenomicFeatures)
ensGTF <- importGTF(file="files/gtf/hg19_ensembl_events_A3_strict.gtf")
annotTrack <- ensGTF[1:1000,]
save(annotTrack,file="annotTrack.rda")
## End(Not run)
data(annotTrack)
annotTrack


