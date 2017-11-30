"  
  Author: Hèctor Garcia Guillen
  Name: generalFunctions.R 
  Description: contain all functions required to run app
  Date: 17/06/07
  Version: 1.1
"

######################################################################################

"
  Name: eventNumberDF
  Description: generate dataftame that contain all number of events per Gene.stable.ID.
  Date: 17/06/07
  Version: 1.1
  Param: inputDF contain events to check it
  Return: dataframe with all Gene.stable.ID repetitions
"
eventNumberDF <- function (inputDF) {
  aux <- inputDF %>% 
    group_by(Gene.stable.ID) %>% 
    count(Gene.stable.ID)
  return(aux)
}

######################################################################################

"
  Name: eventNumber
  Description: Return number of events per Gene.stable.ID. Always prepare data to GENE.STABLE.ID
  Date: 17/06/07
  Version: 1.1
  Param: inputDF, 
         gene, 
         typeSearch, 
  Return: result, 
"
eventNumber <- function (inputDF, gene, typeSearch) {
  result <- NULL;
  if (typeSearch == "GeneStableId") result <- as.numeric(eventFrequencyStableId[eventFrequencyStableId$Gene.stable.ID == gene, 'n'])
  return(result)
}

######################################################################################

"
  Name: getGeneStableId
  Description: search all related gene.stable.id to given inputData
  Date: 17/06/07
  Version: 1.1
  Param: geneName to check
  Return: result, the gene stable id to given gene.name
"
getGeneStableId <- function (inputData) {
  result <- inputData
  if(grepl(pattern = "chr[0-9]{1,2}|X|Y|MT", x = inputData)==T){
    result <- getGeneIdFromLoc(inputData)
  } else if(grepl(pattern = "ENSG[0-9]{11}", x = inputData)==F){
    result <- mart_export[mart_export$Gene.name==inputData, 'Gene.stable.ID']
  }
  return(as.character(result))
}
