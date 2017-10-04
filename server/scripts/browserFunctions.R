"  
  Author: Hèctor Garcia Guillen
  Name: browserFunctions.R 
  Description: contain all functions required to run genome browser function
  Date: 17/06/07
  Version: 1.1
"

########################################################################################

"
  Name: plot_genome
  Description: 
  Date: 17/06/07
  Version: 1.1
  Param: location, contains the location to search in
  Return: plotTracks, graph of the indicated genomic region
"

plot_genome <- function(trackList, location) {
  # Create ideogram and add it to trackList
  ideogram_track <- IdeogramTrack(genome="hg19", chromosome=as.character(location@seqnames@values))
  trackList <- c(ideogram_track, trackList)
  # Calculate number of tracks and assign its' height sizes
  size <- NULL
  for (i in 1:length(trackList)) {
    if(i <=3){
      size <- c(size, 1)
    } else {
      size <- c(size, 8)
    }
  }
  sizes <- size
  # Print tracks
  plotTracks(
    trackList = trackList, 
    sizes=sizes,
    chromosome=as.character(seqnames(location)),
    from=start(location), 
    to=end(location),
    showId=T,
    transcriptAnnotation="gene",
    #extend.left = 4500,
    #extend.right =  4500,
    #shape = "chevron",
    showOverplotting = T
  )
}

########################################################################################

"
  Name: getGeneIdFromLoc
  Description: calculates the ID of genes within a range (location).
  Date: 17/06/07
  Version: 1.1
  Param: location, contains the location to search in
  Return: character vector with all related Gene.Stable.ID
"

getGeneIdFromLoc <- function (location) {

  # Get data from location
  chr <- as.character(strsplit(location, ":")[[1]])[1]
  start <- strsplit(strsplit(location, ":")[[1]][2], "-")[[1]][1]
  end <- strsplit(strsplit(location, ":")[[1]][2], "-")[[1]][2]
  
  # Collect all the data that match the inserted coromosoma in the location
  coincidences <- events_filter_all[grep(paste("\\",chr,"\\b",sep=""), rownames(events_filter_all)),]
  
  # Generate dataframe to split event start and event end from location
  coincidences <- data.frame(
    Event = rownames(coincidences),
    Gene.Stable.ID = substr(rownames(coincidences),1 , 15),
    Type = substr(rownames(coincidences),17 , 18),
    Location = substr(rownames(coincidences),25, nchar(rownames(coincidences))-2),
    Start = as.numeric(gsub("^(.*?)-.*", "\\1", gsub( ":", "-", substr(rownames(coincidences),25, nchar(rownames(coincidences))-2)))),
    End = as.numeric(reverse(gsub("^(.*?)-.*", "\\1", gsub( ":", "-", reverse(substr(rownames(coincidences),25, nchar(rownames(coincidences))-2)))))),
    Strand = substr(rownames(coincidences),nchar(rownames(coincidences)), nchar(rownames(coincidences)))
  )
  
  # Search events tirhin a location
  # Filters the results with a start higher or equal than introduced in location
  coincidences <- coincidences[coincidences$Start>=start,]
  # Filters the results with a end lower or equal than introduced in location
  coincidences <- coincidences[coincidences$End<=end,]
  
  # Return list of Gene.Stable.ID related to input location
  return(unique(as.character(coincidences$Gene.Stable.ID)))
  
}

########################################################################################

"
  Name: getGeneLoc
  Description: contain all server code
  Date: 17/06/07
  Version: 1.1
  Param: genesStableId
  Return: loc
"

getGeneLoc <- function (genesStableId) {
  loc <- as.data.frame(t(as.data.frame(strsplit(ENSEMBLEgenes_df[grep(paste(genesStableId, collapse = "|"), ENSEMBLEgenes_df$gene),]$location, ":"))))
  colnames(loc) <- c("chr", "loc", "strand")
  rownames(loc) <- NULL
  e <- read.table(text = as.character(loc$loc), sep = "-", colClasses = "character", col.names = c("start", "end"))
  loc$loc <- NULL
  loc$start <- as.numeric(e$start)
  loc$end <- as.numeric(e$end)
  chr <- unique(loc$chr)
  start <- min(loc$start)
  end <- min(loc$end)
  strand <- loc$strand[1]
  loc <- paste(
    c(
      as.character(chr), 
      paste(c(as.character(start),as.character(end)), collapse = "-"),
      as.character(strand)
    ), 
    collapse = ":"
  )
  return(as.character(loc))
}

#####################################################################################

"
  Name: addTrack
  Description: 
  Date: 17/06/07
  Version: 1.1
  Param:  trackList,
          newTrack,
  Return: trackList,
"

addTrack <- function(trackList, newTrack) {
  switch(newTrack,
         ucsc = trackList <- c(trackList, ucsc = grtrackUCSC),
         ensemble = trackList <- c(trackList, ensemble =grtrackENSEMBLE)
  )
  return(trackList)
}

#####################################################################################

"
  Name: removeTrack
  Description: 
  Date: 17/06/07
  Version: 1.1
  Param:  trackList,
          deletedTrack,
          typeOfTrack,
  Return: result,
"

removeTrack <- function(trackList, deletedTrack) {
  # If trackList lenght is lower than 1 can't delete any track
  if(length(trackList)>1){
    if(deletedTrack %in% names(trackList)){
      trackList[which(names(trackList) %nin% deletedTrack)] = NULL
    }
  }
  return(trackList)
}

#####################################################################################

