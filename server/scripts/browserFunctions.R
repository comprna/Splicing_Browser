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

plot_genome <- function(trackList, location, gtf) {
  beginning <- Sys.time()
  # Create ideogram and add it to trackList
  gtf <- gtf[gtf$start>=start(location) & gtf$end<=end(location),]
  ideogramTrack <- IdeogramTrack(
    genome="hg19", 
    chromosome=as.character(location@seqnames@values)
  )
  
  #axisTrack <- GenomeAxisTrack(
  #  add53 = T, 
  #  add35 = T#, 
  #  #littleTicks = T
  #)
  #seqTrack <- SequenceTrack(
  #  genome = "hg38"
  #)
  #gtfTrack  <- GeneRegionTrack(
  #  gtf, 
  #  name="GTF", 
  #  showId=T,
  #  chromosome = as.character(seqnames(location)),
  #  start = start(location),
  #  end = end(location)
  #)
  #bioMartTrack <- BiomartGeneRegionTrack(
  #  genome = "hg38", 
  #  name = "ENSEMBL GRCh38.p7", 
  #  biomart=useMart(biomart='ENSEMBL_MART_ENSEMBL',dataset='hsapiens_gene_ensembl'),
  #  gene = "ENSG00000000003"
  #)
  
  if(nrow(gtf)>1) {
    gtfTrack  <- GeneRegionTrack(
      gtf, 
      name="GTF", 
      chromosome = gtf$seqid,
      start = start(location),
      end = end(location),
      strand = gtf$strand,
      transcript = gtf$transcript_id,
      gene = gtf$transcript_id,
      fill="lightblue"
    )
    trackList <- c(ideogramTrack, axisTrack, seqTrack, gtfTrack, grtrackEnsembl)
  } else {
    trackList <- c(ideogramTrack, axisTrack, seqTrack, grtrackEnsembl)
  }

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
    from=start(location), 
    to=end(location),
    showId=T,
    transcriptAnnotation="gene",
    showOverplotting = T,
    groupAnnotation = "group",
    #just.group = "left", # left, right, above or below.
    fontcolor.group="black",
    fontcolor.item="blue",
    col.line="black",
    cex.group=0.8,
    cex=1.1,
    lex=3,
    lineheight=1,
    lty="solid",
    lwd=1,
    mergeGroups=F
  )
  end <- Sys.time()
  print(paste0("Plot genome: ",end - beginning, " sec"))
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
  loc <- as.data.frame(t(as.data.frame(strsplit(as.character(ENSEMBLEgenes_df[grep(paste(genesStableId, collapse = "|"), ENSEMBLEgenes_df$gene),]$location),":"))))
  
  #loc <- as.data.frame(t(as.data.frame(strsplit(ENSEMBLEgenes_df[grep(paste(genesStableId, collapse = "|"), ENSEMBLEgenes_df$gene),]$location, ":"))))
  
  colnames(loc) <- c("chr", "loc", "strand")
  rownames(loc) <- NULL
  e <- read.table(text = as.character(loc$loc), sep = "-", colClasses = "character", col.names = c("start", "end"))
  loc$loc <- NULL
  loc$start <- as.numeric(e$start)
  loc$end <- as.numeric(e$end)
  chr <- unique(loc$chr)
  #chr <- paste0("chr",unique(loc$chr))
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

