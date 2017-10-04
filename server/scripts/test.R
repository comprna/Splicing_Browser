
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

#biocLite("plotSingleChrom")
library(coMET)
library("Gviz")
gen <- "hg38"
chr <- "chr7"
start <- 38290160
end <- 38303219

if(interactive()) {
  genesUcsctrack<-knownGenes_UCSC(gen,chr,start,end,showId=TRUE)
  plotTracks(genesUcsctrack, from = start, to =end)
}else {
  data(genesUcsctrack)
  plotTracks(genesUcsctrack, from = start, to =end)
}

#######################################################################################

Location <- substring(rownames(events_filter_all), 20)

Event <- substring(rownames(events_filter_all), 15, first = T)

eventLocation <- data.frame(Event,Location)

eventLocation


library(Gviz)
cTrack <- CustomTrack(plottingFunction = function(GdObject, prepare) {
  if(!prepare) grid.text("customtrack"); 
  return(invisible(GdObject))
})

gtrack <- GenomeAxisTrack()                          
plotTracks(c(cTrack, gtrack),  from = 1, to = 100) ##Plotting two tracks

library("GenomicFeatures")
library("Gviz")

chrominfo <- data.frame(chrom=c("chr1","chr2","chr3","chr4","chr5","chr6","chr7","chr8","chr9","chr10","chr11","chr12","chr13","chr14","chr15","chr16","chr17","chr18","chr19","chr20","chr21","chr22","chrX","chrY"), 
                        length=c(249250621,243199373,198022430,191154276,180915260,171115067,159138663,146364022,141213431,135534747,135006516,133851895,115204052,107349540,102531392,90354753,81195408,78077248,59128983,63025520,48129895,51304566,155270560,59373566), 
                        is_circular=rep(FALSE,24)
) #add chrM if your GTF has chrM

txDb.mi <- makeTranscriptDbFromGFF(
  file="mitranscriptome.v2.gtf", 
  format="gtf", 
  dataSource="mitranscriptome", 
  species="Homo sapiens", 
  chrominfo=chrominfo, 
  exonRankAttributeName="exon_number"
)

makeGRangesFromDataFrame(eventLocation)

makeTxDbFromGRanges(gr)

########################################################################################

## ---------------------------------------------------------------------
## BASIC EXAMPLES
## ---------------------------------------------------------------------

df <- data.frame(chr="chr1", start=11:15, end=12:16,
                 strand=c("+","-","+","*","."), score=1:5)
df
makeGRangesFromDataFrame(df)  # strand value "." is replaced with "*"

## The strand column is optional:
df <- data.frame(chr="chr1", start=11:15, end=12:16, score=1:5)
makeGRangesFromDataFrame(df)

gr <- makeGRangesFromDataFrame(df, keep.extra.columns=TRUE)
gr2 <- as(df, "GRanges")  # equivalent to the above
stopifnot(identical(gr, gr2))
gr2 <- GRanges(df)        # equivalent to the above
stopifnot(identical(gr, gr2))

makeGRangesFromDataFrame(df, ignore.strand=TRUE)
makeGRangesFromDataFrame(df, keep.extra.columns=TRUE,
                         ignore.strand=TRUE)

makeGRangesFromDataFrame(df, seqinfo=paste0("chr", 4:1))
makeGRangesFromDataFrame(df, seqinfo=c(chrM=NA, chr1=500, chrX=100))
makeGRangesFromDataFrame(df, seqinfo=Seqinfo(paste0("chr", 4:1)))

## ---------------------------------------------------------------------
## ABOUT AUTOMATIC DETECTION OF THE seqnames/start/end/strand COLUMNS
## ---------------------------------------------------------------------

## Automatic detection of the seqnames/start/end/strand columns is
## case insensitive:
df <- data.frame(ChRoM="chr1", StarT=11:15, stoP=12:16,
                 STRAND=c("+","-","+","*","."), score=1:5)
makeGRangesFromDataFrame(df)

## It also ignores a common prefix between the start and end columns:
df <- data.frame(seqnames="chr1", tx_start=11:15, tx_end=12:16,
                 strand=c("+","-","+","*","."), score=1:5)
makeGRangesFromDataFrame(df)

## The common prefix between the start and end columns is used to
## disambiguate between more than one seqnames column:
df <- data.frame(chrom="chr1", tx_start=11:15, tx_end=12:16,
                 tx_chr="chr2", score=1:5)
makeGRangesFromDataFrame(df)

## ---------------------------------------------------------------------
## 0-BASED VS 1-BASED START POSITIONS
## ---------------------------------------------------------------------

if (require(rtracklayer)) {
  session <- browserSession()
  genome(session) <- "sacCer2"
  query <- ucscTableQuery(session, "Most Conserved")
  df <- getTable(query)
  
  ## A common pitfall is to forget that the UCSC Table Browser uses the
  ## "0-based start" convention:
  gr0 <- makeGRangesFromDataFrame(df, keep.extra.columns=TRUE)
  head(gr0)
  min(start(gr0))
  
  ## The start positions need to be converted into 1-based positions,
  ## to adhere to the convention used in Bioconductor:
  gr1 <- makeGRangesFromDataFrame(df, keep.extra.columns=TRUE,
                                  starts.in.df.are.0based=TRUE)
  head(gr1)
}

########################################################################################

library("GenomicFeatures")
library("Gviz")

chrominfo <- data.frame(chrom=c("chr1","chr2","chr3","chr4","chr5","chr6","chr7","chr8","chr9","chr10","chr11","chr12","chr13","chr14","chr15","chr16","chr17","chr18","chr19","chr20","chr21","chr22","chrX","chrY"), length=c(249250621,243199373,198022430,191154276,180915260,171115067,159138663,146364022,141213431,135534747,135006516,133851895,115204052,107349540,102531392,90354753,81195408,78077248,59128983,63025520,48129895,51304566,155270560,59373566), is_circular=rep(FALSE,24)) #add chrM if your GTF has chrM
txDb.mi <- makeTranscriptDbFromGFF(file="mitranscriptome.v2.gtf", format="gtf", dataSource="mitranscriptome", species="Homo sapiens", chrominfo=chrominfo, exonRankAttributeName="exon_number")

gtrack <- GenomeAxisTrack()

#dtrack <- DataTrack(range="mysample.bam", genome="hg19", name="Coverage", chromosome="chr19",type = "histogram", col.histogram= "#377EB8", fill="#377EB8")

itrack <- IdeogramTrack(genome="hg19",chromosome="chr19") #requires internet connection

grtrack <- GeneRegionTrack(txDb.mi, genome = "hg19", chromosome="chr19", name="miTranscriptome")

plotTracks(
  list(itrack, gtrack, grtrack),
  from = 41959684, to = 42007362)


########################################################################################

Loc <- as.data.table(substring(rownames(events_filter_all), 20))

Loc <- split(Loc, ':')

xx <- Loc

require(data.table) ## v1.9.6+
xx<-NULL
xx[, c("Chr", "Start", "End", "Strand", "Score", "f") := tstrsplit(Loc$V1, ":", fixed=TRUE)]

########################################################################################

#### FUNCIONA

#biocLite("genesymbol")
library(ggbio)
library(Homo.sapiens)
#load gene symbol : GRanges, one gene/row
data(genesymbol, package = "biovizBase")
#retrieve information of the gene of interest
wh <- genesymbol[c("BRCA1", "NBR1")]
wh <- range(wh, ignore.strand = TRUE)
#Plot the different transcripts  for our gene of interest
p.txdb <- autoplot(Homo.sapiens, which = wh)
p.txdb
#Change inton geometry, use gap.geom
autoplot(Homo.sapiens, which = wh, gap.geom = "chevron")


#########################################################################################

from <- 65921878
to <- 65980988
knownGenes <- UcscTrack(
  genome = "mm9", 
  chromosome = "chrX",
  track = "knownGene", 
  from = from, 
  to = to, 
  trackType = "GeneRegionTrack",
  symbol = "name", 
  transcript = "name", 
  strand = "strand",
  fill = "#8282d2", 
  name = "UCSC Genes"
)
refGenes <- UcscTrack(
  genome = "mm9", 
  chromosome = "chrX",
  track = "xenoRefGene", 
  from = from, 
  to = to,
  trackType = "GeneRegionTrack", 
  rstarts = "exonStarts",
  rends = "exonEnds", 
  gene = "name", 
  symbol = "name2",
  transcript = "name", 
  strand = "strand", 
  fill = "#8282d2",
  stacking = "dense", 
  name = "Other RefSeq"
)
ensGenes <- UcscTrack(
  genome = "mm9", 
  chromosome = "chrX",
  track = "ensGene", 
  from = from, 
  to = to, 
  trackType = "GeneRegionTrack",
  rstarts = "exonStarts", 
  rends = "exonEnds", 
  gene = "name",
  symbol = "name2", 
  transcript = "name", 
  strand = "strand",
  fill = "#960000", 
  name = "Ensembl Genes"
)

cpgIslands <- UcscTrack(
  genome = "mm9", 
  chromosome = "chrX",
  track = "cpgIslandExt", 
  from = from, 
  to = to,
  trackType = "AnnotationTrack", 
  start = "chromStart",
  end = "chromEnd", 
  id = "name", 
  shape = "box",
  fill = "#006400", 
  name = "CpG Islands"
)
snpLocations <- UcscTrack(
  genome = "mm9", 
  chromosome = "chrX",
  track = "snp128", 
  from = from, 
  to = to, 
  trackType = "AnnotationTrack",
  start = "chromStart", 
  end = "chromEnd", 
  id = "name",
  feature = "func", 
  strand = "strand", 
  shape = "box",
  stacking = "dense", 
  fill = "black", 
  name = "SNPs"
)

conservation <- UcscTrack(
  genome = "mm9", 
  chromosome = "chrX",
  track = "Conservation", 
  table = "phyloP30wayPlacental",
  from = from, to = to, 
  trackType = "DataTrack",
  start = "start", 
  end = "end", 
  data = "score",
  type = "hist", 
  window = "auto", 
  col.histogram = "darkblue",
  fill.histogram = "darkblue", 
  ylim = c(-3.7, 4),
  name = "Conservation"
)

gcContent <- UcscTrack(
  genome = "mm9", 
  chromosome = "chrX",
  track = "GC Percent", 
  table = "gc5Base", 
  from = from,
  to = to, 
  trackType = "DataTrack", 
  start = "start",
  end = "end", 
  data = "score", 
  type = "hist", 
  window = -1,
  windowSize = 1500, 
  fill.histogram = "black",
  col.histogram = "black", 
  ylim = c(30, 70), 
  name = "GC Percent"
)


axTrack <- GenomeAxisTrack()

idxTrack <- IdeogramTrack(
  genome = "mm9", 
  chromosome = "chrX"
)

#And finally we can plot all of our tracks.
plotTracks(
  list(idxTrack, 
       axTrack, 
       knownGenes, 
       refGenes,
       ensGenes, 
       cpgIslands, 
       gcContent, 
       conservation,
       snpLocations
  ), 
  from = from, 
  to = to, 
  showTitle = FALSE
)
