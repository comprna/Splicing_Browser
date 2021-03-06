"  
Author: Hèctor Garcia Guillen
Name: loadFiles.R 
Description:
Date: 17/06/07
Version: 1.1
"

library(refGenome)
var <- ensemblGenome()
read.gtf(var, "files/gtf/hg19_ensembl_events_ALL_strict.gtf")
allGtf <- getGtf(var)
allGtf <- allGtf[!is.na(allGtf$transcript_id),]
#allGtf <- as.data.frame(readGFF("files/gtf/hg19_ensembl_events_ALL_strict.gtf"))
info_clin_all <- read.table('././files/all/info_clin_all.txt', header = TRUE, sep = '\t', dec = ',')
events_filter_all <- read.table('././files/all/events_filter_all.psi', header = TRUE, sep = '\t', dec = ',') # 8 minutos
events_filter_all[events_filter_all==-1]<-NA
info_clin_all[info_clin_all==-1]<-NA
mart_export <- read.table('././files/mart_export.txt', header = TRUE, sep = '\t', dec = ',')
#info_clin_all <- read.table('././files/gtex/GTEX_phenotype.txt', header = TRUE, sep = '\t', dec = ',')
#events_filter_all <- read.table('././files/gtex/headGtx.psi', header = TRUE, sep = '\t', dec = ',') # 8 minutos
colnames(info_clin_all) <- tolower(gsub(".", "_", colnames(info_clin_all), fixed = TRUE))
plotFilters <- as.matrix(as.data.frame(t(as.data.frame(lapply(info_clin_all, class))))[2:nrow(as.data.frame(t(as.data.frame(lapply(info_clin_all, class))))),])
for (i in 1:ncol(info_clin_all)){
  info_clin_all[,i] <- gsub(" ", "", info_clin_all[,i], fixed = TRUE)
}

#colnames(info_clin_all)[1] <- "Sample.ID"
#info_clin_all$Sample.ID <- str_replace(info_clin_all$Sample.ID, "-", ".")
#write.table(info_clin_all, "././files/gtex/GTEX_phenotype", sep = "\t", dec = ",")

#aux <- events_filter_all
#aux1 <- info_clin_all
#colnames(info_clin_all)[1] <- "Sample.ID"
#rownames(events_filter_all) <- events_filter_all[,1]
#events_filter_all[,1] <- NULL
# Oder dataframe by column name
#events_filter_all <- events_filter_all[ , order(names(events_filter_all))]
#info_clin_all <- info_clin_all[order(info_clin_all$Sample.ID),]
#nrow(info_clin_all)
#ncol(events_filter_all)
#info_clin_all <- head(info_clin_all,7863)
#colnames(events_filter_all) <- info_clin_all$Sample.ID
#nrow(info_clin_all)
#ncol(events_filter_all)
#write.table(info_clin_all, "././files/gtex/all/gtex_clin.txt", sep = "\t", dec = ",")
#write.table(events_filter_all, "././files/gtex/all/patient_filter.psi", sep = "\t", dec = ",")

###############################################################################################

#info_clin_all <- read.table("././files/all/info_clin_all.txt", header = TRUE, sep = "\t", dec = ",")
#events_filter_all <- read.table("././files/all/events_filter_all.psi", header = TRUE, sep = "\t", dec = ",")
#mart_export <- read.table("././files/mart_export.txt", header = TRUE, sep = "\t", dec = ",")

#write.table(info_clin_all, "././files/all/info_clin_all.txt", sep = "\t", dec = ",")
#write.table(events_filter_all, "././files/all/events_filter_all.psi", sep = "\t", dec = ",")

###############################################################################################

# Load files
#mart_export <- read.table("././files/mart_export.txt", header = TRUE, sep = "\t", dec = ",")
#info_clin_all <- read.table("././files/info_clin_all.txt", header = TRUE, sep = "\t", dec = ",")
#info_clin_mpc <- read.table("././files/mpc/info_clin_mpc.txt", header = TRUE, sep = "\t", dec = ",")
#events_filter_0.1 <- read.table("././files/events_filter_0.1.psi", header = TRUE, sep = "\t", dec = ",")
#events_filter_0.1_mpc <- read.table("././files/mpc/events_filter_0.1.psi", header = TRUE, sep = "\t", dec = ",")

###############################################################################################

# Oder dataframe by column name
#events_filter_0.1_mpc <- events_filter_0.1_mpc[ , order(names(events_filter_0.1_mpc))]
#info_clin_mpc <- info_clin_mpc[order(info_clin_mpc$Seq.ID),]
#colnames(events_filter_0.1_mpc)<- info_clin_mpc$Sample.ID

###############################################################################################

# Clean not valid patients from 1.0
#filt.samples <- c("vh75","vh166","vh127","vh130","vh124","vh59","vh153","vh118","vh3","vh2","vh1","vh128","vh125","vh100", "vh134")
#events_filter_0.1 <- events_filter_0.1[,colnames(events_filter_0.1) %nin% filt.samples]
#info_clin_all <- info_clin_all[info_clin_all$Sample_ID %nin% filt.samples,]
# Clean not valid patients from mpc
#filt.samples <- c("MP_2","MP_10","MP_11","MP_14")
#events_filter_0.1_mpc <- events_filter_0.1_mpc[,colnames(events_filter_0.1_mpc) %nin% filt.samples]
#info_clin_mpc <- info_clin_mpc[info_clin_mpc$Sample.ID %nin% filt.samples,]
#rm(filt.samples)

###############################################################################################

# Append mpc files with 0.1 files
#events_filter_all <- cbind(events_filter_0.1, droplevels(events_filter_0.1_mpc))
#colnames(events_filter_all) <- toupper(colnames(events_filter_all))

###############################################################################################

# Change colname to merge data
#colnames(info_clin_all)[1] <- "Sample.ID"

###############################################################################################-

# Merge info_clin_all with info_clin_mpc
#info_clin_all <- merge(info_clin_all, info_clin_mpc, by="Sample.ID", all.x=TRUE)
#info_clin_all <- info_clin_all[ , !(names(info_clin_all) %in% c("Seq.ID"))]

###############################################################################################

# Change not calculated data by NA
#events_filter_all[events_filter_all==-1]<-NA

###############################################################################################

# LOAD GENOME DATA
# Load homo sapiens genome un hg19 version
genome <- BSgenome.Hsapiens.UCSC.hg19
#genome <- BSgenome.Hsapiens.UCSC.hg38

###############################################################################################

# Get transcriptome from UCSC
txdbUCSC <- TxDb.Hsapiens.UCSC.hg19.knownGene
#txdbUCSC <- TxDb.Hsapiens.UCSC.hg38.knownGene

###############################################################################################

# Get transcriptome from ENSEMBLE
txdbENSEMBLE <- makeTxDbFromUCSC(genome="hg19", tablename="ensGene")
#source("https://bioconductor.org/biocLite.R")
#biocLite("GenomicFeatures")
#txdbENSEMBLE <- makeTxDbFromBiomart(dataset="hsapiens_gene_ensembl")
#txdbENSEMBLE <- makeTxDbFromUCSC(genome="hg38") 

###############################################################################################

# Generate complete genome track for UCSC
grtrackUCSC  <- GeneRegionTrack(
  txdbUCSC, 
  genome=genome, 
  name="UCSC hg19", 
  showId=T
)

grtrackEnsembl  <- GeneRegionTrack(
  txdbENSEMBLE, 
  genome=genome, 
  name="ENSEMBL hg19", 
  showId=T
)

###############################################################################################

# Generate axis line tack
axisTrack <- GenomeAxisTrack(
  add53 = T, 
  add35 = T#, 
  #littleTicks = T
)
###############################################################################################

# Generate sequence of nucleotides 
seqTrack <- SequenceTrack(
  genome = "hg19"
)

###############################################################################################

# Generate dataframe with all UCSC genes and it's location
#UCSCgenes_df <- data.frame(
#  gene = names(genes(txdbUCSC)),
#  location = as.character(genes(txdbUCSC)),
#  row.names = NULL, stringsAsFactors=FALSE
#)

###############################################################################################

# Generate dataframe with all ENSEMBLE genes and it's location
ENSEMBLEgenes_df <- data.frame(
  gene=names(genes(txdbENSEMBLE)),
  location=as.character(genes(txdbENSEMBLE)),
  row.names=NULL, stringsAsFactors=FALSE
)

ENSEMBLEgenes_df <- as.data.frame(as.character(genes(txdbENSEMBLE)))

ENSEMBLEgenes_df <- data.frame(
  gene=rownames(as.data.frame(as.character(genes(txdbENSEMBLE)))),
  location=as.data.frame(as.character(genes(txdbENSEMBLE)))[,1],
  row.names=NULL, stringsAsFactors=FALSE
)

###############################################################################################
