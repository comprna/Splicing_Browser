
## FUNCIONA
"
  - Genera isoforma y transcritos
  - NO USA GVIZ
"
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
#p.txdb
#Change inton geometry, use gap.geom
autoplot(Homo.sapiens, which = wh, gap.geom = "chevron")

#####################################################################################

## FUNCIONA
# GRanges permite printar tus propios transcritos
library("org.Hs.eg.db")
gene=c("MET", "PRKCB")
aux <- mget(x=gene,envir=org.Hs.egALIAS2EG)
aux1 <- unlist(mget(x=gene,envir=org.Hs.egALIAS2EG))
aux$MET
aux1
## Bimap interface:
x <- org.Hs.egCHRLOC
# Get the entrez gene identifiers that are mapped to chromosome locations
mapped_genes <- mappedkeys(x)
# Convert to a list
xx <- as.list(x[mapped_genes])
if(length(xx) > 0) {
  # Get the CHRLOC for the first five genes
  xx[1:5]
  # Get the first one
  xx[[1]]
}

#####################################################################################

## FUNCIONA
library(org.Hs.eg.db)
human <- org.Hs.eg.db
mapk_gene_family_info <- select(
  human,
  keys = c("MAPK1", "MAPK3", "MAPK6"),
  keytype = "SYMBOL",
  columns = c("ENTREZID", "ENSEMBL", "GENENAME", "UCSCKG", "UNIPROT")
)
mapk_gene_family_info
select(
  human,
  keys = mapk_gene_family_info$ENSEMBL[[1]],
  keytype = "ENSEMBL",
  columns = c("ENSEMBLTRANS")
)
# xxx_DB in the vignette is just a string to the SQLite db file path
ens84_txdb_pth <- './Homo_sapiens.GRCh38.84.sqlite'
if (!file.exists(ens84_txdb_pth)) {
  ens84txdb_pth <- ensDbFromGtf(gtf="Homo_sapiens.GRCh38.84.gtf.gz")
}
txdb_ens84 <- EnsDb(ens84txdb_pth)
txdb_ens84  # Preview the metadata
transcripts(
  txdb_ens84,
  filter=GeneidFilter(mapk_gene_family_info$ENSEMBL[[1]])
)

#####################################################################################

"
 FUNCIONA, crea tabla con todos los datos:
  - seqnames    - id
  - start       - exon
  - end         - transcript
  - width       - gene
  - strand      - symbol
  - feature     - density
"

# ll <- as.data.frame(grtrackENSEMBLE@range)

#####################################################################################

# It allways search by location
location <- GRanges("chrX:99883667-99894988:-", seqinfo=seqinfo(genome))
inputData <- "ENSG00000105976"
inputData <- "MET"
inputData <- "chr7:116312444-116438440:+"
inputData <- "chrX:111111111-111111111:+"

#####################################################################################
gm <- GeneRegionTrack("files/gtf/hg19_ensembl_events_AL_strict.gtf", chromosome="chr12")

## READ GTF FILES
options(ChromosomeNames=FALSE)
gm <- GeneRegionTrack("~/Downloads/gencode.v25.chr_patch_hapl_scaff.annotation.gtf", chromosome="chr12")
gm <- gm[gene(gm) == "ENSG00000161791.13" & feature(gm) %in% c("CDS", "UTR")]
bmTrack <- as(BiomartGeneRegionTrack(genome="hg19", symbol="FMNL3"), "GeneRegionTrack")
bmTrack <- bmTrack[symbol(bmTrack) == "FMNL3"]
seqlevels(ranges(bmTrack)) <- "chr12"
chromosome(bmTrack) <- "chr12"
plotTracks(list(GenomeAxisTrack(), gm, bmTrack), thinBoxFeature=c(Gviz:::.THIN_BOX_FEATURES, "UTR"), collapse=FALSE, transcriptAnnotation="symbol", chromosme=12)

install.packages("refGenome")
library(refGenome)
ens<-ensemblGenome()
basedir(ens)<-system.file("extdata",package="refGenome")
ens_gtf<-"files/gtf/hg19_ensembl_events_A3_strict.gtf"
read.gtf(ens,ens_gtf)
ddx<-extractByGeneName(ens,"DDX11L1")
ddx

#####################################################################################

### TO DO
## REMOVE TRACKS
length(trackList)
addTrack(trackList, "ucsc")
trackList <- addTrack(trackList, "ucsc")
trackList <- removeTrack(trackList, "ucsc")


#####################################################################################

### TO DO
## LOCATION LIMITS


#####################################################################################

### TO DO
## READ AND PRINT GTF


ef<-system.file("extdata", package="refGenome")
en<-ensemblGenome(ef)
read.gtf(en, "files/gtf/hg19_ensembl_events_AL_strict.gtf")


read.gtf(file = "files/gtf/hg19_ensembl_events_AL_strict.gtf", attr = c("split", "intact", "skip"), features = NULL, quiet = FALSE)



biocLite("Rgb")
library(Rgb)

file <- "AAA"
file <- system.file("files/gtf/hg19_ensembl_events_AL_strict.gtf", package="gtf")
tt <- track.table.GTF(file)



require(rtracklayer)
gtf <- readGFF("files/gtf/hg19_ensembl_events_AL_strict.gtf", version=2L)
#    user  system elapsed 
#  34.558   1.541  36.737 
dim(gtf)
# [1] 2572840      26


library(rtracklayer)
library(GenomicRanges)

export(import("files/gtf/hg19_ensembl_events_AL_strict.gtf"),con="test.gtf")

read.gtf <- function(gtf.file, trackline = FALSE, sep = "\t"){
  
  # read gtf file content
  gtf.input <- readr::read_delim("files/gtf/hg19_ensembl_events_AL_strict.gtf",delim = "/t", col_names = FALSE, comment = "#" )
  
  if (!trackline){
    gtf.input <- readr::read_delim(gtf.file,delim = sep, col_names = FALSE, comment = "#" )
  } else {
    gtf.input <- readr::read_delim(gtf.file,delim = sep, col_names = FALSE, comment = "#", skip = 1)
  }
  
  # name standardized columns
  gffNames <- c("seqname","source","feature",
                "start","end","score","strand",
                "frame","attribute")
  names(gtf.input)[1:ncol(gtf.input)] <- gffNames[1:ncol(gtf.input)]
  
  if (ncol(gtf.input) > 9)
    stop ("The gff file format can not store more than 9 columns!")
  
  return(gtf.input)
}


