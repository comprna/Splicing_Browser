"  
  Author: Hèctor Garcia Guillen
  Name: loadFiles.R 
  Description: Load all libraries with pacman
  Date: 17/06/07
  Version: 1.1
"
tryCatch({
  library(pacman)
}, error = function (err) {
  install.packages("pacman")
  library(pacman)
})
pacman::p_load(shiny,           # ->
               shinydashboard,  # ->
               shinyjs,         # ->
               ggplot2,         # ->
               plotly,          # ->
               Hmisc,          # ->
               shinyBS, 
               biomaRt
) 

#devtools::install_github('hadley/ggplot2')
#library(ggplot2)
#devtools::install_github("ropensci/plotly")
#library(plotly)

##################################################################################

#source("http://bioconductor.org/biocLite.R")

tryCatch({
  library(GenomicRanges)
}, error = function (err) {
  cat("Installing library GenomicRanges")
  biocLite("GenomicRanges")
  library(GenomicRanges)
})
tryCatch({
  library(Gviz)
}, error = function (err) {
  cat("Installing library Gviz")
  biocLite("Gviz")
  library(Gviz)
})
tryCatch({
  library(BSgenome.Hsapiens.UCSC.hg38)
}, error = function (err) {
  cat("Installing library BSgenome.Hsapiens.UCSC.hg38")
  biocLite("BSgenome.Hsapiens.UCSC.hg38")
  library(BSgenome.Hsapiens.UCSC.hg38)
})
tryCatch({
  library(TxDb.Hsapiens.UCSC.hg38.knownGene)
}, error = function (err) {
  cat("Installing library TxDb.Hsapiens.UCSC.hg38.knownGene")
  biocLite("TxDb.Hsapiens.UCSC.hg38.knownGene")
  library(TxDb.Hsapiens.UCSC.hg38.knownGene)
})
tryCatch({
  library(BSgenome.Hsapiens.UCSC.hg19)
}, error = function (err) {
  cat("Installing library BSgenome.Hsapiens.UCSC.hg19")
  biocLite("BSgenome.Hsapiens.UCSC.hg19")
  library(BSgenome.Hsapiens.UCSC.hg19)
})
tryCatch({
  library(TxDb.Hsapiens.UCSC.hg19.knownGene)
}, error = function (err) {
  cat("Installing library TxDb.Hsapiens.UCSC.hg19.knownGene")
  biocLite("TxDb.Hsapiens.UCSC.hg19.knownGene")
  library(TxDb.Hsapiens.UCSC.hg38.knownGene)
})
