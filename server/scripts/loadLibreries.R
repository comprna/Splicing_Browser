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
pacman::p_load(shiny,          # ->
               shinydashboard, # ->
               shinyjs,        # ->
               #ggproto,       # ->
               ggplot2,        # ->
               plotly,         # ->
               reshape2,       # ->
               Hmisc,          # ->
               gdata,          # ->
               dtplyr,         # ->
               dplyr,          # ->
               stringr,        # ->
               colourpicker,   # ->
               devtools,
               shinyjs,
               shinyBS
) 

#devtools::install_github('hadley/ggplot2')
#library(ggplot2)
#devtools::install_github("ropensci/plotly")
#library(plotly)

##################################################################################

source("http://bioconductor.org/biocLite.R")

tryCatch({
  library(ggbio)
}, error = function (err) {
  cat("Installing library ggbio")
  biocLite("ggbio")
  library(ggbio)
})

tryCatch({
  library(biovizBase)
}, error = function (err) {
  cat("Installing library biovizBase")
  biocLite("biovizBase")
  library(biovizBase)
})

tryCatch({
  library(GenomicFeatures)
}, error = function (err) {
  cat("Installing library GenomicFeatures")
  biocLite("GenomicFeatures")
  library(GenomicFeatures)
})

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
  library(rtracklayer)
}, error = function (err) {
  cat("Installing library rtracklayer")
  biocLite("rtracklayer")
  library(rtracklayer)
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
  library(TxDb.Hsapiens.UCSC.hg19.knownGene)
})

tryCatch({
  library(EnsDb.Hsapiens.v75)
}, error = function (err) {
  cat("Installing library EnsDb.Hsapiens.v75")
  biocLite("EnsDb.Hsapiens.v75")
  library(EnsDb.Hsapiens.v75)
})

###################################################################################

#unloadNamespace("shiny")

###################################################################################

#install.packages("devtools")#if not alrady installed 
#devtools::install_github("ShinySky","AnalytixWare") 
#require(shinysky) 
#require(shinysky)
#library(shinysky)
#library(shiny)

