
suppressMessages(library("plyr", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("data.table", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("reshape2", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("crayon", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("withr", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("ggplot2", lib.loc = "/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("farver", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("labeling", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("optparse", lib.loc = "/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("dplyr", lib.loc = "/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("withr", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("backports", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("broom", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("rstudioapi", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("cli", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("tzdb", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("svglite", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("ggeasy", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("sandwich", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("digest", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("tidyverse", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("vroom", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("BiocGenerics", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("S4Vectors", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("IRanges", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("GenomeInfoDb", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("GenomicRanges", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("Biobase", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("AnnotationDbi", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("GO.db", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("org.Hs.eg.db", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("TxDb.Hsapiens.UCSC.hg19.knownGene", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("rtracklayer", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("cowplot", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("RColorBrewer", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("DESeq2", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))

opt = NULL

options(warn = 1)

feature_selection_Activity = function(option_list)
{
  opt_in = option_list
  opt <<- option_list
  
  cat("All options:\n")
  printList(opt)
  
  
  #### READ and transform type ----
  
  type = opt$type
  
  cat("TYPE_\n")
  cat(sprintf(as.character(type)))
  cat("\n")
  
  
  #### READ and transform out ----
  
  out = opt$out
  
  cat("OUT_\n")
  cat(sprintf(as.character(out)))
  cat("\n")
  
  #### READ and transform Threshold_selections ----
  
  Threshold_selections = opt$Threshold_selections
  
  cat("Threshold_selections_\n")
  cat(sprintf(as.character(Threshold_selections)))
  cat("\n")
  
  #### READ and transform coefficient_file_Activity ----
  

  coefficient_file_Activity<-readRDS(opt$coefficient_file_Activity)
  
  
  cat("coefficient_file_Activity_0\n")
  cat(str(coefficient_file_Activity))
  cat("\n")
  
  
  coefficient_file_Activity.dt<-data.table(coefficient_file_Activity, key=c("Cell_Type","variable"))
  
  
  coefficient_file_Activity_MAX<-as.data.frame(coefficient_file_Activity.dt[,.SD[which.max(rsq)],by=key(coefficient_file_Activity.dt)], stringsAsFactors=F)
  
  
  cat("coefficient_file_Activity_MAX_0\n")
  cat(str(coefficient_file_Activity_MAX))
  cat("\n")
  
  
  coefficient_file_Activity_MAX_thresholded<-coefficient_file_Activity_MAX[which(coefficient_file_Activity_MAX$Freq > Threshold_selections),]
  
  cat("coefficient_file_Activity_MAX_thresholded_0\n")
  cat(str(coefficient_file_Activity_MAX_thresholded))
  cat("\n")
  
  if(dim(coefficient_file_Activity_MAX_thresholded)[1] >0){
    
    setwd(out)
    
    saveRDS(coefficient_file_Activity_MAX_thresholded, file="coefficient_file_Activity_MAX_thresholded.rds")
    write.table(coefficient_file_Activity_MAX_thresholded, file="coefficient_file_Activity_MAX_thresholded.tsv",sep="\t",quote=F, row.names = F)
    
    
  }#dim(coefficient_file_Activity_MAX_thresholded)[1] >0
 
}

feature_selection_MPRA_CLASS = function(option_list)
{
  opt_in = option_list
  opt <<- option_list
  
  cat("All options:\n")
  printList(opt)
  
  
  #### READ and transform type ----
  
  type = opt$type
  
  cat("TYPE_\n")
  cat(sprintf(as.character(type)))
  cat("\n")
  
  
  #### READ and transform out ----
  
  out = opt$out
  
  cat("OUT_\n")
  cat(sprintf(as.character(out)))
  cat("\n")
  
  #### READ and transform Threshold_selections ----
  
  Threshold_selections = opt$Threshold_selections
  
  cat("Threshold_selections_\n")
  cat(sprintf(as.character(Threshold_selections)))
  cat("\n")
  
  #### READ and transform coefficient_file_MPRA_CLASS ----
  
  coefficient_file_MPRA_CLASS<-readRDS(opt$coefficient_file_MPRA_CLASS)
  
  cat("coefficient_file_MPRA_CLASS_0\n")
  cat(str(coefficient_file_MPRA_CLASS))
  cat("\n")
  
  coefficient_file_MPRA_CLASS.dt<-data.table(coefficient_file_MPRA_CLASS, key=c("Cell_Type","variable"))
  
  
  coefficient_file_MPRA_CLASS_MAX<-as.data.frame(coefficient_file_MPRA_CLASS.dt[,.SD[which.max(rsq)],by=key(coefficient_file_MPRA_CLASS.dt)], stringsAsFactors=F)
  
  
  cat("coefficient_file_MPRA_CLASS_MAX_0\n")
  cat(str(coefficient_file_MPRA_CLASS_MAX))
  cat("\n")
  
  
  coefficient_file_MPRA_CLASS_MAX_thresholded<-coefficient_file_MPRA_CLASS_MAX[which(coefficient_file_MPRA_CLASS_MAX$Freq > Threshold_selections),]
  
  cat("coefficient_file_MPRA_CLASS_MAX_thresholded_0\n")
  cat(str(coefficient_file_MPRA_CLASS_MAX_thresholded))
  cat("\n")
  
  if(dim(coefficient_file_MPRA_CLASS_MAX_thresholded)[1] >0){
    
    setwd(out)
    
    saveRDS(coefficient_file_MPRA_CLASS_MAX_thresholded, file="coefficient_file_MPRA_CLASS_MAX_thresholded.rds")
    write.table(coefficient_file_MPRA_CLASS_MAX_thresholded, file="coefficient_file_MPRA_CLASS_MAX_thresholded.tsv",sep="\t",quote=F, row.names = F)
    
    
  }#dim(coefficient_file_MPRA_CLASS_MAX_thresholded)[1] >0
  
  
}

feature_selection_log2FC_meta = function(option_list)
{
  opt_in = option_list
  opt <<- option_list
  
  cat("All options:\n")
  printList(opt)
  
  
  #### READ and transform type ----
  
  type = opt$type
  
  cat("TYPE_\n")
  cat(sprintf(as.character(type)))
  cat("\n")
  
  
  #### READ and transform out ----
  
  out = opt$out
  
  cat("OUT_\n")
  cat(sprintf(as.character(out)))
  cat("\n")
  
  #### READ and transform Threshold_selections ----
  
  Threshold_selections = opt$Threshold_selections
  
  cat("Threshold_selections_\n")
  cat(sprintf(as.character(Threshold_selections)))
  cat("\n")
  
  #### READ and transform coefficient_file_log2FC_meta ----
  
  
  coefficient_file_log2FC_meta<-readRDS(opt$coefficient_file_log2FC_meta)
  
  
  cat("coefficient_file_log2FC_meta_0\n")
  cat(str(coefficient_file_log2FC_meta))
  cat("\n")
  
  
  coefficient_file_log2FC_meta.dt<-data.table(coefficient_file_log2FC_meta, key=c("Cell_Type","variable"))
  
  
  coefficient_file_log2FC_meta_MAX<-as.data.frame(coefficient_file_log2FC_meta.dt[,.SD[which.max(rsq)],by=key(coefficient_file_log2FC_meta.dt)], stringsAsFactors=F)
  
  
  cat("coefficient_file_log2FC_meta_MAX_0\n")
  cat(str(coefficient_file_log2FC_meta_MAX))
  cat("\n")
  
  
  coefficient_file_log2FC_meta_MAX_thresholded<-coefficient_file_log2FC_meta_MAX[which(coefficient_file_log2FC_meta_MAX$Freq > Threshold_selections),]
  
  cat("coefficient_file_log2FC_meta_MAX_thresholded_0\n")
  cat(str(coefficient_file_log2FC_meta_MAX_thresholded))
  cat("\n")
  
  if(dim(coefficient_file_log2FC_meta_MAX_thresholded)[1] >0){
    
    setwd(out)
    
    saveRDS(coefficient_file_log2FC_meta_MAX_thresholded, file="coefficient_file_log2FC_meta_MAX_thresholded.rds")
    write.table(coefficient_file_log2FC_meta_MAX_thresholded, file="coefficient_file_log2FC_meta_MAX_thresholded.tsv",sep="\t",quote=F, row.names = F)
    
    
  }#dim(coefficient_file_log2FC_meta_MAX_thresholded)[1] >0
  
}

feature_selection_abs_log2Skew_meta = function(option_list)
{
  opt_in = option_list
  opt <<- option_list
  
  cat("All options:\n")
  printList(opt)
  
  
  #### READ and transform type ----
  
  type = opt$type
  
  cat("TYPE_\n")
  cat(sprintf(as.character(type)))
  cat("\n")
  
  
  #### READ and transform out ----
  
  out = opt$out
  
  cat("OUT_\n")
  cat(sprintf(as.character(out)))
  cat("\n")
  
  #### READ and transform Threshold_selections ----
  
  Threshold_selections = opt$Threshold_selections
  
  cat("Threshold_selections_\n")
  cat(sprintf(as.character(Threshold_selections)))
  cat("\n")
  
  #### READ and transform coefficient_file_abs_log2Skew_meta ----
  
  
  coefficient_file_abs_log2Skew_meta<-readRDS(opt$coefficient_file_abs_log2Skew_meta)
  
  
  cat("coefficient_file_abs_log2Skew_meta_0\n")
  cat(str(coefficient_file_abs_log2Skew_meta))
  cat("\n")
  
  
  coefficient_file_abs_log2Skew_meta.dt<-data.table(coefficient_file_abs_log2Skew_meta, key=c("Cell_Type","variable"))
  
  
  coefficient_file_abs_log2Skew_meta_MAX<-as.data.frame(coefficient_file_abs_log2Skew_meta.dt[,.SD[which.max(rsq)],by=key(coefficient_file_abs_log2Skew_meta.dt)], stringsAsFactors=F)
  
  
  cat("coefficient_file_abs_log2Skew_meta_MAX_0\n")
  cat(str(coefficient_file_abs_log2Skew_meta_MAX))
  cat("\n")
  
  
  coefficient_file_abs_log2Skew_meta_MAX_thresholded<-coefficient_file_abs_log2Skew_meta_MAX[which(coefficient_file_abs_log2Skew_meta_MAX$Freq > Threshold_selections),]
  
  cat("coefficient_file_abs_log2Skew_meta_MAX_thresholded_0\n")
  cat(str(coefficient_file_abs_log2Skew_meta_MAX_thresholded))
  cat("\n")
  
  if(dim(coefficient_file_abs_log2Skew_meta_MAX_thresholded)[1] >0){
    
    setwd(out)
    
    saveRDS(coefficient_file_abs_log2Skew_meta_MAX_thresholded, file="coefficient_file_abs_log2Skew_meta_MAX_thresholded.rds")
    write.table(coefficient_file_abs_log2Skew_meta_MAX_thresholded, file="coefficient_file_abs_log2Skew_meta_MAX_thresholded.tsv",sep="\t",quote=F, row.names = F)
    
    
  }#dim(coefficient_file_abs_log2Skew_meta_MAX_thresholded)[1] >0
  
}

printList = function(l, prefix = "    ") {
  list.df = data.frame(val_name = names(l), value = as.character(l))
  list_strs = apply(list.df, MARGIN = 1, FUN = function(x) { paste(x, collapse = " = ")})
  cat(paste(paste(paste0(prefix, list_strs), collapse = "\n"), "\n"))
}


#### main script ----

main = function() {
  cmd_line = commandArgs()
  cat("Command line:\n")
  cat(paste(gsub("--file=", "", cmd_line[4], fixed=T),
            paste(cmd_line[6:length(cmd_line)], collapse = " "),
            "\n\n"))
  option_list <- list(
    make_option(c("--coefficient_file_Activity"), type="character", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required."),
    make_option(c("--coefficient_file_log2FC_meta"), type="character", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required."),
    make_option(c("--coefficient_file_abs_log2Skew_meta"), type="character", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required."),
    make_option(c("--coefficient_file_MPRA_CLASS"), type="character", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required."),
    make_option(c("--Threshold_selections"), type="numeric", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required."),
    make_option(c("--type"), type="character", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required."),
    make_option(c("--out"), type="character", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required.")
  )
  parser = OptionParser(usage = "140__Rscript_v106.R
                        --subset type
                        --TranscriptEXP FILE.txt
                        --cadd FILE.txt
                        --ncboost FILE.txt
                        --type type
                        --out filename",
                        option_list = option_list)
  opt <<- parse_args(parser)
  
  feature_selection_Activity(opt)
  feature_selection_MPRA_CLASS(opt)
  feature_selection_log2FC_meta(opt)
  feature_selection_abs_log2Skew_meta(opt)
 
}

###########################################################################

system.time( main() )