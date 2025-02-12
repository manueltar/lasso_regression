
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

collect_comp1 = function(option_list)
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
  
  
  
  #### Read in all the results of the meta-analisis ----
  
  file_list <- list.files(path=out, include.dirs = FALSE)
  
  
  cat("file_list\n")
  cat(str(file_list))
  cat("\n")
  
  
  indexes_sel <- grep("Lasso_regression_response_comp1",file_list)
  
  file_list_sel <- as.data.frame(file_list[indexes_sel], stringsAsFactors=F)
  colnames(file_list_sel)<-"file"
  
  
  cat("file_list_sel_0\n")
  cat(str(file_list_sel))
  cat("\n")
  
 
  file_list_sel$Cell_Type<-'At_least_one_cell_type'
  
  cat("file_list_sel_0.5\n")
  cat(str(file_list_sel))
  cat("\n")
  
  ############# LOOP --------------------------
  
  List_RESULTS<-list()
  
  
  # Results_DEF<-data.frame()
  
  DEBUG<-1
  
  for(i in 1:dim(file_list_sel)[1]){
    
    read_file_sel<-file_list_sel$file[i]
    Cell_Type_sel<-file_list_sel$Cell_Type[i]
    
    
    cat("-------------------------------------------------------->\t")
    cat(sprintf(as.character(i)))
    cat("\t")
    cat(sprintf(as.character(read_file_sel)))
    cat("\t")
    cat(sprintf(as.character(Cell_Type_sel)))
    cat("\n")
    
    
    setwd(out)
    
    
    
    SIZE_gate<-file.info(read_file_sel)$size
    
    if(DEBUG ==1)
    {
      cat("SIZE_gate\n")
      cat(str(SIZE_gate))
      cat("\n")
    }
    
    
    
    if(SIZE_gate> 0)
    {
      
      
      LINE_gate<-length(readLines(read_file_sel))
      
      if(DEBUG ==1)
      {
        cat("LINE_gate\n")
        cat(str(LINE_gate))
        cat("\n")
      }
      
      if(LINE_gate> 0)
      {
        
        
        
        df<-as.data.frame(fread(file=read_file_sel, sep="\t", header = T, fill=TRUE), stringsAsFactors = F)
        
        if(DEBUG ==1)
        {
          cat("df_0\n")
          cat(str(df))
          cat("\n")
        }
        
        FLAG_rsq_na<-sum(is.na(df$rsq))
        
        if(DEBUG ==1)
        {
          cat("FLAG_rsq_na_0\n")
          cat(str(FLAG_rsq_na))
          cat("\n")
        }
        
        if(FLAG_rsq_na < dim(df)[1]){
          
          FLAG_rsq_not_zero<-sum(df$rsq[!is.na(df$rsq)])
          
          if(DEBUG ==1)
          {
            cat("FLAG_rsq_not_zero_0\n")
            cat(str(FLAG_rsq_not_zero))
            cat("\n")
          }
          
          if(FLAG_rsq_not_zero > 0){
            
            df$Cell_Type<-Cell_Type_sel
            
            if(DEBUG ==1)
            {
              cat("df_1\n")
              cat(str(df))
              cat("\n")
            }
            
            df_coeffs<-df[which(df$s0 != '.'),]
            
            if(DEBUG ==1)
            {
              cat("df_coeffs_0\n")
              cat(str(df_coeffs))
              cat("\n")
            }
            
            if(dim(df_coeffs)[1] >1){
              
              List_RESULTS[[i]]<-df_coeffs
              
              
            }else{
              
              cat("No coefficients apart from Intercept\n")
              
            }#dim(df_coeffs)[1] >1
            
          }else{
            
            cat("rsq 0\n")
            
            
          }#FLAG_rsq_not_zero == 0
        }else{
          
          cat("No rsq\n")
        }# FLAG_rsq_na == 0
      }#LINE_gate
      else{
        
        cat(sprintf("empty_file\n"))
        cat(sprintf(as.character(read_file_sel)))
        cat("\n")
        
        
      }
    }#SIZE_gates
    
  }#i in 1:dim(file_list_sel)[1]
  
  
  
  Results = unique(as.data.frame(data.table::rbindlist(List_RESULTS, fill = T)))
  
  cat("Results_0\n")
  cat(str(Results))
  cat("\n")
  
  Results$Cell_Type<-factor(Results$Cell_Type,
                            levels=c('At_least_one_cell_type'),
                            ordered=T)
  
  
  Results<-Results[order(Results$Cell_Type),]
  
  cat("Results_1\n")
  cat(str(Results))
  cat("\n")
  
  
  Results.dt<-data.table(Results, key=c("Cell_Type","variable"))
  
  
  Freq.table<-as.data.frame(Results.dt[,.(Freq=.N),by=key(Results.dt)], stringsAsFactors=F)
  
  
  cat("Freq.table_0\n")
  cat(str(Freq.table))
  cat("\n")
  
  Results<-merge(Freq.table,
                 Results,
                 by=c("Cell_Type","variable"))
  
  cat("Results_2\n")
  cat(str(Results))
  cat("\n")
  
  
  Results<-Results[order(Results$Cell_Type, Results$Freq, decreasing = T),]
  
  
  ################################################################################## SAVE #############################################################################################################################3
  
  
  setwd(out)
  
  write.table(Results, 
              file=paste("collected_results_comp1",".tsv",sep=""), 
              row.names = F, quote=F, sep="\t")
  
  saveRDS(Results, 
          file=paste("collected_results_comp1",".rds",sep=""))
  
  
}

collect_comp2 = function(option_list)
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
  
  
  
  #### Read in all the results of the meta-analisis ----
  
  file_list <- list.files(path=out, include.dirs = FALSE)
  
  
  cat("file_list\n")
  cat(str(file_list))
  cat("\n")
  
  
  indexes_sel <- grep("Lasso_regression_response_comp2",file_list)
  
  file_list_sel <- as.data.frame(file_list[indexes_sel], stringsAsFactors=F)
  colnames(file_list_sel)<-"file"
  
  
  cat("file_list_sel_0\n")
  cat(str(file_list_sel))
  cat("\n")
  
  file_list_sel$Cell_Type<-gsub("_.+$","",file_list_sel$file)
  
  cat("file_list_sel_0.25\n")
  cat(str(file_list_sel))
  cat("\n")
  
  file_list_sel$Cell_Type<-'At_least_one_cell_type'
  
  cat("file_list_sel_0.5\n")
  cat(str(file_list_sel))
  cat("\n")
  
  ############# LOOP --------------------------
  
  List_RESULTS<-list()
  
  
  # Results_DEF<-data.frame()
  
  DEBUG<-1
  
  for(i in 1:dim(file_list_sel)[1]){
    
    read_file_sel<-file_list_sel$file[i]
    Cell_Type_sel<-file_list_sel$Cell_Type[i]
    
    
    cat("-------------------------------------------------------->\t")
    cat(sprintf(as.character(i)))
    cat("\t")
    cat(sprintf(as.character(read_file_sel)))
    cat("\t")
    cat(sprintf(as.character(Cell_Type_sel)))
    cat("\n")
    
    
    setwd(out)
    
    
    
    SIZE_gate<-file.info(read_file_sel)$size
    
    if(DEBUG ==1)
    {
      cat("SIZE_gate\n")
      cat(str(SIZE_gate))
      cat("\n")
    }
    
    
    
    if(SIZE_gate> 0)
    {
      
      
      LINE_gate<-length(readLines(read_file_sel))
      
      if(DEBUG ==1)
      {
        cat("LINE_gate\n")
        cat(str(LINE_gate))
        cat("\n")
      }
      
      if(LINE_gate> 0)
      {
        
        
        
        df<-as.data.frame(fread(file=read_file_sel, sep="\t", header = T, fill=TRUE), stringsAsFactors = F)
        
        if(DEBUG ==1)
        {
          cat("df_0\n")
          cat(str(df))
          cat("\n")
        }
        
        FLAG_rsq_na<-sum(is.na(df$rsq))
        
        if(DEBUG ==1)
        {
          cat("FLAG_rsq_na_0\n")
          cat(str(FLAG_rsq_na))
          cat("\n")
        }
        
        if(FLAG_rsq_na < dim(df)[1]){
          
          FLAG_rsq_not_zero<-sum(df$rsq[!is.na(df$rsq)])
          
          if(DEBUG ==1)
          {
            cat("FLAG_rsq_not_zero_0\n")
            cat(str(FLAG_rsq_not_zero))
            cat("\n")
          }
          
          if(FLAG_rsq_not_zero > 0){
            
            df$Cell_Type<-Cell_Type_sel
            
            if(DEBUG ==1)
            {
              cat("df_1\n")
              cat(str(df))
              cat("\n")
            }
            
            df_coeffs<-df[which(df$s0 != '.'),]
            
            if(DEBUG ==1)
            {
              cat("df_coeffs_0\n")
              cat(str(df_coeffs))
              cat("\n")
            }
            
            if(dim(df_coeffs)[1] >1){
              
              List_RESULTS[[i]]<-df_coeffs
              
              
            }else{
              
              cat("No coefficients apart from Intercept\n")
              
            }#dim(df_coeffs)[1] >1
            
          }else{
            
            cat("rsq 0\n")
            
            
          }#FLAG_rsq_not_zero == 0
        }else{
          
          cat("No rsq\n")
        }# FLAG_rsq_na == 0
      }#LINE_gate
      else{
        
        cat(sprintf("empty_file\n"))
        cat(sprintf(as.character(read_file_sel)))
        cat("\n")
        
        
      }
    }#SIZE_gates
    
  }#i in 1:dim(file_list_sel)[1]
  
  
  
  Results = unique(as.data.frame(data.table::rbindlist(List_RESULTS, fill = T)))
  
  cat("Results_0\n")
  cat(str(Results))
  cat("\n")
  
  Results$Cell_Type<-factor(Results$Cell_Type,
                            levels=c('At_least_one_cell_type'),
                            ordered=T)
  
  
  Results<-Results[order(Results$Cell_Type),]
  
  cat("Results_1\n")
  cat(str(Results))
  cat("\n")
  
  
  Results.dt<-data.table(Results, key=c("Cell_Type","variable"))
  
  
  Freq.table<-as.data.frame(Results.dt[,.(Freq=.N),by=key(Results.dt)], stringsAsFactors=F)
  
  
  cat("Freq.table_0\n")
  cat(str(Freq.table))
  cat("\n")
  
  Results<-merge(Freq.table,
                 Results,
                 by=c("Cell_Type","variable"))
  
  cat("Results_2\n")
  cat(str(Results))
  cat("\n")
  
  
  Results<-Results[order(Results$Cell_Type, Results$Freq, decreasing = T),]
  
  
  ################################################################################## SAVE #############################################################################################################################3
  
  
  setwd(out)
  
  write.table(Results, 
              file=paste("collected_results_comp2",".tsv",sep=""), 
              row.names = F, quote=F, sep="\t")
  
  saveRDS(Results, 
          file=paste("collected_results_comp2",".rds",sep=""))
  
  
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
  
  collect_comp1(opt)
  collect_comp2(opt)
 
}

###########################################################################

system.time( main() )