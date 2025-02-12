
suppressMessages(library("plyr", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("data.table", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
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
suppressMessages(library("cowplot", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("ggupset", lib.loc = "/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("RColorBrewer", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("splitstackshape", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))
suppressMessages(library("glmnet", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))


opt = NULL

options(warn = 1)

lasso_regression_MPRA_S = function(option_list)
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
  
  cat("out_\n")
  cat(sprintf(as.character(out)))
  cat("\n")

  path_lasso<-paste(out,'lasso','/',sep='')
  
  if(file.exists(path_lasso)){
    
  }else{
    dir.create(file.path(path_lasso))
  }#path_lasso
  
  #### READ and transform Cell_Type_sel ----
  
  Cell_Type_sel = opt$Cell_Type_sel
  
  cat("Cell_Type_sel_\n")
  cat(sprintf(as.character(Cell_Type_sel)))
  cat("\n")
 
  #### Read Enformer_run ----------

  feature_file<-readRDS(file=opt$feature_file)
  
  
  cat("feature_file_0\n")
  cat(str(feature_file))
  cat("\n")
  cat(str(unique(feature_file$VAR)))
  cat("\n")
  
  indx.Enformer<-grep("^[0-9]+_",colnames(feature_file))
  
  
  cat("indx.Enformer_0\n")
  cat(str(indx.Enformer))
  cat("\n")
  
  
  colnames_Enformer<-colnames(feature_file)[indx.Enformer]
  
  cat("colnames_Enformer_0\n")
  cat(str(colnames_Enformer))
  cat("\n")
  
  if(Cell_Type_sel == 'HL60'){
    
    search_term<-'HL-60'
    
  }else{
    
    search_term<-Cell_Type_sel
    
  }#Cell_Type_sel == 'HL60'
  
  indx.Enformer_CT_rel<-grep(search_term,colnames_Enformer)
  
  
  cat("indx.Enformer_CT_rel_0\n")
  cat(str(indx.Enformer_CT_rel))
  cat("\n")
  
  if(length(indx.Enformer_CT_rel)>0){
    
    colnames_Enformer_CT<-colnames_Enformer[indx.Enformer_CT_rel]
    
    cat("colnames_Enformer_CT_0\n")
    cat(str(colnames_Enformer_CT))
    cat("\n")
    
    
    colnames_NO_Enformer<-colnames(feature_file)[-indx.Enformer]
    
    cat("colnames_NO_Enformer_0\n")
    cat(str(colnames_NO_Enformer))
    cat("\n")
    
    selected_colnames<-c(colnames_NO_Enformer,colnames_Enformer_CT)
    
    cat("selected_colnames_0\n")
    cat(str(selected_colnames))
    cat("\n")
    
    indx.selected<-which(colnames(feature_file)%in%selected_colnames)
    
    cat("indx.selected_0\n")
    cat(str(indx.selected))
    cat("\n")
    
    feature_file_sel<-feature_file[,indx.selected]
    
  }else{
    
    feature_file_sel<-feature_file[,-indx.Enformer]
    
  }#length(indx.Enformer_CT_rel)>0

  
  cat("feature_file_sel_0\n")
  cat(str(feature_file_sel))
  cat("\n")
  cat(str(unique(feature_file_sel$VAR)))
  cat("\n")
  
  
  #### READ NEW_Table_S6 ----
  
  NEW_Table_S6<-as.data.frame(readRDS(file=opt$NEW_Table_S6), stringsAsFactors=F)
  
  
  cat("NEW_Table_S6_0\n")
  cat(str(NEW_Table_S6))
  cat("\n")
  cat(str(unique(NEW_Table_S6$VAR)))
  cat("\n")
  
  
 
  #### READ MPRA_result_SNP_and_CT ----
  
  MPRA_result_SNP_and_CT<-as.data.frame(readRDS(file=opt$MPRA_result_SNP_and_CT), stringsAsFactors=F)
  
  
  cat("MPRA_result_SNP_and_CT_0\n")
  cat(str(MPRA_result_SNP_and_CT))
  cat("\n")
  
  MPRA_result_SNP_and_CT_subset<-unique(MPRA_result_SNP_and_CT[which(MPRA_result_SNP_and_CT$Project == 'MPRA_bc_synthesis'),which(colnames(MPRA_result_SNP_and_CT)%in%c("VAR","Cell_Type","log2FC_meta","log2Skew_meta","Activity","MPRA_CLASS"))])
  MPRA_result_SNP_and_CT_subset$abs_log2Skew_meta<-abs(MPRA_result_SNP_and_CT_subset$log2Skew_meta)
  
  cat("MPRA_result_SNP_and_CT_subset_0\n")
  cat(str(MPRA_result_SNP_and_CT_subset))
  cat("\n")
  cat(str(unique(MPRA_result_SNP_and_CT_subset$VAR)))
  cat("\n")
  
  
  MPRA_result_SNP_and_CT_subset<-unique(MPRA_result_SNP_and_CT_subset[which(MPRA_result_SNP_and_CT_subset$VAR%in%NEW_Table_S6$VAR),])
  
  cat("MPRA_result_SNP_and_CT_subset_1\n")
  cat(str(MPRA_result_SNP_and_CT_subset))
  cat("\n")
  cat(str(unique(MPRA_result_SNP_and_CT_subset$VAR)))
  cat("\n")
  
  MPRA_result_SNP_and_CT_subset_CT_sel<-unique(MPRA_result_SNP_and_CT_subset[which(MPRA_result_SNP_and_CT_subset$Cell_Type == Cell_Type_sel),])
  
  
  cat("MPRA_result_SNP_and_CT_subset_CT_sel_0\n")
  cat(str(MPRA_result_SNP_and_CT_subset_CT_sel))
  cat("\n")
  cat(str(unique(MPRA_result_SNP_and_CT_subset_CT_sel$VAR)))
  cat("\n")
  
  ##### merge with features ----
  
  Merged_table<-merge(MPRA_result_SNP_and_CT_subset_CT_sel,
                      feature_file_sel,
                      by="VAR")
  
  
  cat("Merged_table_1\n")
  cat(str(Merged_table))
  cat("\n")
  
  
  variables<-colnames(Merged_table)[-which(colnames(Merged_table)%in%c("VAR","Cell_Type","log2FC_meta","log2Skew_meta","abs_log2Skew_meta","Activity","MPRA_CLASS","PP","Absolute_effect_size"))]
  
  cat("variables_0\n")
  cat(str(variables))
  cat("\n")
 
 
  fold_array<-seq(1,10,by=1)
  
  cat("fold_array_0\n")
  cat(str(fold_array))
  cat("\n")
  
  
  df_abs_log2Skew_meta<-data.frame()
  df_log2FC_meta<-data.frame()
  df_Activity<-data.frame()
  df_MPRA_CLASS<-data.frame()
  
  
  for(iteration_fold_array in 1:length(fold_array)){
    
    fold_array_sel<-fold_array[iteration_fold_array]
    
    cat("----------------------------------------------------------------------->\t")
    cat(sprintf(as.character(fold_array_sel)))
    cat("\n")
    
    #### lasso 1 on abs_log2Skew_meta -----
    
    
    y <- as.numeric(Merged_table$abs_log2Skew_meta)
    x <- data.matrix(Merged_table[, variables])
    row.names(x)<-Merged_table$VAR
    cv_model <- cv.glmnet(x, y, alpha = 1)
    best_lambda <- cv_model$lambda.min
    
    cat("best_lambda\n")
    cat(sprintf(as.character(best_lambda)))
    cat("\n")
    
    best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
    result<-coef(best_model)
    
    cat("result_0\n")
    cat(str(result))
    cat("\n")
    
    index_predictors<-as.numeric(result@i)+1
    
    cat("index_predictors_0\n")
    cat(str(index_predictors))
    cat("\n")
    
    names_predictors<-result@Dimnames[[1]]
    
    cat("names_predictors_0\n")
    cat(str(names_predictors))
    cat("\n")
    
    values_predictors<-rep('.', length(names_predictors))
    
    cat("values_predictors_0\n")
    cat(sprintf(as.character(values_predictors)))
    cat("\n")
    
    predictors<-round(as.numeric(result@x),6)
    
    cat("predictors_0\n")
    cat(sprintf(as.character(predictors)))
    cat("\n")
    
    if(length(index_predictors) > 0)
      for(i in 1:length(index_predictors)){
        
        index_predictors_sel<-index_predictors[i]
        
        cat("----------------------------------------->\t")
        cat(sprintf(as.character(index_predictors_sel)))
        cat("\n")
        
        values_predictors[index_predictors_sel]<-predictors[i]
        
        
        
      }#i in 1:length(index_predictors)
    
    cat("values_predictors_POST\n")
    cat(sprintf(as.character(values_predictors)))
    cat("\n")
    
    y_predicted <- predict(best_model, s = best_lambda, newx = x)
    
    #find SST and SSE
    sst <- sum((y - mean(y))^2)
    sse <- sum((y_predicted - y)^2)
    
    #find R-Squared
    rsq <- 1 - sse/sst
    
    rsq <-round(rsq,6)
    
    cat("rsq_0\n")
    cat(sprintf(as.character(rsq)))
    cat("\n")
    
    FLAG_NA_rsq<-sum(is.na(rsq))
    
    if(FLAG_NA_rsq == 0){
      a.df<-as.data.frame(cbind(names_predictors,values_predictors,rep(rsq,length(names_predictors))), stringsAsFactors=F)
      
      colnames(a.df)<-c("variable","s0","rsq")
      
      cat("a.df_0\n")
      cat(str(a.df))
      cat("\n")
    }else{
      
      a.df<-as.data.frame(cbind(names_predictors,values_predictors,rep("NA",length(names_predictors))), stringsAsFactors=F)
      
      colnames(a.df)<-c("variable","s0","rsq")
      
      cat("a.df_NA\n")
      cat(str(a.df))
      cat("\n")
      
    }#FLAG_NA_rsq == 0
    
    
   
    
    a.df$Fold<-fold_array_sel
    
    df_abs_log2Skew_meta<-rbind(a.df,df_abs_log2Skew_meta)
    
    
    
    #### lasso 1 on log2FC_meta -----
    
    
    y <- as.numeric(Merged_table$log2FC_meta)
    x <- data.matrix(Merged_table[, variables])
    row.names(x)<-Merged_table$VAR
    cv_model <- cv.glmnet(x, y, alpha = 1)
    best_lambda <- cv_model$lambda.min
    
    cat("best_lambda\n")
    cat(sprintf(as.character(best_lambda)))
    cat("\n")
    
    best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
    result<-coef(best_model)
    
    cat("result_0\n")
    cat(str(result))
    cat("\n")
    
    index_predictors<-as.numeric(result@i)+1
    
    cat("index_predictors_0\n")
    cat(str(index_predictors))
    cat("\n")
    
    names_predictors<-result@Dimnames[[1]]
    
    cat("names_predictors_0\n")
    cat(str(names_predictors))
    cat("\n")
    
    values_predictors<-rep('.', length(names_predictors))
    
    cat("values_predictors_0\n")
    cat(sprintf(as.character(values_predictors)))
    cat("\n")
    
    predictors<-round(as.numeric(result@x),6)
    
    cat("predictors_0\n")
    cat(sprintf(as.character(predictors)))
    cat("\n")
    
    if(length(index_predictors) > 0)
      for(i in 1:length(index_predictors)){
        
        index_predictors_sel<-index_predictors[i]
        
        cat("----------------------------------------->\t")
        cat(sprintf(as.character(index_predictors_sel)))
        cat("\n")
        
        values_predictors[index_predictors_sel]<-predictors[i]
        
        
        
      }#i in 1:length(index_predictors)
    
    cat("values_predictors_POST\n")
    cat(sprintf(as.character(values_predictors)))
    cat("\n")
    
    y_predicted <- predict(best_model, s = best_lambda, newx = x)
    
    #find SST and SSE
    sst <- sum((y - mean(y))^2)
    sse <- sum((y_predicted - y)^2)
    
    #find R-Squared
    rsq <- 1 - sse/sst
    
    rsq <-round(rsq,6)
    
    cat("rsq_0\n")
    cat(sprintf(as.character(rsq)))
    cat("\n")
    
    FLAG_NA_rsq<-sum(is.na(rsq))
    
    if(FLAG_NA_rsq == 0){
      a.df<-as.data.frame(cbind(names_predictors,values_predictors,rep(rsq,length(names_predictors))), stringsAsFactors=F)
      
      colnames(a.df)<-c("variable","s0","rsq")
      
      cat("a.df_0\n")
      cat(str(a.df))
      cat("\n")
    }else{
      
      a.df<-as.data.frame(cbind(names_predictors,values_predictors,rep("NA",length(names_predictors))), stringsAsFactors=F)
      
      colnames(a.df)<-c("variable","s0","rsq")
      
      cat("a.df_NA\n")
      cat(str(a.df))
      cat("\n")
      
    }#FLAG_NA_rsq == 0
    
    a.df$Fold<-fold_array_sel
    
    df_log2FC_meta<-rbind(a.df,df_log2FC_meta)
   
    #### lasso 1 on Activity -----
    
    
    y <- as.numeric(Merged_table$Activity)
    x <- data.matrix(Merged_table[, variables])
    row.names(x)<-Merged_table$VAR
    cv_model <- cv.glmnet(x, y, alpha = 1)
    best_lambda <- cv_model$lambda.min
    
    cat("best_lambda\n")
    cat(sprintf(as.character(best_lambda)))
    cat("\n")
    
    best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
    result<-coef(best_model)
    
    cat("result_0\n")
    cat(str(result))
    cat("\n")
    
    index_predictors<-as.numeric(result@i)+1
    
    cat("index_predictors_0\n")
    cat(str(index_predictors))
    cat("\n")
    
    names_predictors<-result@Dimnames[[1]]
    
    cat("names_predictors_0\n")
    cat(str(names_predictors))
    cat("\n")
    
    values_predictors<-rep('.', length(names_predictors))
    
    cat("values_predictors_0\n")
    cat(sprintf(as.character(values_predictors)))
    cat("\n")
    
    predictors<-round(as.numeric(result@x),6)
    
    cat("predictors_0\n")
    cat(sprintf(as.character(predictors)))
    cat("\n")
    
    if(length(index_predictors) > 0)
      for(i in 1:length(index_predictors)){
        
        index_predictors_sel<-index_predictors[i]
        
        cat("----------------------------------------->\t")
        cat(sprintf(as.character(index_predictors_sel)))
        cat("\n")
        
        values_predictors[index_predictors_sel]<-predictors[i]
        
        
        
      }#i in 1:length(index_predictors)
    
    cat("values_predictors_POST\n")
    cat(sprintf(as.character(values_predictors)))
    cat("\n")
    
    y_predicted <- predict(best_model, s = best_lambda, newx = x)
    
    #find SST and SSE
    sst <- sum((y - mean(y))^2)
    sse <- sum((y_predicted - y)^2)
    
    #find R-Squared
    rsq <- 1 - sse/sst
    
    rsq <-round(rsq,6)
    
    cat("rsq_0\n")
    cat(sprintf(as.character(rsq)))
    cat("\n")
    
    FLAG_NA_rsq<-sum(is.na(rsq))
    
    if(FLAG_NA_rsq == 0){
      a.df<-as.data.frame(cbind(names_predictors,values_predictors,rep(rsq,length(names_predictors))), stringsAsFactors=F)
      
      colnames(a.df)<-c("variable","s0","rsq")
      
      cat("a.df_0\n")
      cat(str(a.df))
      cat("\n")
    }else{
      
      a.df<-as.data.frame(cbind(names_predictors,values_predictors,rep("NA",length(names_predictors))), stringsAsFactors=F)
      
      colnames(a.df)<-c("variable","s0","rsq")
      
      cat("a.df_NA\n")
      cat(str(a.df))
      cat("\n")
      
    }#FLAG_NA_rsq == 0
    
    a.df$Fold<-fold_array_sel
    
    df_Activity<-rbind(a.df,df_Activity)
    
    #### lasso 1 on MPRA_CLASS -----
    
    
    y <- as.numeric(Merged_table$MPRA_CLASS)
    x <- data.matrix(Merged_table[, variables])
    row.names(x)<-Merged_table$VAR
    cv_model <- cv.glmnet(x, y, alpha = 1)
    best_lambda <- cv_model$lambda.min
    
    cat("best_lambda\n")
    cat(sprintf(as.character(best_lambda)))
    cat("\n")
    
    best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
    result<-coef(best_model)
    
    cat("result_0\n")
    cat(str(result))
    cat("\n")
    
    index_predictors<-as.numeric(result@i)+1
    
    cat("index_predictors_0\n")
    cat(str(index_predictors))
    cat("\n")
    
    names_predictors<-result@Dimnames[[1]]
    
    cat("names_predictors_0\n")
    cat(str(names_predictors))
    cat("\n")
    
    values_predictors<-rep('.', length(names_predictors))
    
    cat("values_predictors_0\n")
    cat(sprintf(as.character(values_predictors)))
    cat("\n")
    
    predictors<-round(as.numeric(result@x),6)
    
    cat("predictors_0\n")
    cat(sprintf(as.character(predictors)))
    cat("\n")
    
    if(length(index_predictors) > 0)
      for(i in 1:length(index_predictors)){
        
        index_predictors_sel<-index_predictors[i]
        
        cat("----------------------------------------->\t")
        cat(sprintf(as.character(index_predictors_sel)))
        cat("\n")
        
        values_predictors[index_predictors_sel]<-predictors[i]
        
        
        
      }#i in 1:length(index_predictors)
    
    cat("values_predictors_POST\n")
    cat(sprintf(as.character(values_predictors)))
    cat("\n")
    
    y_predicted <- predict(best_model, s = best_lambda, newx = x)
    
    #find SST and SSE
    sst <- sum((y - mean(y))^2)
    sse <- sum((y_predicted - y)^2)
    
    #find R-Squared
    rsq <- 1 - sse/sst
    
    rsq <-round(rsq,6)
    
    cat("rsq_0\n")
    cat(sprintf(as.character(rsq)))
    cat("\n")
    
    FLAG_NA_rsq<-sum(is.na(rsq))
    
    if(FLAG_NA_rsq == 0){
      a.df<-as.data.frame(cbind(names_predictors,values_predictors,rep(rsq,length(names_predictors))), stringsAsFactors=F)
      
      colnames(a.df)<-c("variable","s0","rsq")
      
      cat("a.df_0\n")
      cat(str(a.df))
      cat("\n")
    }else{
      
      a.df<-as.data.frame(cbind(names_predictors,values_predictors,rep("NA",length(names_predictors))), stringsAsFactors=F)
      
      colnames(a.df)<-c("variable","s0","rsq")
      
      cat("a.df_NA\n")
      cat(str(a.df))
      cat("\n")
      
    }#FLAG_NA_rsq == 0
    
    a.df$Fold<-fold_array_sel
    
    df_MPRA_CLASS<-rbind(a.df,df_MPRA_CLASS)
    
  }# iteration_fold_array in 1:length(fold_array)
  
  setwd(path_lasso)
  
  # saveRDS(result, file="result.rds")
  
  write.table(df_MPRA_CLASS,file=paste(Cell_Type_sel,"_Lasso_regression_MPRA_CLASS.tsv",sep="_"),sep="\t",quote = F, row.names = F)
  
  
  setwd(path_lasso)
  
  # saveRDS(result, file="result.rds")
  
  write.table(df_Activity,file=paste(Cell_Type_sel,"_Lasso_regression_Activity.tsv",sep="_"),sep="\t",quote = F, row.names = F)
  
  
  
  setwd(path_lasso)
  
  # saveRDS(result, file="result.rds")
  
  write.table(df_abs_log2Skew_meta,file=paste(Cell_Type_sel,"_Lasso_regression_abs_log2Skew_meta.tsv",sep="_"),sep="\t",quote = F, row.names = F)
  
  
  setwd(path_lasso)
  
  # saveRDS(result, file="result.rds")
  write.table(df_log2FC_meta,file=paste(Cell_Type_sel,"_Lasso_regression_log2FC_meta.tsv",sep="_"),sep="\t",quote = F, row.names = F)
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
    make_option(c("--MPRA_result_SNP_and_CT"), type="character", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required."),
    make_option(c("--NEW_Table_S6"), type="character", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required."),
    make_option(c("--feature_file"), type="character", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required."),
    make_option(c("--Cell_Type_sel"), type="character", default=NULL, 
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
  
  lasso_regression_MPRA_S(opt)
 


  
}


###########################################################################

system.time( main() )