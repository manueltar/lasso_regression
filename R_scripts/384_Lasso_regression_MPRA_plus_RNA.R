
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

lasso_regression_comp_1 = function(option_list)
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
 
 
  #### Read Enformer_run ----------

  feature_file<-readRDS(file=opt$feature_file)
  
  
  cat("feature_file_0\n")
  cat(str(feature_file))
  cat("\n")
  cat(str(unique(feature_file$VAR)))
  cat("\n")
  
 
  #### READ NEW_Table_S6_with_interaction ----
  
  NEW_Table_S6_with_interaction<-as.data.frame(readRDS(file=opt$NEW_Table_S6_with_interaction), stringsAsFactors=F)
  
  
  cat("NEW_Table_S6_with_interaction_0\n")
  cat(str(NEW_Table_S6_with_interaction))
  cat("\n")
  cat(str(unique(NEW_Table_S6_with_interaction$VAR)))
  cat("\n")
  
  
  NEW_Table_S6_with_interaction_subset<-droplevels(NEW_Table_S6_with_interaction[c(which(as.numeric(NEW_Table_S6_with_interaction$interaction) == 1),
                                                                        which(as.numeric(NEW_Table_S6_with_interaction$interaction) == 2)),])
  
  
  
  cat("NEW_Table_S6_with_interaction_subset_0\n")
  cat(str(NEW_Table_S6_with_interaction_subset))
  cat("\n")
  
  NEW_Table_S6_with_interaction_subset$response<-NA
  
  NEW_Table_S6_with_interaction_subset$response[which(as.numeric(NEW_Table_S6_with_interaction_subset$interaction) == 1)]<-1
  NEW_Table_S6_with_interaction_subset$response[which(as.numeric(NEW_Table_S6_with_interaction_subset$interaction) == 2)]<-0
  
  cat("NEW_Table_S6_with_interaction_subset_1\n")
  cat(str(NEW_Table_S6_with_interaction_subset))
  cat("\n")
  cat(sprintf(as.character(names(summary(NEW_Table_S6_with_interaction_subset$interaction)))))
  cat("\n")
  cat(sprintf(as.character(summary(NEW_Table_S6_with_interaction_subset$interaction))))
  cat("\n")
 
  ##### merge with features ----
  
  Merged_table<-merge(NEW_Table_S6_with_interaction_subset,
                      feature_file,
                      by="VAR")
  
  
  cat("Merged_table_1\n")
  cat(str(Merged_table))
  cat("\n")
  
  
  variables<-colnames(Merged_table)[-which(colnames(Merged_table)%in%c("VAR","cell_string_Activity","Activity","cell_string_MPRA_CLASS","MPRA_CLASS","rs","Mechanistic_Class","Manual_curation",
                                                                       "Candidate_effector","OpenTargets_QTL","Whole_blood_DE_HGNC_string","Whole_blood_DTU_HGNC_string","Monocyte_DE_HGNC_string","Tcell_DE_HGNC_string","Neutrophil_DE_HGNC_string","Neutrophil_DTU_HGNC_string","Tcell_DTU_HGNC_string",
                                                                       "Monocyte_DTU_HGNC_string","Proxy_rsid_string","Replication_OT_QTL","VAR_38","Multi_Lineage","Lineage_string","phenotype_DEF_string","maf_origin","VEP_DEF_LABELS_wCSQ",
                                                                       "integration_category","Mechanistic_Class_compressed","interaction","response"))]
  
  cat("variables_0\n")
  cat(str(variables))
  cat("\n")
 
 
  fold_array<-seq(1,10,by=1)
  
  cat("fold_array_0\n")
  cat(str(fold_array))
  cat("\n")
  
  
  df_response<-data.frame()
  df_MPRA_CLASS<-data.frame()
  
  
  for(iteration_fold_array in 1:length(fold_array)){
    
    fold_array_sel<-fold_array[iteration_fold_array]
    
    cat("----------------------------------------------------------------------->\t")
    cat(sprintf(as.character(fold_array_sel)))
    cat("\n")
    
    #### lasso 1 on response -----
    
    
    y <- as.numeric(Merged_table$response)
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
    
    df_response<-rbind(a.df,df_response)
    
    
    
    
  }# iteration_fold_array in 1:length(fold_array)
  
  
  setwd(path_lasso)
  
  # saveRDS(result, file="result.rds")
  
  write.table(df_response,file=paste("MPRA_vs_RNA","_Lasso_regression_response_comp1.tsv",sep="_"),sep="\t",quote = F, row.names = F)
  
  
}


lasso_regression_comp_2 = function(option_list)
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
  
  
  #### Read Enformer_run ----------
  
  feature_file<-readRDS(file=opt$feature_file)
  
  
  cat("feature_file_0\n")
  cat(str(feature_file))
  cat("\n")
  cat(str(unique(feature_file$VAR)))
  cat("\n")
  
  
  #### READ NEW_Table_S6_with_interaction ----
  
  NEW_Table_S6_with_interaction<-as.data.frame(readRDS(file=opt$NEW_Table_S6_with_interaction), stringsAsFactors=F)
  
  
  cat("NEW_Table_S6_with_interaction_0\n")
  cat(str(NEW_Table_S6_with_interaction))
  cat("\n")
  cat(str(unique(NEW_Table_S6_with_interaction$VAR)))
  cat("\n")
  
  
  NEW_Table_S6_with_interaction_subset<-droplevels(NEW_Table_S6_with_interaction[c(which(as.numeric(NEW_Table_S6_with_interaction$interaction) == 1),
                                                                                   which(as.numeric(NEW_Table_S6_with_interaction$interaction) == 3)),])
  
  
  
  cat("NEW_Table_S6_with_interaction_subset_0\n")
  cat(str(NEW_Table_S6_with_interaction_subset))
  cat("\n")
  
  NEW_Table_S6_with_interaction_subset$response<-NA
  
  
  NEW_Table_S6_with_interaction_subset$response[which(as.numeric(NEW_Table_S6_with_interaction_subset$interaction) == 1)]<-1
  NEW_Table_S6_with_interaction_subset$response[which(as.numeric(NEW_Table_S6_with_interaction_subset$interaction) == 2)]<-0
  
  cat("NEW_Table_S6_with_interaction_subset_1\n")
  cat(str(NEW_Table_S6_with_interaction_subset))
  cat("\n")
  cat(sprintf(as.character(names(summary(NEW_Table_S6_with_interaction_subset$interaction)))))
  cat("\n")
  cat(sprintf(as.character(summary(NEW_Table_S6_with_interaction_subset$interaction))))
  cat("\n")
  cat(sprintf(as.character(names(summary(as.factor(NEW_Table_S6_with_interaction_subset$response))))))
  cat("\n")
  cat(sprintf(as.character(summary(as.factor(NEW_Table_S6_with_interaction_subset$response)))))
  cat("\n")
  
  ##### merge with features ----
  
  Merged_table<-merge(NEW_Table_S6_with_interaction_subset,
                      feature_file,
                      by="VAR")
  
  
  cat("Merged_table_1\n")
  cat(str(Merged_table))
  cat("\n")
  
  
  variables<-colnames(Merged_table)[-which(colnames(Merged_table)%in%c("VAR","cell_string_Activity","Activity","cell_string_MPRA_CLASS","MPRA_CLASS","rs","Mechanistic_Class","Manual_curation",
                                                                       "Candidate_effector","OpenTargets_QTL","Whole_blood_DE_HGNC_string","Whole_blood_DTU_HGNC_string","Monocyte_DE_HGNC_string","Tcell_DE_HGNC_string","Neutrophil_DE_HGNC_string","Neutrophil_DTU_HGNC_string","Tcell_DTU_HGNC_string",
                                                                       "Monocyte_DTU_HGNC_string","Proxy_rsid_string","Replication_OT_QTL","VAR_38","Multi_Lineage","Lineage_string","phenotype_DEF_string","maf_origin","VEP_DEF_LABELS_wCSQ",
                                                                       "integration_category","Mechanistic_Class_compressed","interaction","response"))]
  
  cat("variables_0\n")
  cat(str(variables))
  cat("\n")
  
  
  fold_array<-seq(1,10,by=1)
  
  cat("fold_array_0\n")
  cat(str(fold_array))
  cat("\n")
  
  
  df_response<-data.frame()
  df_MPRA_CLASS<-data.frame()
  
  
  for(iteration_fold_array in 1:length(fold_array)){
    
    fold_array_sel<-fold_array[iteration_fold_array]
    
    cat("----------------------------------------------------------------------->\t")
    cat(sprintf(as.character(fold_array_sel)))
    cat("\n")
    
    #### lasso 1 on response -----
    
    
    y <- as.numeric(Merged_table$response)
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
    
    df_response<-rbind(a.df,df_response)
    
    
    
    
  }# iteration_fold_array in 1:length(fold_array)
  
  
  setwd(path_lasso)
  
  # saveRDS(result, file="result.rds")
  
  write.table(df_response,file=paste("MPRA_vs_RNA","_Lasso_regression_response_comp2.tsv",sep="_"),sep="\t",quote = F, row.names = F)
  
  
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
    make_option(c("--NEW_Table_S6_with_interaction"), type="character", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required."),
    make_option(c("--feature_file"), type="character", default=NULL, 
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
  
  lasso_regression_comp_1(opt)
  lasso_regression_comp_2(opt)
 


  
}


###########################################################################

system.time( main() )