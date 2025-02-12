
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

heatmap_function = function(option_list)
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
  

  
  path_graphs = paste(out,'heatmaps','/', sep='')
  
  if (file.exists(path_graphs)){
    
    
  }else{
    
    dir.create(file.path(path_graphs))
    
  }#path_graphs
  
  #### READ and transform GWAS_parameters ----
  
  GWAS_parameters = unlist(strsplit(opt$GWAS_parameters, split=','))
  
  cat("GWAS_parameters_\n")
  cat(sprintf(as.character(GWAS_parameters)))
  cat("\n")
  
  #### READ and transform Variant_based_scores ----
  
  Variant_based_scores = unlist(strsplit(opt$Variant_based_scores, split=','))
  
  cat("Variant_based_scores_\n")
  cat(sprintf(as.character(Variant_based_scores)))
  cat("\n")
  
  
  #### READ and transform out ----
  
  Our_rankings = unique(unlist(strsplit(opt$Our_rankings, split=',')))
  
  cat("Our_rankings_\n")
  cat(sprintf(as.character(Our_rankings)))
  cat("\n")
  
  #### READ and transform Gene_based_features ----
  
  Gene_based_features = unique(unlist(strsplit(opt$Gene_based_features, split=',')))
  
  cat("Gene_based_features_\n")
  cat(sprintf(as.character(Gene_based_features)))
  cat("\n")
  
  #### READ and transform Lineages ----
  
  Lineages = unique(unlist(strsplit(opt$Lineages, split=',')))
  
  cat("Lineages_\n")
  cat(sprintf(as.character(Lineages)))
  cat("\n")
  
  #### READ and transform coefficient_file_Activity_selected ----
  

  coefficient_file_Activity_selected<-readRDS(opt$coefficient_file_Activity_selected)
  
  
  cat("coefficient_file_Activity_selected_0\n")
  cat(str(coefficient_file_Activity_selected))
  cat("\n")
  
  ##### heatmap 1 ----
  
  REP<-unique(coefficient_file_Activity_selected[-which(coefficient_file_Activity_selected$variable == '(Intercept)'),])
  REP$s0<-as.numeric(REP$s0)
  
  REP<-REP[which(REP$s0 > 0 | REP$s0 < 0),]
  
  cat("REP_0\n")
  cat(str(REP))
  cat("\n")
  
  levels_CT<-unique(REP$Cell_Type)
  
  cat("levels_CT_0\n")
  cat(str(levels_CT))
  cat("\n")
  
  REP.dt<-data.table(REP, key="variable")
  
  Freq.table<-as.data.frame(REP.dt[,.(Freq=.N), by=key(REP.dt)], stringsAsFactors=F)
  Freq.table<-Freq.table[order(Freq.table$Freq, decreasing = T),]
  
  cat("Freq.table_0\n")
  cat(str(Freq.table))
  cat("\n")
  
  levels_variable<-unique(Freq.table$variable)
  
  cat("levels_variable_0\n")
  cat(str(levels_variable))
  cat("\n")
  
  REP$variable<-factor(REP$variable,
                       levels=levels_variable,
                       ordered=T)
  
  REP$Cell_Type<-factor(REP$Cell_Type,
                       levels=rev(levels_CT),
                       ordered=T)
  

  cat("REP_1\n")
  cat(str(REP))
  cat("\n")
  
  
  
  
  
  indx_s0<-which(colnames(REP) =='s0')
  
  summary_indx_s0<-summary(REP[,indx_s0])
  
  cat("summary_indx_s0\n")
  cat(sprintf(as.character(names(summary_indx_s0))))
  cat("\n")
  cat(sprintf(as.character(summary_indx_s0)))
  cat("\n")
  
  max_abs_value<-abs(summary_indx_s0[6])
  min_abs_value<-abs(summary_indx_s0[1])
  
  if(max_abs_value > min_abs_value)
  {
    max_abs_value<-1.01*max_abs_value
    
    step<-round(abs(max_abs_value--1*max_abs_value)/4,3)
    
    breaks_s0<-unique(sort(round(c(0,max_abs_value,seq(-1*max_abs_value,max_abs_value, by=step)),3)))
    # labels_s0<-as.character(round(10^breaks_s0,3))
    labels_s0<-as.character(breaks_s0)
    
  }else{
    
    min_abs_value<-1.01*min_abs_value
    
    
    step<-round(abs(min_abs_value--1*min_abs_value)/4,3)
    
    breaks_s0<-unique(sort(round(c(0,min_abs_value,seq(-1*min_abs_value,min_abs_value, by=step)),3)))
    # labels_s0<-as.character(round(10^breaks_s0,3))
    labels_s0<-as.character(breaks_s0)
    
    
  }# max_abs_value > min_abs_value
  
  cat("labels_s0\n")
  cat(sprintf(as.character(labels_s0)))
  cat("\n")
  
  
  # scale_fill_gradient2(name=paste("Lasso","coefficient", sep="\n"),
  #                      low = "blue", high = "red",mid="white",midpoint=0,
  #                      na.value = NA,
  #                      breaks=breaks_s0,
  #                      labels=labels_s0,
  #                      limits=c(breaks_s0[1],
  #                               breaks_s0[length(breaks_s0)]))+
    
  
  heatmap_Activity <-ggplot(data=REP,
                           aes(x=variable, 
                               y=Cell_Type, 
                               fill = REP[,indx_s0]))+
    geom_tile(color = "black", size = 0.5)+
    scale_fill_gradient2(name=paste("Lasso","coefficient", sep="\n"),
                         low = "blue", high = "red",mid="white",midpoint=0,
                         na.value = NA)+
    theme_minimal()+ # minimal theme
    scale_y_discrete(name=NULL, drop=F)+
    scale_x_discrete(name=NULL, drop=F)+
    ggtitle(paste("Lasso regression on MPRA enhancer active/inactive predictors",sep=''))+
    theme(plot.title=element_text(size=8, color="black", family="sans"),
          axis.title=element_blank(),
          axis.title.y=element_blank(),
          axis.title.x=element_blank(),
          axis.text.y=element_text(size=6,color="black", family="sans", face='bold'),
          axis.text.x=element_text(angle=45,hjust=1,vjust=1,size=6,color="black", family="sans"),
          axis.line.x = element_line(size = 0.2),
          axis.ticks.x = element_line(size = 0.2),
          axis.ticks.y = element_line(size = 0.2),
          axis.line.y = element_line(size = 0.2))+
    theme(legend.title = element_text(size=6),
          legend.text = element_text(size=6),
          legend.key.size = unit(0.35, 'cm'), #change legend key size
          legend.key.height = unit(0.35, 'cm'), #change legend key height
          legend.key.width = unit(0.35, 'cm'), #change legend key width
          legend.position="right")+
    coord_fixed()
  
  
  setwd(path_graphs)
  
  svgname<-paste("heatmap_Activity",".svg", sep='')
  
  
  ggsave(svgname, plot= heatmap_Activity,
         device="svg",
         height=5, width=8)
  
  setwd(out)
  
  saveRDS(REP, file="REP_Activity.rds")
 
  
  
  #### READ and transform coefficient_file_MPRA_CLASS_selected ----
  
  coefficient_file_MPRA_CLASS_selected<-readRDS(opt$coefficient_file_MPRA_CLASS_selected)
  
  cat("coefficient_file_MPRA_CLASS_selected_0\n")
  cat(str(coefficient_file_MPRA_CLASS_selected))
  cat("\n")
  
  ##### heatmap 2 ----
  
  REP<-unique(coefficient_file_MPRA_CLASS_selected[-which(coefficient_file_MPRA_CLASS_selected$variable == '(Intercept)' | coefficient_file_MPRA_CLASS_selected$variable == 'MAF'),])
  REP$s0<-as.numeric(REP$s0)
  
  REP<-REP[which(REP$s0 > 0 | REP$s0 < 0),]
  
  cat("REP_0\n")
  cat(str(REP))
  cat("\n")
  
  levels_CT<-unique(REP$Cell_Type)
  
  cat("levels_CT_0\n")
  cat(str(levels_CT))
  cat("\n")
  
  REP.dt<-data.table(REP, key="variable")
  
  Freq.table<-as.data.frame(REP.dt[,.(Freq=.N), by=key(REP.dt)], stringsAsFactors=F)
  Freq.table<-Freq.table[order(Freq.table$Freq, decreasing = T),]
  
  cat("Freq.table_0\n")
  cat(str(Freq.table))
  cat("\n")
  
  levels_variable<-unique(Freq.table$variable)
  
  cat("levels_variable_0\n")
  cat(str(levels_variable))
  cat("\n")
  
  REP$variable<-factor(REP$variable,
                       levels=levels_variable,
                       ordered=T)
  
  REP$Cell_Type<-factor(REP$Cell_Type,
                        levels=rev(levels_CT),
                        ordered=T)
  

  cat("REP_1\n")
  cat(str(REP))
  cat("\n")
  
  
  
  
  
  indx_s0<-which(colnames(REP) =='s0')
  
  summary_indx_s0<-summary(REP[,indx_s0])
  
  cat("summary_indx_s0\n")
  cat(sprintf(as.character(names(summary_indx_s0))))
  cat("\n")
  cat(sprintf(as.character(summary_indx_s0)))
  cat("\n")
  
  max_abs_value<-abs(summary_indx_s0[6])
  min_abs_value<-abs(summary_indx_s0[1])
  
  if(max_abs_value > min_abs_value)
  {
    max_abs_value<-1.01*max_abs_value
    
    step<-round(abs(max_abs_value--1*max_abs_value)/4,3)
    
    breaks_s0<-unique(sort(round(c(0,max_abs_value,seq(-1*max_abs_value,max_abs_value, by=step)),3)))
    # labels_s0<-as.character(round(10^breaks_s0,3))
    labels_s0<-as.character(breaks_s0)
    
  }else{
    
    min_abs_value<-1.01*min_abs_value
    
    
    step<-round(abs(min_abs_value--1*min_abs_value)/4,3)
    
    breaks_s0<-unique(sort(round(c(0,min_abs_value,seq(-1*min_abs_value,min_abs_value, by=step)),3)))
    # labels_s0<-as.character(round(10^breaks_s0,3))
    labels_s0<-as.character(breaks_s0)
    
    
  }# max_abs_value > min_abs_value
  
  cat("labels_s0\n")
  cat(sprintf(as.character(labels_s0)))
  cat("\n")
  
  
  
  # scale_fill_gradient2(name=paste("Lasso","coefficient", sep="\n"),
  #                      low = "blue", high = "red",mid="white",midpoint=0,
  #                      na.value = NA,
  #                      breaks=breaks_s0,
  #                      labels=labels_s0,
  #                      limits=c(breaks_s0[1],
  #                               breaks_s0[length(breaks_s0)]))+
  
  heatmap_MPRA_CLASS <-ggplot(data=REP,
                         aes(x=variable, 
                             y=Cell_Type, 
                             fill = REP[,indx_s0]))+
    geom_tile(color = "black", size = 0.5)+
    scale_fill_gradient2(name=paste("Lasso","coefficient", sep="\n"),
                         low = "blue", high = "red",mid="white",midpoint=0,
                         na.value = NA)+
    theme_minimal()+ # minimal theme
    scale_y_discrete(name=NULL, drop=F)+
    scale_x_discrete(name=NULL, drop=F)+
    ggtitle(paste("Lasso regression on MPRA positive/negative predictors",sep=''))+
    theme(plot.title=element_text(size=8, color="black", family="sans"),
          axis.title=element_blank(),
          axis.title.y=element_blank(),
          axis.title.x=element_blank(),
          axis.text.y=element_text(size=6,color="black", family="sans", face='bold'),
          axis.text.x=element_text(angle=45,hjust=1,vjust=1,size=6,color="black", family="sans"),
          axis.line.x = element_line(size = 0.2),
          axis.ticks.x = element_line(size = 0.2),
          axis.ticks.y = element_line(size = 0.2),
          axis.line.y = element_line(size = 0.2))+
    theme(legend.title = element_text(size=6),
          legend.text = element_text(size=6),
          legend.key.size = unit(0.35, 'cm'), #change legend key size
          legend.key.height = unit(0.35, 'cm'), #change legend key height
          legend.key.width = unit(0.35, 'cm'), #change legend key width
          legend.position="right")+
    coord_fixed()
  
  
  setwd(path_graphs)
  
  svgname<-paste("heatmap_MPRA_CLASS",".svg", sep='')
  
  
  ggsave(svgname, plot= heatmap_MPRA_CLASS,
         device="svg",
         height=5, width=8)
  
  setwd(out)
  
  saveRDS(REP, file="REP_MPRA_CLASS.rds")
  
 
  #### READ and transform coefficient_file_log2FC_meta_selected ----
  
  coefficient_file_log2FC_meta_selected<-readRDS(opt$coefficient_file_log2FC_meta_selected)
  
  cat("coefficient_file_log2FC_meta_selected_0\n")
  cat(str(coefficient_file_log2FC_meta_selected))
  cat("\n")
  
  ##### heatmap 3 ----
  
  REP<-unique(coefficient_file_log2FC_meta_selected[-which(coefficient_file_log2FC_meta_selected$variable == '(Intercept)'),])
  REP$s0<-as.numeric(REP$s0)
  
  REP<-REP[which(REP$s0 > 0 | REP$s0 < 0),]
  
  cat("REP_0\n")
  cat(str(REP))
  cat("\n")
  
  levels_CT<-unique(REP$Cell_Type)
  
  cat("levels_CT_0\n")
  cat(str(levels_CT))
  cat("\n")
  
  REP.dt<-data.table(REP, key="variable")
  
  Freq.table<-as.data.frame(REP.dt[,.(Freq=.N), by=key(REP.dt)], stringsAsFactors=F)
  Freq.table<-Freq.table[order(Freq.table$Freq, decreasing = T),]
  
  cat("Freq.table_0\n")
  cat(str(Freq.table))
  cat("\n")
  
  levels_variable<-unique(Freq.table$variable)
  
  cat("levels_variable_0\n")
  cat(str(levels_variable))
  cat("\n")
  
  REP$variable<-factor(REP$variable,
                       levels=levels_variable,
                       ordered=T)
  
  REP$Cell_Type<-factor(REP$Cell_Type,
                        levels=rev(levels_CT),
                        ordered=T)
  
  
  cat("REP_1\n")
  cat(str(REP))
  cat("\n")
  
  
  
  
  
  indx_s0<-which(colnames(REP) =='s0')
  
  summary_indx_s0<-summary(REP[,indx_s0])
  
  cat("summary_indx_s0\n")
  cat(sprintf(as.character(names(summary_indx_s0))))
  cat("\n")
  cat(sprintf(as.character(summary_indx_s0)))
  cat("\n")
  
  max_abs_value<-abs(summary_indx_s0[6])
  min_abs_value<-abs(summary_indx_s0[1])
  
  if(max_abs_value > min_abs_value)
  {
    max_abs_value<-1.01*max_abs_value
    
    step<-round(abs(max_abs_value--1*max_abs_value)/4,3)
    
    breaks_s0<-unique(sort(round(c(0,max_abs_value,seq(-1*max_abs_value,max_abs_value, by=step)),3)))
    # labels_s0<-as.character(round(10^breaks_s0,3))
    labels_s0<-as.character(breaks_s0)
    
  }else{
    
    min_abs_value<-1.01*min_abs_value
    
    
    step<-round(abs(min_abs_value--1*min_abs_value)/4,3)
    
    breaks_s0<-unique(sort(round(c(0,min_abs_value,seq(-1*min_abs_value,min_abs_value, by=step)),3)))
    # labels_s0<-as.character(round(10^breaks_s0,3))
    labels_s0<-as.character(breaks_s0)
    
    
  }# max_abs_value > min_abs_value
  
  cat("labels_s0\n")
  cat(sprintf(as.character(labels_s0)))
  cat("\n")
  
  
  
  # scale_fill_gradient2(name=paste("Lasso","coefficient", sep="\n"),
  #                      low = "blue", high = "red",mid="white",midpoint=0,
  #                      na.value = NA,
  #                      breaks=breaks_s0,
  #                      labels=labels_s0,
  #                      limits=c(breaks_s0[1],
  #                               breaks_s0[length(breaks_s0)]))+
  
  heatmap_log2FC_meta <-ggplot(data=REP,
                              aes(x=variable, 
                                  y=Cell_Type, 
                                  fill = REP[,indx_s0]))+
    geom_tile(color = "black", size = 0.5)+
    scale_fill_gradient2(name=paste("Lasso","coefficient", sep="\n"),
                         low = "blue", high = "red",mid="white",midpoint=0,
                         na.value = NA)+
    theme_minimal()+ # minimal theme
    scale_y_discrete(name=NULL, drop=F)+
    scale_x_discrete(name=NULL, drop=F)+
    ggtitle(paste("Lasso regression on log2FC_meta predictors",sep=''))+
    theme(plot.title=element_text(size=8, color="black", family="sans"),
          axis.title=element_blank(),
          axis.title.y=element_blank(),
          axis.title.x=element_blank(),
          axis.text.y=element_text(size=6,color="black", family="sans", face='bold'),
          axis.text.x=element_text(angle=45,hjust=1,vjust=1,size=6,color="black", family="sans"),
          axis.line.x = element_line(size = 0.2),
          axis.ticks.x = element_line(size = 0.2),
          axis.ticks.y = element_line(size = 0.2),
          axis.line.y = element_line(size = 0.2))+
    theme(legend.title = element_text(size=6),
          legend.text = element_text(size=6),
          legend.key.size = unit(0.35, 'cm'), #change legend key size
          legend.key.height = unit(0.35, 'cm'), #change legend key height
          legend.key.width = unit(0.35, 'cm'), #change legend key width
          legend.position="right")+
    coord_fixed()
  
  
  setwd(path_graphs)
  
  svgname<-paste("heatmap_log2FC_meta",".svg", sep='')
  
  
  ggsave(svgname, plot= heatmap_log2FC_meta,
         device="svg",
         height=5, width=8)
  
  setwd(out)
  
  saveRDS(REP, file="REP_log2FC_meta.rds")
  
  #### READ and transform coefficient_file_abs_log2Skew_meta_selected ----
  
  coefficient_file_abs_log2Skew_meta_selected<-readRDS(opt$coefficient_file_abs_log2Skew_meta_selected)
  
  cat("coefficient_file_abs_log2Skew_meta_selected_0\n")
  cat(str(coefficient_file_abs_log2Skew_meta_selected))
  cat("\n")
  
  ##### heatmap 4 ----
  
  REP<-unique(coefficient_file_abs_log2Skew_meta_selected[-which(coefficient_file_abs_log2Skew_meta_selected$variable == '(Intercept)'),])
  REP$s0<-as.numeric(REP$s0)
  
  REP<-REP[which(REP$s0 > 0 | REP$s0 < 0),]
  
  cat("REP_0\n")
  cat(str(REP))
  cat("\n")
  
  levels_CT<-unique(REP$Cell_Type)
  
  cat("levels_CT_0\n")
  cat(str(levels_CT))
  cat("\n")
  
  REP.dt<-data.table(REP, key="variable")
  
  Freq.table<-as.data.frame(REP.dt[,.(Freq=.N), by=key(REP.dt)], stringsAsFactors=F)
  Freq.table<-Freq.table[order(Freq.table$Freq, decreasing = T),]
  
  cat("Freq.table_0\n")
  cat(str(Freq.table))
  cat("\n")
  
  levels_variable<-unique(Freq.table$variable)
  
  cat("levels_variable_0\n")
  cat(str(levels_variable))
  cat("\n")
  
  REP$variable<-factor(REP$variable,
                       levels=levels_variable,
                       ordered=T)
  
  REP$Cell_Type<-factor(REP$Cell_Type,
                        levels=rev(levels_CT),
                        ordered=T)
  
  
  cat("REP_1\n")
  cat(str(REP))
  cat("\n")
  
  
  
  
  
  indx_s0<-which(colnames(REP) =='s0')
  
  summary_indx_s0<-summary(REP[,indx_s0])
  
  cat("summary_indx_s0\n")
  cat(sprintf(as.character(names(summary_indx_s0))))
  cat("\n")
  cat(sprintf(as.character(summary_indx_s0)))
  cat("\n")
  
  max_abs_value<-abs(summary_indx_s0[6])
  min_abs_value<-abs(summary_indx_s0[1])
  
  if(max_abs_value > min_abs_value)
  {
    max_abs_value<-1.01*max_abs_value
    
    step<-round(abs(max_abs_value--1*max_abs_value)/4,3)
    
    breaks_s0<-unique(sort(round(c(0,max_abs_value,seq(-1*max_abs_value,max_abs_value, by=step)),3)))
    # labels_s0<-as.character(round(10^breaks_s0,3))
    labels_s0<-as.character(breaks_s0)
    
  }else{
    
    min_abs_value<-1.01*min_abs_value
    
    
    step<-round(abs(min_abs_value--1*min_abs_value)/4,3)
    
    breaks_s0<-unique(sort(round(c(0,min_abs_value,seq(-1*min_abs_value,min_abs_value, by=step)),3)))
    # labels_s0<-as.character(round(10^breaks_s0,3))
    labels_s0<-as.character(breaks_s0)
    
    
  }# max_abs_value > min_abs_value
  
  cat("labels_s0\n")
  cat(sprintf(as.character(labels_s0)))
  cat("\n")
  
  
  
  # scale_fill_gradient2(name=paste("Lasso","coefficient", sep="\n"),
  #                      low = "blue", high = "red",mid="white",midpoint=0,
  #                      na.value = NA,
  #                      breaks=breaks_s0,
  #                      labels=labels_s0,
  #                      limits=c(breaks_s0[1],
  #                               breaks_s0[length(breaks_s0)]))+
  
  heatmap_abs_log2Skew_meta <-ggplot(data=REP,
                               aes(x=variable, 
                                   y=Cell_Type, 
                                   fill = REP[,indx_s0]))+
    geom_tile(color = "black", size = 0.5)+
    scale_fill_gradient2(name=paste("Lasso","coefficient", sep="\n"),
                         low = "blue", high = "red",mid="white",midpoint=0,
                         na.value = NA)+
    theme_minimal()+ # minimal theme
    scale_y_discrete(name=NULL, drop=F)+
    scale_x_discrete(name=NULL, drop=F)+
    ggtitle(paste("Lasso regression on abs_log2Skew_meta predictors",sep=''))+
    theme(plot.title=element_text(size=8, color="black", family="sans"),
          axis.title=element_blank(),
          axis.title.y=element_blank(),
          axis.title.x=element_blank(),
          axis.text.y=element_text(size=6,color="black", family="sans", face='bold'),
          axis.text.x=element_text(angle=45,hjust=1,vjust=1,size=6,color="black", family="sans"),
          axis.line.x = element_line(size = 0.2),
          axis.ticks.x = element_line(size = 0.2),
          axis.ticks.y = element_line(size = 0.2),
          axis.line.y = element_line(size = 0.2))+
    theme(legend.title = element_text(size=6),
          legend.text = element_text(size=6),
          legend.key.size = unit(0.35, 'cm'), #change legend key size
          legend.key.height = unit(0.35, 'cm'), #change legend key height
          legend.key.width = unit(0.35, 'cm'), #change legend key width
          legend.position="right")+
    coord_fixed()
  
  
  setwd(path_graphs)
  
  svgname<-paste("heatmap_abs_log2Skew_meta",".svg", sep='')
  
  
  ggsave(svgname, plot= heatmap_abs_log2Skew_meta,
         device="svg",
         height=5, width=8)
  
  setwd(out)
  
  saveRDS(REP, file="REP_abs_log2Skew_meta.rds")
  
}


heatmap_function_discretized = function(option_list)
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
  
  
  
  path_graphs = paste(out,'heatmaps','/', sep='')
  
  if (file.exists(path_graphs)){
    
    
  }else{
    
    dir.create(file.path(path_graphs))
    
  }#path_graphs
  
  #### READ and transform GWAS_parameters ----
  
  GWAS_parameters = unlist(strsplit(opt$GWAS_parameters, split=','))
  
  cat("GWAS_parameters_\n")
  cat(sprintf(as.character(GWAS_parameters)))
  cat("\n")
  
  #### READ and transform Variant_based_scores ----
  
  Variant_based_scores = unlist(strsplit(opt$Variant_based_scores, split=','))
  
  cat("Variant_based_scores_\n")
  cat(sprintf(as.character(Variant_based_scores)))
  cat("\n")
  
  
  #### READ and transform out ----
  
  Our_rankings = unique(unlist(strsplit(opt$Our_rankings, split=',')))
  
  cat("Our_rankings_\n")
  cat(sprintf(as.character(Our_rankings)))
  cat("\n")
  
  #### READ and transform Gene_based_features ----
  
  Gene_based_features = unique(unlist(strsplit(opt$Gene_based_features, split=',')))
  
  cat("Gene_based_features_\n")
  cat(sprintf(as.character(Gene_based_features)))
  cat("\n")
  
  #### READ and transform Lineages ----
  
  Lineages = unique(unlist(strsplit(opt$Lineages, split=',')))
  
  cat("Lineages_\n")
  cat(sprintf(as.character(Lineages)))
  cat("\n")
  
  
  #### READ and transform coefficient_file_MPRA_CLASS_selected ----
  
  coefficient_file_MPRA_CLASS_selected<-readRDS(opt$coefficient_file_MPRA_CLASS_selected)
  
  cat("coefficient_file_MPRA_CLASS_selected_0\n")
  cat(str(coefficient_file_MPRA_CLASS_selected))
  cat("\n")
  
  ##### heatmap 2 ----
  
  REP<-unique(coefficient_file_MPRA_CLASS_selected[-which(coefficient_file_MPRA_CLASS_selected$variable == '(Intercept)'),])
  REP$s0<-as.numeric(REP$s0)
  
  
  REP<-REP[which(REP$s0 > 0 | REP$s0 < 0),]
  
  cat("REP_0\n")
  cat(str(REP))
  cat("\n")
  
  REP$Y1<-cut(REP$s0,breaks = c(-Inf,-0.1,-0.05,0,0.05,0.1,Inf),right = FALSE)
  
  cat("REP_1\n")
  cat(str(REP))
  cat("\n")
  cat(sprintf(as.character(names(summary(REP$Y1)))))
  cat("\n")
  cat(sprintf(as.character(summary(REP$Y1))))
  cat("\n")
  
  levels_CT<-unique(REP$Cell_Type)
  
  cat("levels_CT_0\n")
  cat(str(levels_CT))
  cat("\n")
  
  REP.dt<-data.table(REP, key="variable")
  
  Freq.table<-as.data.frame(REP.dt[,.(Freq=.N), by=key(REP.dt)], stringsAsFactors=F)
  Freq.table<-Freq.table[order(Freq.table$Freq, decreasing = T),]
  
  cat("Freq.table_0\n")
  cat(str(Freq.table))
  cat("\n")
  
  levels_variable<-unique(Freq.table$variable)
  
  cat("levels_variable_0\n")
  cat(str(levels_variable))
  cat("\n")
  
  REP$variable<-factor(REP$variable,
                       levels=levels_variable,
                       ordered=T)
  
  REP$Cell_Type<-factor(REP$Cell_Type,
                        levels=rev(levels_CT),
                        ordered=T)
  
  
  cat("REP_2\n")
  cat(str(REP))
  cat("\n")
  
  vector_fill<-c(rev(brewer.pal(9, "Blues")[c(4,5,7)]),brewer.pal(9, "Reds")[c(4,5,7)])
  
  cat("vector_fill_0\n")
  cat(str(vector_fill))
  cat("\n")
  
  heatmap_MPRA_CLASS <-ggplot(data=REP,
                              aes(x=variable, 
                                  y=Cell_Type))+
    geom_tile(aes(fill = REP$Y1), color = "black", size = 0.5)+
    scale_fill_manual(name=paste("Lasso","coefficient", sep="\n"),
                         breaks=c("[-Inf,-0.1)", 
                                  "[-0.1,-0.05)",
                                  "[-0.05,0)",
                                  "[0,0.05)",
                                  "[0.05,0.1)",
                                  "[0.1, Inf)"),
                          values=vector_fill)+
    theme_minimal()+ # minimal theme
    scale_y_discrete(name=NULL, drop=F)+
    scale_x_discrete(name=NULL, drop=F)+
    ggtitle(paste("Lasso regression on MPRA positive/negative predictors",sep=''))+
    theme(plot.title=element_text(color="black", family="sans"),
          axis.title=element_blank(),
          axis.title.y=element_blank(),
          axis.title.x=element_blank(),
          axis.text.y=element_text(color="black", family="sans", face='bold'),
          axis.text.x=element_text(angle=45,hjust=1,vjust=1,color="black", family="sans"))+
    theme(legend.title = element_text(family="sans"),
          legend.text = element_text(family="sans"),
          legend.key.size = unit(0.35, 'cm'), #change legend key size
          legend.key.height = unit(0.35, 'cm'), #change legend key height
          legend.key.width = unit(0.35, 'cm'), #change legend key width
          legend.position="right")+
    coord_fixed()
  
  
  setwd(path_graphs)
  
  svgname<-paste("heatmap_MPRA_CLASS_discretized",".svg", sep='')
  
  
  ggsave(svgname, plot= heatmap_MPRA_CLASS,
         device="svg")
  
  #### READ and transform coefficient_file_log2FC_meta_selected ----
  
  coefficient_file_log2FC_meta_selected<-readRDS(opt$coefficient_file_log2FC_meta_selected)
  
  cat("coefficient_file_log2FC_meta_selected_0\n")
  cat(str(coefficient_file_log2FC_meta_selected))
  cat("\n")
  
  ##### heatmap 3 ----
  
  REP<-unique(coefficient_file_log2FC_meta_selected[-which(coefficient_file_log2FC_meta_selected$variable == '(Intercept)'),])
  REP$s0<-as.numeric(REP$s0)
  
  
  REP<-REP[which(REP$s0 > 0 | REP$s0 < 0),]
  
  cat("REP_0\n")
  cat(str(REP))
  cat("\n")
  
  REP$Y1<-cut(REP$s0,breaks = c(-5,-3,-0.05,-0.01,0,0.01,0.05,3,5),right = FALSE)
  
  cat("REP_1\n")
  cat(str(REP))
  cat("\n")
  cat(sprintf(as.character(names(summary(REP$Y1)))))
  cat("\n")
  cat(sprintf(as.character(summary(REP$Y1))))
  cat("\n")
  
  levels_CT<-unique(REP$Cell_Type)
  
  cat("levels_CT_0\n")
  cat(str(levels_CT))
  cat("\n")
  
  REP.dt<-data.table(REP, key="variable")
  
  Freq.table<-as.data.frame(REP.dt[,.(Freq=.N), by=key(REP.dt)], stringsAsFactors=F)
  Freq.table<-Freq.table[order(Freq.table$Freq, decreasing = T),]
  
  cat("Freq.table_0\n")
  cat(str(Freq.table))
  cat("\n")
  
  levels_variable<-unique(Freq.table$variable)
  
  cat("levels_variable_0\n")
  cat(str(levels_variable))
  cat("\n")
  
  REP$variable<-factor(REP$variable,
                       levels=levels_variable,
                       ordered=T)
  
  REP$Cell_Type<-factor(REP$Cell_Type,
                        levels=rev(levels_CT),
                        ordered=T)
  
  
  cat("REP_2\n")
  cat(str(REP))
  cat("\n")
  
  vector_fill<-c(rev(brewer.pal(9, "Blues")[c(4,5,7)]),brewer.pal(9, "Reds")[c(4,5,7)])
  
  cat("vector_fill_0\n")
  cat(str(vector_fill))
  cat("\n")
  
  heatmap_log2FC_meta <-ggplot(data=REP,
                              aes(x=variable, 
                                  y=Cell_Type))+
    geom_tile(aes(fill = REP$Y1), color = "black", size = 0.5)+
    scale_fill_manual(name=paste("Lasso","coefficient", sep="\n"),
                      values=vector_fill)+
    theme_minimal()+ # minimal theme
    scale_y_discrete(name=NULL, drop=F)+
    scale_x_discrete(name=NULL, drop=F)+
    ggtitle(paste("Lasso regression on log2FC_meta predictors",sep=''))+
    theme(plot.title=element_text(color="black", family="sans"),
          axis.title=element_blank(),
          axis.title.y=element_blank(),
          axis.title.x=element_blank(),
          axis.text.y=element_text(color="black", family="sans"),
          axis.text.x=element_text(angle=45,hjust=1,vjust=1,color="black", family="sans"))+
    theme(legend.title = element_text(family="sans"),
          legend.text = element_text(family="sans"),
          legend.key.size = unit(0.35, 'cm'), #change legend key size
          legend.key.height = unit(0.35, 'cm'), #change legend key height
          legend.key.width = unit(0.35, 'cm'), #change legend key width
          legend.position="right")+
    coord_fixed()
  
  
  setwd(path_graphs)
  
  svgname<-paste("heatmap_log2FC_meta_discretized",".svg", sep='')
  
  
  ggsave(svgname, plot= heatmap_log2FC_meta,
         device="svg")
  

  
  #### READ and transform coefficient_file_abs_log2Skew_meta_selected ----
  
  coefficient_file_abs_log2Skew_meta_selected<-readRDS(opt$coefficient_file_abs_log2Skew_meta_selected)
  
  cat("coefficient_file_abs_log2Skew_meta_selected_0\n")
  cat(str(coefficient_file_abs_log2Skew_meta_selected))
  cat("\n")
  
  ##### heatmap 4 ----
  
  REP<-unique(coefficient_file_abs_log2Skew_meta_selected[-which(coefficient_file_abs_log2Skew_meta_selected$variable == '(Intercept)'),])
  REP$s0<-as.numeric(REP$s0)
  
  
  REP<-REP[which(REP$s0 > 0 | REP$s0 < 0),]
  
  cat("REP_0\n")
  cat(str(REP))
  cat("\n")
  
  REP$Y1<-cut(REP$s0,breaks = c(-5,-0.1,-0.01,0,0.01,0.1,5),right = FALSE)
  
  cat("REP_1\n")
  cat(str(REP))
  cat("\n")
  cat(sprintf(as.character(names(summary(REP$Y1)))))
  cat("\n")
  cat(sprintf(as.character(summary(REP$Y1))))
  cat("\n")
  
  levels_CT<-unique(REP$Cell_Type)
  
  cat("levels_CT_0\n")
  cat(str(levels_CT))
  cat("\n")
  
  REP.dt<-data.table(REP, key="variable")
  
  Freq.table<-as.data.frame(REP.dt[,.(Freq=.N), by=key(REP.dt)], stringsAsFactors=F)
  Freq.table<-Freq.table[order(Freq.table$Freq, decreasing = T),]
  
  cat("Freq.table_0\n")
  cat(str(Freq.table))
  cat("\n")
  
  levels_variable<-unique(Freq.table$variable)
  
  cat("levels_variable_0\n")
  cat(str(levels_variable))
  cat("\n")
  
  REP$variable<-factor(REP$variable,
                       levels=levels_variable,
                       ordered=T)
  
  REP$Cell_Type<-factor(REP$Cell_Type,
                        levels=rev(levels_CT),
                        ordered=T)
  
  
  cat("REP_2\n")
  cat(str(REP))
  cat("\n")
  
  vector_fill<-c(rev(brewer.pal(9, "Blues")[c(4,5,7)]),brewer.pal(9, "Reds")[c(4,5,7)])
  
  cat("vector_fill_0\n")
  cat(str(vector_fill))
  cat("\n")
  
  heatmap_abs_log2Skew_meta <-ggplot(data=REP,
                               aes(x=variable, 
                                   y=Cell_Type))+
    geom_tile(aes(fill = REP$Y1), color = "black", size = 0.5)+
    scale_fill_manual(name=paste("Lasso","coefficient", sep="\n"),
                      values=vector_fill)+
    theme_minimal()+ # minimal theme
    scale_y_discrete(name=NULL, drop=F)+
    scale_x_discrete(name=NULL, drop=F)+
    ggtitle(paste("Lasso regression on abs_log2Skew_meta predictors",sep=''))+
    theme(plot.title=element_text(color="black", family="sans"),
          axis.title=element_blank(),
          axis.title.y=element_blank(),
          axis.title.x=element_blank(),
          axis.text.y=element_text(color="black", family="sans"),
          axis.text.x=element_text(angle=45,hjust=1,vjust=1,color="black", family="sans"))+
    theme(legend.title = element_text(family="sans"),
          legend.text = element_text(family="sans"),
          legend.key.size = unit(0.35, 'cm'), #change legend key size
          legend.key.height = unit(0.35, 'cm'), #change legend key height
          legend.key.width = unit(0.35, 'cm'), #change legend key width
          legend.position="right")+
    coord_fixed()
  
  
  setwd(path_graphs)
  
  svgname<-paste("heatmap_abs_log2Skew_meta_discretized",".svg", sep='')
  
  
  ggsave(svgname, plot= heatmap_abs_log2Skew_meta,
         device="svg")
  
  
  
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
    make_option(c("--coefficient_file_Activity_selected"), type="character", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required."),
    make_option(c("--coefficient_file_MPRA_CLASS_selected"), type="character", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required."),
    make_option(c("--coefficient_file_log2FC_meta_selected"), type="character", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required."),
    make_option(c("--coefficient_file_abs_log2Skew_meta_selected"), type="character", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required."),
    make_option(c("--GWAS_parameters"), type="character", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required."),
    make_option(c("--Our_rankings"), type="character", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required."),
    make_option(c("--Variant_based_scores"), type="character", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required."),
    make_option(c("--Gene_based_features"), type="character", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required."),
    make_option(c("--Lineages"), type="character", default=NULL, 
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
  
  # heatmap_function(opt)
  heatmap_function_discretized(opt)

}

###########################################################################

system.time( main() )