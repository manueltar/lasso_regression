
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
suppressMessages(library("ggforce", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/"))


opt = NULL

options(warn = 1)

violin_Activity = function(option_list)
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
  
  path_graphs = paste(out,'heatmaps','/','violin_plots','/',sep='')
  
  if (file.exists(path_graphs)){
    
    
  }else{
    
    dir.create(file.path(path_graphs))
    
  }#path_graphs
  
  path_graphs = paste(out,'heatmaps','/','violin_plots','/','Activity','/',sep='')
  
  if (file.exists(path_graphs)){
    
    
  }else{
    
    dir.create(file.path(path_graphs))
    
  }#path_graphs
 
 
  #### Read Enformer_run ----------

  feature_file<-readRDS(file=opt$feature_file)
  
  
  # cat("feature_file_0\n")
  # cat(str(feature_file))
  # cat("\n")
  # cat(str(unique(feature_file$VAR)))
  # cat("\n")
  
  #### READ REP_Activity ----
  
  REP_Activity<-as.data.frame(readRDS(file=opt$REP_Activity), stringsAsFactors=F)
  
  
  cat("REP_Activity_0\n")
  cat(str(REP_Activity))
  cat("\n")

  #### READ REP_MPRA_CLASS ----
  
  REP_MPRA_CLASS<-as.data.frame(readRDS(file=opt$REP_MPRA_CLASS), stringsAsFactors=F)
  
  
  cat("REP_MPRA_CLASS_0\n")
  cat(str(REP_MPRA_CLASS))
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
  
  
  CT_subset<-unique(MPRA_result_SNP_and_CT_subset[,which(colnames(MPRA_result_SNP_and_CT_subset)%in%c("VAR","Cell_Type","Activity"))])
  
  cat("CT_subset_1\n")
  cat(str(CT_subset))
  cat("\n")
  cat(str(unique(CT_subset$VAR)))
  cat("\n")
  
  NEW_Table_S6_subset<-unique(NEW_Table_S6[,which(colnames(NEW_Table_S6)%in%c("VAR","Activity"))])
  
  NEW_Table_S6_subset$Cell_Type<-'At_least_one_cell_type'
  
  cat("NEW_Table_S6_subset_0\n")
  cat(str(NEW_Table_S6_subset))
  cat("\n")
  cat(str(unique(NEW_Table_S6_subset$VAR)))
  cat("\n")
  
  Total_Activity<-rbind(NEW_Table_S6_subset,
               CT_subset)
  
  Total_Activity$Cell_Type<-factor(Total_Activity$Cell_Type, levels=c('At_least_one_cell_type','K562','CHRF','HL60','THP1'))
  
  cat("Total_Activity_0\n")
  cat(str(Total_Activity))
  cat("\n")
  cat(str(unique(Total_Activity$VAR)))
  cat("\n")
  
 
  ##### merge with features ----
  
  Merged_table<-merge(Total_Activity,
                      feature_file,
                      by="VAR")
  
  
  
 
 
  #### Activity ----
  
  indx.Activity<-which(colnames(Merged_table)%in%REP_Activity$variable)
  
  cat("indx.Activity_0\n")
  cat(str(indx.Activity))
  cat("\n")
  
  Merged_table.Activity<-Merged_table[,c(which(colnames(Merged_table)%in%c("VAR","Cell_Type","Activity")),indx.Activity)]
  
  cat("Merged_table.Activity_0\n")
  cat(str(Merged_table.Activity))
  cat("\n")
  
  
  Merged_table.Activity.m<-melt(Merged_table.Activity, id.vars=c("VAR","Cell_Type","Activity"), variable.name="variable", value.name='value')
  
  
  cat("Merged_table.Activity.m_0\n")
  cat(str(Merged_table.Activity.m))
  cat("\n")
  
  Merged_table.Activity.m$variable<-factor(Merged_table.Activity.m$variable,
                                              levels=levels(REP_Activity$variable),
                                              ordered=T)
  
  
  cat("Merged_table.Activity.m_1\n")
  cat(str(Merged_table.Activity.m))
  cat("\n")
  
  REP<-Merged_table.Activity.m
  
  
  
  array_variables<-levels(REP$variable)
  
  cat("array_variables_0\n")
  cat(str(array_variables))
  cat("\n")
  
  DEBUG<-0
  
  for(i in 1:length(array_variables)){
    
    array_variables_sel<-array_variables[i]
    
    cat("---------------------------------------->\t")
    cat(sprintf(as.character(array_variables_sel)))
    cat("\n")
    
    REP_sel<-REP[which(REP$variable == array_variables_sel),]
    
    REP_sel$variable<-droplevels(REP_sel$variable)
    
    if(DEBUG ==1){
      cat("REP_sel_1\n")
      cat(str(REP_sel))
      cat("\n")
    }
    

    summary_value<-summary(REP_sel$value)
    
    if(DEBUG ==1){
      cat("summary_value\n")
      cat(sprintf(as.character(names(summary_value))))
      cat("\n")
      cat(sprintf(as.character(summary_value)))
      cat("\n")
    }
    
    max_value<-summary_value[6]
    min_value<-summary_value[1]
    
    if(DEBUG ==1){
      cat("max_value_and_min_value\n")
      cat(sprintf(as.character(max_value)))
      cat("\n")
      cat(sprintf(as.character(min_value)))
      cat("\n")
    }
    
    step<-round(abs(max_value-min_value)/4,3)
    
    if(DEBUG ==1){
      cat("step\n")
      cat(sprintf(as.character(step)))
      cat("\n")
    }
    
    breaks_value<-round(unique(sort(c(0,max_value,seq(min_value,max_value, by=step)))),3)
    
    if(DEBUG ==1){
      cat("breaks_value\n")
      cat(sprintf(as.character(breaks_value)))
      cat("\n")
    }
    
    labels_value<-as.character(breaks_value)
    
    if(DEBUG ==1){
      cat("labels_value\n")
      cat(sprintf(as.character(labels_value)))
      cat("\n")
    }
    
    
    violin_plot_Activity<-ggplot(data=REP_sel,
                               aes(x=Activity,
                                   y=value))+
      geom_sina(size=2)
    
    
    violin_plot_Activity<-violin_plot_Activity+
      theme_cowplot(font_size = 2)+
      facet_grid(Cell_Type ~ ., scales='free_x', space='free_x', switch="y")+   
      scale_y_continuous(name=array_variables_sel,
                         breaks=breaks_value,
                         labels=labels_value,
                         limits=c(breaks_value[1],breaks_value[length(breaks_value)]))+
      scale_x_discrete(name='Activity')+
      theme( strip.background = element_blank(),
             strip.placement = "outside",
             strip.text = element_text(size=6),
             panel.spacing = unit(0.2, "lines"),
             panel.background=element_rect(fill="white"),
             panel.border=element_rect(colour="white",size=0,5),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank())+   
      theme_classic()+
      theme(axis.title=element_blank(),
            axis.title.y=element_text(size=8,color="black", family="sans"),
            axis.title.x=element_text(size=8,color="black", family="sans"),
            axis.text.y=element_text(size=6,color="black", family="sans"),
            axis.text.x=element_text(size=6,color="black", family="sans"),
            axis.line.x = element_line(size = 0.2),
            axis.ticks.x = element_line(size = 0.2),
            axis.ticks.y = element_line(size = 0.2),
            axis.line.y = element_line(size = 0.2))+
      theme(legend.title = element_text(size=8),
            legend.text = element_text(size=6),
            legend.key.size = unit(0.35, 'cm'), #change legend key size
            legend.key.height = unit(0.35, 'cm'), #change legend key height
            legend.key.width = unit(0.35, 'cm'), #change legend key width
            legend.position="hidden")+
      ggeasy::easy_center_title()
    
    
    setwd(path_graphs)
    
    svgname<-paste('Violin_plot_',array_variables_sel,'.svg',sep='')
    makesvg = TRUE
    
    if (makesvg == TRUE)
    {
      ggsave(svgname, plot= violin_plot_Activity,
             device="svg",
             height=12, width=3)
    }#makesvg == TRUE
    
  }# i in 1:length(array_variables)
}

violin_MPRA_CLASS = function(option_list)
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
  
  path_graphs = paste(out,'heatmaps','/','violin_plots','/',sep='')
  
  if (file.exists(path_graphs)){
    
    
  }else{
    
    dir.create(file.path(path_graphs))
    
  }#path_graphs
  
  path_graphs = paste(out,'heatmaps','/','violin_plots','/','MPRA_CLASS','/',sep='')
  
  if (file.exists(path_graphs)){
    
    
  }else{
    
    dir.create(file.path(path_graphs))
    
  }#path_graphs
  
  #### Read Enformer_run ----------
  
  feature_file<-readRDS(file=opt$feature_file)
  
  
  # cat("feature_file_0\n")
  # cat(str(feature_file))
  # cat("\n")
  # cat(str(unique(feature_file$VAR)))
  # cat("\n")
  
  #### READ REP_Activity ----
  
  REP_Activity<-as.data.frame(readRDS(file=opt$REP_Activity), stringsAsFactors=F)
  
  
  cat("REP_Activity_0\n")
  cat(str(REP_Activity))
  cat("\n")
  
  #### READ REP_MPRA_CLASS ----
  
  REP_MPRA_CLASS<-as.data.frame(readRDS(file=opt$REP_MPRA_CLASS), stringsAsFactors=F)
  
  
  cat("REP_MPRA_CLASS_0\n")
  cat(str(REP_MPRA_CLASS))
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
  
  
  CT_subset<-unique(MPRA_result_SNP_and_CT_subset[,which(colnames(MPRA_result_SNP_and_CT_subset)%in%c("VAR","Cell_Type","MPRA_CLASS"))])
  
  cat("CT_subset_1\n")
  cat(str(CT_subset))
  cat("\n")
  cat(str(unique(CT_subset$VAR)))
  cat("\n")
  
  NEW_Table_S6_subset<-unique(NEW_Table_S6[,which(colnames(NEW_Table_S6)%in%c("VAR","MPRA_CLASS"))])
  
  NEW_Table_S6_subset$Cell_Type<-'At_least_one_cell_type'
  
  cat("NEW_Table_S6_subset_0\n")
  cat(str(NEW_Table_S6_subset))
  cat("\n")
  cat(str(unique(NEW_Table_S6_subset$VAR)))
  cat("\n")
  
  Total_MPRA_CLASS<-rbind(NEW_Table_S6_subset,
                          CT_subset)
  
  Total_MPRA_CLASS$Cell_Type<-factor(Total_MPRA_CLASS$Cell_Type, levels=c('At_least_one_cell_type','K562','CHRF','HL60','THP1'))
  
  cat("Total_MPRA_CLASS_0\n")
  cat(str(Total_MPRA_CLASS))
  cat("\n")
  cat(str(unique(Total_MPRA_CLASS$VAR)))
  cat("\n")
  
  
  ##### merge with features ----
  
  Merged_table<-merge(Total_MPRA_CLASS,
                      feature_file,
                      by="VAR")
 
  
  
  
  
  #### MPRA_CLASS ----
  
  indx.MPRA_CLASS<-which(colnames(Merged_table)%in%REP_MPRA_CLASS$variable)
  
  cat("indx.MPRA_CLASS_0\n")
  cat(str(indx.MPRA_CLASS))
  cat("\n")
  
  Merged_table.MPRA_CLASS<-Merged_table[,c(which(colnames(Merged_table)%in%c("VAR","Cell_Type","MPRA_CLASS")),indx.MPRA_CLASS)]
  
  cat("Merged_table.MPRA_CLASS_0\n")
  cat(str(Merged_table.MPRA_CLASS))
  cat("\n")
  
  
  Merged_table.MPRA_CLASS.m<-melt(Merged_table.MPRA_CLASS, id.vars=c("VAR","Cell_Type","MPRA_CLASS"), variable.name="variable", value.name='value')
  
  
  cat("Merged_table.MPRA_CLASS.m_0\n")
  cat(str(Merged_table.MPRA_CLASS.m))
  cat("\n")
  
  Merged_table.MPRA_CLASS.m$variable<-factor(Merged_table.MPRA_CLASS.m$variable,
                                              levels=levels(REP_MPRA_CLASS$variable),
                                              ordered=T)
  
  
  cat("Merged_table.MPRA_CLASS.m_1\n")
  cat(str(Merged_table.MPRA_CLASS.m))
  cat("\n")
  
  REP<-Merged_table.MPRA_CLASS.m
  
  array_variables<-levels(REP$variable)
  
  cat("array_variables_0\n")
  cat(str(array_variables))
  cat("\n")
  
  DEBUG<-0
  
  for(i in 1:length(array_variables)){
    
    array_variables_sel<-array_variables[i]
    
    cat("---------------------------------------->\t")
    cat(sprintf(as.character(array_variables_sel)))
    cat("\n")
    
    REP_sel<-REP[which(REP$variable == array_variables_sel),]
    
    REP_sel$variable<-droplevels(REP_sel$variable)
    
    if(DEBUG ==1){
      cat("REP_sel_1\n")
      cat(str(REP_sel))
      cat("\n")
    }
    
    
    summary_value<-summary(REP_sel$value)
    
    if(DEBUG ==1){
      cat("summary_value\n")
      cat(sprintf(as.character(names(summary_value))))
      cat("\n")
      cat(sprintf(as.character(summary_value)))
      cat("\n")
    }
    
    max_value<-summary_value[6]
    min_value<-summary_value[1]
    
    if(DEBUG ==1){
      cat("max_value_and_min_value\n")
      cat(sprintf(as.character(max_value)))
      cat("\n")
      cat(sprintf(as.character(min_value)))
      cat("\n")
    }
    
    step<-round(abs(max_value-min_value)/4,3)
    
    if(DEBUG ==1){
      cat("step\n")
      cat(sprintf(as.character(step)))
      cat("\n")
    }
    
    breaks_value<-round(unique(sort(c(0,max_value,seq(min_value,max_value, by=step)))),3)
    
    if(DEBUG ==1){
      cat("breaks_value\n")
      cat(sprintf(as.character(breaks_value)))
      cat("\n")
    }
    
    labels_value<-as.character(breaks_value)
    
    if(DEBUG ==1){
      cat("labels_value\n")
      cat(sprintf(as.character(labels_value)))
      cat("\n")
    }
    
    
    violin_plot_MPRA_CLASS<-ggplot(data=REP_sel,
                                 aes(x=MPRA_CLASS,
                                     y=value))+
      geom_sina(size=2)
    
    
    violin_plot_MPRA_CLASS<-violin_plot_MPRA_CLASS+
      theme_cowplot(font_size = 2)+
      facet_grid(Cell_Type ~ ., scales='free_x', space='free_x', switch="y")+   
      scale_y_continuous(name=array_variables_sel,
                         breaks=breaks_value,
                         labels=labels_value,
                         limits=c(breaks_value[1],breaks_value[length(breaks_value)]))+
      scale_x_discrete(name='MPRA_CLASS')+
      theme( strip.background = element_blank(),
             strip.placement = "outside",
             strip.text = element_text(size=6),
             panel.spacing = unit(0.2, "lines"),
             panel.background=element_rect(fill="white"),
             panel.border=element_rect(colour="white",size=0,5),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank())+   
      theme_classic()+
      theme(axis.title=element_blank(),
            axis.title.y=element_text(size=8,color="black", family="sans"),
            axis.title.x=element_text(size=8,color="black", family="sans"),
            axis.text.y=element_text(size=6,color="black", family="sans"),
            axis.text.x=element_text(size=6,color="black", family="sans"),
            axis.line.x = element_line(size = 0.2),
            axis.ticks.x = element_line(size = 0.2),
            axis.ticks.y = element_line(size = 0.2),
            axis.line.y = element_line(size = 0.2))+
      theme(legend.title = element_text(size=8),
            legend.text = element_text(size=6),
            legend.key.size = unit(0.35, 'cm'), #change legend key size
            legend.key.height = unit(0.35, 'cm'), #change legend key height
            legend.key.width = unit(0.35, 'cm'), #change legend key width
            legend.position="hidden")+
      ggeasy::easy_center_title()
    
    
  
    
    
    setwd(path_graphs)
    
    svgname<-paste('Violin_plot_',array_variables_sel,'.svg',sep='')
    makesvg = TRUE
    
    if (makesvg == TRUE)
    {
      ggsave(svgname, plot= violin_plot_MPRA_CLASS,
             device="svg",
             height=12, width=3)
    }#makesvg == TRUE
    
    
    # ##################################
    # quit(status = 1)
    
  }# i in 1:length(array_variables)
}

scatter_log2FC_meta = function(option_list)
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
  
  path_graphs = paste(out,'heatmaps','/','scatter_plots','/',sep='')
  
  if (file.exists(path_graphs)){
    
    
  }else{
    
    dir.create(file.path(path_graphs))
    
  }#path_graphs
  
  path_graphs = paste(out,'heatmaps','/','scatter_plots','/','log2FC_meta','/',sep='')
  
  if (file.exists(path_graphs)){
    
    
  }else{
    
    dir.create(file.path(path_graphs))
    
  }#path_graphs
  
  #### Read Enformer_run ----------
  
  feature_file<-readRDS(file=opt$feature_file)
  
  
  # cat("feature_file_0\n")
  # cat(str(feature_file))
  # cat("\n")
  # cat(str(unique(feature_file$VAR)))
  # cat("\n")
  
  #### READ REP_Activity ----
  
  REP_Activity<-as.data.frame(readRDS(file=opt$REP_Activity), stringsAsFactors=F)
  
  
  cat("REP_Activity_0\n")
  cat(str(REP_Activity))
  cat("\n")
  
  #### READ REP_log2FC_meta ----
  
  REP_log2FC_meta<-as.data.frame(readRDS(file=opt$REP_log2FC_meta), stringsAsFactors=F)
  
  
  cat("REP_log2FC_meta_0\n")
  cat(str(REP_log2FC_meta))
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
  
  MPRA_result_SNP_and_CT_subset<-unique(MPRA_result_SNP_and_CT[which(MPRA_result_SNP_and_CT$Project == 'MPRA_bc_synthesis'),which(colnames(MPRA_result_SNP_and_CT)%in%c("VAR","Cell_Type","log2FC_meta","log2Skew_meta","Activity","log2FC_meta"))])
  
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
  
  
  CT_subset<-unique(MPRA_result_SNP_and_CT_subset[,which(colnames(MPRA_result_SNP_and_CT_subset)%in%c("VAR","Cell_Type","log2FC_meta"))])
  
  cat("CT_subset_1\n")
  cat(str(CT_subset))
  cat("\n")
  cat(str(unique(CT_subset$VAR)))
  cat("\n")
  
  # NEW_Table_S6_subset<-unique(NEW_Table_S6[,which(colnames(NEW_Table_S6)%in%c("VAR","log2FC_meta"))])
  # 
  # 
  # cat("NEW_Table_S6_subset_0\n")
  # cat(str(NEW_Table_S6_subset))
  # cat("\n")
  # cat(str(unique(NEW_Table_S6_subset$VAR)))
  # cat("\n")
  
  Total_log2FC_meta<-rbind(CT_subset)
  
  Total_log2FC_meta$Cell_Type<-factor(Total_log2FC_meta$Cell_Type, levels=c('K562','CHRF','HL60','THP1'))
  
  cat("Total_log2FC_meta_0\n")
  cat(str(Total_log2FC_meta))
  cat("\n")
  cat(str(unique(Total_log2FC_meta$VAR)))
  cat("\n")
  
  
  ##### merge with features ----
  
  Merged_table<-merge(Total_log2FC_meta,
                      feature_file,
                      by="VAR")
  
  
  
  
  
  #### log2FC_meta ----
  
  indx.log2FC_meta<-which(colnames(Merged_table)%in%REP_log2FC_meta$variable)
  
  cat("indx.log2FC_meta_0\n")
  cat(str(indx.log2FC_meta))
  cat("\n")
  
  Merged_table.log2FC_meta<-Merged_table[,c(which(colnames(Merged_table)%in%c("VAR","Cell_Type","log2FC_meta")),indx.log2FC_meta)]
  
  cat("Merged_table.log2FC_meta_0\n")
  cat(str(Merged_table.log2FC_meta))
  cat("\n")
  
  
  Merged_table.log2FC_meta.m<-melt(Merged_table.log2FC_meta, id.vars=c("VAR","Cell_Type","log2FC_meta"), variable.name="variable", value.name='value')
  
  
  cat("Merged_table.log2FC_meta.m_0\n")
  cat(str(Merged_table.log2FC_meta.m))
  cat("\n")
  
  Merged_table.log2FC_meta.m$variable<-factor(Merged_table.log2FC_meta.m$variable,
                                             levels=levels(REP_log2FC_meta$variable),
                                             ordered=T)
  
  
  cat("Merged_table.log2FC_meta.m_1\n")
  cat(str(Merged_table.log2FC_meta.m))
  cat("\n")
  
  REP<-Merged_table.log2FC_meta.m
  
  array_variables<-levels(REP$variable)
  
  cat("array_variables_0\n")
  cat(str(array_variables))
  cat("\n")
  
  DEBUG<-1
  
  for(i in 1:length(array_variables)){
    
    array_variables_sel<-array_variables[i]
    
    cat("---------------------------------------->\t")
    cat(sprintf(as.character(array_variables_sel)))
    cat("\n")
    
    REP_sel<-REP[which(REP$variable == array_variables_sel),]
    
    REP_sel$variable<-droplevels(REP_sel$variable)
    
    if(DEBUG ==1){
      cat("REP_sel_1\n")
      cat(str(REP_sel))
      cat("\n")
    }
    
    
    summary_value<-summary(REP_sel$value)
    
    if(DEBUG ==1){
      cat("summary_value\n")
      cat(sprintf(as.character(names(summary_value))))
      cat("\n")
      cat(sprintf(as.character(summary_value)))
      cat("\n")
    }
    
    max_value<-summary_value[6]
    min_value<-summary_value[1]
    
    if(DEBUG ==1){
      cat("max_value_and_min_value\n")
      cat(sprintf(as.character(max_value)))
      cat("\n")
      cat(sprintf(as.character(min_value)))
      cat("\n")
    }
    
    step<-round(abs(max_value-min_value)/4,3)
    
    if(DEBUG ==1){
      cat("step\n")
      cat(sprintf(as.character(step)))
      cat("\n")
    }
    
    breaks_value<-round(unique(sort(c(0,max_value,seq(min_value,max_value, by=step)))),3)
    
    if(DEBUG ==1){
      cat("breaks_value\n")
      cat(sprintf(as.character(breaks_value)))
      cat("\n")
    }
    
    labels_value<-as.character(breaks_value)
    
    if(DEBUG ==1){
      cat("labels_value\n")
      cat(sprintf(as.character(labels_value)))
      cat("\n")
    }
    
    
    scatter_plot_log2FC_meta<-ggplot(data=REP_sel,
                                   aes(x=log2FC_meta,
                                       y=value))+
      geom_point(size=2)
    
    
    scatter_plot_log2FC_meta<-scatter_plot_log2FC_meta+
      theme_cowplot()+
      facet_grid(. ~ Cell_Type, scales='free_x', space='free_x', switch="y")+   
      scale_y_continuous(name=array_variables_sel)+
      scale_x_continuous(name='log2FC_meta')+
      theme( strip.background = element_blank(),
             strip.placement = "outside",
             strip.text = element_text(size=6),
             panel.spacing = unit(0.2, "lines"),
             panel.background=element_rect(fill="white"),
             panel.border=element_rect(colour="white",size=0,5),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank())+   
      theme_classic()+
      theme(axis.title=element_blank(),
            axis.title.y=element_text(color="black", family="sans"),
            axis.title.x=element_text(color="black", family="sans"),
            axis.text.y=element_text(color="black", family="sans"),
            axis.text.x=element_text(color="black", family="sans"))+
      theme(legend.title = element_text(size=8),
            legend.text = element_text(size=6),
            legend.key.size = unit(0.35, 'cm'), #change legend key size
            legend.key.height = unit(0.35, 'cm'), #change legend key height
            legend.key.width = unit(0.35, 'cm'), #change legend key width
            legend.position="hidden")+
      ggeasy::easy_center_title()
    
    
    if(array_variables_sel == 'MAF'){
      
      # scatter_plot_log2FC_meta<-scatter_plot_log2FC_meta+
      #                           scale_y_continuous(name=array_variables_sel, trans="log10")
      
    }#array_variables_sel == 'MAF'
    
    
    setwd(path_graphs)
    
    svgname<-paste('Scatter_plot_',array_variables_sel,'.svg',sep='')
    makesvg = TRUE
    
    if (makesvg == TRUE)
    {
      ggsave(svgname, plot= scatter_plot_log2FC_meta,
             device="svg")
    }#makesvg == TRUE
    
    
    # ##################################
    # quit(status = 1)
    
  }# i in 1:length(array_variables)
}

scatter_abs_log2Skew_meta = function(option_list)
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
  
  path_graphs = paste(out,'heatmaps','/','scatter_plots','/',sep='')
  
  if (file.exists(path_graphs)){
    
    
  }else{
    
    dir.create(file.path(path_graphs))
    
  }#path_graphs
  
  path_graphs = paste(out,'heatmaps','/','scatter_plots','/','abs_log2Skew_meta','/',sep='')
  
  if (file.exists(path_graphs)){
    
    
  }else{
    
    dir.create(file.path(path_graphs))
    
  }#path_graphs
  
  #### Read Enformer_run ----------
  
  feature_file<-readRDS(file=opt$feature_file)
  
  
  # cat("feature_file_0\n")
  # cat(str(feature_file))
  # cat("\n")
  # cat(str(unique(feature_file$VAR)))
  # cat("\n")
  
  #### READ REP_Activity ----
  
  REP_Activity<-as.data.frame(readRDS(file=opt$REP_Activity), stringsAsFactors=F)
  
  
  cat("REP_Activity_0\n")
  cat(str(REP_Activity))
  cat("\n")
  
  #### READ REP_abs_log2Skew_meta ----
  
  REP_abs_log2Skew_meta<-as.data.frame(readRDS(file=opt$REP_abs_log2Skew_meta), stringsAsFactors=F)
  
  
  cat("REP_abs_log2Skew_meta_0\n")
  cat(str(REP_abs_log2Skew_meta))
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
  
  MPRA_result_SNP_and_CT_subset<-unique(MPRA_result_SNP_and_CT[which(MPRA_result_SNP_and_CT$Project == 'MPRA_bc_synthesis'),which(colnames(MPRA_result_SNP_and_CT)%in%c("VAR","Cell_Type","abs_log2Skew_meta","log2Skew_meta","Activity","abs_log2Skew_meta"))])
  
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
  
  
  CT_subset<-unique(MPRA_result_SNP_and_CT_subset[,which(colnames(MPRA_result_SNP_and_CT_subset)%in%c("VAR","Cell_Type","abs_log2Skew_meta"))])
  
  cat("CT_subset_1\n")
  cat(str(CT_subset))
  cat("\n")
  cat(str(unique(CT_subset$VAR)))
  cat("\n")
  
  # NEW_Table_S6_subset<-unique(NEW_Table_S6[,which(colnames(NEW_Table_S6)%in%c("VAR","abs_log2Skew_meta"))])
  # 
  # 
  # cat("NEW_Table_S6_subset_0\n")
  # cat(str(NEW_Table_S6_subset))
  # cat("\n")
  # cat(str(unique(NEW_Table_S6_subset$VAR)))
  # cat("\n")
  
  Total_abs_log2Skew_meta<-rbind(CT_subset)
  
  Total_abs_log2Skew_meta$Cell_Type<-factor(Total_abs_log2Skew_meta$Cell_Type, levels=c('K562','CHRF','HL60','THP1'))
  
  cat("Total_abs_log2Skew_meta_0\n")
  cat(str(Total_abs_log2Skew_meta))
  cat("\n")
  cat(str(unique(Total_abs_log2Skew_meta$VAR)))
  cat("\n")
  
  
  ##### merge with features ----
  
  Merged_table<-merge(Total_abs_log2Skew_meta,
                      feature_file,
                      by="VAR")
  
  
  
  
  
  #### abs_log2Skew_meta ----
  
  indx.abs_log2Skew_meta<-which(colnames(Merged_table)%in%REP_abs_log2Skew_meta$variable)
  
  cat("indx.abs_log2Skew_meta_0\n")
  cat(str(indx.abs_log2Skew_meta))
  cat("\n")
  
  Merged_table.abs_log2Skew_meta<-Merged_table[,c(which(colnames(Merged_table)%in%c("VAR","Cell_Type","abs_log2Skew_meta")),indx.abs_log2Skew_meta)]
  
  cat("Merged_table.abs_log2Skew_meta_0\n")
  cat(str(Merged_table.abs_log2Skew_meta))
  cat("\n")
  
  
  Merged_table.abs_log2Skew_meta.m<-melt(Merged_table.abs_log2Skew_meta, id.vars=c("VAR","Cell_Type","abs_log2Skew_meta"), variable.name="variable", value.name='value')
  
  
  cat("Merged_table.abs_log2Skew_meta.m_0\n")
  cat(str(Merged_table.abs_log2Skew_meta.m))
  cat("\n")
  
  Merged_table.abs_log2Skew_meta.m$variable<-factor(Merged_table.abs_log2Skew_meta.m$variable,
                                              levels=levels(REP_abs_log2Skew_meta$variable),
                                              ordered=T)
  
  
  cat("Merged_table.abs_log2Skew_meta.m_1\n")
  cat(str(Merged_table.abs_log2Skew_meta.m))
  cat("\n")
  
  REP<-Merged_table.abs_log2Skew_meta.m
  
  array_variables<-levels(REP$variable)
  
  cat("array_variables_0\n")
  cat(str(array_variables))
  cat("\n")
  
  DEBUG<-1
  
  for(i in 1:length(array_variables)){
    
    array_variables_sel<-array_variables[i]
    
    cat("---------------------------------------->\t")
    cat(sprintf(as.character(array_variables_sel)))
    cat("\n")
    
    REP_sel<-REP[which(REP$variable == array_variables_sel),]
    
    REP_sel$variable<-droplevels(REP_sel$variable)
    
    if(DEBUG ==1){
      cat("REP_sel_1\n")
      cat(str(REP_sel))
      cat("\n")
    }
    
    
    summary_value<-summary(REP_sel$value)
    
    if(DEBUG ==1){
      cat("summary_value\n")
      cat(sprintf(as.character(names(summary_value))))
      cat("\n")
      cat(sprintf(as.character(summary_value)))
      cat("\n")
    }
    
    max_value<-summary_value[6]
    min_value<-summary_value[1]
    
    if(DEBUG ==1){
      cat("max_value_and_min_value\n")
      cat(sprintf(as.character(max_value)))
      cat("\n")
      cat(sprintf(as.character(min_value)))
      cat("\n")
    }
    
    step<-round(abs(max_value-min_value)/4,3)
    
    if(DEBUG ==1){
      cat("step\n")
      cat(sprintf(as.character(step)))
      cat("\n")
    }
    
    breaks_value<-round(unique(sort(c(0,max_value,seq(min_value,max_value, by=step)))),3)
    
    if(DEBUG ==1){
      cat("breaks_value\n")
      cat(sprintf(as.character(breaks_value)))
      cat("\n")
    }
    
    labels_value<-as.character(breaks_value)
    
    if(DEBUG ==1){
      cat("labels_value\n")
      cat(sprintf(as.character(labels_value)))
      cat("\n")
    }
    
    
    scatter_plot_abs_log2Skew_meta<-ggplot(data=REP_sel,
                                     aes(x=abs_log2Skew_meta,
                                         y=value))+
      geom_point(size=2)
    
    
    scatter_plot_abs_log2Skew_meta<-scatter_plot_abs_log2Skew_meta+
      theme_cowplot()+
      facet_grid(. ~ Cell_Type, scales='free_x', space='free_x', switch="y")+   
      scale_y_continuous(name=array_variables_sel)+
      scale_x_continuous(name='abs_log2Skew_meta')+
      theme( strip.background = element_blank(),
             strip.placement = "outside",
             strip.text = element_text(size=6),
             panel.spacing = unit(0.2, "lines"),
             panel.background=element_rect(fill="white"),
             panel.border=element_rect(colour="white",size=0,5),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank())+   
      theme_classic()+
      theme(axis.title=element_blank(),
            axis.title.y=element_text(color="black", family="sans"),
            axis.title.x=element_text(color="black", family="sans"),
            axis.text.y=element_text(color="black", family="sans"),
            axis.text.x=element_text(color="black", family="sans"))+
      theme(legend.title = element_text(size=8),
            legend.text = element_text(size=6),
            legend.key.size = unit(0.35, 'cm'), #change legend key size
            legend.key.height = unit(0.35, 'cm'), #change legend key height
            legend.key.width = unit(0.35, 'cm'), #change legend key width
            legend.position="hidden")+
      ggeasy::easy_center_title()
    
    
    if(array_variables_sel == 'MAF'){
      
      # scatter_plot_abs_log2Skew_meta<-scatter_plot_abs_log2Skew_meta+
      #                           scale_y_continuous(name=array_variables_sel, trans="log10")
      
    }#array_variables_sel == 'MAF'
    
    
    setwd(path_graphs)
    
    svgname<-paste('Scatter_plot_',array_variables_sel,'.svg',sep='')
    makesvg = TRUE
    
    if (makesvg == TRUE)
    {
      ggsave(svgname, plot= scatter_plot_abs_log2Skew_meta,
             device="svg")
    }#makesvg == TRUE
    
    
    # ##################################
    # quit(status = 1)
    
  }# i in 1:length(array_variables)
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
    make_option(c("--REP_Activity"), type="character", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required."),
    make_option(c("--REP_MPRA_CLASS"), type="character", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required."),
    make_option(c("--REP_log2FC_meta"), type="character", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required."),
    make_option(c("--REP_abs_log2Skew_meta"), type="character", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required."),
    make_option(c("--MPRA_result_SNP_and_CT"), type="character", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required."),
    make_option(c("--NEW_Table_S6"), type="character", default=NULL, 
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
  
  # violin_Activity(opt)
  # violin_MPRA_CLASS(opt)
  scatter_log2FC_meta(opt)
  scatter_abs_log2Skew_meta(opt)
 


  
}


###########################################################################

system.time( main() )