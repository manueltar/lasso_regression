
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

data_wrangling_comp1 = function(option_list)
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
  
  #### READ and transform genes_of_interest ----
  
  genes_of_interest <- unlist(strsplit(opt$genes_of_interest, split=","))
  
  cat("genes_of_interest_0\n")
  cat(sprintf(as.character(genes_of_interest)))
  cat("\n")
  
 
    #### genes_of_interest -> input genes ----
  
  genes_of_interest.df<-as.data.frame(rbind(cbind(genes_of_interest, rep("NA", length(genes_of_interest)))), stringsAsFactors=F)
  
  colnames(genes_of_interest.df)<-c("HGNC","ensembl_gene_id")
  
  cat("genes_of_interest.df_0\n")
  cat(str(genes_of_interest.df))
  cat("\n")
  cat(sprintf(as.character(genes_of_interest.df$HGNC)))
  cat("\n")
  cat(sprintf(as.character(genes_of_interest.df$ensembl_gene_id)))
  cat("\n")
  
  genes_of_interest.df$ensembl_gene_id<-mapIds(org.Hs.eg.db, keys=genes_of_interest.df$HGNC, keytype="SYMBOL",column="ENSEMBL")
  
  cat("genes_of_interest.df_1\n")
  cat(str(genes_of_interest.df))
  cat("\n")
  
  input_genes<-rbind(genes_of_interest.df)
  
  cat("input_genes_1\n")
  cat(str(input_genes))
  cat("\n")
  cat(sprintf(as.character(input_genes$HGNC)))
  cat("\n")
  cat(sprintf(as.character(input_genes$ensembl_gene_id)))
  cat("\n")
  
  input_genes$ensembl_gene_id[which(input_genes$HGNC == 'C11orf30')]<-'ENSG00000158636'
  
  cat("input_genes_2\n")
  cat(str(input_genes))
  cat("\n")
  cat(sprintf(as.character(input_genes$HGNC)))
  cat("\n")
  cat(sprintf(as.character(input_genes$ensembl_gene_id)))
  cat("\n")
  
  
  input_genes_NO_NA<-input_genes[which(input_genes$ensembl_gene_id != "NA"),]
  
  
  cat("input_genes_NO_NA_0\n")
  cat(str(input_genes_NO_NA))
  cat("\n")
  cat(sprintf(as.character(input_genes_NO_NA$HGNC)))
  cat("\n")
  cat(sprintf(as.character(input_genes_NO_NA$ensembl_gene_id)))
  cat("\n")
  
  #### Read Thomas matrix of gene expression ----
  
  rna_K562<-as.data.frame(fread(file=opt$rna_K562, sep="\t", header=T) , stringsAsFactors=F)
  
  colnames(rna_K562)[which(colnames(rna_K562) == 'Feature ID')]<-"ensembl_gene_id"
  colnames(rna_K562)[which(colnames(rna_K562) == 'Gene symbol')]<-"HGNC"
  
  cat("rna_K562_0\n")
  cat(str(rna_K562))
  cat("\n")
  
  rna_K562$ensembl_gene_id<-gsub("\\..+$","",rna_K562$ensembl_gene_id)
  
  cat("rna_K562_1\n")
  cat(str(rna_K562))
  cat("\n")
  
  rna_K562_subset<-rna_K562[which(rna_K562$ensembl_gene_id%in%input_genes_NO_NA$ensembl_gene_id),c(which(colnames(rna_K562)%in%c("ensembl_gene_id","FPKM")))]
  
  cat("rna_K562_0\n")
  cat(str(rna_K562_subset))
  cat("\n")
  
  rna_K562_subset$source<-'K562'
  
  cat("rna_K562_1\n")
  cat(str(rna_K562_subset))
  cat("\n")
  
  #### Read Thomas matrix of gene expression ----
  
  DEBUG<-1
  
  INTERVAL_GENE_EXP<-as.data.frame(fread(file=opt$INTERVAL_GENE_EXP, sep=",", header=T) , stringsAsFactors=F)
  
  
  
  cat("INTERVAL_GENE_EXP\n")
  cat(str(INTERVAL_GENE_EXP))
  cat("\n")
 
  INTERVAL_GENE_EXP_sel<-INTERVAL_GENE_EXP[,c(which(colnames(INTERVAL_GENE_EXP) == "sample_id"),
                                              which(colnames(INTERVAL_GENE_EXP) %in% input_genes_NO_NA$ensembl_gene_id))]
  if(DEBUG == 1)
  {
    cat("INTERVAL_GENE_EXP_sel_1\n")
    cat(str(INTERVAL_GENE_EXP_sel))
    cat("\n")
  }
  
 
  INTERVAL_GENE_EXP_sel.m<-melt(INTERVAL_GENE_EXP_sel, id.vars=c("sample_id"),value.name = "log2FPKM",variable.name = "ensembl_gene_id")
  INTERVAL_GENE_EXP_sel.m$FPKM<-+2^(INTERVAL_GENE_EXP_sel.m$log2FPKM)
  
  
  if(DEBUG == 1)
  {
    cat("INTERVAL_GENE_EXP_sel.m_0\n")
    cat(str(INTERVAL_GENE_EXP_sel.m))
    cat("\n")
    
    cat("sample_id\n")
    cat(str(INTERVAL_GENE_EXP_sel.m$sample_id))
    cat("\n")
    
    cat("ensembl_gene_id\n")
    cat(str(INTERVAL_GENE_EXP_sel.m$ensembl_gene_id))
    cat("\n")
    
    # ########################################
    # quit(status = 1)
  }
  
  wb_subset<-INTERVAL_GENE_EXP_sel.m[,which(colnames(INTERVAL_GENE_EXP_sel.m)%in%c('ensembl_gene_id','FPKM'))]
  
  
  wb_subset$source<-'whole-blood'
  
  if(DEBUG == 1)
  {
    cat("wb_subset_0\n")
    cat(str(wb_subset))
    cat("\n")
  }
  
  
  DEF<-rbind(rna_K562_subset,
             wb_subset)
  
  if(DEBUG == 1)
  {
    cat("DEF_0\n")
    cat(str(DEF))
    cat("\n")
  }
  
  
  DEF$source<-factor(DEF$source, levels=c('K562','whole-blood'), ordered=T)
  
  DEF<-merge(input_genes_NO_NA,
             DEF,
             by='ensembl_gene_id',
             all.x=T)
  
  if(DEBUG == 1)
  {
    cat("DEF_1\n")
    cat(str(DEF))
    cat("\n")
  }
  
  
  setwd(out)
  
  
  saveRDS(DEF, file='selected_genes_EXP_comp1_FKPM.rds')
}


violin_plots_gene_EXP = function(option_list)
{
  library("ggforce", lib.loc="/home/manuel.tardaguila/R/x86_64-pc-linux-gnu-library/4.1/")
  
  
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
  
  cat("OUT\n")
  cat(sprintf(as.character(out)))
  cat("\n")
  
  #### READ and transform genes_of_interest ----
  
  genes_of_interest <- unlist(strsplit(opt$genes_of_interest, split=","))
  
  cat("genes_of_interest_0\n")
  cat(sprintf(as.character(genes_of_interest)))
  cat("\n")
  
  #### Read the contigency table and the n per variant of the previous analysis----
  
  setwd(out)
  
  filename<-"selected_genes_EXP_comp1_FKPM.rds"
  
  genes_EXP_comp1<-readRDS(file=filename)
  
  cat("genes_EXP_comp1_0\n")
  cat(str(genes_EXP_comp1))
  cat("\n")
  cat(str(unique(genes_EXP_comp1$VAR)))
  cat("\n")
  cat(sprintf(as.character(names(summary(genes_EXP_comp1$CLASS_2)))))
  cat("\n")
  cat(sprintf(as.character(summary(genes_EXP_comp1$CLASS_2))))
  cat("\n")
  
  genes_EXP_comp1_subset<-droplevels(genes_EXP_comp1[which(genes_EXP_comp1$source == 'K562'),])
  
  genes_EXP_comp1_subset$HGNC<-factor(genes_EXP_comp1_subset$HGNC,
                                      levels=genes_of_interest,
                                      ordered=T)
  
  cat("genes_EXP_comp1_subset_0\n")
  cat(str(genes_EXP_comp1_subset))
  cat("\n")
  cat(str(unique(genes_EXP_comp1_subset$VAR)))
  cat("\n")
  cat(sprintf(as.character(names(summary(genes_EXP_comp1_subset$CLASS_2)))))
  cat("\n")
  cat(sprintf(as.character(summary(genes_EXP_comp1_subset$CLASS_2))))
  cat("\n")
  
  #### sina plot graph ----
  
  
  summary_FPKM<-summary(genes_EXP_comp1_subset$FPKM[!is.na(genes_EXP_comp1_subset$FPKM)])
  
  max_FPKM<-max(summary_FPKM)
  min_FPKM<-min(summary_FPKM)
  
  breaks.Rank<-seq(from=0,max_FPKM+10, by=50)
  labels.Rank<-as.character(breaks.Rank)
  
  cat("labels.Rank\n")
  cat(sprintf(as.character(labels.Rank)))
  cat("\n")
  
  
  graph<-ggplot(data=genes_EXP_comp1_subset,
                aes(x=HGNC, y=FPKM))+
    geom_violin(scale = "width", adjust = 1, trim = FALSE, linetype = "solid", size=0.25, color='black', fill='white')+
    geom_sina(color='black')+
    scale_x_discrete(name=NULL, drop=F)+
    scale_y_continuous(name="K562 gene expression (FPKM)")+
    theme_classic()+
    theme(plot.title=element_text(color="black", family="sans"),
          axis.title=element_blank(),
          axis.title.y=element_text(color="black", family="sans"),
          axis.title.x=element_blank(),
          axis.text.y=element_text(color="black", family="sans"),
          axis.text.x=element_text(angle=45,vjust=1,hjust=1,
                                   color="black", family="sans"))+
    ggeasy::easy_center_title()
  
  
  
 
  
  
  cat("graph_DONE\n")
  
  setwd(out)
  
  svgname<-paste("sina_plot_","comp1_FKPM",".svg",sep='')
  makesvg = TRUE
  
  if (makesvg == TRUE)
  {
    ggsave(svgname, plot= graph,
           device="svg",
           height=3, width=3)
  }
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
    make_option(c("--rna_K562"), type="character", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required."),
    make_option(c("--INTERVAL_GENE_EXP"), type="character", default=NULL, 
                metavar="type", 
                help="Path to tab-separated input file listing regions to analyze. Required."),
    make_option(c("--genes_of_interest"), type="character", default=NULL, 
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
  
  data_wrangling_comp1(opt)
  violin_plots_gene_EXP(opt)

}

###########################################################################

system.time( main() )