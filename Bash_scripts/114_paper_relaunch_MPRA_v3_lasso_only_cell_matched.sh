#!/bin/bash
  
MASTER_ROUTE=$1
analysis=$2
MPRA_result_SNP_and_CT=$3
MPRA_alignment_results=$4
MPRA_prior_to_meta_analysis=$5
feature_file=$(echo "/group/soranzo/manuel.tardaguila/MPRA_feature_file/Attempt_1/Features_wide_df_with_TF_motifs_filtered_new.rds")
    
Rscripts_path=$(echo "/home/manuel.tardaguila/Scripts/R/")
module load R/4.1.0


bashrc_file=$(echo "/home/manuel.tardaguila/.bashrc")

source $bashrc_file
eval "$(conda shell.bash hook)"
 

output_dir=$(echo "$MASTER_ROUTE""$analysis""/")

Log_files=$(echo "$output_dir""/""Log_files/")
 
rm -rf $Log_files
mkdir -p $Log_files

   
Cell_Type_array=$(echo "K562,CHRF,HL60,THP1")

a=($(echo "$Cell_Type_array" | tr "," '\n'))

declare -a arr

for i  in "${a[@]}"
do

    Cell_Type_sel=${i}
    echo "------------------------------------>""$Cell_Type_sel"

    ### lasso_on_SNP_and_CT ####################################################################


    type=$(echo "$Cell_Type_sel""_""$tag""_""lasso_on_SNP_and_CT")
    outfile_lasso_on_SNP_and_CT=$(echo "$Log_files""outfile_2_""$type"".log")
    touch $outfile_lasso_on_SNP_and_CT
    echo -n "" > $outfile_lasso_on_SNP_and_CT
    name_lasso_on_SNP_and_CT=$(echo "$type""_job")
    seff_name=$(echo "seff""_""$type")


    Rscript_lasso_on_SNP_and_CT=$(echo "$Rscripts_path""370_Lasso_regression_Cell_Type_version_v6_cell_matched.R")
    MPRA_result_SNP_and_CT=$(echo "$output_dir""MPRA_results_meta_analysis_collapsed_by_SNP_and_Cell_Type_Threshold_log2FC_0.255_Threshold_FC_meta_padj_0.01_VAR_added.rds")

    NEW_Table_S6=$(echo "$output_dir""NEW_Table_S6.rds")

    
    
    myjobid_lasso_on_SNP_and_CT=$(sbatch --output=$outfile_lasso_on_SNP_and_CT --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=4 --mem-per-cpu=4096 --parsable --job-name $name_lasso_on_SNP_and_CT --wrap="Rscript $Rscript_lasso_on_SNP_and_CT --MPRA_result_SNP_and_CT $MPRA_result_SNP_and_CT --feature_file $feature_file --Cell_Type_sel $Cell_Type_sel --NEW_Table_S6 $NEW_Table_S6 --type $type --out $output_dir")
    myjobid_seff_lasso_on_SNP_and_CT=$(sbatch --dependency=afterany:$myjobid_lasso_on_SNP_and_CT --open-mode=append --output=$outfile_lasso_on_SNP_and_CT --job-name=$seff_name --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=1 --mem-per-cpu=128M --parsable --wrap="seff $myjobid_lasso_on_SNP_and_CT >> $outfile_lasso_on_SNP_and_CT")


    arr[${#arr[@]}]="$myjobid_lasso_on_SNP_and_CT"
done


done_string=$(echo "--dependency=afterany:"""""${arr[@]}"""")

echo "$done_string"

dependency_string=$(echo $done_string|sed -r 's/ /:/g')

echo "$dependency_string"


### collect_results_Activity_MPRA_CLASS ####################################################################


type=$(echo "collect_results_Activity_MPRA_CLASS")
outfile_collect_results_Activity_MPRA_CLASS=$(echo "$Log_files""outfile_8_""$type"".log")
touch $outfile_collect_results_Activity_MPRA_CLASS
echo -n "" > $outfile_collect_results_Activity_MPRA_CLASS
name_collect_results_Activity_MPRA_CLASS=$(echo "$type""_job")
seff_name=$(echo "seff""_""$type")


Rscript_collect_results_Activity_MPRA_CLASS=$(echo "$Rscripts_path""379_collect_results_lasso_regression_Activity_and_MPRA_CLASS.R")

new_out=$(echo "$output_dir""lasso""/")

###### $dependency_string

myjobid_collect_results_Activity_MPRA_CLASS=$(sbatch $dependency_string --output=$outfile_collect_results_Activity_MPRA_CLASS --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=1 --mem-per-cpu=1024 --parsable --job-name $name_collect_results_Activity_MPRA_CLASS --wrap="Rscript $Rscript_collect_results_Activity_MPRA_CLASS --type $type --out $new_out")
myjobid_seff_collect_results_Activity_MPRA_CLASS=$(sbatch --dependency=afterany:$myjobid_collect_results_Activity_MPRA_CLASS --open-mode=append --output=$outfile_collect_results_Activity_MPRA_CLASS --job-name=$seff_name --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=1 --mem-per-cpu=128M --parsable --wrap="seff $myjobid_collect_results_Activity_MPRA_CLASS >> $outfile_collect_results_Activity_MPRA_CLASS")



### feature_selection_across_kfolds_Activity_MPRA_CLASS ####################################################################


type=$(echo "feature_selection_across_kfolds_Activity_MPRA_CLASS")
outfile_feature_selection_across_kfolds_Activity_MPRA_CLASS=$(echo "$Log_files""outfile_9_""$type"".log")
touch $outfile_feature_selection_across_kfolds_Activity_MPRA_CLASS
echo -n "" > $outfile_feature_selection_across_kfolds_Activity_MPRA_CLASS
name_feature_selection_across_kfolds_Activity_MPRA_CLASS=$(echo "$type""_job")
seff_name=$(echo "seff""_""$type")


Rscript_feature_selection_across_kfolds_Activity_MPRA_CLASS=$(echo "$Rscripts_path""380_feature_selection_across_kfolds_Activity_and_MPRA_CLASS.R")

new_out=$(echo "$output_dir""lasso""/")

coefficient_file_Activity=$(echo "$new_out""collected_results_Activity.rds")
coefficient_file_MPRA_CLASS=$(echo "$new_out""collected_results_MPRA_CLASS.rds")
coefficient_file_log2FC_meta=$(echo "$new_out""collected_results_log2FC_meta.rds")
coefficient_file_abs_log2Skew_meta=$(echo "$new_out""collected_results_abs_log2Skew_meta.rds")
Threshold_selections=$(echo '3')

# --dependency=afterany:$myjobid_collect_results_Activity_MPRA_CLASS

myjobid_feature_selection_across_kfolds_Activity_MPRA_CLASS=$(sbatch --dependency=afterany:$myjobid_collect_results_Activity_MPRA_CLASS --output=$outfile_feature_selection_across_kfolds_Activity_MPRA_CLASS --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=4 --mem-per-cpu=1024 --parsable --job-name $name_feature_selection_across_kfolds_Activity_MPRA_CLASS --wrap="Rscript $Rscript_feature_selection_across_kfolds_Activity_MPRA_CLASS --coefficient_file_Activity $coefficient_file_Activity --coefficient_file_MPRA_CLASS $coefficient_file_MPRA_CLASS --Threshold_selections $Threshold_selections --coefficient_file_log2FC_meta $coefficient_file_log2FC_meta --coefficient_file_abs_log2Skew_meta $coefficient_file_abs_log2Skew_meta --type $type --out $new_out")
myjobid_seff_feature_selection_across_kfolds_Activity_MPRA_CLASS=$(sbatch --dependency=afterany:$myjobid_feature_selection_across_kfolds_Activity_MPRA_CLASS --open-mode=append --output=$outfile_feature_selection_across_kfolds_Activity_MPRA_CLASS --job-name=$seff_name --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=1 --mem-per-cpu=128M --parsable --wrap="seff $myjobid_feature_selection_across_kfolds_Activity_MPRA_CLASS >> $outfile_feature_selection_across_kfolds_Activity_MPRA_CLASS")



### heatmap ####################################################################    REVISIT - REVISIST - REVISIT ############################################################################################################################################################################


type=$(echo "heatmap")
outfile_heatmap=$(echo "$Log_files""outfile_10_""$type"".log")
touch $outfile_heatmap
echo -n "" > $outfile_heatmap
name_heatmap=$(echo "$type""_job")
seff_name=$(echo "seff""_""$type")


Rscript_heatmap=$(echo "$Rscripts_path""381_heatmap_Activity_and_MPRA_CLASS_v2.R")

new_out=$(echo "$output_dir""lasso""/")

coefficient_file_Activity_selected=$(echo "$new_out""coefficient_file_Activity_MAX_thresholded.rds")
coefficient_file_MPRA_CLASS_selected=$(echo "$new_out""coefficient_file_MPRA_CLASS_MAX_thresholded.rds")
coefficient_file_log2FC_meta_selected=$(echo "$new_out""coefficient_file_log2FC_meta_MAX_thresholded.rds")
coefficient_file_abs_log2Skew_meta_selected=$(echo "$new_out""coefficient_file_abs_log2Skew_meta_MAX_thresholded.rds")
GWAS_parameters=$(echo "PP,Absolute_effect_size,credset_size,MAF")
Variant_based_scores=$(echo "CADD_raw,Gnocchi,NCBoost,SpliceAI_DG,SpliceAI_DL,SpliceAI_AG,SpliceAI_AL")
Our_rankings=$(echo "Rank_ATAC_erythroid_lineage,Rank_ATAC_mega_lineage,Rank_ATAC_gran_mono_lineage,Rank_ATAC_lymph_lineage,multi_lineage_ATAC,Rank_PCHiC,Rank_chromstates")
Gene_based_features=$(echo "COGS,oe_lof,Rank_GENE_EXP")
Lineages=$(echo "Lymphocyte_CLASS,GM_CLASS,Megakaryocytic_CLASS,Erythroid_CLASS")


# --dependency=afterany:$myjobid_feature_selection_across_kfolds_Activity_MPRA_CLASS

myjobid_heatmap=$(sbatch --dependency=afterany:$myjobid_feature_selection_across_kfolds_Activity_MPRA_CLASS --output=$outfile_heatmap --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=4 --mem-per-cpu=1024 --parsable --job-name $name_heatmap --wrap="Rscript $Rscript_heatmap --coefficient_file_Activity_selected $coefficient_file_Activity_selected --coefficient_file_MPRA_CLASS_selected $coefficient_file_MPRA_CLASS_selected --coefficient_file_log2FC_meta_selected $coefficient_file_log2FC_meta_selected --coefficient_file_abs_log2Skew_meta_selected $coefficient_file_abs_log2Skew_meta_selected --GWAS_parameters $GWAS_parameters --Variant_based_scores $Variant_based_scores --Our_rankings $Our_rankings --Gene_based_features $Gene_based_features --Lineages $Lineages --type $type --out $new_out")
myjobid_seff_heatmap=$(sbatch --dependency=afterany:$myjobid_heatmap --open-mode=append --output=$outfile_heatmap --job-name=$seff_name --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=1 --mem-per-cpu=128M --parsable --wrap="seff $myjobid_heatmap >> $outfile_heatmap")

### plot_variables ####################################################################


type=$(echo "plot_variables")
outfile_plot_variables=$(echo "$Log_files""outfile_11_""$type"".log")
touch $outfile_plot_variables
echo -n "" > $outfile_plot_variables
name_plot_variables=$(echo "$type""_job")
seff_name=$(echo "seff""_""$type")


Rscript_plot_variables=$(echo "$Rscripts_path""382_Graphs_for_lasso_features_Activity_and_MPRA_CLASS.R")

new_out=$(echo "$output_dir""lasso""/")

REP_Activity=$(echo "$new_out""REP_Activity.rds")
REP_MPRA_CLASS=$(echo "$new_out""REP_MPRA_CLASS.rds")
REP_log2FC_meta=$(echo "$new_out""REP_log2FC_meta.rds")
REP_abs_log2Skew_meta=$(echo "$new_out""REP_abs_log2Skew_meta.rds")
NEW_Table_S6=$(echo "$output_dir""NEW_Table_S6.rds")
MPRA_result_SNP_and_CT=$(echo "$output_dir""MPRA_results_meta_analysis_collapsed_by_SNP_and_Cell_Type_Threshold_log2FC_0.255_Threshold_FC_meta_padj_0.01_VAR_added.rds")

##########################--dependency=afterany:$myjobid_heatmap

myjobid_plot_variables=$(sbatch --dependency=afterany:$myjobid_heatmap --output=$outfile_plot_variables --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=4 --mem-per-cpu=4096 --parsable --job-name $name_plot_variables --wrap="Rscript $Rscript_plot_variables --REP_Activity $REP_Activity --REP_MPRA_CLASS $REP_MPRA_CLASS --REP_log2FC_meta $REP_log2FC_meta --REP_abs_log2Skew_meta $REP_abs_log2Skew_meta --NEW_Table_S6 $NEW_Table_S6 --feature_file $feature_file --MPRA_result_SNP_and_CT $MPRA_result_SNP_and_CT --type $type --out $new_out")
myjobid_seff_plot_variables=$(sbatch --dependency=afterany:$myjobid_plot_variables --open-mode=append --output=$outfile_plot_variables --job-name=$seff_name --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=1 --mem-per-cpu=128M --parsable --wrap="seff $myjobid_plot_variables >> $outfile_plot_variables")


####################################################################################### check_gene_expression_FKPM ####################################################################


type=$(echo "check_gene_expression_FKPM")
outfile_check_gene_expression_FKPM=$(echo "$Log_files""outfile_12_""$type"".log")
touch $outfile_check_gene_expression_FKPM
echo -n "" > $outfile_check_gene_expression_FKPM
name_check_gene_expression_FKPM=$(echo "$type""_job")
seff_name=$(echo "seff""_""$type")


Rscript_check_gene_expression_FKPM=$(echo "$Rscripts_path""394_inspect_the_transcriptomes_v2_MPRApos_MPRAneg.R")

new_out=$(echo "$output_dir""lasso""/")
genes_of_interest=$(echo "GATA2,KLF17,CTCF,MAFF,EP300,RLF,HNF4G,FOSL1")
genes_of_interest=$(echo 'REST,MCM2,BRCA1,CBX5,U2AF2,KLF1,SRSF9,C11orf30,CBX3')
genes_of_interest=$(echo 'STAT2,STAT1,ATF3,MCM2,ZNF24,U2AF2,RFX1,CTCF,KAT2B')
rna_K562=$(echo "/group/soranzo/manuel.tardaguila/paper_relaunch/MPRA_S_no_global_metanalysis/rna_expression_report_2024_12_2_11h_37m.tsv")
INTERVAL_GENE_EXP=$(echo "/group/soranzo/manuel.tardaguila/INTERVAL_results/ge_matrix_residuals_scaled_final.csv")

###### --dependency=afterany:$myjobid_plot_variables

myjobid_check_gene_expression_FKPM=$(sbatch --dependency=afterany:$myjobid_plot_variables --output=$outfile_check_gene_expression_FKPM --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=2 --mem-per-cpu=4096 --parsable --job-name $name_check_gene_expression_FKPM --wrap="Rscript  $Rscript_check_gene_expression_FKPM --genes_of_interest $genes_of_interest --rna_K562 $rna_K562 --rna_K562 $rna_K562 --INTERVAL_GENE_EXP $INTERVAL_GENE_EXP --type $type --out $new_out")
myjobid_seff_check_gene_expression_FKPM=$(sbatch --dependency=afterany:$myjobid_check_gene_expression_FKPM --open-mode=append --output=$outfile_check_gene_expression_FKPM --job-name=$seff_name --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=1 --mem-per-cpu=128M --parsable --wrap="seff $myjobid_check_gene_expression_FKPM >> $outfile_check_gene_expression_FKPM")
