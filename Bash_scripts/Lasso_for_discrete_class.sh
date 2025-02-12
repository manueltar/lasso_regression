#!/bin/bash
 
MASTER_ROUTE=$1
analysis=$2

 

Rscripts_path=$(echo "/home/manuel.tardaguila/Scripts/R/")
module load R/4.1.0


bashrc_file=$(echo "/home/manuel.tardaguila/.bashrc")

source $bashrc_file
eval "$(conda shell.bash hook)"


output_dir=$(echo "$MASTER_ROUTE""$analysis""/")

Log_files=$(echo "$output_dir""/""Log_files/")

rm -rf $Log_files
mkdir -p $Log_files

###################################  Do a lasso on the class MPRA positive|DE and or ATU vs MPRA negative|DE and or ATU
################################### Do a lasso on the class MPRA positive|DE and or ATU vs MPRA positive|No RNA effect

### lasso_on_classes ####################################################################


type=$(echo "lasso_on_classes")
outfile_lasso_on_classes=$(echo "$Log_files""outfile_3_""$type"".log")
touch $outfile_lasso_on_classes
echo -n "" > $outfile_lasso_on_classes
name_lasso_on_classes=$(echo "$type""_job")
seff_name=$(echo "seff""_""$type")
 

Rscript_lasso_on_classes=$(echo "$Rscripts_path""384_Lasso_regression_MPRA_plus_RNA.R")

new_out=$(echo "$output_dir""lasso""/")

NEW_Table_S6_with_interaction=$(echo "$output_dir""NEW_Table_S6_with_interaction.rds")
feature_file=$(echo "/group/soranzo/manuel.tardaguila/MPRA_feature_file/Attempt_1/Features_wide_df_with_TF_motifs_filtered_new.rds")

# --dependency=afterany:$myjobid_UpsetR_modalities

myjobid_lasso_on_classes=$(sbatch --output=$outfile_lasso_on_classes --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=4 --mem-per-cpu=4096 --parsable --job-name $name_lasso_on_classes --wrap="Rscript $Rscript_lasso_on_classes --NEW_Table_S6_with_interaction $NEW_Table_S6_with_interaction --feature_file $feature_file --type $type --out $output_dir")
myjobid_seff_lasso_on_classes=$(sbatch --dependency=afterany:$myjobid_lasso_on_classes --open-mode=append --output=$outfile_lasso_on_classes --job-name=$seff_name --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=1 --mem-per-cpu=128M --parsable --wrap="seff $myjobid_lasso_on_classes >> $outfile_lasso_on_classes")


### collect_results_comp1_comp2 ####################################################################


type=$(echo "collect_results_comp1_comp2")
outfile_collect_results_comp1_comp2=$(echo "$Log_files""outfile_4_""$type"".log")
touch $outfile_collect_results_comp1_comp2
echo -n "" > $outfile_collect_results_comp1_comp2
name_collect_results_comp1_comp2=$(echo "$type""_job")
seff_name=$(echo "seff""_""$type")


Rscript_collect_results_comp1_comp2=$(echo "$Rscripts_path""385_collect_results_lasso_regression_MPRA_plus_RNA.R")

new_out=$(echo "$output_dir""lasso""/")

###### --dependency=afterany:$myjobid_lasso_on_classes

myjobid_collect_results_comp1_comp2=$(sbatch --dependency=afterany:$myjobid_lasso_on_classes --output=$outfile_collect_results_comp1_comp2 --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=1 --mem-per-cpu=1024 --parsable --job-name $name_collect_results_comp1_comp2 --wrap="Rscript  $Rscript_collect_results_comp1_comp2 --type $type --out $new_out")
myjobid_seff_collect_results_comp1_comp2=$(sbatch --dependency=afterany:$myjobid_collect_results_comp1_comp2 --open-mode=append --output=$outfile_collect_results_comp1_comp2 --job-name=$seff_name --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=1 --mem-per-cpu=128M --parsable --wrap="seff $myjobid_collect_results_comp1_comp2 >> $outfile_collect_results_comp1_comp2")



### feature_selection_across_kfolds_comp1_comp2 ####################################################################


type=$(echo "feature_selection_across_kfolds_comp1_comp2")
outfile_feature_selection_across_kfolds_comp1_comp2=$(echo "$Log_files""outfile_5_""$type"".log")
touch $outfile_feature_selection_across_kfolds_comp1_comp2
echo -n "" > $outfile_feature_selection_across_kfolds_comp1_comp2
name_feature_selection_across_kfolds_comp1_comp2=$(echo "$type""_job")
seff_name=$(echo "seff""_""$type")


Rscript_feature_selection_across_kfolds_comp1_comp2=$(echo "$Rscripts_path""386_feature_selection_across_kfolds_MPRA_plus_RNA.R")

new_out=$(echo "$output_dir""lasso""/")

coefficient_file_comp1=$(echo "$new_out""collected_results_comp1.rds")
coefficient_file_comp2=$(echo "$new_out""collected_results_comp2.rds")
Threshold_selections=$(echo '0')

###### --dependency=afterany:$myjobid_collect_results_comp1_comp2

myjobid_feature_selection_across_kfolds_comp1_comp2=$(sbatch --dependency=afterany:$myjobid_collect_results_comp1_comp2 --output=$outfile_feature_selection_across_kfolds_comp1_comp2 --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=1 --mem-per-cpu=1024 --parsable --job-name $name_feature_selection_across_kfolds_comp1_comp2 --wrap="Rscript  $Rscript_feature_selection_across_kfolds_comp1_comp2 --coefficient_file_comp1 $coefficient_file_comp1 --coefficient_file_comp2 $coefficient_file_comp2 --Threshold_selections $Threshold_selections --type $type --out $new_out")
myjobid_seff_feature_selection_across_kfolds_comp1_comp2=$(sbatch --dependency=afterany:$myjobid_feature_selection_across_kfolds_comp1_comp2 --open-mode=append --output=$outfile_feature_selection_across_kfolds_comp1_comp2 --job-name=$seff_name --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=1 --mem-per-cpu=128M --parsable --wrap="seff $myjobid_feature_selection_across_kfolds_comp1_comp2 >> $outfile_feature_selection_across_kfolds_comp1_comp2")



### heatmap_comp1 ####################################################################


type=$(echo "heatmap_comp1")
outfile_heatmap_comp1=$(echo "$Log_files""outfile_6_""$type"".log")
touch $outfile_heatmap_comp1
echo -n "" > $outfile_heatmap_comp1
name_heatmap_comp1=$(echo "$type""_job")
seff_name=$(echo "seff""_""$type")


Rscript_heatmap_comp1=$(echo "$Rscripts_path""392_heatmap_comp1.R")

new_out=$(echo "$output_dir""lasso""/")

coefficient_file_comp1_MAX_thresholded=$(echo "$new_out""coefficient_file_comp1_MAX_thresholded.rds")
GWAS_parameters=$(echo "PP,Absolute_effect_size,credset_size,MAF")
Variant_based_scores=$(echo "CADD_raw,Gnocchi,NCBoost,SpliceAI_DG,SpliceAI_DL,SpliceAI_AG,SpliceAI_AL")
Our_rankings=$(echo "Rank_ATAC_erythroid_lineage,Rank_ATAC_mega_lineage,Rank_ATAC_gran_mono_lineage,Rank_ATAC_lymph_lineage,multi_lineage_ATAC,Rank_PCHiC,Rank_chromstates")
Gene_based_features=$(echo "COGS,oe_lof,Rank_GENE_EXP")
Lineages=$(echo "Lymphocyte_CLASS,GM_CLASS,Megakaryocytic_CLASS,Erythroid_CLASS")


# --dependency=afterany:$myjobid_feature_selection_across_kfolds_comp1_comp2

myjobid_heatmap_comp1=$(sbatch --dependency=afterany:$myjobid_feature_selection_across_kfolds_comp1_comp2 --output=$outfile_heatmap_comp1 --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=4 --mem-per-cpu=1024 --parsable --job-name $name_heatmap_comp1 --wrap="Rscript $Rscript_heatmap_comp1 --coefficient_file_comp1_MAX_thresholded $coefficient_file_comp1_MAX_thresholded --GWAS_parameters $GWAS_parameters --Variant_based_scores $Variant_based_scores --Our_rankings $Our_rankings --Gene_based_features $Gene_based_features --Lineages $Lineages --type $type --out $new_out")
myjobid_seff_heatmap_comp1=$(sbatch --dependency=afterany:$myjobid_heatmap_comp1 --open-mode=append --output=$outfile_heatmap_comp1 --job-name=$seff_name --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=1 --mem-per-cpu=128M --parsable --wrap="seff $myjobid_heatmap_comp1 >> $outfile_heatmap_comp1")


### plot_variables_comp1 ####################################################################


type=$(echo "plot_variables_comp1")
outfile_plot_variables_comp1=$(echo "$Log_files""outfile_7_""$type"".log")
touch $outfile_plot_variables_comp1
echo -n "" > $outfile_plot_variables_comp1
name_plot_variables_comp1=$(echo "$type""_job")
seff_name=$(echo "seff""_""$type")


Rscript_plot_variables_comp1=$(echo "$Rscripts_path""393_Graphs_for_lasso_features_comp1.R")

new_out=$(echo "$output_dir""lasso""/")

REP_comp1=$(echo "$new_out""REP_comp1.rds")
NEW_Table_S6=$(echo "/group/soranzo/manuel.tardaguila/paper_relaunch/MPRA_S_no_global_metanalysis/NEW_Table_S6.rds")
feature_file=$(echo "/group/soranzo/manuel.tardaguila/MPRA_feature_file/Attempt_1/Features_wide_df_with_TF_motifs_filtered_new.rds")

##########################--dependency=afterany:$myjobid_heatmap_comp1

myjobid_plot_variables_comp1=$(sbatch --dependency=afterany:$myjobid_heatmap_comp1 --output=$outfile_plot_variables_comp1 --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=4 --mem-per-cpu=4096 --parsable --job-name $name_plot_variables_comp1 --wrap="Rscript $Rscript_plot_variables_comp1 --REP_comp1 $REP_comp1 --NEW_Table_S6 $NEW_Table_S6 --feature_file $feature_file --type $type --out $new_out")
myjobid_seff_plot_variables_comp1=$(sbatch --dependency=afterany:$myjobid_plot_variables_comp1 --open-mode=append --output=$outfile_plot_variables_comp1 --job-name=$seff_name --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=1 --mem-per-cpu=128M --parsable --wrap="seff $myjobid_plot_variables_comp1 >> $outfile_plot_variables_comp1")

 
### check_gene_expression_FKPM ####################################################################


type=$(echo "check_gene_expression_FKPM")
outfile_check_gene_expression_FKPM=$(echo "$Log_files""outfile_8_""$type"".log")
touch $outfile_check_gene_expression_FKPM
echo -n "" > $outfile_check_gene_expression_FKPM
name_check_gene_expression_FKPM=$(echo "$type""_job")
seff_name=$(echo "seff""_""$type")


Rscript_check_gene_expression_FKPM=$(echo "$Rscripts_path""390_inspect_the_transcriptomes_v3.R")

new_out=$(echo "$output_dir""lasso""/")

coefficient_file_comp1=$(echo "$new_out""coefficient_file_comp1_MAX_thresholded.rds")
rna_K562=$(echo "/group/soranzo/manuel.tardaguila/paper_relaunch/MPRA_S_no_global_metanalysis/rna_expression_report_2024_12_2_11h_37m.tsv")
INTERVAL_GENE_EXP=$(echo "/group/soranzo/manuel.tardaguila/INTERVAL_results/ge_matrix_residuals_scaled_final.csv")

myjobid_check_gene_expression_FKPM=$(sbatch --dependency=afterany:$myjobid_plot_variables_comp1 --output=$outfile_check_gene_expression_FKPM --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=2 --mem-per-cpu=4096 --parsable --job-name $name_check_gene_expression_FKPM --wrap="Rscript  $Rscript_check_gene_expression_FKPM --coefficient_file_comp1 $coefficient_file_comp1 --rna_K562 $rna_K562 --rna_K562 $rna_K562 --INTERVAL_GENE_EXP $INTERVAL_GENE_EXP --type $type --out $new_out")
myjobid_seff_check_gene_expression_FKPM=$(sbatch --dependency=afterany:$myjobid_check_gene_expression_FKPM --open-mode=append --output=$outfile_check_gene_expression_FKPM --job-name=$seff_name --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=1 --mem-per-cpu=128M --parsable --wrap="seff $myjobid_check_gene_expression_FKPM >> $outfile_check_gene_expression_FKPM")
