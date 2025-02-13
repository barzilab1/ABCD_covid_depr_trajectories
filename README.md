# Adolescent depressive symptom trajectories from pre- to post-COVID-19 pandemic (in the ABCD Study)

The current project utilizes tables from ABCD Data Release 5.1, as defined in [ABCD_tables_config.R](configurations/ABCD_tables_config.R).

## 1. Data Creation:
This part uses the [targets](https://books.ropensci.org/targets/) package to create the dataset.   
Ensure the [path_config.R](configurations/path_config.R) is set up with the location of the ABCD 5.1 files.   
To create the datasets, run the function targets::tar_make() which can be found in the [run.R](run.R) file.   
All data cleaning processes are in the [data organization](data organization) folder.
The created depression score data is analyzed using the Mplus scripts available in [mplus_scripts](mplus_scripts) folder.

## 2. Data Analysis (using trajectory classes):
1. Run [1_create_data_with_class_membership.R](analysis/1_create_data_with_class_membership.R) to merge cleaned data with trajectory class solutions.
2. Assessment collection date distributions (Figure 1): [2_figure1.R](analysis/2_figure1.R).
3. Main trajectory plots & Mental health comparisons (Figure 2, eTable 5, and eTable 6): [3_mh_comparisons.R](analysis/3_mh_comparisons.R)
4. Demographics comparisons (Table 1 & eTable 7): [4_demo_comparisons.R](analysis/4_demo_comparisons.R) 
5. Pubertal comparisons and trajectories (Figure 3, eTable 10): [5_pubertal_analyses.R](analysis/5_pubertal_analyses.R)
6. Risk & resilience factors (incl. pubertal status and GxE; Figure 4, eTables 8, 9, 11-18): [6_risk_resilience_analysis.R](analysis/6_risk_resilience_analysis.R)
7. Missingness & sensitivity analyses (eTables 3, 19 & 20, eFigures 1 & 2): [7_secondary_analysis.R](analysis/7_secondary_analysis.R)

## 3. Functions and libraries
Functions and libraries used consistently across all analysis files are available in [analysis_utility_fun.R](analysis/analysis_utility_fun.R). Plotting of trajectory analysis from Mplus models is done using an _rdfh5-dependent_ supplemental script available on the [Mplus website](https://www.statmodel.com/mplus-R/) or in the analysis directory as [mplus.R](analysis/mplus/R)


