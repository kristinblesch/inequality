README 

### folders: 


### raw_data_and_data_cleaning: 

- Description of data sources: SI Section 3

- Source tables to create Lorenz curves on a US county level
1. American Community Survey: Data tables and codebook in folder ACS_data
2. Report on income inequality in the US, table B6, B4 (note that B4 is not relevant for the thesis, but merged for further research that is aimed to be conducted using this data set): Table_B4.csv and Table_B6.csv pulled from https://www.epi.org/publication/income-inequality-in-the-us/#epi-toc-20 

- Data preprocessing
1. Merging source tables: data_cleaning_merge_b4_b6_nhgis_A_B.R  produces  data table merged_b4_b6_nhgis.csv 
2. Using merged_b4_b6_nhgis.csv to create Lorenz curve data: create_Lorenz_curves.R produces population_shares_per_county.csv (cumulative shares of population from poor to rich, i.e. Lorenz curve values on x-axis for each county) and income_shares_per_county.csv (cumulative shares of income, i.e. Lorenz curve values on y-axis for each county). This means that both .csv files, i.e. x and y values, have to be combined if one wants to draw the Lorenz curves for each county. 

- Cross-check on U.S. state level
Aggregate data on a state level: aggregate_state_level.R produces income_shares_per_state.csv and population_shares_per_state.csv 

### estimation_procedure: 

- Estimation procedure for county level Lorenz curves using population_shares_per_county.csv and income_shares_per_county.csv 
1. Code for NLS estimation: NLS_estimation_county.R ; results (estimated parameters): NLS_output_county.csv
2. Code for MLE estimation: MLE_estimation_county.R ; results (estimated parameters): MLE_output_county.csv

- Estimation procedure for state level Lorenz curves using income_shares_per_state.csv and population_shares_per_state.csv: MLE_estimation_state.R ; results: MLE_output_state.csv

### voting

- Aggregating estimation results based on estimated parameters by MLE, i.e. voting results: voting.R
- cross check of voting procedure using the BIC criterion: first calculate the BIC (calculate_BIC.R) and then use the produced data set to rerun the voting procedures (voting_BIC.R)


### ortega_parameters

- Estimated Ortega parameters such that they can be used as inequality measures:
1. US county level: ortega_parameter_alpha_gamma_county.csv
2. US state level:ortega_parameter_alpha_gamma_state.csv

### information_criteria

- calculated information criteria AIC and BIC for the Lorenz curves estimated on a US county level: information_criteria_county.csv
- analyses on the AIC and BIC differences: AIC_differences_histograms_multiple_models.R, BIC_differences_histograms_multiple_models.R


### approximating_empirical_gini
- calculate the empirical Gini coefficient per county (empirical_gini.csv) in a non-parametric way: calc_empirical_gini.R
- calculate the Gini coefficient implied by the Lorenz curve models using integrals and evaluate how they compare to the non-parametric Gini coefficients : calc_model_gini_and_compare.R
