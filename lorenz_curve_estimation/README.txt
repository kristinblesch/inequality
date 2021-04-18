README - Lorenz curve estimation 

- Description of data sources: See supplementary information 

- Source tables to create Lorenz curves on a US county level
1. American Community Survey: Data tables and codebook in folder ACS_data
2. Report on income inequality in the US, table B6, B4 (note that B4 is not relevant for the thesis, but merged for further research that is aimed to be conducted using this data set): Table_B4.csv and Table_B6.csv pulled from https://www.epi.org/publication/income-inequality-in-the-us/#epi-toc-20 

- Data preprocessing
1. Merging source tables: data_cleaning_merge_b4_b6_nhgis_A_B.R  produces  data table merged_b4_b6_nhgis.csv 
2. Using merged_b4_b6_nhgis.csv to create Lorenz curve data: create_Lorenz_curves.R produces population_shares_per_county.csv (cumulative shares of population from poor to rich, i.e. Lorenz curve values on x-axis for each county) and income_shares_per_county.csv (cumulative shares of income, i.e. Lorenz curve values on y-axis for each county). This means that both .csv files, i.e. x and y values, have to be combined if one wants to draw the Lorenz curves for each county. 

- Estimation procedure for county level Lorenz curves using population_shares_per_county.csv and income_shares_per_county.csv 
1. Code for NLS estimation: NLS_estimation_county.R ; results (estimated parameters): NLS_output_county.csv
2. Code for MLE estimation: MLE_estimation_county.R ; results (estimated parameters): MLE_output_county.csv

- Aggregating estimation results based on estimated parameters by MLE, i.e. voting results: voting.R
- cross check of voting procedure using the BIC criterion: first calculate the BIC (calculate_BIC.R) and then use the produced data set to rerun the voting procedures (voting_BIC.R)

- Cross-check on U.S. state level
1. Aggregate data on a state level: aggregate_state_level.R produces income_shares_per_state.csv and population_shares_per_state.csv
2. Code for MLE estimation on a state level: MLE_estimation_state.R ; results: MLE_output_state.csv

- Simulation study: The only variation in the code is the true income share generating model. We have three separate codes such that we can use three machines at the same time. In the resutls .csv files, V1 is the model with minimum AIC_c for that simulation run and V2 gives the number of simulated data points (actual sample size is then V2 +2, because boundary values of 0 and 1 are added) 
1. Code for Ortega being the true income share generating model: ortega_sim.R ; results: ortega_output_sim.csv
2. Code for GB2 being the true income share generating model: gb2_sim.R ; results: gb2_output_sim.csv
3. Code for Wang being the true income share generating model: wang_sim.R ; results: wang_output_sim.csv

- Estimated Ortega parameters such that they can be used as inequality measures:
1. US county level: ortega_parameter_alpha_gamma_county.csv
2. US state level:ortega_parameter_alpha_gamma_state.csv
