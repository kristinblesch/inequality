README - exploratory correlational study

### raw data folder: 

- Source data sets from the American Community Survey and Opportunity Insights: contained in folders ACS_data and opportunity_insights_data. Additional data sets used for the correlational study: countyfipstool20190120.RData (also available and further explained on https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/OSLU4G) , empirical_gini.csv (empirical Gini index calculated from the Lorenz curves used for county-level MLE/NLS estimation, i.e. population_shares_per_county.csv and income_shares_county.csv), county_name_equivalences.csv, and AIC_par_county.csv (a .csv file summarizing the estimation results on a county level for the Ortega model, lognormal model and AIC values)

- The data sets contained in the 'raw data' folder are merged using the code merge_source_tables.R, which produces the .csv file df_exploratory_data_analysis.csv

### other files

- data frame for the exploratory analysis: df_exploratory_data_analysis.csv

- Codebook for data set df_exploratory_data_analysis.csv: codebook_df_exploratory_data_analysis.csv

- Code for the actual correlational study: exploratory_data_analysis.R 