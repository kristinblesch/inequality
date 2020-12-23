README - exploratory correlational study

Source data sets from the American Community Survey and Opportunity Insights: contained in folders ACS_data and opportunity_insights_data. Additional data sets used for the correlational study: countyfipstool20190120.RData (also available and further explained on https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/OSLU4G) , empirical_gini.csv (empirical Gini index calculated from the Lorenz curves used for county-level MLE/NLS estimation, i.e. population_shares_per_county.csv and income_shares_county.csv), county_name_equivalences.csv, and AIC_par_county.csv (a .csv file summarizing the estimation results on a county level for the Ortega model, lognormal model and AIC values)

Those data sets are merged using the code merge_source_tables.R, which produces the .csv file df_exploratory_data_analysis.csv. The codebook for this data set is provided by codebook_df_exploratory_data_analysis.csv

The exploratory analysis can be rerun using the code exploratory_data_analysis that also produces the figures provided in the supplementary material.