#############################
# Merging Data Sets for exploratory correlational study 
#############################
library("readstata13")
library(dplyr)
library(tidyr)
library('RVAideMemoire')
#############################
# Opportunity Insights Data Set 3: "County-Level Causal Place Effects and Covariates"
# source: https://opportunityinsights.org/data/?geographic_level=102&topic=0&paper_id=0#resource-listing
# codebook https://opportunityinsights.org/wp-content/uploads/2018/04/online_table4-2.pdf
# county fips matching tool from https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/OSLU4G
#############################

load("/Users/kristinblesch/Library/Mobile Documents/com~apple~CloudDocs/Paper_inequality/Data/countyfipstool20190120.RData")
dat <- read.dta13("/Users/kristinblesch/Library/Mobile Documents/com~apple~CloudDocs/Paper_inequality/Data/opportunity_insights/data_set_3_county_level_causal_place_effects_and_covariates.dta")
# add fips codes such that it can be merged to other data sets later on
# dat state name = sname = "statename"
# dat county name = cname = "county_name"
dat <-  dat %>% rename(sname = statename) %>% mutate(cname = tolower(county_name))
# manually adjust unusual spellings
dat[dat$cname == "matanuska-susitna", "cname"] <-"matanuskasusitna"
dat[dat$cname == "valdez-cordova", "cname"] <-"valdezcordova"
dat[dat$cname == "wrangell-petersburg", "cname"] <-"wrangellpetersburg"
dat[dat$cname == "yukon-koyukuk", "cname"] <-"yukonkoyukuk"
dat[dat$cname == "queen anne's", "cname"] <-"queenannes"
dat[dat$cname == "st mary's", "cname"] <-"stmarys"
dat[dat$cname == "radford city", "cname"] <-"radford" 
dat[dat$cname == "prince of wales-outer ketchikan", "cname"] <-"prince wales ketchikan borough"
dat[dat$cty1990 == 12025, "cname"] <- "miamidade"
new_frame <- merge(dat, x, by = c("sname", "cname"))
setdiff(dat$cty1990, new_frame$cty1990) # only entry 11001 (district of columbia) missing, which we exclude anyways 
# include relevant covariates only 
new_frame <- new_frame %>% select(sname, cname, fips, poor_share, log_pop_density, inc_share_1perc, gini99,
                                  frac_middleclass, taxrate, subcty_total_taxes_pc, tax_st_diff_top20, ccd_exp_tot, ccd_pup_tch_ratio,
                                  dropout_r, gradrate_r, mig_inflow, mig_outflow, scap_ski90pcm, crime_violent, crime_total, 
                                  cs_fam_wkidsinglemom, cs_divorced, cs_married)
new_frame$fips <- formatC(new_frame$fips, width = 5, format = "d", flag = "0")
data_set_3 <- new_frame


########################
# Opportunity InsightsData Set 17: "County Life Expectancy Estimates by Gender and Income Quartile"
# data set 17 from https://opportunityinsights.org/data/?geographic_level=102&topic=0&paper_id=0#resource-listing 
# codebook https://opportunityinsights.org/wp-content/uploads/2018/04/health_ineq_online_table_11_readme.pdf
########################

df_17 <- read.csv("/Users/kristinblesch/Library/Mobile Documents/com~apple~CloudDocs/Paper_inequality/Data/opportunity_insights/health_ineq_online_table_11.csv")
df_17$cty <- formatC(df_17$cty, width = 5, format = "d", flag = "0")
# relevant variables only -- life expectancy at age 40 by gender and income quartile
df_17 <- df_17 %>% select(cty, county_name, statename, le_raceadj_q1_F, le_raceadj_q2_F, le_raceadj_q3_F ,le_raceadj_q4_F,
                    le_raceadj_q1_M, le_raceadj_q2_M, le_raceadj_q3_M ,le_raceadj_q4_M,
                    le_agg_q1_F, le_agg_q2_F, le_agg_q3_F ,le_agg_q4_F,
                    le_agg_q1_M, le_agg_q2_M, le_agg_q3_M ,le_agg_q4_M)
data_set_17 <- df_17

########################
# Opportunity Insights Data Set 18: "County Characteristics (Described in eTable 9)"
# data set 18 from https://opportunityinsights.org/data/?geographic_level=102&topic=0&paper_id=0#resource-listing 
# codebook from https://opportunityinsights.org/wp-content/uploads/2018/04/health_ineq_online_table_12_readme.pdf
########################

df_18 <- read.csv("/Users/kristinblesch/Library/Mobile Documents/com~apple~CloudDocs/Paper_inequality/Data/opportunity_insights/health_ineq_online_table_12.csv")
df_18$cty <- formatC(df_18$cty, width = 5, format = "d", flag = "0")

df_18 <- df_18 %>% select(cty, county_name, statename, intersects_msa, cur_smoke_q1, cur_smoke_q2, cur_smoke_q3,cur_smoke_q4,
                    bmi_obese_q1, bmi_obese_q2, bmi_obese_q3, bmi_obese_q4, exercise_any_q1, exercise_any_q2,exercise_any_q3,
                    exercise_any_q4, reimb_penroll_adj10, mort_30day_hosp_z,diab_hemotest_10, diab_eyeexam_10,
                    diab_lipids_10, rel_tot, unemp_rate, pop_d_2000_1980, lf_d_2000_1980, e_rank_b)
data_set_18 <- df_18


########################
# American Community Survey 2011-2015 Data on County-Level Covariates: NHGIS code: 2011_2015_ACS5a
# Source Tables:
# B01001, B01002, B01003, B02001, B08301, B08303, B11001, B11015, B15003, C17002, B19013, B19055, B19301, B22010
# B23025, B25001, B25002, B25003, B25064, B25081, B27010, B99051
# codebook: see .csv file


df <- read.csv("/Users/kristinblesch/Library/Mobile Documents/com~apple~CloudDocs/Paper_inequality/Data/nhgis0010_csv/nhgis0010_ds215_20155_2015_county.csv")
df$STATEA <-  formatC(df$STATEA, width = 2, format = "d", flag = "0")
df$COUNTYA <-  formatC(df$COUNTYA, width = 3, format = "d", flag = "0")
df <- df %>% unite("FIPS",c(STATEA, COUNTYA), sep = "",remove = F)

# create variables of interest, calculate fractions
df <- df %>% 
  mutate(frac_male = ADKLE002/ADKLE001) %>%  # fraction of males: Males/Population
  mutate(frac_white = ADKXE002/ADKXE001) %>% # fraction White alone
  mutate(frac_black = ADKXE003/ADKXE001) %>% # fraction Black /African American alone
  mutate(frac_indian = ADKXE004/ADKXE001) %>% # fraction Indian and Alaska Native alone
  mutate(frac_asian = ADKXE005/ADKXE001) %>% # fraction Asian alone
  mutate(frac_pacific = ADKXE006/ADKXE001) %>% # fraction Native Hawaiian and Other Pacific Islander alone
  mutate(frac_multiple_races = ADKXE008/ADKXE001) %>% # fraction Two or more races
  
  mutate(frac_trans_car = ADLME002/ADLME001) %>% # fraction transportation to work by car, truck or van
  mutate(frac_trans_public_trans = ADLME010/ADLME001) %>% # fraction transportation to work by Public transportation
  mutate(frac_trans_taxi = ADLME016/ADLME001) %>% # fraction transportation to work by Taxicab
  mutate(frac_trans_motorcycle = ADLME017/ADLME001) %>% # fraction transportation to work by Motorcycle
  mutate(frac_trans_bicycle = ADLME018/ADLME001) %>% # fraction transportation to work by Bicycle
  mutate(frac_trans_walked = ADLME019/ADLME001) %>% # fraction transportation to work: Walked
  mutate(frac_worked_at_home = ADLME021/ADLME001) %>% # fraction: Worked at home
  
  # Travel Time to Work; Universe:    Workers 16 years and over who did not work at home
  mutate(frac_travel_less_15 = (ADLOE002 + ADLOE003 + ADLOE004)/ADLOE001) %>% # fraction travel time to work <15min
  mutate(frac_travel_15_30 = (ADLOE005 + ADLOE006 + ADLOE007 )/ADLOE001) %>% # fraction travel time to work <30min
  mutate(frac_travel_30_60 = (ADLOE008 + ADLOE009 + ADLOE010 + ADLOE011)/ADLOE001) %>% # fraction travel time to work <60min
  mutate(frac_travel_more_than_60 = (ADLOE012 + ADLOE013)/ADLOE001) %>% # fraction travel time to work >60min
  # household type
  mutate(frac_family_households = ADLUE002/ADLUE001) %>% # fraction of households of type "family household"
  mutate(frac_households_with_nonrelatives =ADMJE002/ADMJE001 ) %>%# fraction of households with one or more nonrelatives
  # educational variables
  # Educational Attainment for the Population 25 Years and Over
  mutate(frac_wo_schooling_completed = ADMZE002/ADMZE001)%>% # fraction  No schooling completed
  mutate(frac_high_school_diploma = ADMZE017/ADMZE001)%>% # fraction  with Regular high school diploma
  mutate(frac_associate_degree = ADMZE021/ADMZE001) %>%# fraction with Associate's degree
  mutate(frac_bachelors_degree = ADMZE022/ADMZE001)%>% # fraction with Bachelor's degree
  mutate(frac_masters_degree = ADMZE023/ADMZE001)%>% # fraction with Master's degree
  mutate(frac_prof_school_degree = ADMZE024/ADMZE001)%>% # fraction with Professional school degree
  mutate(frac_doctorate_degree = ADMZE025 / ADMZE001)%>% # fraction with Doctorate degree
  # Social Security Income in the Past 12 Months for Households
  mutate(frac_socsecincome = ADN2E002/ADN2E001)%>% # fraction of population with Social Security income
  # Receipt of Food Stamps/SNAP in the Past 12 Months 
  mutate(frac_food_stamps = ADPBE002/ADPBE001) %>% # Household received Food Stamps/SNAP in the past 12 months
  # Employment Status for the Population 16 Years and Over
  mutate(frac_not_in_labor_force = ADPIE007/ADPIE001) %>% # fraction not in labor force
  # housing units
  mutate(frac_vacant_housing = ADPZE003 / ADPZE001) %>% # fraction of housing units vacant
  mutate(frac_owner_occupied = ADP0E002 / ADP0E001) %>% # Occupied housing units that are owner occupied
  mutate(frac_housing_mortgage = ADR0E002/ADR0E001) %>% # fraction of Housing units with a mortgage, contract to purchase, or similar debt
  # health insurance
  mutate(frac_wo_health_insurance = ( ADSBE033+ADSBE050+ADSBE066)/(ADSBE018+ADSBE034+ADSBE051))%>% # fraction of population >18 years: No health insurance coverage
  # foreign born
  mutate(frac_foreign_born = ADSGE005/ADSGE001)

# select relevant variables 

df <- df %>% select("STATE", "COUNTY", "FIPS","NAME_E","frac_male","frac_white",  "frac_black",         "frac_indian"  ,                    
                    "frac_asian",                        "frac_pacific" ,                     "frac_multiple_races",         
                    "frac_trans_car",                    "frac_trans_public_trans",           "frac_trans_taxi",                  
                    "frac_trans_motorcycle"  ,           "frac_trans_bicycle"   ,             "frac_trans_walked"      ,          
                    "frac_worked_at_home"   ,            "frac_travel_less_15"     ,          "frac_travel_15_30"   ,           
                    "frac_travel_30_60"      ,         "frac_travel_more_than_60"   ,       "frac_family_households"  ,         
                    "frac_households_with_nonrelatives", "frac_wo_schooling_completed"    ,   "frac_high_school_diploma"  ,       
                    "frac_associate_degree"    ,         "frac_bachelors_degree" ,            "frac_masters_degree"      ,        
                    "frac_prof_school_degree"    ,       "frac_doctorate_degree"     ,        "frac_socsecincome"       ,         
                    "frac_food_stamps"       ,           "frac_not_in_labor_force" ,          "frac_vacant_housing"   ,           
                    "frac_owner_occupied"       ,        "frac_housing_mortgage"   ,          "frac_wo_health_insurance" ,        
                    "frac_foreign_born"        , "ADKWE001",       "ADKME001",  "ADNKE001","ADOLE001", "ADPYE001", "ADRKE001")

# ADKLE001:    Total population
# ADKWE001:    Median age: Total
# ADNKE001:    Median household income in the past 12 months (in 2015 inflation-adjusted dollars)
# ADOLE001:    Per capita income in the past 12 months (in 2015 inflation-adjusted dollars)
# ADPYE001:    Total number of housing units
# ADRKE001:    Median gross rent

data_set_acs <- df

#########################
# Merge Source Tables
#########################

aspects <- merge(x = data_set_3, y = data_set_17, by.x = "fips", by.y = "cty", all.x = T) %>% merge(x = ., y=data_set_18, by.x = "fips", by.y="cty") %>%
  merge(x = ., y = data_set_acs, by.x = "fips", by.y = "FIPS", all.y = T) %>% select(-c("county_name.x", "statename.x","county_name.y", "statename.y", "STATE", "COUNTY"))
# 100 covariates plus csname, sname, fips and NAME_E = 104 variables


##########################
# Add Inequality Measures and AIC values
# AIC, Ortega parameters, lognormal parameter (for comparison) and Gini from ACS 2011-2015
##########################

AIC_raw <- read.csv("/Users/kristinblesch/Library/Mobile Documents/com~apple~CloudDocs/Paper_inequality/Data/AIC_par_county.csv")
county_names <- read.csv("/Users/kristinblesch/Library/Mobile Documents/com~apple~CloudDocs/Paper_inequality/Data/county_name_equivalences.csv")
# equivalent spellings of county names and fips
empirical_gini <- read.csv("/Users/kristinblesch/Library/Mobile Documents/com~apple~CloudDocs/Paper_inequality/Data/empirical_gini.csv")
# empirical Gini = Gini index calculated from empirical Lorenz curves we have used to estimate the Ortega parameters (added for comparison)
AIC_diff <- spread(AIC_raw, key = form, value = par_1 ) %>% rename(ortega_1 = ORTEGA) %>% rename(ortega_2 = par_2) %>% 
  rename(lognormal_1 = LOGNORMAL) %>% select(COUNTY, AIC_c_diff, ortega_1, ortega_2,lognormal_1 ) %>% fill(ortega_1, ortega_2) %>% drop_na()
AIC_diff <- merge(AIC_diff, empirical_gini, by.x = "COUNTY", by.y = "county" ) %>% select(-X) %>%
  merge(x = ., y = county_names, by.x = "COUNTY", by.y = "modified_NAME_E") %>%# relevant name $modified_NAME_E
  select(fips, cname, sname, AIC_c_diff, ortega_1, ortega_2,lognormal_1,  empirical_gini)
AIC_diff$fips <-  formatC(AIC_diff$fips, width = 5, format = "d", flag = "0")
df_AIC_aspects <- AIC_diff %>% select(-c("cname", "sname")) %>% merge(x = ., y = aspects, by ="fips" )

# add ACS Gini 2011-2015
# NHGIS code 2011_2015_ACS5b, Source Table: B19083  
acs_gini <- read.csv("/Users/kristinblesch/Library/Mobile Documents/com~apple~CloudDocs/Paper_inequality/Data/Archiv/nhgis0008_csv/nhgis0008_ds216_20155_2015_county.csv")
acs_gini$COUNTYA <-  formatC(acs_gini$COUNTYA, width = 3, format = "d", flag = "0")
acs_gini <- acs_gini %>% unite("FIPS",c(STATEA, COUNTYA), sep = "",remove = F) %>% select("FIPS", "AD4BE001")
acs_gini$FIPS <- formatC(as.integer(acs_gini$FIPS), width = 5, format = "d", flag = "0")
df_final <- merge(acs_gini, df_AIC_aspects, by.y = "fips", by.x = "FIPS") %>% rename(acs_gini = AD4BE001)

write.csv(df_final, "/Users/kristinblesch/Library/Mobile Documents/com~apple~CloudDocs/Paper_inequality/Data/df_exploratory_data_analysis.csv",
                  row.names = F)
# COMMENT: We have estimated 3056 Lorenz curves but for the exploratory data analysis, however, we can only use 
# 3049 because the merged data sets were not from the exact same year so for counties in Alaska, substantial changes in fips code assignments
# have occured such that we omit these counties, for details, see https://www.census.gov/programs-surveys/geography/technical-documentation/county-changes.2000.html
