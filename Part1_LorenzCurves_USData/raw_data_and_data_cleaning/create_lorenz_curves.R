###############################################################################
### Data cleaning - Step 2: calculating Lorenz curves per county
###############################################################################
### author: Kristin Blesch, May 2020

### Descrption of general procedure:
### for NHGIS data that is in the format of counts of people per income bucket:
### - calculate percentage of people per income bucket
### - estimate the share of total income per income bucket (assuming a symmetric distribution within the bucket)
### scale NHGIS data such that it matches the B6 data at the 95% percentile of the population
### create two data tables:
### - cumulative share of people per county
### - cumulative share of total income per county

### ------------------------------------------------------------------------------------------------------

# load relevant librarys
library(dplyr)

# load data
# 
dat <- read.csv("../raw_data_and_data_cleaning/merged_b4_b6_nhgis.csv")

## relevant variables for calculating income shares per bucket
##  ADNJE001:    number of people per county (total)
##        ADNJE002:    Less than $10,000
##        ADNJE003:    $10,000 to $14,999
##        ADNJE004:    $15,000 to $19,999
##        ADNJE005:    $20,000 to $24,999
##        ADNJE006:    $25,000 to $29,999
##        ADNJE007:    $30,000 to $34,999
##        ADNJE008:    $35,000 to $39,999
##        ADNJE009:    $40,000 to $44,999
##        ADNJE010:    $45,000 to $49,999
##        ADNJE011:    $50,000 to $59,999
##        ADNJE012:    $60,000 to $74,999
##        ADNJE013:    $75,000 to $99,999
##        ADNJE014:    $100,000 to $124,999
##        ADNJE015:    $125,000 to $149,999
##        ADNJE016:    $150,000 to $199,999
##        ADNJE017:    $200,000 or more
##  ANDLE001:     aggregate income per county

income_buckets_means <- c(4999.5, 12499.5,  17499.5,  22499.5,  27499.5,  32499.5,  37499.5,  42499.5,
                          47499.5,  54999.5, 67499.5,  87499.5, 112499.5, 137499.5, 174999.5)

# calculating the income share held by bucket, assuming a symmetric distribution within the bucket, so we can take the 
# mean * people per bucket / aggregate income as a approximation for the share of income earned by that bucket;
# only closed buckets considered, i.e. we omit ADNJE017: $200,000 or more
buckets <- colnames(dat)[24:38] # variable names of income buckets ADNJE002-ADNJE016
income_share_estimate_csum <- income_buckets_means %>% rep(nrow(dat)) %>% 
  matrix(ncol = length(income_buckets_means), byrow = T) %>% "*" (dat[,buckets]) %>%
  "/"(dat[,"ADNLE001"]) %>% apply(MARGIN = 1, FUN = cumsum) %>% t()

# cumulative share of people in buckets
people_share_csum <- as.matrix(dat[,buckets]/dat[,"ADNJE001"]) %>% apply(MARGIN = 1, FUN = cumsum) %>% t()

# information of NHGIS table B: income shares held by the 20th, 40th, 60th, 80th and 95th percentile of the population
quintiles <- c(0.2, 0.4, 0.6, 0.8, 0.95,1)
quintile_shares <- dat %>% mutate(bottom.20 = AD4AE001,
                                  bottom.40 = bottom.20 + AD4AE002,
                                  bottom.60 = bottom.40 + AD4AE003, 
                                  bottom.80 = bottom.60 + AD4AE004,
                                  bottom.95 = 100 - AD4AE006,
                                  bottom.100 = bottom.80 + AD4AE005) %>% 
  select("bottom.20", "bottom.40", "bottom.60", "bottom.80", "bottom.95", "bottom.100") %>% "/"(.$bottom.100)

### ------------------------------------------------------------------------------------------------------

# evaluate our estimates for the income share per buckets: Are our estimates coherent with the information on the true
# income shares held by the 20th, 40th, 60th, 80th and 95th percentile of the population?
# visual inspection: --> data on true income shares (green) fits very well our estimates (red)

par(mfrow = c(3,3))
for (i in 211:219) { # some exemplary counties
  plot(x = people_share_csum[i,], y = income_share_estimate_csum[i,], type = "p", col = "red" , main = dat[i, "NAME_E"],
       xlim = c(0,1), ylim = c(0,1),  xlab = "% of people", ylab = "% of total income", pch = 20)
  points(x = quintiles, y = quintile_shares[i,], col = "green3", pch = 20)
}

### ------------------------------------------------------------------------------------------------------

# adding information from table B6 on income shares held by the 90th, 95th, 99th percentile of the population
top_pop <- c(0.9, 0.95, 0.99, 1) 
top_pop_csum <- dat %>% select(c("Bottom.90.","X90th..95th.percentiles","X95th..99th.percentiles",
                                 "Top.1...99th.100th.percentiles.")) %>% apply(MARGIN = 1, FUN = cumsum) %>% 
  t() %>% "/" (.[,"Top.1...99th.100th.percentiles."])

### ------------------------------------------------------------------------------------------------------

# visual inspection: how far off are NHGIS data and table B6
# --> B6 systematically suggests a higher inequality (blue points systematically below the red/green points). This
# is plausible as B6 is on an individual tax payer level and NHGIS on household level (which is already some kind
# of aggregate); aggregated data typically leads to Lorenz curves exhibiting less inequality, see e.g. Farris (2010)

par(mfrow = c(3,3)) 
for (i in 211:219) {
  plot(x = people_share_csum[i,], y = income_share_estimate_csum[i,], type = "p", col = "red" , main = dat[i, "NAME_E"],
       xlim = c(0,1), ylim = c(0,1),  xlab = "% of people", ylab = "% of total income", pch = 20) # income bucket share est.
  points(x = quintiles, y = quintile_shares[i,], col = "green3", pch = 20) # true income share per quintile 
  points(x = top_pop, y = top_pop_csum[i,], col = "blue", pch = 20) # B6 data on top income shares
}

### ------------------------------------------------------------------------------------------------------

# We know that the B6 data systematically suggests higher inequality which is suspected to origin from the fact that the B6
# data is on tax payer leven and the NHGIS data on household level. The tax payer's level is more granular and hence
# assumed to show a more realistic picture of the true income inequality. Therefore, the household-level NHGIS data 
# is scaled down to match the B6 tax payer level data. We have quantiles for the top 95% for both NHGIS and B6, 
# hence we use this point to determine the amount of scaling needed to adjust for the aggregation level discrepancy. 

scaled_nhgis <- bind_cols(as.data.frame(income_share_estimate_csum), quintile_shares) %>%
  "*" (top_pop_csum[,"X90th..95th.percentiles"])%>%
  "/" (quintile_shares$bottom.95) 

all_perc_people <- quintiles %>% rep(nrow(dat)) %>% matrix(ncol =length(quintiles), byrow = T) 
colnames(all_perc_people) <- colnames(quintile_shares)
all_perc_people <- cbind(people_share_csum, all_perc_people)

### ------------------------------------------------------------------------------------------------------

# visual inspection of scaled data
par(mfrow = c(3,3))
for (i in 3031:3039) {
  plot(x = all_perc_people[i,], y = scaled_nhgis[i,], type = "p", col = "orange" , main = dat[i, "NAME_E"],
       xlim = c(0,1), ylim = c(0,1),  xlab = "% of people", ylab = "% of total income", pch = 20) # scaled NHGIS data
  points(x = top_pop, y = top_pop_csum[i,], col = "blue", pch = 20) # B6 data
  points(x = all_perc_people[i,], y = bind_cols(as.data.frame(income_share_estimate_csum), quintile_shares)[i,],
         pch = 20, col = "red") # unscaled NHGIS data
}

### ------------------------------------------------------------------------------------------------------

# finally merge Lorenz curve data:
# add starting point 0% of population earn 0% of total income
# < 95%: NHGIS data
# = 95%: NHGIS data = B6 data
# > 95%: B6 data

scaled_nhgis <- as.matrix(scaled_nhgis)
scaled_nhgis[all_perc_people > 0.95] <-  999
scaled_nhgis <- cbind(scaled_nhgis[,- which(colnames(scaled_nhgis)=="bottom.100")], 0) 
all_perc_people[all_perc_people > 0.95] <- 999
all_perc_people <- cbind(all_perc_people[,- which(colnames(all_perc_people)=="bottom.100")], 0)

lorenz_income_shares <- scaled_nhgis %>%
  cbind(top_pop_csum[, c("X95th..99th.percentiles", "Top.1...99th.100th.percentiles." )]) %>%
  apply(., MARGIN = 1, sort) %>% t() 
lorenz_income_shares[lorenz_income_shares > 1] <- NA

lorenz_people_shares <- all_perc_people %>%  cbind(0.99,1) %>% apply(., MARGIN = 1, sort) %>% t() 
lorenz_people_shares[lorenz_people_shares > 1] <- NA

### ------------------------------------------------------------------------------------------------------

# visual inspection of final Lorenz curves:
par(mfrow = c(3,3))
for (i in 211:219) {
  plot(x = lorenz_people_shares[i,], y = lorenz_income_shares[i,], type = "l", col = "red" , main = dat[i, "NAME_E"],
       xlim = c(0,1), ylim = c(0,1),  xlab = "% of people", ylab = "% of total income", pch = 20)
}

### ------------------------------------------------------------------------------------------------------

# write information on population and income shares per county in two csv files
lorenz_income_shares <- lorenz_income_shares %>% as.data.frame() %>% cbind(COUNTY = dat$NAME_E,.)
lorenz_people_shares <- lorenz_people_shares %>% as.data.frame() %>% cbind(COUNTY = dat$NAME_E,.)

#write.csv(lorenz_income_shares,
  #                  "income_shares_per_county.csv")
#write.csv(lorenz_people_shares, 
 #                          "population_shares_per_county.csv")

