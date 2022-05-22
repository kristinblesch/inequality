# calculate empirical gini per county

pop <- read.csv("../raw_data_and_data_cleaning/population_shares_per_county.csv")
inc <- read.csv("../raw_data_and_data_cleaning/income_shares_per_county.csv")

area <- c()
empirical_gini <- c()
# see https://towardsdatascience.com/clearly-explained-gini-coefficient-and-lorenz-curve-fe6f5dcdc07
for (n in 1:nrow(pop)) {
  for (i in 4:length(pop[n,])) {
    fraction_pop <- pop[n, i] - pop[n, i-1]
    prev_cum_inc <- inc[n, i-1]
    cum_inc <- inc[n, i]
    area[i] <- (cum_inc + prev_cum_inc)*0.5*fraction_pop
  }
  area_B <- sum(area, na.rm = T)
  area_A <- 0.5-area_B
  empirical_gini[n] <- area_A/(area_A + area_B)
}

res <- data.frame(county=pop[,"COUNTY"], empirical_gini)
#write.csv(res, file = "/Users/kristinblesch/Library/Mobile Documents/com~apple~CloudDocs/Paper_inequality/Data/empirical_gini.csv" )
