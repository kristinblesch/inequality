# load relevant files
dat <- read.csv("../raw_data_and_data_cleaning/merged_b4_b6_nhgis.csv")
pop <- read.csv("../raw_data_and_data_cleaning/population_shares_per_county.csv")
inc <- read.csv("../raw_data_and_data_cleaning/income_shares_per_county.csv")

# ADNJE001 is total count per county - 
# add that information to population shares/income shares in order to calculate weights for state aggregate
pop_count <- dat[, c("NAME_E", "ADNJE001", "STATE")]
pop_df <- merge(pop[,-1], pop_count, by.y = "NAME_E", by.x = "COUNTY") 
inc_df <- merge(inc[,-1], pop_count, by.y = "NAME_E", by.x = "COUNTY")

# determine sequence of desired population shares on a state level
state_pop <- c(0,0.15,seq(0.175,1,by=0.025))
new_frame <- data.frame(matrix(nrow = nrow(pop_df), ncol = length(state_pop)))

# extract the income shares at desired population shares 
for (i in 1:nrow(pop_df)) {
  new_frame[i,] <- approx(x = pop_df[i,2:24], y = inc_df[i,2:24], xout = state_pop, ties = "ordered")$y
}

new_frame$COUNTY <- pop_df$COUNTY
new_frame$STATE <- pop_df$STATE
new_frame$ADNJE001 <- pop_df$ADNJE001

# calculate weighted mean (with population count ADNJE001 as weight) for each state
state_inc <- new_frame %>% group_by(STATE) %>% 
  summarise_at(vars(colnames(new_frame)[1:length(state_pop)]), list(~weighted.mean(., w=ADNJE001))) %>% data.frame()

state_pop <- matrix(rep(state_pop, nrow(state_inc)), nrow = nrow(state_inc), byrow = T) %>% data.frame() 
state_pop <- cbind(state_inc$STATE, state_pop)
colnames(state_pop) = colnames(state_inc)

# write csv files for created Lorenz curve data on a state level
#write.csv(state_inc, file = "income_shares_per_state.csv")
#write.csv(state_pop, file = "population_shares_per_state.csv")
