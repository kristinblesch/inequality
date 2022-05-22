# simulation study for criteria datasets should meet for a comparative study 

# ------------------------------------------------------------------

# criterion (I): data granularity - from AIC detection simulation study, 
# we already figured out that we should have at least 15 data points at hand to estimate the Lorenz curves from

# criterion (II): sparsity in top incomes - using the code below, we can evaluate the increase in uncertainty 
# when there is sparse data among the top income percentiles (above the 90th income percentile)

# criterion (III): number of Lorenz curves that are necessary to reduce uncertainty in model choice
# i.e. what is a sufficient number of Lorenz curves that need to be taken into consideration for determining an optimal model

# ------------------------------------------------------------------

library(dplyr)
library(ggplot2)
set.seed(12)

# criterion (II):

granularity = 20 # fix number of data points to a number >15, we use 20 here (including starting point 0 and end point 1)
noise_level = 0.002 # add observational noise when sampling data points from the actual Lorenz curve model

# import US Ortega parameters to get "realistic" Lorenz curve parameters 
# we use all estimated US county Lorenz curves in this analysis -> N = 3056
ortega_parameters <- read.csv("ortega_parameters_county_alpha_beta.csv") %>%
  select(par_1, par_2)

# income share generation with fixed granularity, but varying number of top income shares available (above 90th percentile)
pop_gen <- function(granularity = 20, top_incomes = 0){
  unique(c(seq(0, 0.9, length.out = (granularity - top_incomes-1)), seq(0.9,1,length.out = (2+top_incomes))))
}
# example: population shares with one data point above the 90th percentile
top_inc = 1 # vary number of top income points here! 
pop <- pop_gen(granularity = granularity, top_incomes = top_inc) 
# pop <- seq(0,1,length.out=granularity) # use this line for equidistant data of granularity (=20)
# -> to create file ./est_002.csv used in est_n later on

# define Ortega function to generate income shares for the respective population shares
ortega <- function(pop_csum, theta){ 
  # alpha = theta[1], gamma = 1-theta[2]
  # 0 <= alpha, 0 <= gamma < 1 
  pop_csum^theta[1] * (1- (1- pop_csum)^(1-theta[2]))
}

# calculate income shares, ensuring no negative Lorenz curve values:
# resample noise in case the Lorenz curve value would have been negative
# resample noise in case any Lorenz curve value was > 1

inc <- matrix(0, nrow = nrow(ortega_parameters), ncol = granularity)
for (i in 1:nrow(ortega_parameters)) {
  inc_pure <- ortega(pop_csum = pop, theta = c(ortega_parameters[i,1], ortega_parameters[i,2]))
  noise <- c(0,  rnorm(granularity-2, mean = 0, sd = noise_level ),0)
  inc_noisy <- inc_pure + noise 
  while (sum((inc_noisy) < 0) > 0) { # no negative values
    new_noise <- c(0,  rnorm(granularity-2, mean = 0, sd = noise_level ),0)
    inc_noisy <-  inc_pure + new_noise
  }
  while (sum((inc_noisy) > 1) > 0) { # no values greater 1
    new_noise <- c(0,  rnorm(granularity-2, mean = 0, sd = noise_level ),0)
    inc_noisy <-  inc_pure + new_noise
  }
  inc[i,] <- inc_noisy
}

# get MLE estimation function for model detection via AIC
source("/Users/kristinblesch/Library/Mobile Documents/com~apple~CloudDocs/Paper_inequality/submission_NHB/R&R/MLE_estimation.R")
names(unlist(models))
models <-  models[which( !names(unlist(models)) %in% c( "GENERALIZED_GAMMA", "GB1"))] # exclude GG and GB1 as they have troubles to converge (same reasoning as in paper)

# estimate optimum model for each of the N=3056 Lorenz curves for a fixed number of top income shares
# repeat this estimation procedure for different numbers of top income shares (top_inc),
# hence not in a loop but done manually here for number of top income shares in [0,1,2,3,4,5,6,7,8,9]
registerDoParallel(cores=7)
getDoParName()
system.time(
  est <- foreach(i = 1:nrow(inc), .combine = 'rbind', .multicombine = F ) %dopar% {
  lapply(models, function(x){get_aic_c(model = x, inc_csum = inc[i,], pop_csum = pop)}) %>% unlist()
})

colnames(est) <- names(unlist(models))
est
#write.csv(data.frame(est), file = "./est_gen_9_002.csv") # save results in a file, here with 9 data points in top income


# do it manually for income 
gen_0 <- read.csv("./est_gen_0_002.csv")
gen_1 <- read.csv("./est_gen_1_002.csv")
gen_2 <- read.csv("./est_gen_2_002.csv")
gen_3 <- read.csv("./est_gen_3_002.csv")
gen_4 <- read.csv("./est_gen_4_002.csv")
gen_5 <- read.csv("./est_gen_5_002.csv")
gen_6 <- read.csv("./est_gen_6_002.csv")
gen_7 <- read.csv("./est_gen_7_002.csv")
gen_8 <- read.csv("./est_gen_8_002.csv")
gen_9 <- read.csv("./est_gen_9_002.csv")
gen_10 <- read.csv("./est_gen_10_002.csv")

#colnames(gen_0) <- colnames(gen_1)


sim = 10000 # repetitions
N = 20 # Lorenz curves available per draw
listed_propostions <-  list(gen_0, gen_1,  gen_2,  gen_3,  gen_4,
                            gen_5,  gen_6,  gen_7,  gen_8,  gen_9,  gen_10) # gen_0 means no top income shares, gen_1 one top income share etc.
mean_correct <- c()
sd_correct <- c()

for (n in 1:length(listed_propostions)) {
  est <- listed_propostions[[n]]
  est_min <- apply(est[,!names(est)%in%c("X")], 1,function(x){which.min(x) %>% names(unlist(models))[.]}) %>% data.frame()
  est_min$is_ortega <-  est_min[,1] == "ORTEGA" # was Ortega the model with minimum AIC?
  # how often out of the N = 20 draws do we detect the correct Lorenz curve model that generated the data (Ortega)
  # calculate the percentage of correct model detections for sim = 10.000 replicates:
  perc_correct <- replicate(sim,{sum(est_min$is_ortega[sample(1:nrow(est_min), N, replace = T)]) / N}) 
  print(perc_correct)
  mean_correct[n] <-  mean(perc_correct)
  sd_correct[n] <-  sd(perc_correct)
}
sd_correct
mean_correct

## make plot
ggplot() +
  geom_ribbon(aes(x=0:10, ymin=mean_correct-sd_correct, ymax=mean_correct+sd_correct), fill = "yellow", alpha = 0.5)+
  geom_line(aes(x = 0:10, y = mean_correct), col = "orange") +
  geom_line(aes(x = 0:10, y = 0.5), col = "darkgreen", lty = 2) +
  labs(title = "Average model detection rate for varying information density in top incomes",
       subtitle = "Fixed data granularity (20 data points), N = 20 Lorenz curves, \naverage and standard deviation across 10,000 simulation runs",
       x = "number of data points > 90th income percentile", y = "percentage of true model detection",
       tag = "")+
  ylim(0,1) + scale_x_continuous(breaks= c(seq(0,10, 2)))


# -----------------------------

# criterion (III)
# estimating the certainty of detecting the correct model if having only N Lorenz curves at hand
# sample N Lorenz curves from the N=3056 US Lorenz curves estimated 
# we can interpret this as a "success probability" of detecting the correct Ortega model for 
# a given number of observations within the top percentiles, here 0 (using file "./est_gen_0_002.csv" )


sim  = 10000
N = c(3,5,10,15,20,30,50,80,100,200,300,400,500,750,1000, 1500,2000,3000,5000)

est <- read.csv(file = "./est_gen_0_002.csv")
est_min <- apply(est[,!names(est)%in%c("X")], 1,function(x){which.min(x) %>% names(unlist(models))[.]}) %>% data.frame()
est_min$is_ortega <-  est_min[,1] == "ORTEGA" # was Ortega the model with minimum AIC? -> yes = success
mean(est_min$is_ortega)

mean_correct <- c()
sd_correct <- c()
for (n in 1:length(N)) {
  # if we draw N Lorenz curves, how often do we detect the correct model? With which certainty?
    perc_correct <- replicate(sim,{sum(est_min$is_ortega[sample(1:nrow(est_min), N[n], replace = T)]) / N[n]})
    print(perc_correct)
     mean_correct[n] <-  mean(perc_correct) # how often was the correct model detected
     sd_correct[n] <-  sd(perc_correct) # sd of correct model detection
}

# by the way: sd_correct is just the std.error of the bernoulli MLE 
# because we here have the interpretation of "success probability" of detecting the correct model 
p = mean(est_min$is_ortega)
theoretical_sd_bern <- unlist(lapply(N, function(n){sqrt((p*(1-p))/n)}))

# visualize results in a plot
ggplot() +
  geom_ribbon(aes(x=N, ymin=mean_correct-sd_correct, ymax=mean_correct+sd_correct), fill = "lightblue")+
  geom_line(aes(x = N, y = mean_correct), col = "blue") +
 # geom_hline(yintercept = mean(mean_correct), col = "red" )+
 # geom_line(aes(x = N, y =mean_correct + theoretical_sd_bern), col = "green" )+
  labs(title = "Uncertainty in true model detection for varying sample sizes: No top income data",
       subtitle = "Average model detection rate and its standard deviation across simulation runs",
       x = "Lorenz curve samples N", y = "percentage of true model detection",
       tag = "B")


# full plot : compare equidistant granularity of 20 to case where we have 0 data points within top income shares

est_e <- read.csv(file = "./est_002.csv") # equidistant data granularity = 20
est_n <- read.csv(file = "./est_gen_0_002.csv") # data granularity = 20, but no information on top income shares

sim  = 10000
N = c(3,5,10,15,20,30,50,60,70,80,85,90,95,100,200,300,400,500,750,1000, 1500,2000,3000,4000)

est_min_e <- apply(est_e[,!names(est_e)%in%c("X")], 1,function(x){which.min(x) %>% names(unlist(models))[.]}) %>% data.frame()
est_min_e$is_ortega <-  est_min_e[,1] == "ORTEGA" # was Ortega the model with minimum AIC?
mean(est_min_e$is_ortega)

mean_correct_e <- c()
sd_correct_e <- c()
for (n in 1:length(N)) {
  perc_correct <- replicate(sim,{sum(est_min_e$is_ortega[sample(1:nrow(est_min_e), N[n], replace = T)]) / N[n]})
  print(perc_correct)
  mean_correct_e[n] <-  mean(perc_correct)
  sd_correct_e[n] <-  sd(perc_correct)
}

est_min_n <- apply(est_n[,!names(est_n)%in%c("X")], 1,function(x){which.min(x) %>% names(unlist(models))[.]}) %>% data.frame()
est_min_n$is_ortega <-  est_min_n[,1] == "ORTEGA" # was Ortega the model with minimum AIC?
mean(est_min_n$is_ortega)

mean_correct_n <- c()
sd_correct_n <- c()
for (n in 1:length(N)) {
  perc_correct <- replicate(sim,{sum(est_min_n$is_ortega[sample(1:nrow(est_min_n), N[n], replace = T)]) / N[n]})
  print(perc_correct)
  mean_correct_n[n] <-  mean(perc_correct)
  sd_correct_n[n] <-  sd(perc_correct)
}

# plot with both equidistant on no top income share data
ggplot() +
  geom_ribbon(aes(x=N, ymin=mean_correct_e-sd_correct_e, ymax=mean_correct_e+sd_correct_e), fill = "lightblue", alpha = 0.5)+
  geom_line(aes(x = N, y = mean_correct_e), col = "blue") +
  geom_ribbon(aes(x=N, ymin=mean_correct_n-sd_correct_n, ymax=mean_correct_n+sd_correct_n), fill = "yellow", alpha = 0.5)+
  geom_line(aes(x = N, y = mean_correct_n), col = "orange") +
  geom_line(aes(x = N, y = 0.5), col = "darkgreen", lty = 2) +
  # geom_hline(yintercept = mean(mean_correct), col = "red" )+
  # geom_line(aes(x = N, y =mean_correct + theoretical_sd_bern), col = "green" )+
  labs(title = "Uncertainty in true model detection for varying sample sizes with observational noise",
       subtitle = "Average model detection rate and its standard deviation across simulation runs",
       x = "Lorenz curve samples N", y = "percentage of true model detection",
       tag = "")+
  ylim(0.2,0.8)
# plot with equidistant data only
ggplot() +
  geom_ribbon(aes(x=N, ymin=mean_correct_e-sd_correct_e, ymax=mean_correct_e+sd_correct_e), fill = "lightblue", alpha = 0.5)+
  geom_line(aes(x = N, y = mean_correct_e), col = "blue") +
  geom_line(aes(x = N, y = 0.5), col = "darkgreen", lty = 2) +
  labs(title = "Uncertainty in true model detection for varying sample sizes with observational noise",
       subtitle = "Average model detection rate and its standard deviation across simulation runs",
       x = "Lorenz curve samples N", y = "percentage of true model detection",
       tag = "")+
  ylim(0.2,0.9)+
  xlim(0,200)


