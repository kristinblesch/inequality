set.seed(1)
###########################################################
# implement functional forms of various Lorenz curve models
###########################################################

# ad-hoc Lorenz curve models
sarabia <- function(pop_csum, theta){ 
  # alpha = theta[1], gamma = theta[2], a = theta[3], beta = theta[4]
  # theta[1] >= 0, theta[2] >=1, 0<=theta[3]<=1, 0<= theta[4]<=1
  pop_csum^{theta[1] + theta[2]} * (1-theta[3]*(1-pop_csum)^theta[4])^theta[2]
}
wang <- function(pop_csum, theta){ 
  # delta = theta[1], alpha = theta[2], beta_1 = theta[3], beta = theta[4], nu = theta[5] 
  # 0<theta[1]<=1, theta[2] >= 0,  theta[5] >= 0, theta[2]+theta[5] >= 1, 0<theta[3]<=1, 0 <theta[4]<=1
  theta[1]*pop_csum^theta[2] * (1-(1-pop_csum)^theta[4]) + (1- theta[1]) * (1-(1-pop_csum)^theta[3])^theta[5]
}
kakwani_podder <-  function(pop_csum, theta){ 
  # beta = theta
  # theta > 0 
  pop_csum * exp(-theta * (1 - pop_csum))
}
rasche <- function(pop_csum, theta){ 
  # alpha = theta[1], beta = theta[2]
  # 0 <= theta[1], theta[2] <= 1
  (1- (1- pop_csum)^theta[1])^(1/theta[2])
}
kakwani <- function(pop_csum, theta){ 
  # a = theta[1], alpha = theta[2], beta = theta[3]
  # 0 < theta[1], 0 < theta[2] & theta[3] <= 1
  pop_csum - theta[1] * pop_csum^theta[2] * (1-pop_csum)^theta[3]
}
ortega <- function(pop_csum, theta){ 
  # alpha = theta[1], beta = theta[2]
  # 0 <= alpha, 0 < beta <= 1
  pop_csum^theta[1] * (1- (1- pop_csum)^theta[2])
}
chotikapanich <- function(pop_csum, theta){ 
  # k = theta
  # theta > 0
  (exp(theta * pop_csum) - 1) / (exp(theta) - 1)
}
abdalla_hassan <- function(pop_csum, theta){ 
  # alpha = theta[1], beta = theta[2], delta = theta[3]
  # 0 <= theta[1], 0 <= theta[2] <= theta[3] <= 1
  pop_csum^theta[1] * (1- (1- pop_csum)^theta[3] * exp(theta[2] * pop_csum))
}
rhode <- function(pop_csum, theta){ 
  # beta = theta
  # theta > 1
  pop_csum * (theta - 1) / (theta - pop_csum)
}

# Lorenz curve models based on distributions

pareto <- function(pop_csum, theta){ 
  # alpha = theta
  # alpha > 1
  1- (1- pop_csum)^(1-(1/theta))
}
lognormal <- function(pop_csum, theta){ 
  # sigma = theta
  pnorm(qnorm(pop_csum) - theta)
}
gamma_fun <- function(pop_csum, theta){ 
  # p = theta
  pgamma(q = qgamma(p = pop_csum, shape = theta), shape = theta + 1)
}
weibull <- function(pop_csum, theta){ 
  # alpha = theta
  pgamma(q = -log(1 - pop_csum), shape = 1 + 1/theta)
}
generalized_gamma <- function(pop_csum, theta){ 
  # p = theta[1], alpha = theta[2]
  # a,p > 0
  pgamma(q = qgamma(p=pop_csum, shape = theta[1] ) , shape= theta[1] + 1/theta[2])
}
dagum <- function(pop_csum, theta){ 
  # q = theta[1], a = theta[2] 
  # q > 0, a > 1
  pbeta(q = pop_csum^(1/theta[1]), shape1 = theta[1] + 1/theta[2], shape2 = 1 - 1/theta[2]) 
}
singh_maddala <- function(pop_csum, theta){ 
  # q = theta[1], a = 1/theta[2]
  # q > 1/a --> theta[1] > theta[2]
  pbeta(q = 1- (1- pop_csum)^(1/ theta[1]), shape1 = 1 + 1/(1/theta[2]), shape2 = theta[1] - 1/ (1/theta[2]))
}
gb1 <- function(pop_csum, theta){ 
  # p = theta[1], q = theta[2], a = theta[3] 
  pbeta(q = qbeta(p = pop_csum, shape1 = theta[1], shape2 = theta[2] ), shape1 = theta[1] + 1/ theta[3], shape2 =theta[2] )
}
gb2 <- function(pop_csum, theta){ 
  # p = theta[1], q = theta[2], a = 1/theta[3] 
  # q > 1/a --> theta[2] > theta[3]
  pbeta(q = qbeta(p = pop_csum, shape1 = theta[1], shape2 = theta[2] ), 
        shape1 = theta[1] + 1/ (1/theta[3]), shape2 =theta[2] - 1/ (1/theta[3]) )
}

##################################################################
# write relevant functions for parameter estimation
##################################################################

almost_zero <- 0.0001
# length of grid search
grid_length <- 10
# optimization function to get least squares estimates
optim_fit_LC <- function(county_ind, model, inc_csum, pop_csum){
  RSS_fun <- function(theta) {sum((inc_csum - model[[1]](pop_csum , theta = theta))^2, na.rm = T)}
  
  # initialize parameters for grid search and define side constraints via ui and ci, see ?contrOptim() for details
  
  if (names(model) == "KAKWANI_PODDER"){ 
    par_1 <- seq(2*almost_zero, to = 5, l = grid_length)
    grid <- matrix(par_1, nrow = length(par_1), ncol = 1) 
    ui <- 1 # theta > 0 
    ci <- almost_zero
  }
  if (names(model) == "RASCHE"){
    par_1 <- seq(2*almost_zero, to = 1-almost_zero, l = grid_length)
    par_2 <- seq(2*almost_zero, to = 1-almost_zero, l = grid_length)
    grid <- expand.grid(par_1, par_2) 
    ui <- rbind(c(1,0),c(-1,0), c(0,1), c(0,-1) ) 
    ci <- c(almost_zero,-1,almost_zero,-1)
  }
  if (names(model) == "KAKWANI"){
    par_1 <- seq(2*almost_zero, to = 5, l = grid_length)
    par_2 <- seq(2*almost_zero, to = 1-almost_zero, l = grid_length)
    par_3 <- seq(2*almost_zero, to = 1-almost_zero, l = grid_length)
    grid <- expand.grid(par_1, par_2, par_3) 
    ui <- rbind(c(1,0,0), c(0,1,0), c(0,-1,0), 
                c(0,0,1), c(0,0,-1))
    ci <- c(almost_zero, almost_zero, -1, almost_zero, -1)
  }
  if (names(model) == "ORTEGA"){
    par_1 <- seq(2*almost_zero, to = 5, l = grid_length) 
    par_2 <- seq(2*almost_zero, to = 1-almost_zero, l = grid_length) 
    grid <- expand.grid(par_1, par_2) 
    ui <- rbind(c(1,0), c(0,1), c(0,-1))
    ci <- c(0,almost_zero,-1 )
  }
  if (names(model) == "CHOTIKAPANICH"){
    par_1 <- seq(2*almost_zero, to = 5, l = grid_length) 
    grid <- matrix(par_1, nrow = length(par_1), ncol = 1) 
    ui <- 1
    ci <- almost_zero
  }
  if (names(model) == "ABDALLA_HASSAN"){ 
    par_1 <- seq(2*almost_zero, to = 5, l = 10) 
    par_2 <- seq(2*almost_zero, to = 1-almost_zero, l = 10) 
    par_3 <- seq(2*almost_zero, to = 1-almost_zero, l = 10) 
    grid <- expand.grid(par_1, par_2, par_3) %>% filter(Var2 < Var3)
    ui <- rbind(c(1,0,0), c(0,1,0), c(0,0,-1), c(0,-1,1))
    ci <- c(0,0,-1,0)
  }
  if (names(model) == "RHODE"){
    par_1 <- seq(1+ 2*almost_zero, to = 10, l = grid_length) 
    grid <- matrix(par_1, nrow = length(par_1), ncol = 1) 
    ui <- 1
    ci <- 1 + almost_zero
  }
  if (names(model) == "PARETO"){
    par_1 <- seq(1+2*almost_zero, to = 10, l = grid_length) 
    grid <- matrix(par_1, nrow = length(par_1), ncol = 1) 
    ui <- 1
    ci <- 1 + almost_zero
  }
  if (names(model) == "LOGNORMAL"){
    par_1 <- seq(2*almost_zero, to = 5, l = grid_length) 
    grid <- matrix(par_1, nrow = length(par_1), ncol = 1) 
    ui <- 1
    ci <- almost_zero
  }
  if (names(model) == "GAMMA"){ 
    par_1 <- seq(2*almost_zero, to = 5, l = grid_length) 
    grid <- matrix(par_1, nrow = length(par_1), ncol = 1) 
    ui <- 1
    ci <- almost_zero
  }
  if (names(model) == "WEIBULL"){
    par_1 <- seq(2*almost_zero, to = 5, l = grid_length) 
    grid <- matrix(par_1, nrow = length(par_1), ncol = 1) 
    ui <- 1
    ci <- almost_zero
  }
  if (names(model) == "GENERALIZED_GAMMA"){
    par_1 <- seq(2*almost_zero, to = 5, l = grid_length) 
    par_2 <- seq(2*almost_zero, to = 5, l = grid_length) 
    grid <- expand.grid(par_1, par_2) 
    ui <- rbind(c(1,0), c(0,1))
    ci <- c(almost_zero, almost_zero)
  }
  if (names(model) == "DAGUM"){
    par_1 <- seq(2*almost_zero, to = 5, l = grid_length)
    par_2 <- seq(1+2*almost_zero, to = 5, l = grid_length) 
    grid <- expand.grid(par_1, par_2) 
    ui <- rbind(c(1,0), c(0,1))
    ci <- c(almost_zero, 1)
  }
  if (names(model) == "SINGH_MADDALA"){ 
    par_1 <- seq(2*almost_zero, to = 5, l = grid_length) 
    par_2 <- seq(2*almost_zero, to = 5, l = grid_length)
    grid <- expand.grid(par_1, par_2) %>% filter(Var1 > Var2)
    ui <- rbind(c(1,0), c(0,1), c(1, -1))
    ci <- c(almost_zero, almost_zero, almost_zero)
  }
  if (names(model) == "GB1"){ 
    par_1 <- seq(2*almost_zero, to = 5, l = grid_length) 
    par_2 <- seq(2*almost_zero, to = 5, l = grid_length) 
    par_3 <- seq(2*almost_zero, to = 5, l = grid_length) 
    grid <- expand.grid(par_1, par_2, par_3) 
    ui <- rbind(c(1,0,0), c(0,1,0), c(0,0,1))
    ci <- c(almost_zero, almost_zero, almost_zero)
  }
  if (names(model) == "GB2"){ 
    par_1 <- seq(2*almost_zero, to = 5, l = grid_length) 
    par_2 <- seq(2*almost_zero, to = 5, l = grid_length) 
    par_3 <- seq(2*almost_zero, to = 5, l = grid_length) 
    grid <- expand.grid(par_1, par_2, par_3) %>% filter(Var2 > Var3)
    ui <- rbind(c(1,0,0), c(0,1,0), c(0,0,1), c(0,1,-1))
    ci <- c(almost_zero, almost_zero, almost_zero, almost_zero)
  }
  if(names(model) == "SARABIA"){
    par_1 <- seq(2*almost_zero, to = 5, l = grid_length) 
    par_2 <- seq(1+almost_zero, to =5 , l = grid_length) 
    par_3 <- seq(2*almost_zero, to = 1-almost_zero, l = grid_length) 
    par_4 <- seq(2*almost_zero, to = 1 - almost_zero, l = grid_length) 
    grid <- expand.grid(par_1, par_2, par_3, par_4) 
    ui <- rbind(c(1,0,0,0), c(0,1,0,0), c(0,0,1,0), c(0,0,-1,0),c(0,0,0,1), c(0,0,0,-1))
    ci <- c(almost_zero,1, almost_zero, -1, almost_zero, -1)
  }
  if (names(model) == "WANG"){
    par_1 <- seq(2*almost_zero, to = 1-almost_zero, l = grid_length) 
    par_2 <- seq(2*almost_zero, to = 5, l = grid_length) 
    par_3 <- seq(2*almost_zero, to = 1-almost_zero, l = grid_length)
    par_4 <- seq(2*almost_zero, to = 1-almost_zero, l = grid_length)
    par_5 <- seq(2*almost_zero, to = 5, l = grid_length) 
    grid <- expand.grid(par_1, par_2, par_3, par_4, par_5) %>% filter(Var2+Var5 > 1)
    ui <- rbind(c(0,1,0,0,0),c(0,0,0,0,1), c(0,1,0,0,1), c(0,0,1,0,0), c(0,0,-1,0,0),
                c(0,0,0,1,0), c(0,0,0,-1,0), c(1,0,0,0,0), c(-1,0,0,0,0))
    ci <- c(0, 0, 1, almost_zero, -1, almost_zero, -1, almost_zero, -1)
  }
  # initial values determined by grid search:
  theta_initial <- apply(grid, 1, RSS_fun) %>% which.min() %>% grid[.,] %>% unlist()
  # optimization if single-parameter model:
  if (length(theta_initial) == 1){
    mod <- optim(theta_initial, fn = function(theta) {sum((inc_csum - model[[1]](pop_csum , theta = theta))^2, na.rm = T)}, 
                 method = "L-BFGS-B", lower = ci, upper = Inf)
  } 
  # optimization if multi-parameter model:
  else {
    mod <- constrOptim(theta = theta_initial, f = function(theta) {sum((inc_csum - model[[1]](pop_csum , theta = theta))^2, na.rm = T)},
                       method = "Nelder-Mead", ui = ui, ci = ci, hessian = F)}
  # create data frame for output of interest:
  df <- data.frame(county_index = county_ind, form = names(model), 
                   RSS = mod$value, R_sq =1- (mod$value)/(sum((inc_csum - mean(inc_csum, na.rm = T))^2, na.rm = T)))#,
  df$form <-  as.character(df$form)
  for (i in 1:length(theta_initial)) { # add parameter values to output data frame
    df <- cbind(df, mod$par[i])
    colnames(df)[ncol(df)] <- paste("par", i, sep = "_")
  }
  if(names(model) == "SINGH_MADDALA"){ # need to invert output because of definition of a = 1/theta[2] for side constraint
    df$par_2 <-  1 / df$par_2
  }
  if(names(model) == "GB2"){  # need to invert output because of definition of a = 1/theta[3] for side constraint
    df$par_3 <-  1 / df$par_3
  }
  return(df)
}
# list models we want to estimate parameters for
models <- list(c(KAKWANI_PODDER = kakwani_podder),
               c(RASCHE = rasche),
               c(KAKWANI = kakwani),
               c(ORTEGA = ortega),
               c(CHOTIKAPANICH = chotikapanich),
               c(ABDALLA_HASSAN = abdalla_hassan),
               c(RHODE = rhode),
               c(PARETO = pareto),
               c(LOGNORMAL = lognormal),
               c(GAMMA = gamma_fun),
               c(WEIBULL = weibull),
               c(GENERALIZED_GAMMA = generalized_gamma),
               c(DAGUM = dagum),
               c(SINGH_MADDALA = singh_maddala),
               c(GB1 = gb1),
               c(GB2 = gb2),
               c(SARABIA = sarabia),
               c(WANG = wang)
)
###########################################################
# set working directory to empirical data set for US counties and load relevant libraries
###########################################################

#setwd("/Users/kristinblesch/Library/Mobile Documents/com~apple~CloudDocs/USA/Master_Thesis/Data")
library(dplyr)
library(foreach)
library(doParallel)

pop_df <- read.csv("../raw_data_and_data_cleaning/population_shares_per_county.csv") 
inc_df <- read.csv("../raw_data_and_data_cleaning/income_shares_per_county.csv")

# remove rows with duplicated values: 
rm_duplicats <- pop_df %>% select(-c("X", "COUNTY")) %>%apply(., 1, function(x) !any(duplicated(na.omit(c(x)))==TRUE)) 
pop_df <- pop_df %>% filter(rm_duplicats)
inc_df <- inc_df %>% filter(rm_duplicats)

# put empirical data in adequate format for out optimization function optim_MLE_LC():
pop <- matrix(unlist(pop_df[,!names(pop_df) %in% c("X", "COUNTY")]), nrow = nrow(pop_df)) 
inc <- matrix(unlist(inc_df[,!names(inc_df) %in% c("X", "COUNTY")]), nrow = nrow(inc_df)) 

###########################################################
# set up parallel computing environment
###########################################################
cl <- makeCluster(24)
registerDoParallel(cl)
result <- foreach (i=1:nrow(pop), .combine = 'rbind', .packages = "dplyr") %dopar% 
  {lapply(models, function(x){optim_fit_LC(county_ind = i,
                                           model = x,
                                           inc_csum = inc[i,],
                                           pop_csum = pop[i,])}) %>% Reduce(bind_rows, .)}
stopCluster(cl)
# match county names
result <- result %>% mutate(COUNTY = pop_df$COUNTY[county_index])

#result <- data.frame(matrix(nrow = 15*4002, ncol = 10))
save(result, file = 'NLS_output.RData') # save results

