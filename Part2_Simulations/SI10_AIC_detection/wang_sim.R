### AIC simulation study: WANG income share generation ### 

######################################
# load relevant libraries and set seed
######################################

set.seed(1)
library(dplyr)
library(foreach)
library(doParallel)

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
# log-likelihood:
ll <- function(th, pi, qi, model){ 
  l_pi <- model[[1]](pop_csum = pi[-1], theta = th[-1])
  l_pi_min1 <- model[[1]](pop_csum = pi, theta = th[-1])[1:length(l_pi)]
  lgamma(th[1]) + sum((th[1] * (l_pi - l_pi_min1) - 1) * log(qi), na.rm = T) - sum(lgamma(th[1] * (l_pi - l_pi_min1)), na.rm = T)}
# lenght for grid search
grid_length <- 5

# optimization function to get AIC values
# basically the same optimization function as with maximum likelihood estimation, but returns AIC values only

get_aic_c <- function(model, inc_csum, pop_csum){
  options(warn=0) # turn on warning messages again, warning messages are suppressed for the models GB1, GB2
  # and generalized gamma as they arise due to the small sample sizes for these models
  qi <- inc_csum[-1] - inc_csum[1:length(inc_csum[-1])] # for the loglikelihood we need differences in income shares
  lambda <- seq(100, to = 1000, l = grid_length) # initialize lambda parameter for grid search
  ll_grid <- function(theta){ll(th = theta, pi = pop_csum, qi = qi, model = model)} # help function for grid search
  
  # initialize parameters for grid search and define side constraints via ui and ci, see ?contrOptim() for details
  
  if (names(model) == "KAKWANI_PODDER"){ 
    par_1 <- seq(2*almost_zero, to = 5, l = grid_length) 
    grid <- expand.grid(lambda, par_1) 
    ui <- c(0,1) 
    ci <- almost_zero
  }
  if (names(model) == "RASCHE"){
    par_1 <- seq(2*almost_zero, to = 1-almost_zero, l = grid_length) 
    par_2 <- seq(2*almost_zero, to = 1-almost_zero, l = grid_length) 
    grid <- expand.grid(lambda, par_1, par_2) 
    ui <- rbind(c(0,1,0),c(0,-1,0), c(0,0,1), c(0,0,-1) ) 
    ci <- c(almost_zero,-1,almost_zero,-1)
  }
  if (names(model) == "ORTEGA"){
    par_1 <- seq(2*almost_zero, to = 5, l = grid_length) 
    par_2 <- seq(2*almost_zero, to = 1-almost_zero, l = grid_length) 
    grid <- expand.grid(lambda, par_1, par_2) 
    ui <- rbind(c(0,1,0), c(0,0,1), c(0,0,-1))
    ci <- c(0,almost_zero,-1 )
  }
  if (names(model) == "CHOTIKAPANICH"){
    par_1 <- seq(2*almost_zero, to = 5, l = grid_length)
    grid <- expand.grid(lambda, par_1) 
    ui <- c(0,1)
    ci <- almost_zero
  }
  if (names(model) == "ABDALLA_HASSAN"){ 
    par_1 <- seq(2*almost_zero, to = 5, l = grid_length) 
    par_2 <- seq(2*almost_zero, to = 1-almost_zero, l = grid_length) 
    par_3 <- seq(2*almost_zero, to = 1-almost_zero, l = grid_length) 
    grid <- expand.grid(lambda, par_1, par_2, par_3) %>% filter(Var3 < Var4)
    ui <- rbind(c(0,1,0,0), c(0,0,1,0), c(0,0,0,-1), c(0,0,-1,1))
    ci <- c(0,0,-1,0)
  }
  if (names(model) == "RHODE"){
    par_1 <- seq(1+ 2*almost_zero, to = 10, l = grid_length) 
    grid <- expand.grid(lambda, par_1) 
    ui <- c(0,1)
    ci <- 1 + almost_zero
  }
  if (names(model) == "PARETO"){
    par_1 <- seq(1+2*almost_zero, to = 10, l = grid_length) 
    grid <- expand.grid(lambda, par_1) 
    ui <- c(0,1)
    ci <- 1 + almost_zero
  }
  if(names(model) == "SARABIA"){
    par_1 <- seq(2*almost_zero, to = 5, l = grid_length) 
    par_2 <- seq(1+almost_zero, to =5 , l = grid_length) 
    par_3 <- seq(2*almost_zero, to = 1-almost_zero, l = grid_length) 
    par_4 <- seq(2*almost_zero, to = 1 - almost_zero, l = grid_length) 
    grid <- expand.grid(lambda, par_1, par_2, par_3, par_4) 
    ui <- rbind(c(0,1,0,0,0), c(0,0,1,0,0), c(0,0,0,1,0), c(0,0,0,-1,0),c(0,0,0,0,1), c(0,0,0,0,-1))
    ci <- c(almost_zero,1, almost_zero, -1, almost_zero, -1)
    
  }
  if (names(model) == "WANG"){
    par_1 <- seq(2*almost_zero, to = 1-almost_zero, l = grid_length) 
    par_2 <- seq(2*almost_zero, to = 5, l = grid_length) 
    par_3 <- seq(2*almost_zero, to = 1-almost_zero, l = grid_length) 
    par_4 <- seq(2*almost_zero, to = 1-almost_zero, l = grid_length) 
    par_5 <- seq(2*almost_zero, to = 5, l = grid_length) 
    grid <- expand.grid(lambda, par_1, par_2, par_3, par_4, par_5) %>% filter(Var3 + Var6 >= 1)
    ui <- rbind(c(0,0,1,0,0,0),c(0,0,0,0,0,1), c(0,0,1,0,0,1), c(0,0,0,1,0,0), c(0,0,0,-1,0,0),
                c(0,0,0,0,1,0), c(0,0,0,0,-1,0), c(0,1,0,0,0,0), c(0,-1,0,0,0,0))
    ci <- c(0, 0, 1, almost_zero, -1, almost_zero, -1, almost_zero, -1)
  }
  if (names(model) == "LOGNORMAL"){
    par_1 <- seq(2*almost_zero, to = 5, l = grid_length) 
    grid <- expand.grid(lambda, par_1) 
    ui <- c(0,1)
    ci <- almost_zero
  }
  if (names(model) == "GAMMA"){ 
    par_1 <- seq(2*almost_zero, to = 5, l = grid_length) 
    grid <- expand.grid(lambda, par_1) 
    ui <- c(0,1)
    ci <- almost_zero
  }
  if (names(model) == "WEIBULL"){
    par_1 <- seq(2*almost_zero, to = 5, l = grid_length) 
    grid <- expand.grid(lambda, par_1) 
    ui <- c(0,1)
    ci <- almost_zero
  }
  if (names(model) == "GENERALIZED_GAMMA"){ 
    par_1 <- seq(2*almost_zero, to = 5, l = grid_length) 
    par_2 <- seq(2*almost_zero, to = 5, l = grid_length) 
    grid <- expand.grid(lambda, par_1, par_2) 
    ui <- rbind(c(0,1,0), c(0,0,1))
    ci <- c(almost_zero, almost_zero)
    options(warn=-1)
  }
  if (names(model) == "DAGUM"){
    par_1 <- seq(2*almost_zero, to = 5, l = grid_length) 
    par_2 <- seq(1+2*almost_zero, to = 5, l = grid_length) 
    grid <- expand.grid(lambda, par_1, par_2) 
    ui <- rbind(c(0,1,0), c(0,0,1))
    ci <- c(almost_zero, 1)
  }
  if (names(model) == "SINGH_MADDALA"){ 
    par_1 <- seq(2*almost_zero, to = 5, l = grid_length) 
    par_2 <- seq(2*almost_zero, to = 5, l = grid_length) 
    grid <- expand.grid(lambda, par_1, par_2) %>% filter(Var2 > Var3)
    ui <- rbind(c(0,1,0), c(0,0,1), c(0,1, -1))
    ci <- c(almost_zero, almost_zero, almost_zero)
  }
  if (names(model) == "GB1"){ 
    par_1 <- seq(2*almost_zero, to = 5, l = grid_length) 
    par_2 <- seq(2*almost_zero, to = 5, l = grid_length) 
    par_3 <- seq(2*almost_zero, to = 5, l = grid_length)
    grid <- expand.grid(lambda, par_1, par_2, par_3) 
    ui <- rbind(c(0,1,0,0), c(0,0,1,0), c(0,0,0,1))
    ci <- c(almost_zero, almost_zero, almost_zero)
    options(warn=-1)
  }
  if (names(model) == "GB2"){ 
    par_1 <- seq(2*almost_zero, to = 5, l = grid_length) 
    par_2 <- seq(2*almost_zero, to = 5, l = grid_length) 
    par_3 <- seq(2*almost_zero, to = 5, l = grid_length) 
    grid <- expand.grid(lambda, par_1, par_2, par_3) %>% filter(Var3 > Var4)
    ui <- rbind(c(0,1,0,0), c(0,0,1,0), c(0,0,0,1), c(0,0,1,-1))
    ci <- c(almost_zero, almost_zero, almost_zero, almost_zero)
    options(warn=-1)
  }
  # initial values determined by grid search:
  theta_initial <- apply(grid, 1, ll_grid) %>% which.max() %>% grid[.,] %>% unlist() 
  # actual optimization function, extract model parameters and value of loglikelihood
  mod <- constrOptim(theta =theta_initial, 
                     f = function(theta){ll(th = theta, pi = pop_csum, qi = qi, model = model)},
                     control = list(fnscale = -1),method = "Nelder-Mead", 
                     ui = ui, ci = ci, hessian = F)[1:2]
  n <- length(pop_csum[!is.na(pop_csum)])
  p <- length(mod$par)
  AIC_c =  -2* mod$value + 2 * p + 2*p*(p+1)/(n-p+1)
  return(AIC_c)
}
# function to generate cumulative income shares
generate_income <- function(pop, model){ # chosen parameters: median values of estimates across US counties
  if (names(model) == "ORTEGA"){inc <- apply(pop, 2, function(x) ortega(pop_csum = x, theta = c(0.627,0.477)))}
  if (names(model) == "GB2"){inc <- apply(pop, 2, function(x) gb2(pop_csum = x, theta = c(0.221,0.379,1/5.29)))} #theta[3]=1/a
  if (names(model) == "WANG"){inc <- apply(pop, 2, function(x) wang(pop_csum = x, theta = c(0.802,0.769,1,0.675,2.15)))}
  return(inc)
}
# function to generate cumulative population shares
generate_pop <- function(data_points, simulate = sim){matrix(runif(data_points*sim, min = 0 + almost_zero, max = 1-almost_zero), nrow = sim, ncol = data_points) %>%
    apply(1, function(x) sort(x))%>% t()%>% cbind(0, . ,1)}

# list models we want to estimate parameters for
models <- list(c(KAKWANI_PODDER = kakwani_podder),
               c(RASCHE = rasche),
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

sim = 1000 # simulation runs
data_points <-  c(4:25,30,40,50,75,100,125,150,175,200) # sample sizes

# generate population shares and income shares
# simulation for Wang being the true income share generating model

gen <- foreach(d = data_points, .combine = 'list', .multicombine = T) %do% { 
  pop <- generate_pop(d)
  inc <- generate_income(pop= pop, model = list(WANG = wang))
  list(pop, inc)
  }

popi <- lapply(1:length(data_points), function(x){gen[[x]][[1]]}) # list with data frames of cumulative population shares 
inci <- lapply(1:length(data_points), function(x){gen[[x]][[2]]}) # list with data frames of cumulative income shares 

###########################################################
# set up parallel computing environment 
###########################################################

cl <- makeCluster(24) 
registerDoParallel(cl) 
system.time(chosen <- foreach (n = 1:length(data_points), .combine = 'rbind', .multicombine = T)%:% # loop over sample sizes
              foreach( i=1:sim, .combine = 'rbind', .packages = "dplyr")%dopar% { # loop over simulated shares per sample size
                selected <- lapply(models, function(x){get_aic_c(model= x,pop_csum = popi[[n]][i,], inc_csum = inci[[n]][i,])}) %>% 
                  which.min() %>% models[[.]] %>% names() %>% c(data_points[n])
              })
stopCluster(cl)

#chosen <- data.frame(matrix(nrow = sim*length(data_points), ncol = 2)) # for storage
save(chosen, file = 'wang_output.RData') # save results

