# Calculate the Gini using the Lorenz curve model, integrate

model_gini_init <- function(modelname, u){
  theta =1 
  formula = function(u, theta){0.5*u*theta}
  helper = function(u){formula(u=u, theta = theta)}
  1- 2 * integrate(helper, lower = 0, upper = 1)$value
}
model_gini_init()

## Lorenz curve model functions
#####

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

#####
MLE <- read.csv("../estimation_procedure/MLE_output_county.csv")
MLE_ex <-  MLE[2:5,]
model_gini <- function(MLE_data){
  if (MLE_data$form == "KAKWANI_PODDER"){ 
    model= function(u){kakwani_podder(pop_csum = u, theta = MLE_data$par_1)}
    }
  if (MLE_data$form == "RASCHE"){
    model = function(u){rasche(pop_csum = u, theta = c(MLE_data$par_1, MLE_data$par_2))}
  }
  if (MLE_data$form == "ORTEGA"){
    model = function(u){ortega(pop_csum = u, theta =c(MLE_data$par_1, MLE_data$par_2))}
  }
  if (MLE_data$form == "CHOTIKAPANICH"){
    model = function(u){chotikapanich(pop_csum = u, theta = MLE_data$par_1)}
  }
  if (MLE_data$form == "ABDALLA_HASSAN"){
    model = function(u){abdalla_hassan(pop_csum = u, theta =c(MLE_data$par_1, MLE_data$par_2, MLE_data$par_3))}
  }
  if (MLE_data$form == "RHODE"){
    model = function(u){rhode(pop_csum = u, theta = MLE_data$par_1)}
  }
  if (MLE_data$form == "PARETO"){
    model = function(u){pareto(pop_csum = u, theta = MLE_data$par_1)}
  }
  if (MLE_data$form == "SARABIA"){
    model = function(u){sarabia(pop_csum = u, theta = c(MLE_data$par_1, MLE_data$par_2, MLE_data$par_3, MLE_data$par_4))}
  }
  if (MLE_data$form == "WANG"){
    model = function(u){wang(pop_csum = u, theta = c(MLE_data$par_1, MLE_data$par_2, MLE_data$par_3, MLE_data$par_4, MLE_data$par_5))}
  }
  if (MLE_data$form == "LOGNORMAL"){
    model = function(u){lognormal(pop_csum = u, theta = MLE_data$par_1)}
  }
  if (MLE_data$form == "GAMMA"){
    model = function(u){gamma_fun(pop_csum = u, theta = MLE_data$par_1)}
  }
  if (MLE_data$form == "WEIBULL"){
    model = function(u){weibull(pop_csum = u, theta = MLE_data$par_1)}
  }
  if (MLE_data$form == "GENERALIZED_GAMMA"){
    model = function(u){generalized_gamma(pop_csum = u, theta = c(MLE_data$par_1, MLE_data$par_2))}
  }
  if (MLE_data$form == "DAGUM"){
    model = function(u){dagum(pop_csum = u, theta = c(MLE_data$par_1, MLE_data$par_2))}
  }
  if (MLE_data$form == "SINGH_MADDALA"){
    model = function(u){singh_maddala(pop_csum = u, theta = c(MLE_data$par_1, MLE_data$par_2))}
  }
  if (MLE_data$form == "GB1"){
    model = function(u){gb1(pop_csum = u, theta = c(MLE_data$par_1, MLE_data$par_2, MLE_data$par_3))}
  }
  if (MLE_data$form == "GB2"){
    model = function(u){gb2(pop_csum = u, theta = c(MLE_data$par_1, MLE_data$par_2, MLE_data$par_3))}
  }
  gini <- 1- 2 * integrate(model, lower = 0, upper = 1, stop.on.error = FALSE)$value
#  gini <- integrate(model, lower = 0, upper = 1, stop.on.error = FALSE)$message
  return(data.frame("model" = MLE_data$form, "COUNTY" = MLE_data$COUNTY, "model_gini" = gini))
}

##########
library(dplyr)
MLE_new = MLE %>% filter(form != "GB2") %>% filter(form != "SINGH_MADDALA") %>%
  filter(form != "GB1") %>% filter(form != "GENERALIZED_GAMMA")
# integration not working for GB2 and SINGH_MADDALA
# GB1 and Generalized Gamma excluded anyways because of instabilities

res <- list()
for (i in 1:nrow(MLE_new)) {
  MLE_dat <-  MLE_new[i,]
  res[[i]] <- model_gini(MLE_data = MLE_dat)
}
model_ginis <- do.call(rbind, res)
which(model_ginis$model_gini < 0)
#model_ginis
#model_gini(MLE_data = MLE_new[34325,])

# add empirical Gini
############
empirical_ginis <- read.csv("../approximating_empirical_Gini/empirical_gini.csv")

# merge two data sets
merged_ginis <- merge(x = model_ginis, y = empirical_ginis, by.x = "COUNTY", by.y = "county")

# calculate average divergence 

merged_ginis_means <- merged_ginis %>% mutate(difference = abs(model_gini - empirical_gini)) %>% group_by(model) %>% summarise(avrg_deviation = mean(difference))
num_parameters <- read.csv("../raw_data_and_data_cleaning/models_parameters.csv", sep = ";")

tab <- merge(num_parameters, merged_ginis_means, by.x = "model", by.y = "model" )

library(ggplot2)
library(ggrepel)
plt <- ggplot(tab, aes(x= Parameters, y= avrg_deviation, label=model))+
  geom_point() + ggtitle("Average deviation between Gini implied by Lorenz curve model and empirical Gini \n across N = 3056 US counties versus Lorenz curve model parameters")
plt + 
  geom_label_repel(aes(label = model),
                   box.padding   = 0.35, 
                   point.padding = 0.1,
                   segment.color = 'grey50') +
  theme_classic()

### try generating a boxplot

gini_deviations <- merged_ginis %>% mutate(difference = model_gini - empirical_gini) %>% group_by(model) 
#gini_deviations <- gini_deviations %>% filter(model %in% c("ORTEGA", "WANG"))
ggplot(gini_deviations, aes(x=difference, y=model)) + 
  geom_boxplot() + xlab("deviation from empirical Gini") + ggtitle("Deviation between model implied Gini and empirical Gini\nacross N = 3056 US counties")

# raindrop plot
library(ggdist)
library(tidyquant)
library(tidyverse)

# add num par to models
##
dat <- merge(gini_deviations,num_parameters, by.x = "model", by.y = "model" )
# reorer models 
ggplot(dat, aes(x=difference, y=reorder(model, -Parameters), fill = factor(Parameters))) +
  
  ggdist::stat_halfeye(
    ## custom bandwidth
    adjust = .5, 
    ## adjust height
    width = .6, 
    ## move geom to the right
    justification = -.2, 
    ## remove slab interval
    .width = 0, 
    point_colour = NA
  ) + 
  geom_boxplot(
    width = .12, 
    ## remove outliers
    outlier.color = NA ## `outlier.shape = NA` works as well
  ) +
  
  xlim(c(-0.15,0.05))+
  geom_vline(xintercept =0, linetype ="dotted")+
  xlab("deviation from empirical Gini") + 
  ylab("Model") + 
  ggtitle("Deviation between model implied Gini and empirical Gini\nacross N = 3056 US counties (outliers removed)")


