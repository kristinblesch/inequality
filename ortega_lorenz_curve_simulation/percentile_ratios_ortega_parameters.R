library(dplyr)
library(RVAideMemoire)

###############
# Evaluate relationship between Ortega parameters and 95/50, 50/10 ratio
# simulate Ortega Lorenz curves
###############

ortega <- function(pop_csum, theta){ 
  # alpha = theta[1], gamma = 1-theta[2]
  # 0 <= alpha, 0 <= gamma < 1 
  pop_csum^theta[1] * (1- (1- pop_csum)^(1-theta[2]))
}

# parameter range
theta_1 <- seq(0.01,1,by = 0.01)
theta_2 <- seq(0,0.99,by = 0.01)
th <- expand.grid(theta_1, theta_2)
# relevant percentiles
o_95 <- ortega(0.95, theta = th[1:nrow(th),])
o_50 <- ortega(0.50, theta = th[1:nrow(th),])
o_10 <- ortega(0.10, theta = th[1:nrow(th),])

# calculate partial correlations

# top ratio/bottom ratio
pcor(x = unlist(o_95/o_50), y = th[,1], z = list(th[,2], o_50/o_10))
pcor(x = unlist(o_95/o_50), y = th[,2], z = list(th[,1], o_50/o_10))
pcor(x = unlist(o_50/o_10), y = th[,1], z = list(th[,2], o_95/o_50))
pcor(x = unlist(o_50/o_10), y = th[,2], z = list(th[,1], o_95/o_50))


# --> the higher theta_1 (alpha), the higher the 50/10 ratio (bottom inequality)
# --> the higher - theta_2 (gamma), the higher the 95/50 ratio (top inequality)

# Note of caution: 
# empirical range of Ortega parameters on US county-level: 
# ortega 1: 0.12-1.23, ortega 2: 0.3-0.93
# --> correlational dependency gets distorted when simluated Ortega Lorenz curves are in that range 

# from the visual inspection of Lorenz curves, we can see that Ortega gamma rather
# affects the *very* top of the distribution
# we suggest therefore differing 'top' and 'bottom' not at the median, but at a higher percentile
# below we explore dependency structures for gamma and alpha with different percentile ratios

# --> we find the 90th percentile a suitable border value,
# --> we find that 99/90 relates to gamma and 90/10 ratios to alpha 
# (this result is also robust against the parameter range)

# parameter range
theta_1 <- seq(0.01,1,by = 0.01)
theta_2 <- seq(0,0.99,by = 0.01)
th <- expand.grid(theta_1, theta_2)
# relevant percentiles
top <- ortega(0.99, theta = th[1:nrow(th),]) # 0.99 = 99th percentile
border_value <- ortega(0.90, theta = th[1:nrow(th),]) # 0.90 = 90th percentile
bottom <- ortega(0.10, theta = th[1:nrow(th),]) # 0.10 = 10th percentile

# calculate partial correlations

# top ratio/bottom ratio
# with transformation of beta for interpretability: theta_2 = beta --> gamma = -1*beta
pcor(x = unlist(top/border_value), y = th[,1], z = list(th[,2], border_value/bottom))
pcor(x = unlist(top/border_value), y = th[,2], z = list(th[,1], border_value/bottom))
pcor(x = unlist(border_value/bottom), y = th[,1], z = list(th[,2], top/border_value))
pcor(x = unlist(border_value/bottom), y = th[,2], z = list(th[,1], top/border_value))
