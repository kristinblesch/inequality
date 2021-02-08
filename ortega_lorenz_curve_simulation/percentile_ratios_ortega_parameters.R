library(dplyr)
library(RVAideMemoire)

###############
# Evaluate relationship between Ortega parameters and 95/50, 50/10 ratio
# simulate Ortega Lorenz curves
###############

ortega <- function(pop_csum, theta){ 
  # alpha = theta[1], beta = theta[2]
  # 0 <= alpha, 0 < beta <= 1
  pop_csum^theta[1] * (1- (1- pop_csum)^theta[2])
}

# parameter range
theta_1 <- seq(0.01,1,by = 0.01)
theta_2 <- seq(0.01,1,by = 0.01)
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

# with transformation of beta for interpretability: theta_2 = beta --> gamma = -1*beta
pcor(x = unlist(o_95/o_50), y = th[,1], z = list(-th[,2], o_50/o_10))
pcor(x = unlist(o_95/o_50), y = -th[,2], z = list(th[,1], o_50/o_10))
pcor(x = unlist(o_50/o_10), y = th[,1], z = list(-th[,2], o_95/o_50))
pcor(x = unlist(o_50/o_10), y = -th[,2], z = list(th[,1], o_95/o_50))

# --> the higher theta_1 (alpha), the higher the 50/10 ratio (bottom inequality)
# --> the higher - theta_2 (gamma), the higher the 95/50 ratio (top inequality)

# Note of caution: 
# empirical range of Ortega parameters on US county-level: 
# ortega 1: 0.12-1.23, ortega 2: 0.07-0.7 
# --> correlational dependency gets distorted when simluated Ortega Lorenz curves are in that range 
