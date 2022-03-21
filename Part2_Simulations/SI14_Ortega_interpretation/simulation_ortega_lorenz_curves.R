# Code to simulate Ortega Lorenz curves
# generates plots that show how Ortega Lorenz curves are affected by changes in their parameters

library('colorspace')
library('NMOF')
library('ggplot2')

# Ortega Lorenz curve model
# Ortega parameter 1: alpha = theta[1]
# Ortega parameter 2: beta = theta[2] --> for ease of interpretation, 
# we use the transformation gamma = -beta = -theta[2]

ortega <- function(pop_csum, theta){ 
  # alpha = theta[1], gamma = 1-theta[2]
  # 0 <= alpha, 0 <= gamma < 1 
  pop_csum^theta[1] * (1- (1- pop_csum)^(1-theta[2]))
}

############################
# Varying one Ortega parameter while keeping the other one fixed
# How is the shape of the Lorenz curve affected by this? 
# Typical values for both Ortega 1 and 2 range around 0.5/0.6 in our empirical estimation,
# we therefore fix the other Ortega parameter at this level for realistic comparison
############################

# varying Ortega parameter 2
x_values <- sort(c(0,1,runif(1000, min = 0, max = 1)), decreasing = F)
ortega_1 <- 0.5
ortega_2 <- c(seq(0.01, 0.99,by=0.05)) # side constraint: -1 <= gamma < 0 
col <- diverge_hcl(length(ortega_2))
plot(x = x_values, ortega(x_values, theta = c(ortega_1[1], ortega_2[1])), xlim = c(0,1), ylim = c(0,1), type = "l",
     col =col[1], main = expression(paste("Ortega Lorenz curve: Variation in ", gamma, ", ", alpha, " fixed at 0.5")),
     xlab = "cumultative share of population", ylab = "cumulative share of income")
lines(x = x_values, y = x_values)
lines(x = x_values, y = 1-x_values, lty = 2)
for (i in 2:length(ortega_2)) {
  lines(x = x_values, ortega(x_values, theta = c(ortega_1[1], ortega_2[i])), col = col[i])
}
legend("topleft", col = c(col[1], col[length(ortega_2)]), lty = 1, 
       legend = c("low gamma", "high gamma"))
# we can see that there is a disproportionate change on the right side of the distribution (top incomes)

# varying Ortega parameter 1
ortega_1 <- c(seq(0.01, 1.5,by=0.15)) # side constraint: 0 <= alpha
# sidenote: in our empirical estimates (US county-level) we know alpha values typically are around 0.5 to 1
ortega_2 <- 0.5
col <- diverge_hcl(length(ortega_1))
plot(x = x_values, ortega(x_values, theta = c(ortega_1[1], ortega_2[1])), xlim = c(0,1), ylim = c(0,1), type = "l",
     col =col[1], main = expression(paste("Ortega Lorenz curve: Variation in ", alpha, ", ", gamma, " fixed at 0.5")),
     xlab = "cumultative share of population", ylab = "cumulative share of income")
lines(x = x_values, y = x_values)
lines(x = x_values, y = 1-x_values, lty = 2)
for (i in 2:length(ortega_1)) {
  lines(x = x_values, ortega(x_values, theta = c(ortega_1[i], ortega_2[1])), col = col[i])
}
legend("topleft", col = c(col[1], col[length(ortega_1)]), lty = 1, 
       legend = c("low alpha", "high alpha"))
# we can see that there is a disproportionate change on the left side of the distribution (low incomes)

##########################
# What if the other Ortega parameter was fixed at another level? 
# We basically face the same picture regarding the disproportionate changes
# You can play around with the fixed values to verify
##########################

# varying Ortega parameter 2
x_values <- sort(c(0,1,runif(1000, min = 0, max = 1)), decreasing = F)
ortega_1 <- 0.1 # play around here, e.g 0.1 instead of 0.5
ortega_2 <- c(seq(0.01,0.99,by=0.05)) # side constraint: 0 <= gamma < 1 
col <- diverge_hcl(length(ortega_2))
plot(x = x_values, ortega(x_values, theta = c(ortega_1[1], ortega_2[1])), xlim = c(0,1), ylim = c(0,1), type = "l",
     col =col[1], main = expression(paste("Ortega Lorenz curve: Variation in ", gamma, ", ", alpha, " fixed at 0.5")),
     xlab = "cumultative share of population", ylab = "cumulative share of income")
lines(x = x_values, y = x_values)
lines(x = x_values, y = 1-x_values, lty = 2)
for (i in 2:length(ortega_2)) {
  lines(x = x_values, ortega(x_values, theta = c(ortega_1[1], ortega_2[i])), col = col[i])
}

# varying Ortega parameter 1
ortega_1 <- c(seq(0.1, 3,by=0.2)) # side constraint: 0 <= alpha
# from our empirical estimates we know alpha values typically are around 0.5 to 1
ortega_2 <- 0.7 # play around here, e.g. 0.7 instead of 0.5
col <- diverge_hcl(length(ortega_1))
plot(x = x_values, ortega(x_values, theta = c(ortega_1[1], ortega_2[1])), xlim = c(0,1), ylim = c(0,1), type = "l",
     col =col[1], main = expression(paste("Ortega Lorenz curve: Variation in ", alpha, ", ", gamma, " fixed at 0.5")),
     xlab = "cumultative share of population", ylab = "cumulative share of income")
lines(x = x_values, y = x_values)
lines(x = x_values, y = 1-x_values, lty = 2)
for (i in 2:length(ortega_1)) {
  lines(x = x_values, ortega(x_values, theta = c(ortega_1[i], ortega_2[1])), col = col[i])
}
# we can see that there is a disproportionate change on the left side of the distribution (low incomes)

#################################
# We suggest that Ortega 1/2 can disentangle bottom from top concentrated inequality
# Ortega 1 = alpha, Ortega 2 = gamma
# Hence we want to compare this for same levels of overall inequality
# Generate Lorenz curves with approx. same Gini coefficient but different Ortega parameters
#################################

# Exaxt formula for calculating the Gini index for Ortega Lorenz curves, see Ortega et al. (1991)
gini_ortega <- function(x){(x[1] - 1)/(x[1] +1) + 2*beta(x[1]+ 1, 1-x[2]+ 1) } 

# grid search: Which Ortega parameter combinations yield approx. the same Gini index?
o1 <- c(seq(0.01,1, by = 0.01))
o2 <- c(c(seq(0.01,0.99, by = 0.01)))
grid <- gridSearch(gini_ortega, list(o1,o2)) 
which(grid$values > 0.45 & grid$values< 0.451) # Lorenz curves with approx same Gini coefficient of 0.45
grid$levels[which(grid$values > 0.45 & grid$values< 0.451)] # parameter combinations exhibiting this Gini index

# take examplary parameter combination that yields a Gini of approx. 0.45
gini <- 0.45 
a <- 0.05
b <- 0.61
c <- 0.96
d <- 0.35

# plot 
ggplot()+
  geom_line(aes(x=x_values, y=ortega(x_values, theta = c(a,b)), colour = paste0("Gini:  ", round(gini_ortega(c(a,b)), digits = 4), 
                                                                                "\nOrtega alpha: ", a, " \nOrtega gamma: ", b)))+ 
  geom_line(aes(x=x_values, y=ortega(x_values, theta = c(c,d)), colour = paste0("Gini:  ", round(gini_ortega(c(c,d)), digits = 4), 
                                                                                "\nOrtega alpha: ", c, " \nOrtega gamma: ", d)))+ 
  geom_line(aes(x=x_values, y=x_values), col = "grey")+
  geom_line(aes(x = x_values, y = 1-x_values), lty = 2, col = "grey")+
  theme(legend.position="bottom", legend.title = element_blank() )+
  scale_color_brewer(palette="Paired")+
  labs(title="Exemplary Lorenz curves exhibiting the approx. same Gini coefficient\n but different Ortega parameters",
       x="cumulative share of population", y = "cumulative share of income")
  

# Plot: Rising Gini (overall inequality)
# We can see that usign the Ortega parameters, we can tell where the rise comes from, 
# i.e. from an increase in top or bottom concentrated income inequality

o1 <- c(seq(0.01,1, by = 0.01))
o2 <- c(c(seq(0.01,0.99, by = 0.01)))
grid <- gridSearch(gini_ortega, list(o1,o2) )
grid$levels[which(grid$values > 0.45 & grid$values< 0.451)] # Lorenz curves with approx same Gini coefficient of 0.45
grid$levels[which(grid$values > 0.5 & grid$values< 0.51)] # Lorenz curves with approx same Gini coefficient of 0.5

# Parameter combination 1: Gini 0.45
a <- 0.53
b <- 0.49
gini_1 <- round(gini_ortega(c(a,b)), digits = 4) # approx. 0.45

# Parameter combination 2: Gini now 0.5, but Ortega 1 the same as in comb. 1
c <- 0.53
d <- 0.56
gini_2 <- round(gini_ortega(c(c,d)), digits = 4) # approx. 0.5

# Parameter combination 3: Gini now 0.5, but Ortega 2 approx. same as in comb. 1
e <- 0.78
f <- 0.5
gini_3 <- round(gini_ortega(c(e,f)), digits = 4) # approx. 0.5

ggplot()+
  geom_line(aes(x=x_values, y=ortega(x_values, theta = c(a,b)), colour = paste0("Gini:  ", gini_1, 
                                                                                "\nOrtega alpha: ", a, " \nOrtega gamma: ", b)))+ 
  geom_line(aes(x=x_values, y=ortega(x_values, theta = c(c,d)), colour =  paste0("Gini:  ", gini_2, 
                                                                                 "\nOrtega alpha: ", c, " \nOrtega gamma: ", d)))+
  geom_line(aes(x=x_values, y=ortega(x_values, theta = c(e,f)), colour =  paste0("Gini:  ", gini_2, 
                                                                                 "\nOrtega alpha: ", e, " \nOrtega gamma: ", f)))+
  geom_line(aes(x=x_values, y=x_values), col = "grey")+
  geom_line(aes(x = x_values, y = 1-x_values), lty = 2, col = "grey")+
  theme(legend.position="bottom", legend.title = element_blank() )+
  labs(title="Exemplary Lorenz curves",
       x="cumulative share of population", y = "cumulative share of income")

# red line: Combination of Ortega parameters yielding a Gini index of 0.45
# green line: in comparison to the red line, the Gini has risen to 0.5, but Ortega 1 is the same 
  # -> we can see that in this case, the rise in Gini is due to a change in Ortega 2
  # -> the overall inequality (Gini) has risen because of more top concentrated inequality (Ortega 2)
# blue line: in comparison to the red line, the Gini has risen to 0.5, but Ortega 2 is approx. the same
  # -> we can see that in this case, the rise in Gini is due to a change in Ortega 1
  # -> the overall inequality (Gini) has risen because of a more bottom concentrated inequality (Ortega 1)
# comparing the blue and green line, the Gini index is the same, however, both Ortega parameters differ, and we
# can clearly see from the plot that the green line exhibits more top concentrated inequality while the blue 
# line exhibits more bottom concentrated inequality. 


