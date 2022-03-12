ortega <- function(pop_csum, theta){ 
  # alpha = theta[1], gamma = 1-theta[2]
  # 0 <= alpha, 0 <= gamma < 1 
  pop_csum^theta[1] * (1- (1- pop_csum)^(1-theta[2]))
}

deriv_ortega_a <- function(pop_csum, theta){
  pop_csum^theta[1] * (1- (1- pop_csum)^(1-theta[2])) * log(pop_csum)
}

deriv_ortega_g <- function(pop_csum, theta){
  pop_csum^theta[1] * (1- pop_csum)^(1-theta[2]) * log(1-pop_csum)
}

pop = seq(0,1,0.01)

plot(x = pop, y = ortega(pop, theta = c(0.5,0.5)), type = "l")
plot(x = pop, y = deriv_ortega_a(pop, theta = c(0.5,0.5)), type = "l", 
     xlab = "share of population", ylab = "function value")
plot(x = pop, y = deriv_ortega_g(pop, theta = c(0.5,0.5)), type = "l", 
     xlab = "share of population", ylab = "function value")
