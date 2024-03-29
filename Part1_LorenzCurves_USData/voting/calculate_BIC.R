MLE_output <- read.csv("../estimation_procedure/MLE_output_county.csv")%>% 
  mutate(p = 6-rowSums(is.na(cbind(par_1, par_2, par_3, par_4, par_5))))%>%
  mutate(n = ((1-p)*AIC + 2*p*(p+1)-(1-p)*AIC_c)/(AIC_c - AIC)) %>%
  mutate(BIC = AIC-2*p + p*log(n))

#write.csv(MLE_output, file = "information_criteria_county.csv" )                      
