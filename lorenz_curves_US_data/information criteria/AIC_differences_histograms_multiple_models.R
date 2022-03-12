library(dplyr)
library(tidyr)
library(ggplot2)

AIC_raw <- read.csv("/Users/kristinblesch/Library/Mobile Documents/com~apple~CloudDocs/Paper_inequality/GitHub_upload/lorenz_curve_estimation/MLE_output_county.csv")
AIC_diff <- AIC_raw %>% select(county_index,COUNTY, form, AIC_c) %>% spread(key = form, value = AIC_c) %>%
  transmute(county_index = county_index,
            COUNTY = COUNTY,
            ORTEGA_LOGNORMAL = LOGNORMAL - ORTEGA, 
            ORTEGA_WANG = ORTEGA-WANG, 
            ORTEGA_GB2 = ORTEGA-GB2) %>% filter(COUNTY != "Teton County, Wyoming")# Teton, Wyoming is a clear outlier, exclude it 

### histogram: ORTEGA vs. LOGNORMAL
ggplot(AIC_diff, aes(x = ORTEGA_LOGNORMAL)) +
  geom_histogram(data = AIC_diff[AIC_diff$ORTEGA_LOGNORMAL < -4,], breaks = c(-20,-10,-4), aes(fill = "1"))+
  geom_histogram(data = AIC_diff[AIC_diff$ORTEGA_LOGNORMAL <= 4,], breaks = c(-4, 0,4), aes(fill = "2")) +
  geom_histogram(data = AIC_diff[AIC_diff$ORTEGA_LOGNORMAL <= 10,], breaks = c(4,10), aes(fill = "3")) +
  geom_histogram(data = AIC_diff[AIC_diff$ORTEGA_LOGNORMAL >10,], breaks = seq(10,90,by=5),aes(fill = "4")) +
  scale_fill_manual(name="Strength of evidence:\n2-parameter Ortega model \nproviding substantially \nmore information than \na 1-parameter lognormal model", values=c("red", "lightblue", "green3", "green4"),labels=c("counter evidence","inconclusive evidence","some evidence", "decisive evidence"))+
  labs(title="Historgram of AIC_c differences between Ortega and Lognormal model",
       x="AIC_c difference", y = "frequency across 3055 US counties")

### histogram: ORTEGA vs. GB2
ggplot(AIC_diff, aes(x = ORTEGA_GB2)) +
  geom_histogram(data = AIC_diff[AIC_diff$ORTEGA_GB2 < -4,], breaks = c(-20,-10,-4), aes(fill = "1"))+
  geom_histogram(data = AIC_diff[AIC_diff$ORTEGA_GB2 <= 4,], breaks = c(-4, 0,4), aes(fill = "2")) +
  geom_histogram(data = AIC_diff[AIC_diff$ORTEGA_GB2 <= 10,], breaks = c(4,10), aes(fill = "3")) +
  geom_histogram(data = AIC_diff[AIC_diff$ORTEGA_GB2 >10,], breaks = seq(10,30,by=5),aes(fill = "4")) +
  scale_fill_manual(name="Strength of evidence:\n3-parameter GB2 model \nproviding substantially \nmore information than \na 2-parameter Ortega model", values=c("red", "lightblue", "green3", "green4"),labels=c("counter evidence","inconclusive evidence","some evidence", "decisive evidence"))+
  labs(title="Historgram of AIC_c differences between Ortega and GB2 model",
       x="AIC_c difference", y = "frequency across 3055 US counties")

### histogram: ORTEGA vs. WANG
ggplot(AIC_diff, aes(x = ORTEGA_WANG)) +
  geom_histogram(data = AIC_diff[AIC_diff$ORTEGA_WANG < -4,], breaks = c(-20,-10,-4), aes(fill = "1"))+
  geom_histogram(data = AIC_diff[AIC_diff$ORTEGA_WANG <= 4,], breaks = c(-4, 0,4), aes(fill = "2")) +
  geom_histogram(data = AIC_diff[AIC_diff$ORTEGA_WANG <= 10,], breaks = c(4,10), aes(fill = "3")) +
  geom_histogram(data = AIC_diff[AIC_diff$ORTEGA_WANG >10,], breaks = seq(10,70,by=5),aes(fill = "4")) +
  scale_fill_manual(name="Strength of evidence:\n5-parameter Wang model \nproviding substantially \nmore information than \na 2-parameter Ortega model", values=c("red", "lightblue", "green3", "green4"),labels=c("counter evidence","inconclusive evidence","some evidence", "decisive evidence"))+
  labs(title="Historgram of AIC_c differences between Ortega and GB2 model",
       x="AIC_c difference", y = "frequency across 3055 US counties")
