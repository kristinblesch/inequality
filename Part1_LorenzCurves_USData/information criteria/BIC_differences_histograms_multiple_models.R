library(dplyr)
library(tidyr)
library(ggplot2)

BIC_raw <- read.csv("/Users/kristinblesch/Library/Mobile Documents/com~apple~CloudDocs/Paper_inequality/Data/information_criteria_county.csv")
BIC_diff <- BIC_raw %>% select(county_index,COUNTY, form, BIC) %>% spread(key = form, value = BIC) %>%
  transmute(county_index = county_index,
            COUNTY = COUNTY,
            ORTEGA_LOGNORMAL = LOGNORMAL - ORTEGA, 
            ORTEGA_WANG = ORTEGA-WANG, 
            ORTEGA_GB2 = ORTEGA-GB2) %>% filter(COUNTY != "Teton County, Wyoming")# Teton, Wyoming is a clear outlier, exclude it 

### histogram: ORTEGA vs. LOGNORMAL
ggplot(BIC_diff, aes(x = ORTEGA_LOGNORMAL)) +
  geom_histogram(data = BIC_diff[BIC_diff$ORTEGA_LOGNORMAL < -2,], breaks = c(-20,-10,-2), aes(fill = "1"))+
  geom_histogram(data = BIC_diff[BIC_diff$ORTEGA_LOGNORMAL <= 2,], breaks = c(-2, 2), aes(fill = "2")) +
  geom_histogram(data = BIC_diff[BIC_diff$ORTEGA_LOGNORMAL <= 6,], breaks = c(2,6), aes(fill = "3")) +
  geom_histogram(data = BIC_diff[BIC_diff$ORTEGA_LOGNORMAL <= 10,], breaks = c(6,10), aes(fill = "4"))+
  geom_histogram(data = BIC_diff[BIC_diff$ORTEGA_LOGNORMAL >10,], breaks = seq(10,90,by=5),aes(fill = "5")) +
  scale_fill_manual(name="Strength of evidence:\n2-parameter Ortega model \nproviding substantially \nmore information than \na 1-parameter lognormal model", values=c("red", "lightblue", "green2","green3", "green4"),labels=c("counter evidence","inconclusive (weak) evidence","positive evidence", "strong evidence","very strong evidence"))+
  labs(title="Historgram of BIC_c differences between Ortega and Lognormal model",
       x="BIC difference", y = "frequency across 3055 US counties")

### histogram: ORTEGA vs. GB2
ggplot(BIC_diff, aes(x = ORTEGA_GB2)) +
  geom_histogram(data = BIC_diff[BIC_diff$ORTEGA_GB2 < -2,], breaks = c(-20,-10,-2), aes(fill = "1"))+
  geom_histogram(data = BIC_diff[BIC_diff$ORTEGA_GB2 <= 2,], breaks = c(-2, 2), aes(fill = "2")) +
  geom_histogram(data = BIC_diff[BIC_diff$ORTEGA_GB2 <= 6,], breaks = c(2,6), aes(fill = "3")) +
  geom_histogram(data = BIC_diff[BIC_diff$ORTEGA_GB2 <= 10,], breaks = c(6,10), aes(fill = "4"))+
  geom_histogram(data = BIC_diff[BIC_diff$ORTEGA_GB2 >10,], breaks = seq(10,30,by=5),aes(fill = "5")) +
  scale_fill_manual(name="Strength of evidence:\n3-parameter GB2 model \nproviding substantially \nmore information than \na 2-parameter Ortega model", values=c("red", "lightblue", "green2","green3", "green4"),labels=c("counter evidence","inconclusive (weak) evidence","positive evidence", "strong evidence","very strong evidence"))+
  labs(title="Historgram of BIC_c differences between Ortega and GB2",
       x="BIC difference", y = "frequency across 3055 US counties")

### histogram: ORTEGA vs. WANG
ggplot(BIC_diff, aes(x = ORTEGA_WANG)) +
  geom_histogram(data = BIC_diff[BIC_diff$ORTEGA_WANG < -2,], breaks = c(-20,-10,-2), aes(fill = "1"))+
  geom_histogram(data = BIC_diff[BIC_diff$ORTEGA_WANG <= 2,], breaks = c(-2, 2), aes(fill = "2")) +
  geom_histogram(data = BIC_diff[BIC_diff$ORTEGA_WANG <= 6,], breaks = c(2,6), aes(fill = "3")) +
  geom_histogram(data = BIC_diff[BIC_diff$ORTEGA_WANG <= 10,], breaks = c(6,10), aes(fill = "4"))+
  geom_histogram(data = BIC_diff[BIC_diff$ORTEGA_WANG >10,], breaks = seq(10,90,by=5),aes(fill = "5")) +
  scale_fill_manual(name="Strength of evidence:\n5-parameter Wang model \nproviding substantially \nmore information than \na 2-parameter Ortega model", values=c("red", "lightblue", "green2","green3", "green4"),labels=c("counter evidence","inconclusive (weak) evidence","positive evidence", "strong evidence","very strong evidence"))+
  labs(title="Historgram of BIC_c differences between Ortega and Wang",
       x="BIC difference", y = "frequency across 3055 US counties")
