###############################################################################
### Data cleaning - Step 1: merging source data tables into a single data frame
###############################################################################
### author: Kristin Blesch, May 2020

### Tables that are merged:
### - Table B4, Table B6 from https://www.epi.org/publication/income-inequality-in-the-us/#epi-toc-20
### - Tables NHGIS A and NHGIS B from https://data2.nhgis.org/main American Community Survey 2011-2015
###     - source table NHGIS A: NHGIS code 2011_2015_ACSa, source codes: B19001, B19013, B19025
###     - source table NHGIS B: NHGIS code 2011_2015_ACSb, source codes: B19080, B19081, B19082, B19083
### additional information: states_code.csv, file with abrreviations and full names of US States, from
### https://developers.google.com/public-data/docs/canonical/states_csv

### ------------------------------------------------------------------------------------------------------

# load relevant librarys
library(dplyr)
library(stringr)
library(stringi)
library(fuzzyjoin)

# load data, exclude Puerto Rico and DC
# setwd("/Users/kristinblesch/Library/Mobile Documents/com~apple~CloudDocs/USA/Master_Thesis/Data")

nhgis_A <- read.csv("ACS_Data/nhgis0007_ds215_20155_2015_county.csv", encoding = "ISO-8859-1") %>% 
  select("STATE", "COUNTY", "NAME_E", 37:56) %>% 
  filter(!STATE %in% c("Puerto Rico","District of Columbia"))
nhgis_B <- read.csv("ACS_Data/nhgis0007_ds216_20155_2015_county.csv", encoding = "ISO-8859-1")%>% 
  select("STATE", "COUNTY", "NAME_E", "AD4AE001", 47:52, 66:71) %>% 
  filter(!STATE %in% c("Puerto Rico","District of Columbia"))
b6 <- read.csv("Table_B6.csv", sep = ";") %>% 
  filter(!County %in% c("United States","District of Columbia, DC"))
b4 <- read.csv("Table_B4.csv")%>% 
  filter(!County.state %in% c("District of Columbia, DC")) 

# merge nhgis_A to nhgis_B
nhgis <- nhgis_A %>% select(-c("STATE", "COUNTY")) %>% merge(nhgis_B,., by = "NAME_E")
# merge b4 to b6
b6 <- merge(b6, b4, by.x = "County", by.y = "County.state") %>% select(-c("Rank..by.top.1..share.of.all.income.","Rank"))

# adjust county names in B6 for merge - make county names look like in nhgis data
b6$County <- as.character(b6$County)
county <- strsplit(b6$County, ", ")
df <- data.frame(matrix(unlist(county), nrow = nrow(b6), ncol = 2, byrow = T))
colnames(df) <- c("county", "state_code")
b6 <- bind_cols(b6,df)
code <-  read.csv("states_code.csv")[,-2] # import state codes - states names table
colnames(code) <- c("state_name", "state_code")
b6 <- merge(b6, code, by.x = "state_code", by.y = "state_code")
b6$county <-  as.character(b6$county)

# preprocessing of special cases 
b6 <- b6%>% mutate(county = ifelse(state_code == "AK", gsub( " .*$", "", county), county ))%>%
  mutate(county = ifelse(state_code == "AK", gsub( "-.*$", "", county), county )) #cut alaska's names, 
# there are no counties in alaska, but cities and boroughs and census areas, 
# nhgis names them with their city/borough/... title, 
# b6 doesnt, so we cut everything after the first word (which is a unique determinant of the actual area) in both data sets
b6$countystate <- ifelse(str_detect(b6$county, "Census Area") | str_detect(b6$county, "City") | 
                           str_detect(b6$state_code,"LA")| str_detect(b6$state_code,"AK"), 
                         paste(b6$county, b6$state_name, sep=", "),
                         paste(paste(b6$county, "County", sep = " "), b6$state_name, sep=", "))
b6$countystate <- ifelse(str_detect(b6$state_code,"LA"), 
                         paste(paste(b6$county, "Parish", sep = " "), b6$state_name, sep=", "),
                         b6$countystate) # Louisiana is divided in Parishes, not Counties

# correct typical things by hand, e.g. " " vs. "'", "St. " vs. "St "
nhgis$COUNTY <- as.character(nhgis$COUNTY)
nhgis$NAME_E <- as.character(nhgis$NAME_E)
nhgis <- nhgis[-which(nhgis$COUNTY == "Aleutians East Borough"),] # not contained in B6, remove Aleutians East Borough
# oth. confusion with Aleutians West Borough
nhgis <- nhgis %>%mutate(COUNTY = ifelse(STATE == "Alaska", gsub( " .*$", "", COUNTY), COUNTY )) %>%
  mutate(COUNTY = ifelse(STATE == "Alaska", gsub( "-.*$", "", COUNTY), COUNTY )) %>%
  mutate(NAME_E = ifelse(STATE == "Alaska", paste(COUNTY, STATE, sep = ", "), NAME_E))
nhgis$NAME_E <- str_replace_all(nhgis$NAME_E, "St. ", "St ")
b6$countystate <- str_replace_all(b6$countystate, "St. ", "St ")
b6$countystate <- str_replace_all(b6$countystate,fixed(" ") , "")
nhgis$countystate <- as.character(nhgis$NAME_E) %>% stri_encode(from = 'ISO-8859-1', to = 'UTF-8') %>% 
  str_replace_all(fixed(" ") , "") %>%str_replace_all(fixed("'") , "")

# use fuzzy matching algorithm to combine the two data sets
fuzzy_combined <- stringdist_inner_join(b6, nhgis, by = "countystate", distance_col = "distance", ignore_case=TRUE,
                                        method ="osa")
# to check fuzzy matches of county names
# check0 <- fuzzy_combined %>% select(c("countystate.x", "countystate.y", "distance")) %>% 
# filter(distance ==0) #perfect matches
# check1 <- fuzzy_combined %>% select(c("countystate.x", "countystate.y", "distance")) %>% 
# filter(distance ==1) #not so perfect matches
# check2 <- fuzzy_combined %>% select(c("countystate.x", "countystate.y", "distance")) %>% 
# filter(distance ==2) #not so perfect matches
# setdiff(b6$countystate, check0$countystate.x) # contained in b6 but not in check0/1/2
# setdiff(nhgis$countystate, check0$countystate.x) # contained in nhgis but not in check0/1/2

# match not perfect matches by hand, first find perfect matches, then add imperfect matches by hand to combined data set
combined_tables <- fuzzy_combined %>% filter(distance ==0)

# Wade, Alaska = Kusilvak, Alaska 
new_kusilvak <- cbind(b6[which(b6$countystate == "Wade,Alaska"),], 
                      nhgis[which(nhgis$countystate == "Kusilvak,Alaska"),], 0)
# mistake in B6, Montgomery already named as county, resulty in Montgomery County County
new_mtg <- cbind(b6[which(b6$countystate == "MontgomeryCountyCounty,Arkansas"),], 
                 nhgis[which(nhgis$countystate == "MontgomeryCounty,Arkansas"),], 0)
# Ste. Genevieve County, Missouri = St. Genevieve County, Missouri
new_stgenevieve <- cbind(b6[which(b6$countystate == "StGenevieveCounty,Missouri"),], 
                         nhgis[which(nhgis$countystate == "Ste.GenevieveCounty,Missouri"),], 0)
# bad encoding of Dona Ana County
new_dona <- cbind(b6[which(b6$countystate == "DonaAnaCounty,NewMexico"),], 
                  nhgis[which(nhgis$countystate == "Doï¿½aAnaCounty,NewMexico"),], 0)
# Shannon County, South Dakota = Oglala Lakota County, South Dakota
new_shannon <- cbind(b6[which(b6$countystate == "ShannonCounty,SouthDakota"),], 
                     nhgis[which(nhgis$countystate == "OglalaLakotaCounty,SouthDakota"),], 0)
# inconsistent naming of Viginia counties James City, Charles City, Radford City
new_james <- cbind(b6[which(b6$countystate == "JamesCity,Virginia"),], 
                   nhgis[which(nhgis$countystate == "JamesCityCounty,Virginia"),], 0)
new_charles <- cbind(b6[which(b6$countystate == "CharlesCity,Virginia"),], 
                     nhgis[which(nhgis$countystate == "CharlesCityCounty,Virginia"),], 0)
new_radford <- cbind(b6[which(b6$countystate == "RadfordCounty,Virginia"),], 
                     nhgis[which(nhgis$countystate == "Radfordcity,Virginia"),], 0)

# add imperfect matches to data frame of combined tables
combined_tables <- bind_rows(combined_tables, new_kusilvak, new_mtg, new_stgenevieve,
                             new_dona,new_shannon, new_james, new_charles, 
                             new_radford) %>% select(-c("state_code", "state_name", "county", "countystate", "0",
                                                        "countystate.x", "countystate.y", "distance"))
# write csv file to save merged data set
#write.csv(combined_tables, 
 #         file.path("/Users/kristinblesch/Library/Mobile Documents/com~apple~CloudDocs/USA/Master_Thesis/Data", 
  #                  "merged_b4_b6_nhgis.csv"))

