
##################################################
## Creation of Table 1, Stratified by race/eth ##
##################################################

#written by: Emma Gause 
#Date: 03/08/23
#Last updated: 05/17/23

#load libraries:
library("tidyverse")
library("dplyr")
library("tableone")

#Create path to directory
datadir <- "/Users/egause/Library/CloudStorage/OneDrive-BostonUniversity/ClimLab/MPJ/KHANDLE/Analysis/SENAS/"

#read in data
dat <- readRDS(paste0(datadir, "Analysis_Data_081823.rds"))
#keep wave 1 for baseline covars
wave1 <- dat %>% filter(waveseq==1)

##------------------------------------------------------------------------##
summary(wave1)

#create complete case data - no missingness in covariates
ccdat <- dat %>% filter(!is.na(NIHTLBX_depr_theta)&
                         !is.na(race_fact)&
                         !is.na(female)&
                         !is.na(age_bl_x)&
                         !is.na(college)&
                         !is.na(inc_gt55k)&
                         !is.na(married_partner))
summary(ccdat)

#now remove anyone missing all SENAS data 
#how many people missing all senas waves?
ccdat <- ccdat %>% group_by(STUDYID) %>% mutate(all_miss_ex = all(is.na(exec_z)),
                                                  all_miss_vr = all(is.na(vrmem_z)),
                                                  all_miss_se = all(is.na(sem_z))) %>% ungroup()

outmiss <- ccdat %>% select(STUDYID, all_miss_ex:all_miss_se) %>% unique()
table(outmiss$all_miss_ex, useNA = "ifany")
table(outmiss$all_miss_vr, useNA = "ifany")
table(outmiss$all_miss_se, useNA = "ifany")

table(outmiss$all_miss_ex, outmiss$all_miss_vr, useNA = "ifany", deparse.level = 2)
table(outmiss$all_miss_se, outmiss$all_miss_vr, useNA = "ifany", deparse.level = 2)

#remove them!
ccdat <- ccdat %>% filter(all_miss_ex==0&all_miss_vr==0&all_miss_se==0)

#how many people -- use baseline wave 1
ccw1 <- ccdat %>% filter(waveseq==1) #2227 complete cases

#by race?
table(ccw1$race_fact, useNA = "ifany")

#how many people in our sensitivity analysis?
senscc <- ccdat %>% filter(!is.na(social)&
                             !is.na(ever_smoke)&
                             !is.na(heavy_drink_fact)&
                             !is.na(daily_physical)&
                             !is.na(exc_health)&
                             !is.na(setting))
sensccw1 <- senscc %>% filter(waveseq==1) #2127 complete cases


##------------------------------------------------------------------------##

#save the complete case dataset 
saveRDS(ccdat, paste0(datadir, "Complete_Cases_083023.rds"))


##------------------------------------------------------------------------##

#misingness in outcome by wave

ccw1 #already wave 1 complete case data
ccw2 <- ccdat %>% filter(waveseq==2)
ccw3 <- ccdat %>% filter(waveseq==3)

summary(ccw1)
summary(ccw2)
summary(ccw3)


##------------------------------------------------------------------------##

#Create Table 1 from complete cases
  #first overall and then by race/eth
str(ccw1)

tab1r <- CreateCatTable(vars = c("study","female", "college", "inc_gt55k", 
                                 "married_partner", "social", "exc_health",
                                 "ever_smoke", "heavy_drink_fact", "daily_physical"),
                        data = ccw1, strata = "race_fact", test = FALSE, includeNA = TRUE,
                        addOverall = TRUE)
tab1r

tab2r <- CreateContTable(vars = c("NIHTLBX_depr_theta", "age_bl_x", "exec_z", "vrmem_z", "sem_z"),
                         data = ccw1, strata = "race_fact",
                         funcNames = c("n", "miss", "p.miss", "mean", "sd", "median", "p25", "p75"),
                         test = FALSE, addOverall = TRUE)
summary(tab2r)



summary(ccw1)
















