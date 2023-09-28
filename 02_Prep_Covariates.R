
#########################################
## Prepare covariate data for analysis ##
#########################################

#written by: Emma Gause 
#Date: 12/08/22
#Last updated: 05/17/23

#load libraries:
library("tidyverse")
library("dplyr")
library("mice") #for visualizing missingness

#Create path to directory
datadir <- "/Users/egause/Library/CloudStorage/OneDrive-BostonUniversity/ClimLab/MPJ/KHANDLE/Analysis/SENAS/"

#read in cohort data
dat <- readRDS(paste0(datadir, "KHANDLE_STAR_Cohorts_081823.rds"))

##------------------------------------------------------------------------##
colnames(dat)
#exposure: NIHTLBX_depr_theta
hist(dat$NIHTLBX_depr_theta[dat$study=="STAR"&dat$waveseq==1])
hist(dat$NIHTLBX_depr_theta[dat$study=="KHANDLE"&dat$waveseq==1])
hist(dat$NIHTLBX_depr_theta[dat$waveseq==1])

#outcomes:
#MULTI-LEVEL:
  #exec
  #vrmem
  #sem (only available in two waves)

#timepoint: years since baseline (TIME VARYING)
table(dat$waves_tot[dat$waveseq==1]) # 132 participants only have 2 waves of data... 71 w/ only 1.

#Covariates
  #female
  #education
  #INCOME_RANGE
  #age_bl_x
  #MARITAL_STATUS
  #study -- can't do this when stratifying by race because positivity violations
  #testlang -- can't do this when stratifying by race because positivity violations

#SENSITIVITY:
  #self-rated health
  #alcohol
  #smoking
  #physical activity
  #practice setting ("COMPLETED_AT" vars)

##------------------------------------------------------------------------##

#re z-score the SENAS measures for the combined cohorts
hist(dat$exec)
hist(dat$vrmem)
hist(dat$sem)

#z-score formula: z = (x-[pop mean])/[SD]
#use the baseline values to z-score all timepoints
em <- mean(dat$exec[dat$waveseq==1], na.rm = TRUE)
es <- sd(dat$exec[dat$waveseq==1], na.rm = TRUE)

vm <- mean(dat$vrmem[dat$waveseq==1], na.rm = TRUE)
vs <- sd(dat$vrmem[dat$waveseq==1], na.rm = TRUE)

sm <- mean(dat$sem[dat$waveseq==1], na.rm = TRUE)
ss <- sd(dat$sem[dat$waveseq==1], na.rm = TRUE)

dat <- dat %>% mutate(exec_z = (exec - 0.2812613)/0.7476444,
                      vrmem_z = (vrmem - 0.1594515)/0.8765611,
                      sem_z = (sem - -0.0007670343)/0.8898971)

hist(dat$exec)
hist(dat$exec_z)

hist(dat$vrmem)
hist(dat$vrmem_z)

hist(dat$sem)
hist(dat$sem_z)

test <- dat %>% select(waveseq, exec, exec_z, vrmem, vrmem_z, sem, sem_z)
summary(test)

test1 <- test %>% filter(waveseq==1)
summary(test1) #yes! All z-means are now 0 at wave 1


##------------------------------------------------------------------------##
colnames(dat)
#separate time-varying and baseline variables to prepare analysis data
vary <- dat %>% select(id, waveseq, yrsbl, aget89, exec, vrmem, sem, exec_z, vrmem_z, sem_z, COMPLETED_AT)
base <- dat %>% filter(waveseq==1) %>% 
                select(id, STUDYID, testlang, study, education, gender, female, 
                       race_summary, age_bl_x, INCOME_RANGE, INCOME_EST,
                       CA_SOC, MARITAL_STATUS, HEALTH, SMK, ALC,
                       ALC_FREQ, ALC_VOL, PA_HVY_WRK:PA_LT_HSE,
                       NIHTLBX_depr_theta)

### TIME-VARYING PREPPING ###
#make sure all participants have three waves of data (expand) - even if missing
vary_x <- vary %>% tidyr::expand(id, waveseq)
vary <- merge(vary_x, vary, by = c("id", "waveseq"), all.x = TRUE)
table(vary$waveseq) # all w/ same #

#Create a new years since baseline variable - setting missing to mean of existing
#This is our TIME variable for the deltas
vary$yrsbl_x <- vary$yrsbl
vary$yrsbl_x[is.na(vary$yrsbl_x)&vary$waveseq==1] <- mean(vary$yrsbl[vary$waveseq==1], na.rm=TRUE)
vary$yrsbl_x[is.na(vary$yrsbl_x)&vary$waveseq==2] <- mean(vary$yrsbl[vary$waveseq==2], na.rm=TRUE)
vary$yrsbl_x[is.na(vary$yrsbl_x)&vary$waveseq==3] <- mean(vary$yrsbl[vary$waveseq==3], na.rm=TRUE)

#create indicator for practice setting
table(vary$COMPLETED_AT, useNA = "ifany") #1==respondent's home; 2==clinic office; 3==phone
vary <- vary %>% mutate(setting = case_when(
  COMPLETED_AT==1 ~ "home",
  COMPLETED_AT==2 ~ "clinic",
  COMPLETED_AT==3 ~ "phone"))
table(vary$setting, useNA = "ifany") 

                        
### TIME-INVARIANT PREPPING ###

#set to missing for coded missingness in data 
summary(base)
base$INCOME_EST[base$INCOME_EST==77|base$INCOME_EST==88|base$INCOME_EST==99] <- NA
base$INCOME_RANGE[base$INCOME_RANGE==77|base$INCOME_RANGE==88|base$INCOME_RANGE==99] <- NA
base$CA_SOC[base$CA_SOC==77|base$CA_SOC==88|base$CA_SOC==99] <- NA
base$MARITAL_STATUS[base$MARITAL_STATUS==7|base$MARITAL_STATUS==77|base$MARITAL_STATUS==88|base$MARITAL_STATUS==99] <- NA
base$education[base$education==9|base$education==10|base$education==0] <- NA
base$HEALTH[base$HEALTH==88|base$HEALTH==99] <- NA
base$SMK[base$SMK==88] <- NA
base$ALC[base$ALC==88] <- NA
base$ALC_FREQ[base$ALC_FREQ==88|base$ALC_FREQ==99] <- NA
base$ALC_VOL[base$ALC_VOL==88|base$ALC_VOL==99] <- NA
base$PA_HVY_WRK[base$PA_HVY_WRK==88] <- NA
base$PA_LT_WRK[base$PA_LT_WRK==88] <- NA
base$PA_VIG_EX[base$PA_VIG_EX==88] <- NA
base$PA_LT_EX[base$PA_LT_EX==88] <- NA
base$PA_VIG_HSE[base$PA_VIG_HSE==88] <- NA
base$PA_LT_HSE[base$PA_LT_HSE==88] <- NA



#Prep covars for inclusion in analysis

#english test language - create binary from string field
table(base$testlang, useNA = "ifany")
base <- base %>% mutate(engl_test = if_else(testlang=="English", 1, 0))
table(base$engl_test, useNA = "ifany") #only LatinX participants use Spanish language test

#dichotomize education variable
table(base$education, useNA = "ifany") #
base <- base %>% mutate(college = if_else(education==5|education==6, 1, 0)) #college or graduate
table(base$college, useNA = "ifany") #

#three categories of income
table(base$INCOME_RANGE, base$INCOME_EST, useNA = "ifany") #more missingness in detailed...
  #use the existing dichotomy?
base <- base %>% mutate(inc_gt55k = if_else(INCOME_EST==2, 1, 0))
table(base$inc_gt55k, useNA = "ifany")

#age at baseline -- already supplemented by taking age at first wave

#Marital status: dichotomize
base$MARITAL_STATUS #married or with partner vs. not
base <- base %>% mutate(married_partner = if_else(MARITAL_STATUS==1|MARITAL_STATUS==2, 1, 0))

#create social network variable:
table(base$CA_SOC, useNA = "ifany") # almost every day vs. not
base <- base %>% mutate(social = if_else(CA_SOC==1, 1, 0))
table(base$social, useNA = "ifany")

#create poor health 
table(base$HEALTH, useNA = "ifany") #excellent or very good vs. not
base <- base %>% mutate(exc_health = if_else(HEALTH==1|HEALTH==2, 1, 0))
table(base$exc_health, useNA = "ifany")

#make race into factor var
base$race_fact <- factor(base$race_summary, levels = c("White", "Asian", "Black", "LatinX"),
                         labels = c("White", "Asian", "Black", "LatinX"))

#create study factor
base <- base %>% mutate(khandle = if_else(study=="KHANDLE", 1, 0))

#create binary for smoking: ever vs. never
table(base$SMK, useNA = "ifany")
base$ever_smoke <- factor(base$SMK, level = c(0, 1), 
                          labels = c("Never Smoked", "Ever Smoked"))

#create indicator for heavy drinking:
#drinking every day OR drink 4+ drinks when drinking
table(base$ALC, base$ALC_FREQ, useNA = "ifany", deparse.level = 2)
table(base$ALC, base$ALC_VOL, useNA = "ifany", deparse.level = 2)

#Freq and vol are branching logic questions(I believe?)
  #set missing in vol and freq to no drinks if ever drink is no.
base$ALC_FREQ[is.na(base$ALC_FREQ)&base$ALC==0] <- 1
base$ALC_VOL[is.na(base$ALC_VOL)&base$ALC==0] <- 0

base <- base %>% mutate(heavy_drink = if_else((ALC_FREQ==3|ALC_VOL==4), 1, 0))
base$heavy_drink_fact = factor(base$heavy_drink, levels = c(0, 1),
                               labels = c("No heavy drinking", " Heavy Drinking"))
table(base$heavy_drink_fact, useNA = "ifany")

#create indicator for daily physical activity
table(base$PA_HVY_WRK, useNA = "ifany")
table(base$PA_LT_WRK, useNA = "ifany")
table(base$PA_VIG_EX, useNA = "ifany")
table(base$PA_LT_EX, useNA = "ifany")
table(base$PA_VIG_HSE, useNA = "ifany")
table(base$PA_LT_HSE, useNA = "ifany")

base <- base %>% mutate(daily_physical = if_else((PA_HVY_WRK==1|
                                                    PA_VIG_EX==1|
                                                    PA_LT_EX==1|
                                                    PA_VIG_HSE==1|
                                                    PA_LT_HSE==1), 1, 0))
#REMOVED "PA_LT_WRK" (standing or walking) from daily physical activity indicator
table(base$daily_physical, useNA = "ifany")




#Create missingness plot for baselines
colnames(base)
missbase<- base %>% select(khandle, NIHTLBX_depr_theta,
                           female, race_fact, age_bl_x, 
                            college, inc_gt55k, married_partner) %>% 
  md.pattern(rotate.names = TRUE)
2237/nrow(base) #90.6% of cohort has no missingness in exposure/covariates

#what about the sensitivity analysis?
missbase2<- base %>% select(khandle, NIHTLBX_depr_theta,
                            female, race_fact, age_bl_x, 
                           college, inc_gt55k, married_partner, 
                           social, exc_health, ever_smoke, heavy_drink,
                           daily_physical) %>% 
  md.pattern(rotate.names = TRUE)
2138/nrow(base) #86.6% of cohort has no missingness in exposure/baseline covariates [not including time-varying setting]


#merge together w/ time varying!
data <- merge(base, vary, by = "id", all = TRUE)
  
##------------------------------------------------------------------------##

#Create missingness plot w/ outcomes
colnames(data)

png(paste(datadir, "Missingness_081823.png"),
    width = 600, height = 1100, units = "px")

missmain1 <- data %>% filter(waveseq==1) %>% 
                select(khandle, NIHTLBX_depr_theta,
                            female, race_fact, age_bl_x, 
                            college, inc_gt55k, married_partner,
                            yrsbl_x, exec_z, vrmem_z, sem_z) %>% 
  md.pattern(rotate.names = TRUE)

dev.off()



##------------------------------------------------------------------------##

#save analysis data
saveRDS(data, paste0(datadir, "Analysis_Data_081823.rds"))


