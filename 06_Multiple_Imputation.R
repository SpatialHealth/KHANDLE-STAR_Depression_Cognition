
##################################
## Multiply Impute Missing Data ##
##################################

#written by: Emma Gause 
#Date: 12/15/22
#Last updated: 08/30/23

#load libraries:
library("tidyverse")
library("dplyr")
library("stringr")
library("mice") 
library("mitml")

#Create path to directory
datadir <- "/Users/egause/Library/CloudStorage/OneDrive-BostonUniversity/ClimLab/MPJ/KHANDLE/Analysis/SENAS/"

#read in cohort data
dat <- readRDS(paste0(datadir, "Analysis_Data_081823.rds"))

##------------------------------------------------------------------------##

str(dat)

#EXTRACT DATA TO INCLUDE IN IMPUTATION MODEL
#Don't include the MRI vars - too much missingness
#extract the baseline and time-varying data separately (Time-varying needs to be transformed to wide)
base <- dat %>% select(id, waveseq, khandle, race_fact, female, age_bl_x, exc_health, social, 
                       married_partner, inc_gt55k, college, NIHTLBX_depr_theta,
                       ever_smoke, heavy_drink_fact, daily_physical)
vary <- dat %>% select(id, waveseq, yrsbl_x, exec_z:sem_z, setting)

#convert time-varying to wide
str(vary)
vary_wide <- reshape(vary, idvar = "id", timevar = "waveseq", direction = "wide")

#remove sem.3 because too few measures taken
str(vary_wide)
vary_wide <- vary_wide %>% select(-sem_z.3)

#merge with baseline variables
base <- base %>% filter(waveseq==1) %>% select(-waveseq)
midat <- merge(base, vary_wide, by = "id", all = TRUE)

#make binary as logical
str(midat)
midat <- midat %>% mutate(khandle = as.logical(khandle),
                          female = as.logical(female),
                          exc_health = as.logical(exc_health),
                          social = as.logical(social),
                          married_partner = as.logical(married_partner),
                          inc_gt55k = as.logical(inc_gt55k),
                          college = as.logical(college),
                          daily_physical = as.logical(daily_physical),
                          setting.1 = as.factor(setting.1),
                          setting.2 = as.factor(setting.2),
                          setting.3 = as.factor(setting.3))

str(midat)
summary(midat$setting.1)
summary(midat$setting.2)
summary(midat$setting.3)

#there is only 1 NA in setting.1 - causing dimensionality errors. 
  # Set to most likely value and then we can run mice with no error
midat$setting.1[is.na(midat$setting.1)] <- "clinic"

##------------------------------------------------------------------------##

#remove id since we don't want this to inform the prediction (but arrange first to we know what to merge back in)
midat <- midat %>% arrange(id)
#lsids <- midat %>% select(id)
#midat <- midat %>% select(-id)


#Imputation methods --> specify the transformation of the data 
#Leave empty method for id
colnames(midat) 
method<-c("", "logreg", "polyreg", "logreg", "norm.nob", rep("logreg", 5),
          "norm.nob", rep("logreg", 3), rep("norm.nob", 4), "polyreg",
          rep("norm.nob", 4), "polyreg", rep("norm.nob", 3), "polyreg")
#logistic regression for categorical variables, norm.nob for continuous variables, 
# polygreg for polytomous variables, etc.

#perform the imputation for 10 datasets
miceout<-mice(midat, m=10, maxit=50, seed=500, method=method)

#Assess performance
summary(miceout)
miceout$predictorMatrix

#look at imputed values for exposure & outcomes
summary(miceout$imp$NIHTLBX_depr_theta)
summary(miceout$imp$exec_z.2)
summary(miceout$imp$vrmem_z.2)
summary(miceout$imp$sem_z.2)

#look at convergence
plot(miceout)

#look at distrinutions of observed vs. imputed values
densityplot(miceout)

##------------------------------------------------------------------------##

#get each dataset on it's own to create a list to call in the for loop
mi01 <- complete(miceout,1)
mi02 <- complete(miceout,2)
mi03 <- complete(miceout,3)
mi04 <- complete(miceout,4)
mi05 <- complete(miceout,5)
mi06 <- complete(miceout,6)
mi07 <- complete(miceout,7)
mi08 <- complete(miceout,8)
mi09 <- complete(miceout,9)
mi10 <- complete(miceout,10)

micel <- list(mi01, mi02, mi03, mi04, mi05, mi06, mi07, mi08, mi09, mi10)

#create an empty list to store our results
micelist <- list()

#convert to long - one at a time
for (i in seq(length(micel))) {
  
  #extract data
  data <- micel[[i]]
  
  #get time-invariant baseline data
  timeinvar <- data %>% select(id, khandle, race_fact, female, age_bl_x, exc_health, social, 
                               married_partner, inc_gt55k, college, NIHTLBX_depr_theta,
                               ever_smoke, heavy_drink_fact, daily_physical)
  
  
  #not time-varying
  timevar <- data %>% select(id, yrsbl_x.1, yrsbl_x.2, yrsbl_x.3,
                             exec_z.1, exec_z.2, exec_z.3, 
                             vrmem_z.1, vrmem_z.2, vrmem_z.3,
                             sem_z.1, sem_z.2, setting.1, setting.2, setting.3)
  
  #extract to pivot longer
  yrs <- timevar %>% select(id, contains("yrsbl_x"))
  ex <- timevar %>% select(id, contains("exec"))
  vr <- timevar %>% select(id, contains("vrmem"))
  sem <- timevar %>% select(id, contains("sem"))
  setting <- timevar %>% select(id, contains("setting"))
  
  #pivot longer
  yrsl <- yrs %>% pivot_longer(contains("yrsbl_x"), names_to = "waveseq", values_to="yrsbl_x")
  yrsl$waveseq <- substr(yrsl$waveseq, 9,9) #fix waveseq value to integer
  exl <- ex %>% pivot_longer(contains("exec"), names_to = "waveseq", values_to="exec_z")
  exl$waveseq <- substr(exl$waveseq, 8,8)
  vrl <- vr %>% pivot_longer(contains("vrmem"), names_to = "waveseq", values_to="vrmem_z")
  vrl$waveseq <- substr(vrl$waveseq, 9,9)
  seml <- sem %>% pivot_longer(contains("sem"), names_to = "waveseq", values_to="sem_z")
  seml$waveseq <- substr(seml$waveseq, 7,7)
  setting <- setting %>% pivot_longer(contains("setting"), names_to = "waveseq", values_to="setting")
  setting$waveseq <- substr(setting$waveseq, 9,9)
  
  #merge together
  x1 <- merge(yrsl, exl, by = c("id", "waveseq"), all = TRUE)
  x2 <- merge(x1, vrl, by = c("id", "waveseq"), all = TRUE)
  x3 <- merge(x2, seml, by = c("id", "waveseq"), all = TRUE)
  x4 <- merge(x3, setting, by = c("id", "waveseq"), all = TRUE)
  
  #merge to time-invariant
  datl <- merge(timeinvar, x4, by = "id", all = TRUE)
  
  #save it to list again
  name_i <- paste(i)
  micelist[[name_i]] <- datl
  
}

str(micelist)

#make list an mi object
implist <- as.mitml.list(micelist)

#save implist as an .RData
saveRDS(implist, paste0(datadir, "MImputed_Data_List_083023.rds"))

