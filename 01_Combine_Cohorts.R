
#######################################
## Combine KHANDLE, and STAR Cohorts ##
#######################################

#written by: Emma Gause (from code by Marcia P. Jimenez)
#Date: 10/26/22
#Last updated:05/17/23

#load libraries:
library("tidyverse")
library("dplyr")
library("stringr")
library("tableone")
library("ggplot2")
library("ggthemes")

#Create path to directory
datadir <- "/Users/egause/Library/CloudStorage/OneDrive-BostonUniversity/ClimLab/MPJ/KHANDLE/"

#pull in data:
merged <- readRDS(paste0(datadir, "Raw_Data/Merged KHANDLE STAR LA90/Data/khan_star_la90_senas_ecog_longitudinal.rds"))
k <- read.csv(paste0(datadir, "Raw_Data/KHANDLE/Data/khandle_all_waves_20210818.csv"))
star <- read.csv(paste0(datadir, "Raw_Data/STAR/Data/star_all_waves_20220309.csv"))


#There is an extra participant in KHANDLE merged vs. KHANDLE raw data - this seems to be an error
km <- merged %>% filter(study == "KHANDLE") %>% select(id) %>% unique()
kmids <- as.vector(km$id)
k$id <- paste0("K", sprintf("%06d",k$STUDYID)) #format the ids
kids <- as.vector(k$id)
notin <- km %>% filter(!id %in% kids) # ID is K001160 -- zero data recorded, remove empty rows 

merged <- merged %>% filter(id != "K001160")

##------------------------------------------------------------------------##

length(unique(merged$id)) # N=3443 unique Ids 

#must remove the LA90 cohort from merged...
str(merged)
table(merged$study)

cohort <- merged %>% filter(study!="LA90")
length(unique(cohort$id)) # N=2476 unique Ids 

#not all IDs have all rows... 
#Create a wave # ID that always begins with 1
#how many rows per ID
cohort <- cohort %>% group_by(id) %>% mutate(waveseq = row_number(),
                                             waves_tot = n())
table(cohort$waves_tot, useNA = "ifany") # up to 3, 74 with only 1

#missing race?
table(cohort$race_summary, cohort$study, useNA = "ifany") #missing for 5 
cohort$race_summary[cohort$race_summary==""|cohort$race_summary=="Refused/Missing"] <- NA

#is race missing for all rows if it is missing, or can it be supplemented with other rows?
test <- cohort %>% select(id, race_summary, wave) %>% group_by(id) %>% filter(any(is.na(race_summary)))
listid <- as.vector(test$id)
testing <- cohort %>% filter(id%in%listid) # missing for all rows 


#remove participants with missing race or native american race
  # do this with wave 1 data to get number of person (not waves) removed
wave1 <- cohort %>% filter(waveseq==1)
table(wave1$race_summary, useNA = "ifany")
wave1 <- wave1 %>% filter(race_summary!="Native American"&
                          !is.na(race_summary))

#remove missing from long data 
listids <- as.vector(wave1$id)
data <- cohort %>% filter(id%in%listids)
length(unique(data$id)) # N=2469 unique Ids (perfect)
rm(test, testing, listid, listids, wave1, km, kmids, kids, notin)


##------------------------------------------------------------------------##

#IDs are 7 characters long, prefixed with study letter, padded with zeros

#Pad study IDs for KHANDLE data
#k$STUDYID
k$id <- paste0("K", sprintf("%06d",k$STUDYID))

#Pad study IDs for STAR data
#star$STUDYID
star$id <- paste0("S", sprintf("%06d",star$STUDYID))

#K "x" columns are blank -- remove them
k <- k %>% select(-X:-X.86)

##------------------------------------------------------------------------##

#Convert STAR PROMIS questions into KHANDLE score -- They used PROMIS 8a 
colnames(star)
#PROMIS: 10, 11, 12, 14, 19, 24, 28, 30
#score of 88 is refused/missing -- set to missing [there is probably a more efficient way of doing this]
star$W1_PROMIS_10[star$W1_PROMIS_10==88] <- NA
star$W1_PROMIS_11[star$W1_PROMIS_11==88] <- NA
star$W1_PROMIS_12[star$W1_PROMIS_12==88] <- NA
star$W1_PROMIS_14[star$W1_PROMIS_14==88] <- NA
star$W1_PROMIS_19[star$W1_PROMIS_19==88] <- NA
star$W1_PROMIS_24[star$W1_PROMIS_24==88] <- NA
star$W1_PROMIS_28[star$W1_PROMIS_28==88] <- NA
star$W1_PROMIS_30[star$W1_PROMIS_30==88] <- NA

star$W2_PROMIS_10[star$W2_PROMIS_10==88] <- NA
star$W2_PROMIS_11[star$W2_PROMIS_11==88] <- NA
star$W2_PROMIS_12[star$W2_PROMIS_12==88] <- NA
star$W2_PROMIS_14[star$W2_PROMIS_14==88] <- NA
star$W2_PROMIS_19[star$W2_PROMIS_19==88] <- NA
star$W2_PROMIS_24[star$W2_PROMIS_24==88] <- NA
star$W2_PROMIS_28[star$W2_PROMIS_28==88] <- NA
star$W2_PROMIS_30[star$W2_PROMIS_30==88] <- NA

star$W3_PROMIS_10[star$W3_PROMIS_10==88] <- NA
star$W3_PROMIS_11[star$W3_PROMIS_11==88] <- NA
star$W3_PROMIS_12[star$W3_PROMIS_12==88] <- NA
star$W3_PROMIS_14[star$W3_PROMIS_14==88] <- NA
star$W3_PROMIS_19[star$W3_PROMIS_19==88] <- NA
star$W3_PROMIS_24[star$W3_PROMIS_24==88] <- NA
star$W3_PROMIS_28[star$W3_PROMIS_28==88] <- NA
star$W3_PROMIS_30[star$W3_PROMIS_30==88] <- NA

#add up questions to get raw score
test <- star %>% select(STUDYID, contains("W1_PROMIS"))
star <- star %>% rowwise() %>%
  mutate(W1_RAW_PROMIS = sum(W1_PROMIS_10, W1_PROMIS_11, W1_PROMIS_12, W1_PROMIS_14,
                                            W1_PROMIS_19, W1_PROMIS_24, W1_PROMIS_28, W1_PROMIS_30,
                                            na.rm = FALSE), 
                        W2_RAW_PROMIS = sum(W2_PROMIS_10, W2_PROMIS_11, W2_PROMIS_12, W2_PROMIS_14,
                                            W2_PROMIS_19, W2_PROMIS_24, W2_PROMIS_28, W2_PROMIS_30,
                                            na.rm = FALSE),
                        W3_RAW_PROMIS = sum(W3_PROMIS_10, W3_PROMIS_11, W3_PROMIS_12, W3_PROMIS_14,
                                            W3_PROMIS_19, W3_PROMIS_24, W3_PROMIS_28, W3_PROMIS_30,
                                            na.rm = FALSE))

#ungroup from rowwise
star <- ungroup(star)

#set zeros to missing (no scores recorded for any) -- DON'T NEED THIS IF na.rm=FALSE!
#star$W1_RAW_PROMIS[star$W1_RAW_PROMIS==0] <- NA
#star$W2_RAW_PROMIS[star$W2_RAW_PROMIS==0] <- NA
#star$W3_RAW_PROMIS[star$W3_RAW_PROMIS==0] <- NA

table(is.na(star$W1_RAW_PROMIS)) #39 missing
table(is.na(star$W2_RAW_PROMIS)) #382 missing (approximately half!)
table(is.na(star$W3_RAW_PROMIS)) #764 missing (100%)

##------------------------------------------------------------------------##

#Merged dataset only contains SOME variables - we need to supplement with the wide data files
#what data are we gonna need?
colnames(data)
#depression
#household income
#social network scale: married or living with partner; weekly contact with friends/neighbors;weekly contact with a child; participated in a volunteer activity in the past year
#index of medical illness: hypertension, diabetes, cancer, pulmonary disease, heart disease and stroke [WE DON'T HAVE THESE]
#CA_SOC: 1 = Every day, or almost every day || 2 = Several times a week || 3 = Several times a month || 4 = Several times a year || 5 = Never || 88 = Refused


#use "contains" to get all wave vars in wide datasets
kdat <- k %>% select(STUDYID, id,
                     contains("INCOME_EST"),
                     contains("INCOME_RANGE"),
                     contains("FRIENDS_SEE"),
                     contains("CA_SOC"), 
                     contains("MARITAL_STATUS"), #social network scale
                     contains("VOLUNTEER_WORK"),
                     contains("VOLUNTEER_FREQ"), #social network scale
                     contains("NIHTLBX_depr_raw"), 
                     contains("NIHTLBX_depr_theta"), # exposure
                     contains("HEALTH"),
                     contains("SMK"), #smoking
                     contains("ALC"), #alcohol
                     contains("_PA_"), #physical activity questions
                     contains("COMPLETED_AT")) # where exam took place

sdat <- star %>% select(STUDYID, id, 
                        contains("INCOME_EST"),
                        contains("INCOME_RANGE"), 
                        contains("FRIENDS_SEE"),
                        contains("CA_SOC"),  
                        contains("MARITAL_STATUS"), #social network scale
                        contains("VOLUNTEER_WORK"),
                        contains("VOLUNTEER_FREQ"), #social network scale
                        contains("RAW_PROMIS"), # exposure
                        contains("HEALTH"),
                        contains("SMK"), #smoking
                        contains("ALC"), #alcohol
                        contains("_PA_"), #physical activity questions
                        contains("COMPLETED_AT")) # where exam took place

kdat <- kdat %>% select(!contains("_TEXT"))
kdat <- kdat %>% select(!contains("_SENAS_pa_se"))
kdat <- kdat %>% select(!contains("_SMK_QUIT"))

sdat <- sdat %>% select(!contains("_TEXT"))
sdat <- sdat %>% select(!contains("_SENAS_pa_se"))
sdat <- sdat %>% select(!contains("_SMK_QUIT"))
sdat <- sdat %>% select(!contains("W3_PROCEED_INTVW_HEALTH"))

str(kdat)
str(sdat)


#convert kdat and sdat to long format (why is this always so hard...)
kdat1 <- kdat %>% select(STUDYID, id, contains("W1_")) %>% mutate(wave =1)
kdat2 <- kdat %>% select(STUDYID, id, contains("W2_")) %>% mutate(wave =2,
                                                                  W2_SMK = NA)
kdat3 <- kdat %>% select(STUDYID, id, contains("W3_")) %>% mutate(wave =3,
                                                                  W3_SMK = NA)

#need to rearrange columns so they are in the same order
kdat1 <- kdat1 %>% select(STUDYID:W1_ALC_VOL, W1_PA_HVY_WRK, W1_PA_LT_WRK,
                          W1_PA_VIG_EX, W1_PA_LT_EX, W1_PA_VIG_HSE, W1_PA_LT_HSE, 
                          W1_COMPLETED_AT, wave)
kdat2 <- kdat2 %>% select(STUDYID:W2_HEALTH, W2_SMK, W2_SMK_NOW:W2_ALC_VOL,
                          W2_PA_HVY_WRK, W2_PA_LT_WRK,
                          W2_PA_VIG_EX, W2_PA_LT_EX, W2_PA_VIG_HSE, W2_PA_LT_HSE, 
                          W2_COMPLETED_AT, wave)
kdat3 <- kdat3 %>% select(STUDYID:W3_HEALTH, W3_SMK, W3_SMK_NOW:W3_ALC_VOL,
                          W3_PA_HVY_WRK, W3_PA_LT_WRK,
                          W3_PA_VIG_EX, W3_PA_LT_EX, W3_PA_VIG_HSE, W3_PA_LT_HSE, 
                          W3_COMPLETED_AT, wave)
colnames(kdat1)
colnames(kdat2)
colnames(kdat3)
klong <- rbind(kdat1, 
                setNames(kdat2, names(kdat1)),
                setNames(kdat3, names(kdat1)))

# now convert sdat to long format
sdat1 <- sdat %>% select(STUDYID, id, contains("W1_")) %>% mutate(wave =1)
sdat2 <- sdat %>% select(STUDYID, id, contains("W2_")) %>% mutate(wave =2,
                                                                  W2_SMK = NA)
sdat3 <- sdat %>% select(STUDYID, id, contains("W3_")) %>% mutate(wave =3,
                                                                  W3_SMK = NA,
                                                                  W3_INCOME_EST = NA,
                                                                  W3_INCOME_RANGE = NA)

#need to rearrange columns so they are in the same order
colnames(sdat1)
colnames(sdat2)
colnames(sdat3)
sdat1 <- sdat1 %>% select(STUDYID:id, W1_INCOME_EST, W1_INCOME_RANGE, W1_FRIENDS_SEE:W1_HEALTH,
                          W1_SMK, W1_SMK_NOW:W1_ALC_VOL, W1_PA_HVY_WRK, W1_PA_LT_WRK,
                          W1_PA_VIG_EX, W1_PA_LT_EX, W1_PA_VIG_HSE, W1_PA_LT_HSE, 
                          W1_COMPLETED_AT, wave)
sdat2 <- sdat2 %>% select(STUDYID:id, W2_INCOME_EST, W2_INCOME_RANGE, W2_FRIENDS_SEE:W2_HEALTH,
                          W2_SMK, W2_SMK_NOW:W2_ALC_VOL, W2_PA_HVY_WRK, W2_PA_LT_WRK,
                          W2_PA_VIG_EX, W2_PA_LT_EX, W2_PA_VIG_HSE, W2_PA_LT_HSE, 
                          W2_COMPLETED_AT, wave)
sdat3 <- sdat3 %>% select(STUDYID:id, W3_INCOME_EST, W3_INCOME_RANGE, W3_FRIENDS_SEE:W3_HEALTH,
                          W3_SMK, W3_SMK_NOW:W3_ALC_VOL, W3_PA_HVY_WRK, W3_PA_LT_WRK,
                          W3_PA_VIG_EX, W3_PA_LT_EX, W3_PA_VIG_HSE, W3_PA_LT_HSE, 
                          W3_COMPLETED_AT, wave)
colnames(sdat1)
colnames(sdat2)
colnames(sdat3)
slong <- rbind(sdat1, 
               setNames(sdat2, names(sdat1)),
               setNames(sdat3, names(sdat1)))

rm(kdat, sdat, kdat1, sdat1, kdat2, sdat2, kdat3, sdat3)

#now remove wave from column names since data is long (i.e. waves are rows)
colnames(klong) = gsub("W1_", "", colnames(klong))
colnames(slong) = gsub("W1_", "", colnames(slong))

colnames(klong)
colnames(slong)
#all the same except the PROMIS data which have two diff formats between datasets

##------------------------------------------------------------------------##
## Convert STAR raw depression scores to scores using conversion
convert <- read.csv(paste0(datadir, "Analysis/PROMIS_8a_Conversion.csv"))
slongx <- merge(slong, convert, by.x = "RAW_PROMIS", by.y = "raw_score", all.x = TRUE)
hist(slongx$RAW_PROMIS)
hist(slongx$t_score)
table(slongx$RAW_PROMIS, slongx$t_score)

#now convert t-scores to theta: t-score = (theta*10)+50
# therefore: theta = (t-score - 50)/10
slongx$promis_theta = (slongx$t_score-50)/10


summary(slongx$promis_theta[slongx$wave==1])
summary(klong$NIHTLBX_depr_theta[klong$wave==1])

brks = seq(-2, 4, .25)
hist(klong$NIHTLBX_depr_theta, breaks = brks)
hist(slongx$promis_theta, breaks = brks)

hist(slongx$t_score)
mean(slongx$t_score, na.rm = TRUE) #46.60
#lots of people in STAR with low depression theta

##------------------------------------------------------------------------##

#time to rbind and then merge to data
colnames(klong)
colnames(slongx)
#align columns
klong <- klong %>% select(-NIHTLBX_depr_raw)
slongx <- slongx %>% select(STUDYID:VOLUNTEER_FREQ, 
                            NIHTLBX_depr_theta=promis_theta, HEALTH:PA_LT_HSE, COMPLETED_AT, wave)
extra <- rbind(klong, slongx)

#merge em'!
colnames(extra)
dat <- merge(data, extra, by = c("id", "wave"), all.x = TRUE) # all good!

##------------------------------------------------------------------------##

#If age at baseline is missing, set it to age when waveseq==1
summary(dat$age_bl)

dat <- dat %>% group_by(id) %>% mutate(min_age = aget89[waveseq==1]) %>% ungroup()
dat <- dat %>% mutate(age_bl_x = if_else(is.na(age_bl), min_age, age_bl))


#SAVE BASELINE COHORT DATA
saveRDS(dat, paste0(datadir, "Analysis/SENAS/KHANDLE_STAR_Cohorts_081823.rds"))


##------------------------------------------------------------------------##
