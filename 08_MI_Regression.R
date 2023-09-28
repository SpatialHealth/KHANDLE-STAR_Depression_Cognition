
###################################################
## Multi-Level Regression Models on Imputed Data ##
###################################################

#written by: Emma Gause 
#Date: 12/15/22
#Last updated: 08/30/23

#load libraries:
library("tidyverse")
library("dplyr")
library("mice") 
library("miceadds")
library("mitml")
library("lme4")

#Create path to directory
datadir <- "/Users/egause/Library/CloudStorage/OneDrive-BostonUniversity/ClimLab/MPJ/KHANDLE/Analysis/SENAS/"

#read in multiply imputed data
dat <- readRDS(paste0(datadir, "MImputed_Data_List_083023.rds"))
str(dat)

##------------------------------------------------------------------------##

########################################################
#create MI sets
#create mids/mitml objects from lists
mimids <- datalist2mids(dat)
mimitml <- as.mitml.list(dat)

#Get race-specific data

#FOR MI DATA
#create empty lists for results
asianlist <- list()
blacklist <- list()
latinlist <- list()
whitelist <- list()

#filter all lists for each race/eth strata
for (i in seq(length(dat))) {
  #extract data
  data <- dat[[i]]
  
  #filter to Asian
  dataa <- data %>% filter(race_fact=="Asian")
  datab <- data %>% filter(race_fact=="Black")
  datal <- data %>% filter(race_fact=="LatinX")
  dataw <- data %>% filter(race_fact=="White")
  
  #save it to list again
  name_i <- paste(i)
  asianlist[[name_i]] <- dataa
  blacklist[[name_i]] <- datab
  latinlist[[name_i]] <- datal
  whitelist[[name_i]] <- dataw
}

#create mids objects from lists
adat <- datalist2mids(asianlist)
bdat <- datalist2mids(blacklist)
ldat <- datalist2mids(latinlist)
wdat <- datalist2mids(whitelist)

#create mitml objects
adat2 <- as.mitml.list(asianlist)
bdat2 <- as.mitml.list(blacklist)
ldat2 <- as.mitml.list(latinlist)
wdat2 <- as.mitml.list(whitelist)

##------------------------------------------------------------------------##

####################
## SENAS MEASURES ##
####################

#analysis on full sample - NO TIME INTERACTION
miexc <- with(mimitml, lmer(exec_z~NIHTLBX_depr_theta+yrsbl_x+ race_fact+ 
                              female+age_bl_x+college+inc_gt55k+married_partner+
                              (1 | id)))
mixests <- testEstimates(miexc, extra.pars=TRUE)
mixconfints <- confint.mitml.testEstimates(mixests)
oee <- c(round(mixests$estimates[2, "Estimate"], 2), round(mixconfints[2, "2.5 %"], 2), round(mixconfints[2, "97.5 %"], 2)) 

mivrbep <- with(mimitml, lmer(vrmem_z~NIHTLBX_depr_theta+yrsbl_x+ race_fact+ 
                                female+age_bl_x+college+inc_gt55k+married_partner+
                                (1 | id)))
mivests <- testEstimates(mivrbep, extra.pars=TRUE)
mivconfints <- confint.mitml.testEstimates(mivests)
ove <- c(round(mivests$estimates[2, "Estimate"], 2), round(mivconfints[2, "2.5 %"], 2), round(mivconfints[2, "97.5 %"], 2)) 

misem_z <- with(mimitml, lmer(sem_z~NIHTLBX_depr_theta+yrsbl_x+ race_fact+ 
                              female+age_bl_x+college+inc_gt55k+married_partner+
                              (1 | id)))
misests <- testEstimates(misem_z, extra.pars=TRUE)
misconfints <- confint.mitml.testEstimates(misests)
ose <- c(round(misests$estimates[2, "Estimate"], 2), round(misconfints[2, "2.5 %"], 2), round(misconfints[2, "97.5 %"], 2)) 

rbind(oee, ove, ose)



#analysis on full sample - W/ TIME INTERACTION
miexct <- with(mimitml, lmer(exec_z~NIHTLBX_depr_theta*yrsbl_x+ race_fact+ 
                              female+age_bl_x+college+inc_gt55k+married_partner+
                              (1 | id)))
mixestst <- testEstimates(miexct, extra.pars=TRUE)
mixconfintst <- confint.mitml.testEstimates(mixestst)
oete <- c(round(mixestst$estimates[12, "Estimate"], 2), round(mixconfintst[12, "2.5 %"], 2), round(mixconfintst[12, "97.5 %"], 2))

mivrbept <- with(mimitml, lmer(vrmem_z~NIHTLBX_depr_theta*yrsbl_x+ race_fact+ 
                                female+age_bl_x+college+inc_gt55k+married_partner+
                                (1 | id)))
mivestst <- testEstimates(mivrbept, extra.pars=TRUE)
mivconfintst <- confint.mitml.testEstimates(mivestst)
ovte <- c(round(mivestst$estimates[12, "Estimate"], 2), round(mivconfintst[12, "2.5 %"], 2), round(mivconfintst[12, "97.5 %"], 2))

misem_zt <- with(mimitml, lmer(sem_z~NIHTLBX_depr_theta*yrsbl_x+ race_fact+ 
                              female+age_bl_x+college+inc_gt55k+married_partner+
                              (1 | id)))
misestst <- testEstimates(misem_zt, extra.pars=TRUE)
misconfintst <- confint.mitml.testEstimates(misestst)
oste <- c(round(misestst$estimates[12, "Estimate"], 2), round(misconfintst[12, "2.5 %"], 2), round(misconfintst[12, "97.5 %"], 2))

rbind(oete, ovte, oste)

##------------------------------------------------------------------------##

#STRATIFY BY RACE/ETH -- SENAS measures


## Asian [WITHOUT TIME INTERACTION]
aexc <- with(adat2, lmer(exec_z~NIHTLBX_depr_theta+yrsbl_x+
                           female+age_bl_x+college+inc_gt55k+married_partner+
                           (1 | id)))
axests <- testEstimates(aexc, extra.pars=TRUE)
axconfints <- confint.mitml.testEstimates(axests)
aee <- c(round(axests$estimates[2, "Estimate"], 2), round(axconfints[2, "2.5 %"], 2), round(axconfints[2, "97.5 %"], 2)) 

avrm <- with(adat2, lmer(vrmem_z~NIHTLBX_depr_theta+yrsbl_x+
                           female+age_bl_x+college+inc_gt55k+married_partner+
                           (1 | id)))
avests <- testEstimates(avrm, extra.pars=TRUE)
avconfints <- confint.mitml.testEstimates(avests)
ave <- c(round(avests$estimates[2, "Estimate"], 2), round(avconfints[2, "2.5 %"], 2), round(avconfints[2, "97.5 %"], 2)) 

asem_z <- with(adat2, lmer(sem_z~NIHTLBX_depr_theta+yrsbl_x+
                           female+age_bl_x+college+inc_gt55k+married_partner+
                           (1 | id)))
asests <- testEstimates(asem_z, extra.pars=TRUE)
asconfints <- confint.mitml.testEstimates(asests)
ase <- c(round(asests$estimates[2, "Estimate"], 2), round(asconfints[2, "2.5 %"], 2), round(asconfints[2, "97.5 %"], 2)) 

rbind(aee, ave, ase)


## Asian [WITH TIME INTERACTION]
aexc <- with(adat2, lmer(exec_z~NIHTLBX_depr_theta*yrsbl_x+
                           female+age_bl_x+college+inc_gt55k+married_partner+
                           (1 | id)))
axests <- testEstimates(aexc, extra.pars=TRUE)
axconfints <- confint.mitml.testEstimates(axests)
aete <- c(round(axests$estimates[9, "Estimate"], 2), round(axconfints[9, "2.5 %"], 2), round(axconfints[9, "97.5 %"], 2))

avrm <- with(adat2, lmer(vrmem_z~NIHTLBX_depr_theta*yrsbl_x+
                           female+age_bl_x+college+inc_gt55k+married_partner+
                           (1 | id)))
avests <- testEstimates(avrm, extra.pars=TRUE)
avconfints <- confint.mitml.testEstimates(avests)
avte <- c(round(avests$estimates[9, "Estimate"], 2), round(avconfints[9, "2.5 %"], 2), round(avconfints[9, "97.5 %"], 2))

asem_z <- with(adat2, lmer(sem_z~NIHTLBX_depr_theta*yrsbl_x+
                           female+age_bl_x+college+inc_gt55k+married_partner+
                           (1 | id)))
asests <- testEstimates(asem_z, extra.pars=TRUE)
asconfints <- confint.mitml.testEstimates(asests)
aste <- c(round(asests$estimates[9, "Estimate"], 2), round(asconfints[9, "2.5 %"], 2), round(asconfints[9, "97.5 %"], 2)) 

rbind(aete, avte, aste)


##-------------------------------------##

## Black [WITHOUT TIME INTERACTION]
bexc <- with(bdat2, lmer(exec_z~NIHTLBX_depr_theta+yrsbl_x+
                           female+age_bl_x+college+inc_gt55k+married_partner+
                           (1 | id)))
bxests <- testEstimates(bexc, extra.pars=TRUE)
bxconfints <- confint.mitml.testEstimates(bxests)
bee <- c(round(bxests$estimates[2, "Estimate"], 2), round(bxconfints[2, "2.5 %"], 2), round(bxconfints[2, "97.5 %"],2)) 

bvrm <- with(bdat2, lmer(vrmem_z~NIHTLBX_depr_theta+yrsbl_x+
                           female+age_bl_x+college+inc_gt55k+married_partner+
                           (1 | id)))
bvests <- testEstimates(bvrm, extra.pars=TRUE)
bvconfints <- confint.mitml.testEstimates(bvests)
bve <- c(round(bvests$estimates[2, "Estimate"], 2), round(bvconfints[2, "2.5 %"], 2), round(bvconfints[2, "97.5 %"],2)) 

bsem_z <- with(bdat2, lmer(sem_z~NIHTLBX_depr_theta+yrsbl_x+
                           female+age_bl_x+college+inc_gt55k+married_partner+
                           (1 | id)))
bsests <- testEstimates(bsem_z, extra.pars=TRUE)
bsconfints <- confint.mitml.testEstimates(bsests)
bse <- c(round(bsests$estimates[2, "Estimate"],2), round(bsconfints[2, "2.5 %"], 2), round(bsconfints[2, "97.5 %"],2)) 

rbind(bee, bve, bse)


## Black [WITH TIME INTERACTION]
bexc <- with(bdat2, lmer(exec_z~NIHTLBX_depr_theta*yrsbl_x+
                           female+age_bl_x+college+inc_gt55k+married_partner+
                           (1 | id)))
bxests <- testEstimates(bexc, extra.pars=TRUE)
bxconfints <- confint.mitml.testEstimates(bxests)
bete <- c(round(bxests$estimates[9, "Estimate"],2), round(bxconfints[9, "2.5 %"], 2), round(bxconfints[9, "97.5 %"],2)) 

bvrm <- with(bdat2, lmer(vrmem_z~NIHTLBX_depr_theta*yrsbl_x+
                           female+age_bl_x+college+inc_gt55k+married_partner+
                           (1 | id)))
bvests <- testEstimates(bvrm, extra.pars=TRUE)
bvconfints <- confint.mitml.testEstimates(bvests)
bvte <- c(round(bvests$estimates[9, "Estimate"],2), round(bvconfints[9, "2.5 %"], 2), round(bvconfints[9, "97.5 %"],2)) 

bsem_z <- with(bdat2, lmer(sem_z~NIHTLBX_depr_theta*yrsbl_x+
                           female+age_bl_x+college+inc_gt55k+married_partner+
                           (1 | id)))
bsests <- testEstimates(bsem_z, extra.pars=TRUE)
bsconfints <- confint.mitml.testEstimates(bsests)
bste <- c(round(bsests$estimates[9, "Estimate"],2), round(bsconfints[9, "2.5 %"], 2), round(bsconfints[9, "97.5 %"],2)) 

rbind(bete, bvte, bste)


##-------------------------------------##


## LatinX [WITHOUT TIME INTERACTION]
lexc <- with(ldat2, lmer(exec_z~NIHTLBX_depr_theta+yrsbl_x+
                           female+age_bl_x+college+inc_gt55k+married_partner+
                           (1 | id)))
lxests <- testEstimates(lexc, extra.pars=TRUE)
lxconfints <- confint.mitml.testEstimates(lxests)
lee <- c(round(lxests$estimates[2, "Estimate"],2), round(lxconfints[2, "2.5 %"], 2), round(lxconfints[2, "97.5 %"],2)) 

lvrm <- with(ldat2, lmer(vrmem_z~NIHTLBX_depr_theta+yrsbl_x+
                           female+age_bl_x+college+inc_gt55k+married_partner+
                           (1 | id)))
lvests <- testEstimates(lvrm, extra.pars=TRUE)
lvconfints <- confint.mitml.testEstimates(lvests)
lve <- c(round(lvests$estimates[2, "Estimate"], 2), round(lvconfints[2, "2.5 %"], 2), round(lvconfints[2, "97.5 %"],2)) 

lsem_z <- with(ldat2, lmer(sem_z~NIHTLBX_depr_theta+yrsbl_x+
                           female+age_bl_x+college+inc_gt55k+married_partner+
                           (1 | id)))
lsests <- testEstimates(lsem_z, extra.pars=TRUE)
lsconfints <- confint.mitml.testEstimates(lsests)
lse <- c(round(lsests$estimates[2, "Estimate"], 2), round(lsconfints[2, "2.5 %"], 2), round(lsconfints[2, "97.5 %"],2)) 

rbind(lee, lve, lse)


## LatinX [WITH TIME INTERACTION]
lexc <- with(ldat2, lmer(exec_z~NIHTLBX_depr_theta*yrsbl_x+
                           female+age_bl_x+college+inc_gt55k+married_partner+
                           (1 | id)))
lxests <- testEstimates(lexc, extra.pars=TRUE)
lxconfints <- confint.mitml.testEstimates(lxests)
lete <- c(round(lxests$estimates[9, "Estimate"], 2), round(lxconfints[9, "2.5 %"], 2), round(lxconfints[9, "97.5 %"],2)) 

lvrm <- with(ldat2, lmer(vrmem_z~NIHTLBX_depr_theta*yrsbl_x+
                           female+age_bl_x+college+inc_gt55k+married_partner+
                           (1 | id)))
lvests <- testEstimates(lvrm, extra.pars=TRUE)
lvconfints <- confint.mitml.testEstimates(lvests)
lvte <- c(round(lvests$estimates[9, "Estimate"], 2), round(lvconfints[9, "2.5 %"], 2), round(lvconfints[9, "97.5 %"],2)) 

lsem_z <- with(ldat2, lmer(sem_z~NIHTLBX_depr_theta*yrsbl_x+
                           female+age_bl_x+college+inc_gt55k+married_partner+
                           (1 | id)))
lsests <- testEstimates(lsem_z, extra.pars=TRUE)
lsconfints <- confint.mitml.testEstimates(lsests)
lste <- c(round(lsests$estimates[9, "Estimate"], 2), round(lsconfints[9, "2.5 %"], 2), round(lsconfints[9, "97.5 %"],2)) 

rbind(lete, lvte, lste)


##-------------------------------------##


## White [WITHOUT TIME INTERACTION]
wexc <- with(wdat2, lmer(exec_z~NIHTLBX_depr_theta+yrsbl_x+
                           female+age_bl_x+college+inc_gt55k+married_partner+
                           (1 | id)))
wxests <- testEstimates(wexc, extra.pars=TRUE)
wxconfints <- confint.mitml.testEstimates(wxests)
wee <- c(round(wxests$estimates[2, "Estimate"],2), round(wxconfints[2, "2.5 %"], 2), round(wxconfints[2, "97.5 %"],2)) 

wvrm <- with(wdat2, lmer(vrmem_z~NIHTLBX_depr_theta+yrsbl_x+
                           female+age_bl_x+college+inc_gt55k+married_partner+
                           (1 | id)))
wvests <- testEstimates(wvrm, extra.pars=TRUE)
wvconfints <- confint.mitml.testEstimates(wvests)
wve <- c(round(wvests$estimates[2, "Estimate"], 2), round(wvconfints[2, "2.5 %"], 2), round(wvconfints[2, "97.5 %"],2)) 

wsem_z <- with(wdat2, lmer(sem_z~NIHTLBX_depr_theta+yrsbl_x+
                           female+age_bl_x+college+inc_gt55k+married_partner+
                           (1 | id)))
wsests <- testEstimates(wsem_z, extra.pars=TRUE)
wsconfints <- confint.mitml.testEstimates(wsests)
wse <- c(round(wsests$estimates[2, "Estimate"], 2), round(wsconfints[2, "2.5 %"], 2), round(wsconfints[2, "97.5 %"],2)) 

rbind(wee, wve, wse)



## White [WITH TIME INTERACTION]
wexc <- with(wdat2, lmer(exec_z~NIHTLBX_depr_theta*yrsbl_x+
                           female+age_bl_x+college+inc_gt55k+married_partner+
                           (1 | id)))
wxests <- testEstimates(wexc, extra.pars=TRUE)
wxconfints <- confint.mitml.testEstimates(wxests)
wete <- c(round(wxests$estimates[9, "Estimate"], 2), round(wxconfints[9, "2.5 %"], 2), round(wxconfints[9, "97.5 %"],2)) 

wvrm <- with(wdat2, lmer(vrmem_z~NIHTLBX_depr_theta*yrsbl_x+
                           female+age_bl_x+college+inc_gt55k+married_partner+
                           (1 | id)))
wvests <- testEstimates(wvrm, extra.pars=TRUE)
wvconfints <- confint.mitml.testEstimates(wvests)
wvte <- c(round(wvests$estimates[9, "Estimate"], 2), round(wvconfints[9, "2.5 %"], 2), round(wvconfints[9, "97.5 %"],2)) 

wsem_z <- with(wdat2, lmer(sem_z~NIHTLBX_depr_theta*yrsbl_x+
                           female+age_bl_x+college+inc_gt55k+married_partner+
                           (1 | id)))
wsests <- testEstimates(wsem_z, extra.pars=TRUE)
wsconfints <- confint.mitml.testEstimates(wsests)
wste <- c(round(wsests$estimates[9, "Estimate"], 2), round(wsconfints[9, "2.5 %"], 2), round(wsconfints[9, "97.5 %"],2)) 

rbind(wete, wvte, wste)


##------------------------------------------------------------------------##

wave1 <- dat$`10` %>% filter(waveseq==1)
str(wave1)

table(wave1$race_fact)
