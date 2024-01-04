
#####################################################
## Sensitivity Analysis with Additional Covariates ##
#####################################################

#written by: Emma Gause 
#Date: 12/15/22
#Last updated: 09/07/23

#load libraries:
library("tidyverse")
library("dplyr")
library("lme4")

#turn off scientific notation
options(scipen = 999)

#Create path to directory
datadir <- "/Users/egause/Library/CloudStorage/OneDrive-BostonUniversity/ClimLab/MPJ/KHANDLE/Analysis/SENAS/"

#read in complete case data
ccdat <- readRDS(paste0(datadir, "Complete_Cases_083023.rds"))
str(ccdat)

##------------------------------------------------------------------------##

#create factor var for setting
ccdat$setting <- factor(ccdat$setting, levels = c("home", "clinic", "phone"))

#create binary indicator for first wave vs. not
ccdat <- ccdat %>% mutate(firstwave = if_else(waveseq==1, 1, 0))

#stratified by race
#get race stratified data
asian <- ccdat %>% filter(race_fact=="Asian")
black <- ccdat %>% filter(race_fact=="Black")
latin <- ccdat %>% filter(race_fact=="LatinX")
white <- ccdat %>% filter(race_fact=="White")


#get complete case numbers with additional covars
basecc <- ccdat %>% filter(waveseq==1) %>%
  filter(!is.na(social) & !is.na(ever_smoke) & !is.na(heavy_drink_fact) &
           !is.na(daily_physical) & !is.na(exc_health) & !is.na(setting))
table(basecc$race_fact, useNA = "ifany")

##------------------------------------------------------------------------##

####################
## SENAS MEASURES ##
####################

#analysis on full sample - WITHOUT TIME INTERACTION
exc <- lmer(exec~NIHTLBX_depr_theta+yrsbl_x+ race_fact+ 
              social+ever_smoke+heavy_drink_fact+daily_physical+exc_health+
              female+age_bl_x+college+inc_gt55k+married_partner+setting+firstwave+
              (1 | id), data=ccdat)
ee <- c(round(exc@beta[2], 2), round(confint(exc)[4,], 2))


vrbep <- lmer(vrmem~NIHTLBX_depr_theta+yrsbl_x+ race_fact+ 
                social+ever_smoke+heavy_drink_fact+daily_physical+exc_health+
                female+age_bl_x+college+inc_gt55k+married_partner+setting+firstwave+
                (1 | id), data=ccdat)
ve <- c(round(vrbep@beta[2], 2), round(confint(vrbep)[4,], 2))


sem <- lmer(sem~NIHTLBX_depr_theta+yrsbl_x+ race_fact+ 
              social+ever_smoke+heavy_drink_fact+daily_physical+exc_health+
              female+age_bl_x+college+inc_gt55k+married_partner+setting+firstwave+
              (1 | id), data=ccdat)
se <- c(round(sem@beta[2], 2), round(confint(sem)[4,], 2))

rbind(ee, ve, se)



#analysis on full sample - WITH TIME INTERACTION
exct <- lmer(exec~NIHTLBX_depr_theta*yrsbl_x+ race_fact+ 
              social+ever_smoke+heavy_drink_fact+daily_physical+exc_health+
              female+age_bl_x+college+inc_gt55k+married_partner+setting+firstwave+
              (1 | id), data=ccdat)
ete <- c(round(exct@beta[20], 2), round(confint(exct)[22,], 2))


vrbept <- lmer(vrmem~NIHTLBX_depr_theta*yrsbl_x+ race_fact+ 
                social+ever_smoke+heavy_drink_fact+daily_physical+exc_health+
                female+age_bl_x+college+inc_gt55k+married_partner+setting+firstwave+
                (1 | id), data=ccdat)
vte <- c(round(vrbept@beta[20], 2), round(confint(vrbept)[22,], 2))


semt <- lmer(sem~NIHTLBX_depr_theta*yrsbl_x+ race_fact+ 
              social+ever_smoke+heavy_drink_fact+daily_physical+exc_health+
              female+age_bl_x+college+inc_gt55k+married_partner+setting+firstwave+
              (1 | id), data=ccdat)
ste <- c(round(semt@beta[19], 2), round(confint(semt)[21,], 2)) #only two levels of setting so coef is in place 18 

rbind(ete, vte, ste)


##------------------------------------------------------------------------##

#STRATIFY BY RACE/ETH -- SENAS measures

## Asian [WITHOUT TIME INTERACTION]
adjexc <- lmer(exec~NIHTLBX_depr_theta+yrsbl_x+
                 social+ever_smoke+heavy_drink_fact+daily_physical+exc_health+
                 female+age_bl_x+college+inc_gt55k+married_partner+setting+firstwave+
                 (1 | id), data=asian)
adjvrbep <- lmer(vrmem~NIHTLBX_depr_theta+yrsbl_x+
                   social+ever_smoke+heavy_drink_fact+daily_physical+exc_health+
                   female+age_bl_x+college+inc_gt55k+married_partner+setting+firstwave+
                   (1 | id), data=asian)
adjsem <- lmer(sem~NIHTLBX_depr_theta+yrsbl_x+
                 social+ever_smoke+heavy_drink_fact+daily_physical+exc_health+
                 female+age_bl_x+college+inc_gt55k+married_partner+setting+firstwave+
                 (1 | id), data=asian)

aee <- c(round(adjexc@beta[2], 2), round(confint(adjexc)[4,], 2))
ave <- c(round(adjvrbep@beta[2], 2), round(confint(adjvrbep)[4,], 2))
ase <- c(round(adjsem@beta[2], 2), round(confint(adjsem)[4,], 2))

rbind(aee, ave, ase)



## Asian [WITH TIME INTERACTION]
adjexc <- lmer(exec~NIHTLBX_depr_theta*yrsbl_x+
                 social+ever_smoke+heavy_drink_fact+daily_physical+exc_health+
                 female+age_bl_x+college+inc_gt55k+married_partner+setting+firstwave+
                 (1 | id), data=asian)
adjvrbep <- lmer(vrmem~NIHTLBX_depr_theta*yrsbl_x+
                   social+ever_smoke+heavy_drink_fact+daily_physical+exc_health+
                   female+age_bl_x+college+inc_gt55k+married_partner+setting+firstwave+
                   (1 | id), data=asian)
adjsem <- lmer(sem~NIHTLBX_depr_theta*yrsbl_x+
                 social+ever_smoke+heavy_drink_fact+daily_physical+exc_health+
                 female+age_bl_x+college+inc_gt55k+married_partner+setting+firstwave+
                 (1 | id), data=asian)

aete <- c(round(adjexc@beta[17], 2), round(confint(adjexc)[19,], 2))
avte <- c(round(adjvrbep@beta[17], 2), round(confint(adjvrbep)[19,], 2))
aste <- c(round(adjsem@beta[16], 2), round(confint(adjsem)[1,], 2))

rbind(aete, avte, aste)

##-------------------------------------##

## Black [WITHOUT TIME INTERACTION]
adjexc <- lmer(exec~NIHTLBX_depr_theta+yrsbl_x+
                 social+ever_smoke+heavy_drink_fact+daily_physical+exc_health+
                 female+age_bl_x+college+inc_gt55k+married_partner+setting+firstwave+
                 (1 | id), data=black)
adjvrbep <- lmer(vrmem~NIHTLBX_depr_theta+yrsbl_x+
                   social+ever_smoke+heavy_drink_fact+daily_physical+exc_health+
                   female+age_bl_x+college+inc_gt55k+married_partner+setting+firstwave+
                   (1 | id), data=black)
adjsem <- lmer(sem~NIHTLBX_depr_theta+yrsbl_x+
                 social+ever_smoke+heavy_drink_fact+daily_physical+exc_health+
                 female+age_bl_x+college+inc_gt55k+married_partner+setting+firstwave+
                 (1 | id), data=black)

bee <- c(round(adjexc@beta[2], 2), round(confint(adjexc)[4,], 2))
bve <- c(round(adjvrbep@beta[2], 2), round(confint(adjvrbep)[4,], 2))
bse <- c(round(adjsem@beta[2], 2), round(confint(adjsem)[4,], 2))

rbind(bee, bve, bse)



## Black [WITH TIME INTERACTION]
adjexc <- lmer(exec~NIHTLBX_depr_theta*yrsbl_x+
                 social+ever_smoke+heavy_drink_fact+daily_physical+exc_health+
                 female+age_bl_x+college+inc_gt55k+married_partner+setting+firstwave+
                 (1 | id), data=black)
adjvrbep <- lmer(vrmem~NIHTLBX_depr_theta*yrsbl_x+
                   social+ever_smoke+heavy_drink_fact+daily_physical+exc_health+
                   female+age_bl_x+college+inc_gt55k+married_partner+setting+firstwave+
                   (1 | id), data=black)
adjsem <- lmer(sem~NIHTLBX_depr_theta*yrsbl_x+
                 social+ever_smoke+heavy_drink_fact+daily_physical+exc_health+
                 female+age_bl_x+college+inc_gt55k+married_partner+setting+firstwave+
                 (1 | id), data=black)

bete <- c(round(adjexc@beta[17], 2), round(confint(adjexc)[19,], 2))
bvte <- c(round(adjvrbep@beta[17], 2), round(confint(adjvrbep)[19,], 2))
bste <- c(round(adjsem@beta[16], 2), round(confint(adjsem)[18,], 2))

rbind(bete, bvte, bste)


##-------------------------------------##

## LatinX [WITHOUT TIME INTERACTION]
adjexc <- lmer(exec~NIHTLBX_depr_theta+yrsbl_x+
                 social+ever_smoke+heavy_drink_fact+daily_physical+exc_health+
                 female+age_bl_x+college+inc_gt55k+married_partner+setting+firstwave+
                 (1 | id), data=latin)
adjvrbep <- lmer(vrmem~NIHTLBX_depr_theta+yrsbl_x+
                   social+ever_smoke+heavy_drink_fact+daily_physical+exc_health+
                   female+age_bl_x+college+inc_gt55k+married_partner+setting+firstwave+
                   (1 | id), data=latin)
adjsem <- lmer(sem~NIHTLBX_depr_theta+yrsbl_x+
                 social+ever_smoke+heavy_drink_fact+daily_physical+exc_health+
                 female+age_bl_x+college+inc_gt55k+married_partner+setting+firstwave+
                 (1 | id), data=latin)

lee <- c(round(adjexc@beta[2], 2), round(confint(adjexc)[4,], 2))
lve <- c(round(adjvrbep@beta[2], 2), round(confint(adjvrbep)[4,], 2))
lse <- c(round(adjsem@beta[2], 2), round(confint(adjsem)[4,], 2))

rbind(lee, lve, lse)



## LatinX [WITH TIME INTERACTION]
adjexc <- lmer(exec~NIHTLBX_depr_theta*yrsbl_x+
                 social+ever_smoke+heavy_drink_fact+daily_physical+exc_health+
                 female+age_bl_x+college+inc_gt55k+married_partner+setting+firstwave+
                 (1 | id), data=latin)
adjvrbep <- lmer(vrmem~NIHTLBX_depr_theta*yrsbl_x+
                   social+ever_smoke+heavy_drink_fact+daily_physical+exc_health+
                   female+age_bl_x+college+inc_gt55k+married_partner+setting+firstwave+
                   (1 | id), data=latin)
adjsem <- lmer(sem~NIHTLBX_depr_theta*yrsbl_x+
                 social+ever_smoke+heavy_drink_fact+daily_physical+exc_health+
                 female+age_bl_x+college+inc_gt55k+married_partner+setting+firstwave+
                 (1 | id), data=latin)


lete <- c(round(adjexc@beta[17], 2), round(confint(adjexc)[19,], 2))
lvte <- c(round(adjvrbep@beta[17], 2), round(confint(adjvrbep)[19,], 2))
lste <- c(round(adjsem@beta[16], 2), round(confint(adjsem)[18,], 2))

rbind(lete, lvte, lste)


##-------------------------------------##

## White [WITHOUT TIME INTERACTION]
adjexc <- lmer(exec~NIHTLBX_depr_theta+yrsbl_x+
                 social+ever_smoke+heavy_drink_fact+daily_physical+exc_health+
                 female+age_bl_x+college+inc_gt55k+married_partner+setting+firstwave+
                 (1 | id), data=white)
adjvrbep <- lmer(vrmem~NIHTLBX_depr_theta+yrsbl_x+
                   social+ever_smoke+heavy_drink_fact+daily_physical+exc_health+
                   female+age_bl_x+college+inc_gt55k+married_partner+setting+firstwave+
                   (1 | id), data=white)
adjsem <- lmer(sem~NIHTLBX_depr_theta+yrsbl_x+
                 social+ever_smoke+heavy_drink_fact+daily_physical+exc_health+
                 female+age_bl_x+college+inc_gt55k+married_partner+setting+firstwave+
                 (1 | id), data=white)

wee <- c(round(adjexc@beta[2], 2), round(confint(adjexc)[4,], 2))
wve <- c(round(adjvrbep@beta[2], 2), round(confint(adjvrbep)[4,], 2))
wse <- c(round(adjsem@beta[2], 2), round(confint(adjsem)[4,], 2))

rbind(wee, wve, wse)


## White [WITH TIME INTERACTION]
adjexc <- lmer(exec~NIHTLBX_depr_theta*yrsbl_x+
                 social+ever_smoke+heavy_drink_fact+daily_physical+exc_health+
                 female+age_bl_x+college+inc_gt55k+married_partner+setting+firstwave+
                 (1 | id), data=white)
adjvrbep <- lmer(vrmem~NIHTLBX_depr_theta*yrsbl_x+
                   social+ever_smoke+heavy_drink_fact+daily_physical+exc_health+
                   female+age_bl_x+college+inc_gt55k+married_partner+setting+firstwave+
                   (1 | id), data=white)
adjsem <- lmer(sem~NIHTLBX_depr_theta*yrsbl_x+
                 social+ever_smoke+heavy_drink_fact+daily_physical+exc_health+
                 female+age_bl_x+college+inc_gt55k+married_partner+setting+firstwave+
                 (1 | id), data=white)

wete <- c(round(adjexc@beta[17], 2), round(confint(adjexc)[19,], 2))
wvte <- c(round(adjvrbep@beta[17], 2), round(confint(adjvrbep)[19,], 2))
wste <- c(round(adjsem@beta[16], 2), round(confint(adjsem)[18,], 2))

rbind(wete, wvte, wste)

