
###################################################
## Multi-Level Regression Models on Imputed Data ##
###################################################

#written by: Emma Gause 
#Date: 12/15/22
#Last updated: 08/30/23

#load libraries:
library("tidyverse")
library("dplyr")
library("lme4")

#Create path to directory
datadir <- "/Users/egause/Library/CloudStorage/OneDrive-BostonUniversity/ClimLab/MPJ/KHANDLE/Analysis/SENAS/"

#read in complete case data
ccdat <- readRDS(paste0(datadir, "Complete_Cases_083023.rds"))

##------------------------------------------------------------------------##

#stratified by sex
#get sex stratified data
male <- ccdat %>% filter(female=="0")
female <- ccdat %>% filter(female=="1")


sexw1 <- ccdat %>% filter(waveseq==1)
table(sexw1$female)

##------------------------------------------------------------------------##

####################
## SENAS MEASURES ##
####################

#analysis on full sample

#All waves
excti <- lmer(exec_z~NIHTLBX_depr_theta+yrsbl_x+ race_fact+ 
              female+age_bl_x+college+inc_gt55k+married_partner+
              (1 | id), data=ccdat)

vrbepti <- lmer(vrmem_z~NIHTLBX_depr_theta+yrsbl_x+ race_fact+ 
                female+age_bl_x+college+inc_gt55k+married_partner+
                (1 | id), data=ccdat)

semmti <- lmer(sem_z~NIHTLBX_depr_theta+yrsbl_x+ race_fact+ 
               female+age_bl_x+college+inc_gt55k+married_partner+
               (1 | id), data=ccdat)


#assess effect modification by sex 
exc_int <- lmer(exec_z~NIHTLBX_depr_theta*female+yrsbl_x+race_fact+ 
                  age_bl_x+college+inc_gt55k+married_partner+
                  (1 | id), data=ccdat)
anova(excti, exc_int)

vrbep_int <- lmer(vrmem_z~NIHTLBX_depr_theta*female+yrsbl_x+race_fact+ 
                    age_bl_x+college+inc_gt55k+married_partner+
                    (1 | id), data=ccdat)
anova(vrbepti, vrbep_int)

sem_int <- lmer(sem_z~NIHTLBX_depr_theta*female+yrsbl_x+race_fact+ 
                  age_bl_x+college+inc_gt55k+married_partner+
                  (1 | id), data=ccdat)
anova(semmti, sem_int)



#Time Interaction
exc <- lmer(exec_z~NIHTLBX_depr_theta*yrsbl_x+ race_fact+ 
              female+age_bl_x+college+inc_gt55k+married_partner+
              (1 | id), data=ccdat)

vrbep <- lmer(vrmem_z~NIHTLBX_depr_theta*yrsbl_x+ race_fact+ 
                female+age_bl_x+college+inc_gt55k+married_partner+
                (1 | id), data=ccdat)

semm <- lmer(sem_z~NIHTLBX_depr_theta*yrsbl_x+ race_fact+ 
              female+age_bl_x+college+inc_gt55k+married_partner+
              (1 | id), data=ccdat)



##------------------------------------------------------------------------##

#STRATIFY BY SEX -- SENAS measures

## Male
adjexc <- lmer(exec_z~NIHTLBX_depr_theta*yrsbl_x+
                 race_fact+age_bl_x+college+inc_gt55k+married_partner+
                 (1 | id), data=male)
adjvrbep <- lmer(vrmem_z~NIHTLBX_depr_theta*yrsbl_x+
                   race_fact+age_bl_x+college+inc_gt55k+married_partner+
                   (1 | id), data=male)
adjsem <- lmer(sem_z~NIHTLBX_depr_theta*yrsbl_x+
                 race_fact+age_bl_x+college+inc_gt55k+married_partner+
                 (1 | id), data=male)


round(adjexc@beta[2], 2) 
round(confint(adjexc)[4,], 2)
round(adjexc@beta[11], 2)
round(confint(adjexc)[13,], 2)

round(adjvrbep@beta[2], 2) 
round(confint(adjvrbep)[4,], 2)
round(adjvrbep@beta[11], 2)
round(confint(adjvrbep)[13,], 2)

round(adjsem@beta[2], 2)
round(confint(adjsem)[4,], 2)
round(adjsem@beta[11], 2)
round(confint(adjsem)[13,], 2)

##-------------------------------------##
## female
adjexc <- lmer(exec_z~NIHTLBX_depr_theta*yrsbl_x+
                 race_fact+age_bl_x+college+inc_gt55k+married_partner+
                 (1 | id), data=female)
adjvrbep <- lmer(vrmem_z~NIHTLBX_depr_theta*yrsbl_x+
                   race_fact+age_bl_x+college+inc_gt55k+married_partner+
                   (1 | id), data=female)
adjsem <- lmer(sem_z~NIHTLBX_depr_theta*yrsbl_x+
                 race_fact+age_bl_x+college+inc_gt55k+married_partner+
                 (1 | id), data=female)

round(adjexc@beta[2], 2) 
round(confint(adjexc)[4,], 2)
round(adjexc@beta[11], 2)
round(confint(adjexc)[13,], 2)

round(adjvrbep@beta[2], 2) 
round(confint(adjvrbep)[4,], 2)
round(adjvrbep@beta[11], 2)
round(confint(adjvrbep)[13,], 2)

round(adjsem@beta[2], 2)
round(confint(adjsem)[4,], 2)
round(adjsem@beta[11], 2)
round(confint(adjsem)[13,], 2)


