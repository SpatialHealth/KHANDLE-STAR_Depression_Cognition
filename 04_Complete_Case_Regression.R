
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

# investigate missingness in outcomes
test <- ccdat %>% select(id, exec_z, waveseq) %>% group_by(id) %>% 
  mutate(any_miss = any(is.na(exec_z)))

test2 <- ccdat %>% group_by(id) %>% 
  summarize(count_na = sum(is.na(exec_z)))
table(test2$count_na) #4 ppl missing all exec measures

test3 <- ccdat %>% group_by(id) %>% 
  summarize(count_na = sum(is.na(vrmem_z)))
table(test3$count_na) # 7 people missing all vrmem measures

test4 <- ccdat %>% group_by(id) %>% 
  summarize(count_na = sum(is.na(sem_z)))
table(test4$count_na) #17 ppl missing all sem measures


#exmiss <- test2 %>% filter(count_na==3)
#vrmiss <- test3 %>% filter(count_na==3)
#semiss <- test4 %>% filter(count_na>=3)

rm(test, test2, test3, test4)

##------------------------------------------------------------------------##

#stratified by race
#get race stratified data
asian <- ccdat %>% filter(race_fact=="Asian")
black <- ccdat %>% filter(race_fact=="Black")
latin <- ccdat %>% filter(race_fact=="LatinX")
white <- ccdat %>% filter(race_fact=="White")

##------------------------------------------------------------------------##

####################
## SENAS MEASURES ##
####################

#analysis on full sample

#All Waves
excti <- lmer(exec_z~NIHTLBX_depr_theta+race_fact+yrsbl_x+  
              female+age_bl_x+college+inc_gt55k+married_partner+
              (1 | id), data=ccdat)

ee <- c(round(excti@beta[2], 2), round(confint(excti)[4,], 2))

vrbepti <- lmer(vrmem_z~NIHTLBX_depr_theta+race_fact+yrsbl_x+  
                female+age_bl_x+college+inc_gt55k+married_partner+
                (1 | id), data=ccdat)

ve <- c(round(vrbepti@beta[2], 2), round(confint(vrbepti)[4,], 2))


semmti <- lmer(sem_z~NIHTLBX_depr_theta+race_fact+yrsbl_x+  
               female+age_bl_x+college+inc_gt55k+married_partner+
               (1 | id), data=ccdat)

se <- c(round(semmti@beta[2], 2), round(confint(semmti)[4,], 2))

rbind(ee, ve, se)


##
##assess effect modification by race/eth [no time interaction]
##

exc_int <- lmer(exec_z~NIHTLBX_depr_theta*race_fact+yrsbl_x+ 
                  female+age_bl_x+college+inc_gt55k+married_partner+
                  (1 | id), data=ccdat)
anova(excti, exc_int)

vrbep_int <- lmer(vrmem_z~NIHTLBX_depr_theta*race_fact+yrsbl_x+ 
                    female+age_bl_x+college+inc_gt55k+married_partner+
                    (1 | id), data=ccdat)
anova(vrbepti, vrbep_int)

sem_int <- lmer(sem_z~NIHTLBX_depr_theta*race_fact+yrsbl_x+ 
                  female+age_bl_x+college+inc_gt55k+married_partner+
                  (1 | id), data=ccdat)
anova(semmti, sem_int)


#Full sample Time Interaction
exc <- lmer(exec_z~NIHTLBX_depr_theta*yrsbl_x+ race_fact+ 
              female+age_bl_x+college+inc_gt55k+married_partner+
              (1 | id), data=ccdat)

ete <- c(round(exc@beta[12], 2), round(confint(exc)[14,], 2))


vrbep <- lmer(vrmem_z~NIHTLBX_depr_theta*yrsbl_x+ race_fact+ 
                female+age_bl_x+college+inc_gt55k+married_partner+
                (1 | id), data=ccdat)

vte <- c(round(vrbep@beta[12], 2), round(confint(vrbep)[14,], 2))


semm <- lmer(sem_z~NIHTLBX_depr_theta*yrsbl_x+ race_fact+ 
              female+age_bl_x+college+inc_gt55k+married_partner+
              (1 | id), data=ccdat)

ste <- c(round(semm@beta[12], 2), round(confint(semm)[14,], 2))

rbind(ete, vte, ste)



##------------------------------------------------------------------------##

#STRATIFY BY RACE/ETH -- SENAS measures

## Asian

#all waves
adjexcti <- lmer(exec_z~NIHTLBX_depr_theta+yrsbl_x+
                 female+age_bl_x+college+inc_gt55k+married_partner+
                 (1 | id), data=asian)
adjvrbepti <- lmer(vrmem_z~NIHTLBX_depr_theta+yrsbl_x+
                   female+age_bl_x+college+inc_gt55k+married_partner+
                   (1 | id), data=asian)
adjsemti <- lmer(sem_z~NIHTLBX_depr_theta+yrsbl_x+
                 female+age_bl_x+college+inc_gt55k+married_partner+
                 (1 | id), data=asian)

aee <- c(round(adjexcti@beta[2], 2), round(confint(adjexcti)[4,], 2))
ave <- c(round(adjvrbepti@beta[2], 2), round(confint(adjvrbepti)[4,], 2))
ase <- c(round(adjsemti@beta[2], 2), round(confint(adjsemti)[4,], 2))

rbind(aee, ave, ase)



#Time Interaction
adjexc <- lmer(exec_z~NIHTLBX_depr_theta*yrsbl_x+
                 female+age_bl_x+college+inc_gt55k+married_partner+
                 (1 | id), data=asian)
adjvrbep <- lmer(vrmem_z~NIHTLBX_depr_theta*yrsbl_x+
                   female+age_bl_x+college+inc_gt55k+married_partner+
                   (1 | id), data=asian)
adjsem <- lmer(sem_z~NIHTLBX_depr_theta*yrsbl_x+
                 female+age_bl_x+college+inc_gt55k+married_partner+
                 (1 | id), data=asian)

aete <- c(round(adjexc@beta[9], 2), round(confint(adjexc)[11,], 2))
avte <- c(round(adjvrbep@beta[9], 2), round(confint(adjvrbep)[11,], 2))
aste <- c(round(adjsem@beta[9], 2), round(confint(adjsem)[11,], 2))

rbind(aete, avte, aste)

##-------------------------------------##
## Black

#All waves
adjexcti <- lmer(exec_z~NIHTLBX_depr_theta+yrsbl_x+
                 female+age_bl_x+college+inc_gt55k+married_partner+
                 (1 | id), data=black)
adjvrbepti <- lmer(vrmem_z~NIHTLBX_depr_theta+yrsbl_x+
                   female+age_bl_x+college+inc_gt55k+married_partner+
                   (1 | id), data=black)
adjsemti <- lmer(sem_z~NIHTLBX_depr_theta+yrsbl_x+
                 female+age_bl_x+college+inc_gt55k+married_partner+
                 (1 | id), data=black)

bee <- c(round(adjexcti@beta[2], 2), round(confint(adjexcti)[4,], 2))
bve <- c(round(adjvrbepti@beta[2], 2), round(confint(adjvrbepti)[4,], 2))
bse <- c(round(adjsemti@beta[2], 2), round(confint(adjsemti)[4,], 2))

rbind(bee, bve, bse)


#Time Interaction
adjexc <- lmer(exec_z~NIHTLBX_depr_theta*yrsbl_x+
                 female+age_bl_x+college+inc_gt55k+married_partner+
                 (1 | id), data=black)
adjvrbep <- lmer(vrmem_z~NIHTLBX_depr_theta*yrsbl_x+
                   female+age_bl_x+college+inc_gt55k+married_partner+
                   (1 | id), data=black)
adjsem <- lmer(sem_z~NIHTLBX_depr_theta*yrsbl_x+
                 female+age_bl_x+college+inc_gt55k+married_partner+
                 (1 | id), data=black)

bete <- c(round(adjexc@beta[9], 2), round(confint(adjexc)[11,], 2))
bvte <- c(round(adjvrbep@beta[9], 2), round(confint(adjvrbep)[11,], 2))
bste <- c(round(adjsem@beta[9], 2), round(confint(adjsem)[11,], 2))

rbind(bete, bvte, bste)


##-------------------------------------##
## LatinX

#All Waves
adjexcti <- lmer(exec_z~NIHTLBX_depr_theta+yrsbl_x+
                 female+age_bl_x+college+inc_gt55k+married_partner+
                 (1 | id), data=latin)
adjvrbepti <- lmer(vrmem_z~NIHTLBX_depr_theta+yrsbl_x+
                   female+age_bl_x+college+inc_gt55k+married_partner+
                   (1 | id), data=latin)
adjsemti <- lmer(sem_z~NIHTLBX_depr_theta+yrsbl_x+
                 female+age_bl_x+college+inc_gt55k+married_partner+
                 (1 | id), data=latin)

lee <- c(round(adjexcti@beta[2], 2), round(confint(adjexcti)[4,], 2))
lve <- c(round(adjvrbepti@beta[2], 2), round(confint(adjvrbepti)[4,], 2))
lse <- c(round(adjsemti@beta[2], 2), round(confint(adjsemti)[4,], 2))

rbind(lee, lve, lse)


#Time Interaction
adjexc <- lmer(exec_z~NIHTLBX_depr_theta*yrsbl_x+
                 female+age_bl_x+college+inc_gt55k+married_partner+
                 (1 | id), data=latin)
adjvrbep <- lmer(vrmem_z~NIHTLBX_depr_theta*yrsbl_x+
                   female+age_bl_x+college+inc_gt55k+married_partner+
                   (1 | id), data=latin)
adjsem <- lmer(sem_z~NIHTLBX_depr_theta*yrsbl_x+
                 female+age_bl_x+college+inc_gt55k+married_partner+
                 (1 | id), data=latin)

lete <- c(round(adjexc@beta[9], 2), round(confint(adjexc)[11,], 2))
lvte <- c(round(adjvrbep@beta[9], 2), round(confint(adjvrbep)[11,], 2))
lste <- c(round(adjsem@beta[9], 2), round(confint(adjsem)[11,], 2))

rbind(lete, lvte, lste)

##-------------------------------------##
## White

#All waves
adjexcti <- lmer(exec_z~NIHTLBX_depr_theta+yrsbl_x+
                 female+age_bl_x+college+inc_gt55k+married_partner+
                 (1 | id), data=white)
adjvrbepti <- lmer(vrmem_z~NIHTLBX_depr_theta+yrsbl_x+
                   female+age_bl_x+college+inc_gt55k+married_partner+
                   (1 | id), data=white)
adjsemti <- lmer(sem_z~NIHTLBX_depr_theta+yrsbl_x+
                 female+age_bl_x+college+inc_gt55k+married_partner+
                 (1 | id), data=white)

wee <- c(round(adjexcti@beta[2], 2), round(confint(adjexcti)[4,], 2))
wve <- c(round(adjvrbepti@beta[2], 2), round(confint(adjvrbepti)[4,], 2))
wse <- c(round(adjsemti@beta[2], 2), round(confint(adjsemti)[4,], 2))

rbind(wee, wve, wse)


#Time Interaction
adjexc <- lmer(exec_z~NIHTLBX_depr_theta*yrsbl_x+
                 female+age_bl_x+college+inc_gt55k+married_partner+
                 (1 | id), data=white)
adjvrbep <- lmer(vrmem_z~NIHTLBX_depr_theta*yrsbl_x+
                   female+age_bl_x+college+inc_gt55k+married_partner+
                   (1 | id), data=white)
adjsem <- lmer(sem_z~NIHTLBX_depr_theta*yrsbl_x+
                 female+age_bl_x+college+inc_gt55k+married_partner+
                 (1 | id), data=white)

wete <- c(round(adjexc@beta[9], 2), round(confint(adjexc)[11,], 2))
wvte <- c(round(adjvrbep@beta[9], 2), round(confint(adjvrbep)[11,], 2))
wste <- c(round(adjsem@beta[9], 2), round(confint(adjsem)[11,], 2))

rbind(wete, wvte, wste)

##------------------------------------------------------------------------##



##------------------------------------------------------------------------##
##------------------------------------------------------------------------##
#### CONDUCT SENSITIVITY ANALYSIS WITH INTERACTIONS FOR ALL COVARS W/TIME

#Full sample Time Interaction
exc <- lmer(exec_z~NIHTLBX_depr_theta*yrsbl_x+race_fact*yrsbl_x+ 
              female*yrsbl_x+age_bl_x*yrsbl_x+college*yrsbl_x+inc_gt55k*yrsbl_x+
              married_partner*yrsbl_x+
              (1 | id), data=ccdat)

vrbep <- lmer(vrmem_z~NIHTLBX_depr_theta*yrsbl_x+race_fact*yrsbl_x+ 
                female*yrsbl_x+age_bl_x*yrsbl_x+college*yrsbl_x+inc_gt55k*yrsbl_x+
                married_partner*yrsbl_x+
                (1 | id), data=ccdat)

semm <- lmer(sem_z~NIHTLBX_depr_theta*yrsbl_x+race_fact*yrsbl_x+ 
               female*yrsbl_x+age_bl_x*yrsbl_x+college*yrsbl_x+inc_gt55k*yrsbl_x+
               married_partner*yrsbl_x+
               (1 | id), data=ccdat)

oett <- c(round(exc@beta[12], 2), round(confint(exc)[14,], 2))
ovtt <- c(round(vrbep@beta[12], 2), round(confint(vrbep)[14,], 2))
ostt <- c(round(semm@beta[12], 2), round(confint(semm)[14,], 2))

rbind(oett, ovtt, ostt)

##------------------------------------------------------------------------##

#STRATIFY BY RACE/ETH -- SENAS measures

## Asian

#Time Interaction
adjexc <- lmer(exec_z~NIHTLBX_depr_theta*yrsbl_x+ 
                 female*yrsbl_x+age_bl_x*yrsbl_x+college*yrsbl_x+inc_gt55k*yrsbl_x+
                 married_partner*yrsbl_x+
                 (1 | id), data=asian)
adjvrbep <- lmer(vrmem_z~NIHTLBX_depr_theta*yrsbl_x+
                   female*yrsbl_x+age_bl_x*yrsbl_x+college*yrsbl_x+inc_gt55k*yrsbl_x+
                   married_partner*yrsbl_x+
                   (1 | id), data=asian)
adjsem <- lmer(sem_z~NIHTLBX_depr_theta*yrsbl_x+ 
                 female*yrsbl_x+age_bl_x*yrsbl_x+college*yrsbl_x+inc_gt55k*yrsbl_x+
                 married_partner*yrsbl_x+
                 (1 | id), data=asian)

aett <- c(round(adjexc@beta[9], 2), round(confint(adjexc)[11,], 2))
avtt <- c(round(adjvrbep@beta[9], 2), round(confint(adjvrbep)[11,], 2))
astt <- c(round(adjsem@beta[9], 2), round(confint(adjsem)[11,], 2))

rbind(aett, avtt, astt)


##-------------------------------------##
## Black

#Time Interaction
adjexc <- lmer(exec_z~NIHTLBX_depr_theta*yrsbl_x+ 
                 female*yrsbl_x+age_bl_x*yrsbl_x+
                 college*yrsbl_x+inc_gt55k*yrsbl_x+
                 married_partner*yrsbl_x+
                 (1 | id), data=black)
adjvrbep <- lmer(vrmem_z~NIHTLBX_depr_theta*yrsbl_x+ 
                   female*yrsbl_x+age_bl_x*yrsbl_x+
                   college*yrsbl_x+inc_gt55k*yrsbl_x+
                   married_partner*yrsbl_x+
                   (1 | id), data=black)
adjsem <- lmer(sem_z~NIHTLBX_depr_theta*yrsbl_x+ 
                 female*yrsbl_x+age_bl_x*yrsbl_x+
                 college*yrsbl_x+inc_gt55k*yrsbl_x+
                 married_partner*yrsbl_x+
                 (1 | id), data=black)

bett <- c(round(adjexc@beta[9], 2), round(confint(adjexc)[11,], 2))
bvtt <- c(round(adjvrbep@beta[9], 2), round(confint(adjvrbep)[11,], 2))
bstt <- c(round(adjsem@beta[9], 2), round(confint(adjsem)[11,], 2))

rbind(bett, bvtt, bstt)

##-------------------------------------##
## LatinX

#Time Interaction
adjexc <- lmer(exec_z~NIHTLBX_depr_theta*yrsbl_x+ 
                 female*yrsbl_x+age_bl_x*yrsbl_x+college*yrsbl_x+inc_gt55k*yrsbl_x+
                 married_partner*yrsbl_x+
                 (1 | id), data=latin)
adjvrbep <- lmer(vrmem_z~NIHTLBX_depr_theta*yrsbl_x+ 
                   female*yrsbl_x+age_bl_x*yrsbl_x+college*yrsbl_x+inc_gt55k*yrsbl_x+
                   married_partner*yrsbl_x+
                   (1 | id), data=latin)
adjsem <- lmer(sem_z~NIHTLBX_depr_theta*yrsbl_x+ 
                 female*yrsbl_x+age_bl_x*yrsbl_x+college*yrsbl_x+inc_gt55k*yrsbl_x+
                 married_partner*yrsbl_x+
                 (1 | id), data=latin)

lett <- c(round(adjexc@beta[9], 2), round(confint(adjexc)[11,], 2))
lvtt <- c(round(adjvrbep@beta[9], 2), round(confint(adjvrbep)[11,], 2))
lstt <- c(round(adjsem@beta[9], 2), round(confint(adjsem)[11,], 2))

rbind(lett, lvtt, lstt)

##-------------------------------------##
## White


#Time Interaction
adjexc <- lmer(exec_z~NIHTLBX_depr_theta*yrsbl_x+ 
                 female*yrsbl_x+age_bl_x*yrsbl_x+college*yrsbl_x+inc_gt55k*yrsbl_x+
                 married_partner*yrsbl_x+
                 (1 | id), data=white)
adjvrbep <- lmer(vrmem_z~NIHTLBX_depr_theta*yrsbl_x+ 
                   female*yrsbl_x+age_bl_x*yrsbl_x+college*yrsbl_x+inc_gt55k*yrsbl_x+
                   married_partner*yrsbl_x+
                   (1 | id), data=white)
adjsem <- lmer(sem_z~NIHTLBX_depr_theta*yrsbl_x+ 
                 female*yrsbl_x+age_bl_x*yrsbl_x+college*yrsbl_x+inc_gt55k*yrsbl_x+
                 married_partner*yrsbl_x+
                 (1 | id), data=white)

wett <- c(round(adjexc@beta[9], 2), round(confint(adjexc)[11,], 2))
wvtt <- c(round(adjvrbep@beta[9], 2), round(confint(adjvrbep)[11,], 2))
wstt <- c(round(adjsem@beta[9], 2), round(confint(adjsem)[11,], 2))

rbind(wett, wvtt, wstt)



