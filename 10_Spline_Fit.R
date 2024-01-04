
##################################################
## Multi-Level Regression -- Fitted with Splines##
##################################################

#written by: Emma Gause 
#Date: 09/07/23
#Last updated: 09/28/23

#load libraries:
library("tidyverse")
library("dplyr")
library("lme4")
library("mgcv")
library("splines")

#Create path to directory
datadir <- "/Users/egause/Library/CloudStorage/OneDrive-BostonUniversity/ClimLab/MPJ/KHANDLE/Analysis/SENAS/"

#read in complete case data
ccdat <- readRDS(paste0(datadir, "Complete_Cases_083023.rds"))


##------------------------------------------------------------------------##


plot(ccdat$NIHTLBX_depr_theta, ccdat$exec_z)
plot(ccdat$NIHTLBX_depr_theta, ccdat$vrmem_z)
plot(ccdat$NIHTLBX_depr_theta, ccdat$sem_z)


#stratified by race
#get race stratified data
asian <- ccdat %>% filter(race_fact=="Asian")
black <- ccdat %>% filter(race_fact=="Black")
latin <- ccdat %>% filter(race_fact=="LatinX")
white <- ccdat %>% filter(race_fact=="White")

##------------------------------------------------------------------------##

#### EXPLORATORY ANALYSIS OF NON-LINEAR ASSOCIATIONS


####################
## SENAS MEASURES ##
####################


#STRATIFY BY RACE/ETH -- SENAS measures

## Asian

#all waves
aadjexcti <- gamm(exec_z~s(NIHTLBX_depr_theta)+
                  female+age_bl_x+college+inc_gt55k+married_partner,
                  random=list(id=~1), data=asian)

tiff(filename = "/Users/egause/Library/CloudStorage/OneDrive-BostonUniversity/ClimLab/MPJ/KHANDLE/Figures/SENAS/Splines/Asian_Exec.tiff",
     res = 300, width = 5, height = 5, units = "in")
plot(aadjexcti$gam, 
     pages=1, all.terms=F, rug=T, residuals=F, se=T, shade=T, seWithMean=T,
     ylab="Executive Function", xlab= "Depressive Symptoms",
     xlim = c(-1.5, 2.5), ylim = c(-1, 0.4))
dev.off()

aadjvrbepti <- gamm(vrmem_z~s(NIHTLBX_depr_theta)+
                   female+age_bl_x+college+inc_gt55k+married_partner,
                   random=list(id=~1), data=asian)

tiff(filename = "/Users/egause/Library/CloudStorage/OneDrive-BostonUniversity/ClimLab/MPJ/KHANDLE/Figures/SENAS/Splines/Asian_Vrmem.tiff",
     res = 300, width = 5, height = 5, units = "in")
plot(aadjvrbepti$gam,  
     pages=1, all.terms=F, rug=T, residuals=F, se=T, shade=T, seWithMean=T,
     ylab="Verbal Episodic", xlab= "Depressive Symptoms",
     xlim = c(-1.5, 2.5), ylim = c(-0.6, 0.4))
dev.off()


aadjsemti <- gamm(sem_z~s(NIHTLBX_depr_theta)+
                     female+age_bl_x+college+inc_gt55k+married_partner,
                   random=list(id=~1), data=asian)

tiff(filename = "/Users/egause/Library/CloudStorage/OneDrive-BostonUniversity/ClimLab/MPJ/KHANDLE/Figures/SENAS/Splines/Asian_Sem.tiff",
     res = 300, width = 5, height = 5, units = "in")
plot(aadjsemti$gam,
     pages=1, all.terms=F, rug=T, residuals=F, se=T, shade=T, seWithMean=T,
     ylab="Semantic", xlab= "Depressive Symptoms",
     xlim = c(-1.5, 2.5), ylim = c(-1, 0.4))
dev.off()


##-------------------------------------##
## Black

#All waves

badjexcti <- gamm(exec_z~s(NIHTLBX_depr_theta)+
                   female+age_bl_x+college+inc_gt55k+married_partner,
                 random=list(id=~1), data=black)

tiff(filename = "/Users/egause/Library/CloudStorage/OneDrive-BostonUniversity/ClimLab/MPJ/KHANDLE/Figures/SENAS/Splines/Black_Exec.tiff",
     res = 300, width = 5, height = 5, units = "in")
plot(badjexcti$gam, 
     pages=1, all.terms=F, rug=T, residuals=F, se=T, shade=T, seWithMean=T,
     ylab="Executive Function", xlab= "Depressive Symptoms",
     xlim = c(-1.5, 2.5), ylim = c(-1, 0.4))
dev.off()


badjvrbepti <- gamm(vrmem_z~s(NIHTLBX_depr_theta, k=10)+
                     female+age_bl_x+college+inc_gt55k+married_partner,
                   random=list(id=~1), data=black)

tiff(filename = "/Users/egause/Library/CloudStorage/OneDrive-BostonUniversity/ClimLab/MPJ/KHANDLE/Figures/SENAS/Splines/Black_Vrmem.tiff",
     res = 300, width = 5, height = 5, units = "in")
plot(badjvrbepti$gam,  
     pages=1, all.terms=F, rug=T, residuals=F, se=T, shade=T, seWithMean=T,
     ylab="Verbal Episodic", xlab= "Depressive Symptoms",
     xlim = c(-1.5, 2.5), ylim = c(-0.6, 0.4))
dev.off()


badjsemti <- gamm(sem_z~s(NIHTLBX_depr_theta, k=10)+
                   female+age_bl_x+college+inc_gt55k+married_partner,
                 random=list(id=~1), data=black)

tiff(filename = "/Users/egause/Library/CloudStorage/OneDrive-BostonUniversity/ClimLab/MPJ/KHANDLE/Figures/SENAS/Splines/Black_Sem.tiff",
     res = 300, width = 5, height = 5, units = "in")
plot(badjsemti$gam,
     pages=1, all.terms=F, rug=T, residuals=F, se=T, shade=T, seWithMean=T,
     ylab="Semantic", xlab= "Depressive Symptoms",
     xlim = c(-1.5, 2.5), ylim = c(-1, 0.2))
dev.off()




##-------------------------------------##
## LatinX

#All Waves
ladjexcti <- gamm(exec_z~s(NIHTLBX_depr_theta)+
                   female+age_bl_x+college+inc_gt55k+married_partner,
                 random=list(id=~1), data=latin)

tiff(filename = "/Users/egause/Library/CloudStorage/OneDrive-BostonUniversity/ClimLab/MPJ/KHANDLE/Figures/SENAS/Splines/LatinX_Exec.tiff",
     res = 300, width = 5, height = 5, units = "in")
plot(ladjexcti$gam, 
     pages=1, all.terms=F, rug=T, residuals=F, se=T, shade=T, seWithMean=T,
     ylab="Executive Function", xlab= "Depressive Symptoms",
     xlim = c(-1.5, 2.5), ylim = c(-1, 0.4))
dev.off()


ladjvrbepti <- gamm(vrmem_z~s(NIHTLBX_depr_theta)+
                     female+age_bl_x+college+inc_gt55k+married_partner,
                   random=list(id=~1), data=latin)

tiff(filename = "/Users/egause/Library/CloudStorage/OneDrive-BostonUniversity/ClimLab/MPJ/KHANDLE/Figures/SENAS/Splines/LatinX_Vrmem.tiff",
     res = 300, width = 5, height = 5, units = "in")
plot(ladjvrbepti$gam,  
     pages=1, all.terms=F, rug=T, residuals=F, se=T, shade=T, seWithMean=T,
     ylab="Verbal Episodic", xlab= "Depressive Symptoms",
     xlim = c(-1.5, 2.5), ylim = c(-0.6, 0.4))
dev.off()


ladjsemti <- gamm(sem_z~s(NIHTLBX_depr_theta)+
                   female+age_bl_x+college+inc_gt55k+married_partner,
                 random=list(id=~1), data=latin)

tiff(filename = "/Users/egause/Library/CloudStorage/OneDrive-BostonUniversity/ClimLab/MPJ/KHANDLE/Figures/SENAS/Splines/LatinX_Sem.tiff",
     res = 300, width = 5, height = 5, units = "in")
plot(ladjsemti$gam,
     pages=1, all.terms=F, rug=T, residuals=F, se=T, shade=T, seWithMean=T,
     ylab="Semantic", xlab= "Depressive Symptoms",
     xlim = c(-1.5, 2.5), ylim = c(-1, 0.2))
dev.off()




##-------------------------------------##
## White

#All Waves
wadjexcti <- gamm(exec_z~s(NIHTLBX_depr_theta)+
                   female+age_bl_x+college+inc_gt55k+married_partner,
                 random=list(id=~1), data=white)

tiff(filename = "/Users/egause/Library/CloudStorage/OneDrive-BostonUniversity/ClimLab/MPJ/KHANDLE/Figures/SENAS/Splines/White_Exec.tiff",
     res = 300, width = 5, height = 5, units = "in")
plot(wadjexcti$gam, 
     pages=1, all.terms=F, rug=T, residuals=F, se=T, shade=T, seWithMean=T,
     ylab="Executive Function", xlab= "Depressive Symptoms",
     xlim = c(-1.5, 2.5), ylim = c(-1, 0.4))
dev.off()


wadjvrbepti <- gamm(vrmem_z~s(NIHTLBX_depr_theta)+
                     female+age_bl_x+college+inc_gt55k+married_partner,
                   random=list(id=~1), data=white)

tiff(filename = "/Users/egause/Library/CloudStorage/OneDrive-BostonUniversity/ClimLab/MPJ/KHANDLE/Figures/SENAS/Splines/White_Vrmem.tiff",
     res = 300, width = 5, height = 5, units = "in")
plot(wadjvrbepti$gam,  
     pages=1, all.terms=F, rug=T, residuals=F, se=T, shade=T, seWithMean=T,
     ylab="Verbal Episodic", xlab= "Depressive Symptoms",
     xlim = c(-1.5, 2.5), ylim = c(-0.6, 0.4))
dev.off()


wadjsemti <- gamm(sem_z~s(NIHTLBX_depr_theta)+
                   female+age_bl_x+college+inc_gt55k+married_partner,
                 random=list(id=~1), data=white)

tiff(filename = "/Users/egause/Library/CloudStorage/OneDrive-BostonUniversity/ClimLab/MPJ/KHANDLE/Figures/SENAS/Splines/White_Sem.tiff",
     res = 300, width = 5, height = 5, units = "in")
plot(wadjsemti$gam,
     pages=1, all.terms=F, rug=T, residuals=F, se=T, shade=T, seWithMean=T,
     ylab="Semantic", xlab= "Depressive Symptoms",
     xlim = c(-1.5, 2.5), ylim = c(-1, 0.2))
dev.off()


##------------------------------------------------------------------------##
