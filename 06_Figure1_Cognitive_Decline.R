
##################################################
## Visualization of cognition over time by race ##
##################################################

#written by: Emma Gause 
#Date: 12/19/22
#Last updated: 08/30/23

#load libraries:
library("tidyverse")
library("dplyr")
library("mice") 
library("miceadds")
library("mitml")
library("lme4")
library("Hmisc")
library("lcmm")
library("ggforce")
library("ggdist")
library("gghalves")
library("ggthemes")
library("tidyquant")

#Create path to directory
datadir <- "/Users/egause/Library/CloudStorage/OneDrive-BostonUniversity/ClimLab/MPJ/KHANDLE/Analysis/SENAS/"

#read in complete case data
ccdat <- readRDS(paste0(datadir, "Complete_Cases_083023.rds"))
#keep wave 1 for baseline covars
wave1 <- ccdat %>% filter(waveseq==1)

##------------------------------------------------------------------------##

#get race stratified results
#FOR CC DATA
asian <- ccdat %>% filter(race_fact=="Asian")
black <- ccdat %>% filter(race_fact=="Black")
latin <- ccdat %>% filter(race_fact=="LatinX")
white <- ccdat %>% filter(race_fact=="White")

##------------------------------------------------------------------------##
##------------------------------------------------------------------------##

#id must be numeric?
ccdat <- ccdat %>% mutate(newseq = as.integer(factor(id)))
asian <- asian %>% mutate(newseq = as.integer(factor(id)))
black <- black %>% mutate(newseq = as.integer(factor(id)))
latin <- latin %>% mutate(newseq = as.integer(factor(id)))
white <- white %>% mutate(newseq = as.integer(factor(id)))


#set new models so we can use "predictY"
  # CREATE FOR ALL OUTCOMES

#EXCFUNC
oexc <- hlme(exec_z ~ NIHTLBX_depr_theta+yrsbl_x+NIHTLBX_depr_theta*yrsbl_x+
               female+age_bl_x+college+inc_gt55k+married_partner, 
             random = ~1, subject="newseq", data = ccdat)
summary(oexc)

aexc <- hlme(exec_z ~ NIHTLBX_depr_theta+yrsbl_x+NIHTLBX_depr_theta*yrsbl_x+
               female+age_bl_x+college+inc_gt55k+married_partner, 
             random = ~1, subject="newseq", data = asian)
summary(aexc)

bexc <- hlme(exec_z ~ NIHTLBX_depr_theta+yrsbl_x+NIHTLBX_depr_theta*yrsbl_x+
               female+age_bl_x+college+inc_gt55k+married_partner, 
             random = ~1, subject="newseq", data = black)
summary(bexc)

lexc <- hlme(exec_z ~ NIHTLBX_depr_theta+yrsbl_x+NIHTLBX_depr_theta*yrsbl_x+
               female+age_bl_x+college+inc_gt55k+married_partner, 
             random = ~1, subject="newseq", data = latin)
summary(lexc)

wexc <- hlme(exec_z ~ NIHTLBX_depr_theta+yrsbl_x+NIHTLBX_depr_theta*yrsbl_x+
               female+age_bl_x+college+inc_gt55k+married_partner, 
             random = ~1, subject="newseq", data = white)
summary(wexc)



#VRMEM
ovrm <- hlme(vrmem_z ~ NIHTLBX_depr_theta+yrsbl_x+NIHTLBX_depr_theta*yrsbl_x+
                   female+age_bl_x+college+inc_gt55k+married_partner, 
                 random = ~1, subject="newseq", data = ccdat)
summary(ovrm)

avrm <- hlme(vrmem_z ~ NIHTLBX_depr_theta+yrsbl_x+NIHTLBX_depr_theta*yrsbl_x+
                 female+age_bl_x+college+inc_gt55k+married_partner, 
               random = ~1, subject="newseq", data = asian)
summary(avrm)

bvrm <- hlme(vrmem_z ~ NIHTLBX_depr_theta+yrsbl_x+NIHTLBX_depr_theta*yrsbl_x+
                 female+age_bl_x+college+inc_gt55k+married_partner, 
               random = ~1, subject="newseq", data = black)
summary(bvrm)

lvrm <- hlme(vrmem_z ~ NIHTLBX_depr_theta+yrsbl_x+NIHTLBX_depr_theta*yrsbl_x+
                 female+age_bl_x+college+inc_gt55k+married_partner, 
               random = ~1, subject="newseq", data = latin)
summary(lvrm)

wvrm <- hlme(vrmem_z ~ NIHTLBX_depr_theta+yrsbl_x+NIHTLBX_depr_theta*yrsbl_x+
                 female+age_bl_x+college+inc_gt55k+married_partner, 
               random = ~1, subject="newseq", data = white)
summary(wvrm)


#SEM
osem <- hlme(sem_z ~ NIHTLBX_depr_theta+yrsbl_x+NIHTLBX_depr_theta*yrsbl_x+
               female+age_bl_x+college+inc_gt55k+married_partner, 
             random = ~1, subject="newseq", data = ccdat)
summary(osem)

asem <- hlme(sem_z ~ NIHTLBX_depr_theta+yrsbl_x+NIHTLBX_depr_theta*yrsbl_x+
               female+age_bl_x+college+inc_gt55k+married_partner, 
             random = ~1, subject="newseq", data = asian)
summary(asem)

bsem <- hlme(sem_z ~ NIHTLBX_depr_theta+yrsbl_x+NIHTLBX_depr_theta*yrsbl_x+
               female+age_bl_x+college+inc_gt55k+married_partner, 
             random = ~1, subject="newseq", data = black)
summary(bsem)

lsem <- hlme(sem_z ~ NIHTLBX_depr_theta+yrsbl_x+NIHTLBX_depr_theta*yrsbl_x+
               female+age_bl_x+college+inc_gt55k+married_partner, 
             random = ~1, subject="newseq", data = latin)
summary(lsem)

wsem <- hlme(sem_z ~ NIHTLBX_depr_theta+yrsbl_x+NIHTLBX_depr_theta*yrsbl_x+
               female+age_bl_x+college+inc_gt55k+married_partner, 
             random = ~1, subject="newseq", data = white)
summary(wsem)


#predict new data for visualization - use 4 waves since this is average time since baseline 
newdat_o = data.frame(yrsbl_x = seq(0, 4, length=10))
newdat_a = data.frame(yrsbl_x = seq(0, 4, length=10))
newdat_b = data.frame(yrsbl_x = seq(0, 4, length=10))
newdat_l = data.frame(yrsbl_x = seq(0, 4, length=10))
newdat_w = data.frame(yrsbl_x = seq(0, 4, length=10))


#set the covars to be the mean
#female+age_bl_x+college+inc_gt55k+married_partner
newdat_o$female <- round(mean(ccdat$female))
newdat_o$age_bl_x <- mean(ccdat$age_bl_x)
newdat_o$college <- round(mean(ccdat$college))
newdat_o$inc_gt55k <- round(mean(ccdat$inc_gt55k))
newdat_o$married_partner <- round(mean(ccdat$married_partner))

newdat_a$female <- round(mean(ccdat$female))
newdat_a$age_bl_x <- mean(ccdat$age_bl_x)
newdat_a$college <- round(mean(ccdat$college))
newdat_a$inc_gt55k <- round(mean(ccdat$inc_gt55k))
newdat_a$married_partner <- round(mean(ccdat$married_partner))

newdat_b$female <- round(mean(ccdat$female))
newdat_b$age_bl_x <- mean(ccdat$age_bl_x)
newdat_b$college <- round(mean(ccdat$college))
newdat_b$inc_gt55k <- round(mean(ccdat$inc_gt55k))
newdat_b$married_partner <- round(mean(ccdat$married_partner))

newdat_l$female <- round(mean(ccdat$female))
newdat_l$age_bl_x <- mean(ccdat$age_bl_x)
newdat_l$college <- round(mean(ccdat$college))
newdat_l$inc_gt55k <- round(mean(ccdat$inc_gt55k))
newdat_l$married_partner <- round(mean(ccdat$married_partner))

newdat_w$female <- round(mean(ccdat$female))
newdat_w$age_bl_x <- mean(ccdat$age_bl_x)
newdat_w$college <- round(mean(ccdat$college))
newdat_w$inc_gt55k <- round(mean(ccdat$inc_gt55k))
newdat_w$married_partner <- round(mean(ccdat$married_partner))

#assign the exposed/unexposed
#predict the new!
#needs to be done together because covar name must match that of model

hplot <- wave1 %>% ggplot(aes(x=NIHTLBX_depr_theta, fill=race_fact)) + geom_histogram()
hplot

describe(wave1$NIHTLBX_depr_theta)
newdat_o$NIHTLBX_depr_theta = -1.1800 #10th percentile for depression
o_10exc = predictY(oexc, newdat_o, var.time="yrsbl_x",draws = T)
o_10vrm = predictY(ovrm, newdat_o, var.time="yrsbl_x",draws = T)
o_10sem = predictY(osem, newdat_o, var.time="yrsbl_x",draws = T)
newdat_o$NIHTLBX_depr_theta = 0.7592 #90th percentile for depression
o_90exc = predictY(oexc, newdat_o, var.time="yrsbl_x",draws = T)
o_90vrm = predictY(ovrm, newdat_o, var.time="yrsbl_x",draws = T)
o_90sem = predictY(osem, newdat_o, var.time="yrsbl_x",draws = T)


describe(wave1$NIHTLBX_depr_theta[wave1$race_fact=="Asian"])
newdat_a$NIHTLBX_depr_theta = -1.5833 #10th percentile for depression
a_10exc = predictY(aexc, newdat_a, var.time="yrsbl_x",draws = T)
a_10vrm = predictY(avrm, newdat_a, var.time="yrsbl_x",draws = T)
a_10sem = predictY(asem, newdat_a, var.time="yrsbl_x",draws = T)
newdat_a$NIHTLBX_depr_theta = 0.8842 #90th percentile for depression
a_90exc = predictY(aexc, newdat_a, var.time="yrsbl_x",draws = T)
a_90vrm = predictY(avrm, newdat_a, var.time="yrsbl_x",draws = T)
a_90sem = predictY(asem, newdat_a, var.time="yrsbl_x",draws = T)


describe(wave1$NIHTLBX_depr_theta[wave1$race_fact=="Black"])
newdat_b$NIHTLBX_depr_theta = -1.1800 #10th percentile for depression
b_10exc = predictY(bexc, newdat_b, var.time="yrsbl_x",draws = T)
b_10vrm = predictY(bvrm, newdat_b, var.time="yrsbl_x",draws = T)
b_10sem = predictY(bsem, newdat_b, var.time="yrsbl_x",draws = T)
newdat_b$NIHTLBX_depr_theta = 0.6908 #90th percentile for depression
b_90exc = predictY(bexc, newdat_b, var.time="yrsbl_x",draws = T)
b_90vrm = predictY(bvrm, newdat_b, var.time="yrsbl_x",draws = T)
b_90sem = predictY(bsem, newdat_b, var.time="yrsbl_x",draws = T)


describe(wave1$NIHTLBX_depr_theta[wave1$race_fact=="LatinX"])
newdat_l$NIHTLBX_depr_theta = -1.0472 #10th percentile for depression
l_10exc = predictY(lexc, newdat_l, var.time="yrsbl_x",draws = T)
l_10vrm = predictY(lvrm, newdat_l, var.time="yrsbl_x",draws = T)
l_10sem = predictY(lsem, newdat_l, var.time="yrsbl_x",draws = T)
newdat_l$NIHTLBX_depr_theta = 0.9713 #90th percentile for depression
l_90exc = predictY(lexc, newdat_l, var.time="yrsbl_x",draws = T)
l_90vrm = predictY(lvrm, newdat_l, var.time="yrsbl_x",draws = T)
l_90sem = predictY(lsem, newdat_l, var.time="yrsbl_x",draws = T)


describe(wave1$NIHTLBX_depr_theta[wave1$race_fact=="White"])
newdat_w$NIHTLBX_depr_theta = -0.7367 #10th percentile for depression
w_10exc = predictY(wexc, newdat_w, var.time="yrsbl_x",draws = T)
w_10vrm = predictY(wvrm, newdat_w, var.time="yrsbl_x",draws = T)
w_10sem = predictY(wsem, newdat_w, var.time="yrsbl_x",draws = T)
newdat_w$NIHTLBX_depr_theta = 0.7465 #90th percentile for depression
w_90exc = predictY(wexc, newdat_w, var.time="yrsbl_x",draws = T)
w_90vrm = predictY(wvrm, newdat_w, var.time="yrsbl_x",draws = T)
w_90sem = predictY(wsem, newdat_w, var.time="yrsbl_x",draws = T)



##------------------------------------------------------------------------##


## plot


tiff(filename = "/Users/egause/Library/CloudStorage/OneDrive-BostonUniversity/ClimLab/MPJ/KHANDLE/Figures/SENAS_Figure2_083023.tiff",
     res = 300, width = 3000, height = 3700)

par(mfrow=c(5,3), oma=c(6,6,6,1), mai=c(0.4,0.4,0.2,0.2), xpd=NA)
  

#overall - EXC
base::plot(o_10exc, shades = T, col="red", las=1,
     xlab = NA, ylab = NA, main = NA, legend = NULL,
     xlim = c(0, 4), ylim = c(-1, 1), lwd=3)
base::plot(o_90exc, shades = T, col="cyan4", lty=3, add=T, lwd=3)

mtext(~bold("Overall Cohort"), side=2, line = 4, cex=1.3)
mtext(~bold("Executive Function"), side=3, line = 3, cex=1.3)


#overall - VRM
base::plot(o_10vrm, shades = T, col="red", las=1,
     xlim = c(0, 4), ylim = c(-1, 1), lwd=3,
     xlab = NA, ylab = NA, main = NA, legend = NULL)
base::plot(o_90vrm, shades = T, col="cyan4", lty=3, add=T, lwd=3)
mtext(~bold("Verbal Episodic Memory"), side=3, line = 3, cex=1.3)

#overall - SEM
base::plot(o_10sem, shades = T, col="red", las=1,
     xlab = NA, ylab = NA, main = NA, legend = NULL, 
     xlim = c(0, 4), ylim = c(-1, 1), lwd=3)
base::plot(o_90sem, shades = T, col="cyan4", lty=3, add=T, lwd=3)
mtext(~bold("Semantic Memory"), side=3, line = 3, cex=1.3)


###############

#Asian - EXC
base::plot(a_10exc, shades = T, col="red", las=1,
     xlim = c(0, 4), ylim = c(-1, 1), lwd=3,
     xlab = NA, ylab = NA, main = NA, legend = NULL)
base::plot(a_90exc, shades = T, col="cyan4", lty=3, add=T, lwd=3)

mtext(~bold("Asian"), side=2, line = 4, cex=1.3)


#Asian - VRM
base::plot(a_10vrm, shades = T, col="red", las=1,  
     xlim = c(0, 4), ylim = c(-1, 1), lwd=3,
     xlab = NA, ylab = NA, main = NA, legend = NULL)
base::plot(a_90vrm, shades = T, col="cyan4", lty=3, add=T, lwd=3)


#Asian - SEM
base::plot(a_10sem, shades = T, col="red", las=1, 
     xlim = c(0, 4), ylim = c(-1, 1), lwd=3,
     xlab = NA, ylab = NA, main = NA, legend = NULL)
base::plot(a_90sem, shades = T, col="cyan4", lty=3, add=T, lwd=3)


###############


#Black - EXC
base::plot(b_10exc, shades = T, col="red", las=1,
     xlim = c(0, 4), ylim = c(-1, 1), lwd=3,
     xlab = NA, ylab = NA, main = NA, legend = NULL)
base::plot(b_90exc, shades = T, col="cyan4", lty=3, add=T, lwd=3)

mtext(~bold("Black"), side=2, line = 4, cex=1.3)


#Black - VRM
base::plot(b_10vrm, shades = T, col="red", las=1,
     xlim = c(0, 4), ylim = c(-1, 1), lwd=3,
     xlab = NA, ylab = NA, main = NA, legend = NULL)
base::plot(b_90vrm, shades = T, col="cyan4", lty=3, add=T, lwd=3)


#Black - SEM
base::plot(b_10sem, shades = T, col="red", las=1, 
     xlim = c(0, 4), ylim = c(-1, 1), lwd=3,
     xlab = NA, ylab = NA, main = NA, legend = NULL)
base::plot(b_90sem, shades = T, col="cyan4", lty=3, add=T, lwd=3)



###############


#LatinX - EXC
base::plot(l_10exc, shades = T, col="red", las=1,
     xlim = c(0, 4), ylim = c(-1, 1), lwd=3,
     xlab = NA, ylab = NA, main = NA, legend = NULL)
base::plot(l_90exc, shades = T, col="cyan4", lty=3, add=T, lwd=3)

mtext(~bold("LatinX"), side=2, line = 4, cex=1.3)


#LatinX - VRM
base::plot(l_10vrm, shades = T, col="red", las=1, 
     xlim = c(0, 4), ylim = c(-1, 1), lwd=3,
     xlab = NA, ylab = NA, main = NA, legend = NULL)
base::plot(l_90vrm, shades = T, col="cyan4", lty=3, add=T, lwd=3)


#LatinX - SEM
base::plot(l_10sem, shades = T, col="red", las=1, 
     xlim = c(0, 4), ylim = c(-1, 1), lwd=3,
     xlab = NA, ylab = NA, main = NA, legend = NULL)
base::plot(l_90sem, shades = T, col="cyan4", lty=3, add=T, lwd=3)


###############


#White - EXC
base::plot(w_10exc, shades = T, col="red", las=1,
     xlim = c(0, 4), ylim = c(-1, 1), lwd=3,
     xlab = NA, ylab = NA, main = NA, legend = NULL)
base::plot(w_90exc, shades = T, col="cyan4", lty=3, add=T, lwd=3)
mtext("Follow-Up Years", side=1, line=2.5, cex=0.8)
mtext(~bold("White"), side=2, line = 4, cex=1.3)


#White - VRM
base::plot(w_10vrm, shades = T, col="red", las=1, 
     xlim = c(0, 4), ylim = c(-1, 1), lwd=3,
     xlab = NA, ylab = NA, main = NA, legend = NULL)
base::plot(w_90vrm, shades = T, col="cyan4", lty=3, add=T, lwd=3)
mtext("Follow-Up Years", side=1, line=2.5, cex=0.8)


#White - SEM
base::plot(w_10sem, shades = T, col="red", las=1,
     xlim = c(0, 4), ylim = c(-1, 1), lwd=3,
     xlab = NA, ylab = NA, main = NA, legend = NULL)
base::plot(w_90sem, shades = T, col="cyan4", lty=3, add=T, lwd=3)
mtext("Follow-Up Years", side=1, line=2.5, cex=0.8)

# Add a legend
legend(legend=c("Lowest Depression", "Highest Depression"),
       col=c("red", "cyan4"), ncol = 2,
       lty=1:3, cex=1.3, xpd=NA, 
       x =-7, y = -2)


dev.off()

##------------------------------------------------------------------------##

#now plot cloud lots of baseline depression symptoms stratified by race/ethnicity
str(wave1)


rc1 <- wave1 %>% ggplot(aes(race_summary, NIHTLBX_depr_theta)) + 
  ggdist::stat_halfeye(adjust = .5, width = .3, .width = 0, justification = -.3, point_colour = NA) + 
  geom_boxplot(width = .1, outlier.shape = NA) +
  ggdist::stat_dots(side = "left", dotsize = .3, justification = 1.1, binwidth = .1)
rc1


rc2 <- wave1 %>% ggplot(aes(race_summary, NIHTLBX_depr_theta)) + 
  ggdist::stat_halfeye(adjust = .5, width = .7, .width = 0, justification = -.2, point_colour = NA) + 
  geom_boxplot(width = .2, outlier.shape = NA) + 
  geom_jitter(width = .05, alpha = .3)
rc2
  

#I like the barcode version best

#save it

tiff(filename = "/Users/egause/Library/CloudStorage/OneDrive-BostonUniversity/ClimLab/MPJ/KHANDLE/Figures/SENAS_Figure1_083123.tiff",
     res = 300, width = 2500, height = 2000)

rc4 <- wave1 %>% ggplot(aes(race_summary, NIHTLBX_depr_theta, fill = race_summary)) + 
  geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.8) + 
  gghalves::geom_half_point(side = "l", range_scale = 0, shape = 95, size = 8, alpha = .3) + 
  theme_gdocs() + 
  labs(x = "Race/Ethnicity Groups",
       y = "Baseline Depressive Symptoms")
  
rc4 + theme(legend.position = "none")

dev.off()


#do the same thing by STAR vs. KHANDLE
str(wave1)
studyplot <- wave1 %>% ggplot(aes(study, NIHTLBX_depr_theta, fill = study)) + 
  ggdist::stat_halfeye(adjust = .6, width = .8, .width = 0, justification = -.2, point_colour = NA) + 
  geom_boxplot(width = .1, outlier.shape = NA, alpha = 0.8) + 
  gghalves::geom_half_point(side = "l", range_scale = 0, shape = 95, size = 8, alpha = .3) + 
  theme_gdocs() + 
  labs(x = "Study Cohort",
       y = "Baseline Depressive Symptoms")

studyplot + theme(legend.position = "none")


