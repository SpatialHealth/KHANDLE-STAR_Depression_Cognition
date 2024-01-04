
###################################################
## Multi-Level Regression Models on Imputed Data ##
###################################################

#written by: Emma Gause 
#Date: 09/06/23
#Last updated: "

#load libraries:
library("tidyverse")
library("dplyr")
library("lme4")
library("boot")

#Create path to directory
datadir <- "/Users/egause/Library/CloudStorage/OneDrive-BostonUniversity/ClimLab/MPJ/KHANDLE/Analysis/SENAS/"

#read in complete case data
ccdat <- readRDS(paste0(datadir, "Complete_Cases_083023.rds"))

##------------------------------------------------------------------------##


#add in function from Ruijia for comparing race-specific differences
  #edit for our context -- this is interaction for the depression exposure by race

test <- lmer(exec_z~ NIHTLBX_depr_theta+yrsbl_x+age_bl_x+female+college+inc_gt55k+married_partner+(1|id), data=ccdat)
test

######
boot.long <- function (data, B, out_var, race_var) {
  id.list = unique(ccdat$id)
  stat = c()
  for (i in 1:B) {
    set.seed(i)
    id.boot = sample(id.list,replace = T)
    row.boot = unlist(lapply(id.boot, function(x)which(data$id == x)))
    x <- ccdat[row.boot,] # allows boot to select sample
    dat_non_Black <- x %>% filter(race_fact==race_var)
    dat_Black <- x %>% filter(race_fact=="Black")
    f <- paste0(out_var, "~ NIHTLBX_depr_theta+yrsbl_x+age_bl_x+female+college+inc_gt55k+married_partner+(1|id)")
    m2_non_Black <-lmer(as.formula(f), data=dat_non_Black)
    m2_Black<-lmer(as.formula(f), data=dat_Black)
    stat = c(stat,fixef(m2_non_Black)[2]-fixef(m2_Black)[2])
  }
  return(stat)
}


#outcomes: exec_z, vrmem_z, sem_z
#strata: race_fact

##run the bootstrap
best_w_ex <-boot.long(data=ccdat, B=1000, out_var = "exec_z", race_var = "White")
best_l_ex <-boot.long(data=ccdat, B=1000, out_var = "exec_z", race_var = "LatinX")
best_a_ex <-boot.long(data=ccdat, B=1000, out_var = "exec_z", race_var = "Asian")

best_w_vr <-boot.long(data=ccdat, B=1000, out_var = "vrmem_z", race_var = "White")
best_l_vr <-boot.long(data=ccdat, B=1000, out_var = "vrmem_z", race_var = "LatinX")
best_a_vr <-boot.long(data=ccdat, B=1000, out_var = "vrmem_z", race_var = "Asian")

best_w_sm <-boot.long(data=ccdat, B=1000, out_var = "sem_z", race_var = "White")
best_l_sm <-boot.long(data=ccdat, B=1000, out_var = "sem_z", race_var = "LatinX")
best_a_sm <-boot.long(data=ccdat, B=1000, out_var = "sem_z", race_var = "Asian")

###get the median and Cis after bootstrap
bci_w_ex <-quantile(best_w_ex, c(0.50, 0.025, 0.975))
bci_l_ex <-quantile(best_l_ex, c(0.50, 0.025, 0.975))
bci_a_ex <-quantile(best_a_ex, c(0.50, 0.025, 0.975))

bci_w_vr <-quantile(best_w_vr, c(0.50, 0.025, 0.975))
bci_l_vr <-quantile(best_l_vr, c(0.50, 0.025, 0.975))
bci_a_vr <-quantile(best_a_vr, c(0.50, 0.025, 0.975))

bci_w_sm <-quantile(best_w_sm, c(0.50, 0.025, 0.975))
bci_l_sm <-quantile(best_l_sm, c(0.50, 0.025, 0.975))
bci_a_sm <-quantile(best_a_sm, c(0.50, 0.025, 0.975))


#combine estimates
output <- rbind(bci_w_ex, bci_l_ex, bci_a_ex, 
                bci_w_vr, bci_l_vr, bci_a_vr,
                bci_w_sm, bci_l_sm, bci_a_sm)


write.csv(output, paste0(datadir, "Results/Bootstrapped_Estimates.csv"))




##------------------------------------------------------------------##

## Now test for the interaction w/ time (i.e. decline)
test_int <- lmer(exec_z~ NIHTLBX_depr_theta*yrsbl_x+age_bl_x+female+college+inc_gt55k+married_partner+(1|id), data=ccdat)
test_int


######
boot.long.int <- function (data, B, out_var, race_var) {
  id.list = unique(ccdat$id)
  stat = c()
  for (i in 1:B) {
    set.seed(i)
    id.boot = sample(id.list,replace = T)
    row.boot = unlist(lapply(id.boot, function(x)which(data$id == x)))
    x <- ccdat[row.boot,] # allows boot to select sample
    dat_non_Black <- x %>% filter(race_fact==race_var)
    dat_Black <- x %>% filter(race_fact=="Black")
    f <- paste0(out_var, "~ NIHTLBX_depr_theta*yrsbl_x+age_bl_x+female+college+inc_gt55k+married_partner+(1|id)")
    m2_non_Black <-lmer(as.formula(f), data=dat_non_Black)
    m2_Black<-lmer(as.formula(f), data=dat_Black)
    stat = c(stat,fixef(m2_non_Black)[9]-fixef(m2_Black)[9])
  }
  return(stat)
}


#outcomes: exec_z, vrmem_z, sem_z
#strata: race_fact

##run the bootstrap
best_w_ex_int <-boot.long.int(data=ccdat, B=1000, out_var = "exec_z", race_var = "White")
best_l_ex_int <-boot.long.int(data=ccdat, B=1000, out_var = "exec_z", race_var = "LatinX")
best_a_ex_int <-boot.long.int(data=ccdat, B=1000, out_var = "exec_z", race_var = "Asian")

best_w_vr_int <-boot.long.int(data=ccdat, B=1000, out_var = "vrmem_z", race_var = "White")
best_l_vr_int <-boot.long.int(data=ccdat, B=1000, out_var = "vrmem_z", race_var = "LatinX")
best_a_vr_int <-boot.long.int(data=ccdat, B=1000, out_var = "vrmem_z", race_var = "Asian")

best_w_sm_int <-boot.long.int(data=ccdat, B=1000, out_var = "sem_z", race_var = "White")
best_l_sm_int <-boot.long.int(data=ccdat, B=1000, out_var = "sem_z", race_var = "LatinX")
best_a_sm_int <-boot.long.int(data=ccdat, B=1000, out_var = "sem_z", race_var = "Asian")

###get the median and Cis after bootstrap
bci_w_ex_int <-quantile(best_w_ex_int, c(0.50, 0.025, 0.975))
bci_l_ex_int <-quantile(best_l_ex_int, c(0.50, 0.025, 0.975))
bci_a_ex_int <-quantile(best_a_ex_int, c(0.50, 0.025, 0.975))

bci_w_vr_int <-quantile(best_w_vr_int, c(0.50, 0.025, 0.975))
bci_l_vr_int <-quantile(best_l_vr_int, c(0.50, 0.025, 0.975))
bci_a_vr_int <-quantile(best_a_vr_int, c(0.50, 0.025, 0.975))

bci_w_sm_int <-quantile(best_w_sm_int, c(0.50, 0.025, 0.975))
bci_l_sm_int <-quantile(best_l_sm_int, c(0.50, 0.025, 0.975))
bci_a_sm_int <-quantile(best_a_sm_int, c(0.50, 0.025, 0.975))


#combine estimates
output_int <- rbind(bci_w_ex_int, bci_l_ex_int, bci_a_ex_int, 
                bci_w_vr_int, bci_l_vr_int, bci_a_vr_int,
                bci_w_sm_int, bci_l_sm_int, bci_a_sm_int)


write.csv(output_int, paste0(datadir, "Results/Bootstrapped_Estimates_Interaction.csv"))
