#----------------------------------------------------------------------------------#
#
#   Script to report demographic data for DPhil fTCD data
#
#   08/01/2019
# 
#----------------------------------------------------------------------------------#

rootdir <- "H:/github/DPhil_Chapter4_fTCD/"

demographics <- read.csv(paste0(rootdir,'Chpt4_fTCD_demographics.csv'), stringsAsFactors=FALSE)
demographics$include <- demographics$wordgen * demographics$PPTT

demographics_mod <- demographics[which(demographics$include == 1), c(1,2,3,4)]
colnames(demographics_mod) <- c('Filename','Age','Sex','Hand')

#----------------------------------------------------------------------------------#

# Report demographic summary statistics - this is using ALL 156 participants

age_mean <- mean(demographics_mod$Age)
age_sd <- sd(demographics_mod$Age)
female_n <- length(which(demographics_mod$Sex == 'F'))

# Contingency table of left/right handedness and male/female sex.
# Hand 0=left, Hand 1=right
table(demographics_mod$Sex,demographics_mod$Hand)
