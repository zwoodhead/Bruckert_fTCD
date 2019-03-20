#----------------------------------------------------------------------------------#
#
#   Model script to test Hypotheses 1 to 4
#
#   Created 26/11/2018.
#   Modified 27/11/2018.
#   Syntax error fixed 11/12/2018
# 
#----------------------------------------------------------------------------------#

#Following the example procedure described here: https://quantdev.ssri.psu.edu/sites/qdev/files/ILD_Ch06_2017_MLMwithHeterogeneousVariance.html

#WE have chosen to use nlme rather than lme4 as the specification of heterogeneous within-person error is more convenient in nlme (lme4 is only implemented via a messy solution).

#----------------------------------------------------------------------------------#

#Check for required R packages and install if not on system

list.of.packages <- c("nlme")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(nlme)
#library(lmerTest)
library(stringr)
library(reshape2)
library(ggplot2)

#Load in data and clean if necessary.

rootdir <- "H:/github/DPhil_Chapter4_fTCD/"

wordgen <- read.csv(paste0(rootdir,'WordGen_results.csv'), stringsAsFactors=FALSE) # The expressive task
pptt <- read.csv(paste0(rootdir,'PPTT_results.csv'), stringsAsFactors=FALSE)       # The receptive task

wordgen <- wordgen[ , c(1,7,14)]
pptt <- pptt[ , c(1,7,14)]

demographics <- read.csv(paste0(rootdir,'Chpt4_fTCD_demographics.csv'), stringsAsFactors=FALSE)
demographics <- demographics[, c(1,4)]
colnames(demographics) <- c('Filename','Hand')

#Now we have three data frames that we need to merge into alldat, but the filenames may be in a different order
demoindex <- vector(mode='integer', dim(wordgen)[1])
ppttindex <- vector(mode='integer', dim(wordgen)[1])

for (i in 1:dim(wordgen)[1]) # start by running through participants in wordgen data
{shortname <- substr(wordgen$Filename[i],1,6) # ignore final characters of name
  demoindex[i] <- which(str_detect(demographics$Filename, shortname))
 ppttindex[i] <- which(str_detect(pptt$Filename, shortname)) 
 }

fTCD_dat <- data.frame('id' = as.factor(wordgen$Filename),
                       'hand' = as.factor(demographics$Hand[demoindex]),
                       'exclude' = as.factor(wordgen$exclusions + pptt$exclusions[ppttindex]), # Participants have to have good data for BOTH tasks to be included
                       'WordGen' = wordgen$LI,
                       'PPTT' = pptt$LI[ppttindex])

fTCD_mod_dat_short <- fTCD_dat[which(fTCD_dat$exclude==0), ] # Removes excluded participants
fTCD_mod_dat<- melt(fTCD_dat) 
colnames(fTCD_mod_dat) <- c('id','hand','exclude','task','LI')

# Normality tests
shapiro.test(fTCD_mod_dat_short$WordGen[which(fTCD_mod_dat_short$hand==1)])
shapiro.test(fTCD_mod_dat_short$WordGen[which(fTCD_mod_dat_short$hand==0)])
shapiro.test(fTCD_mod_dat_short$PPTT[which(fTCD_mod_dat_short$hand==1)])
shapiro.test(fTCD_mod_dat_short$PPTT[which(fTCD_mod_dat_short$hand==0)])

#----------------------------------------------------------------------------------#

#Hypothesis 1: stronger left lateralization for the expressive than the receptive task

#we fit the heterogenous and homogeneous models, chose the optimal fitting model and then interpret the main effect of task.

#Hypothesis 2: stronger left lateralization for right handers than left handers

#we fit the heterogenous and homogeneous models, chose the optimal fitting model and then interpret the main effect of handedness.

#----------------------------------------------------------------------------------#

#Homogeneous variance model

mod0<-lme(fixed=LI~1+hand+task, random=list(id=pdSymm(form=~1)),data=fTCD_mod_dat, na.action="na.exclude", method="REML")

#to extract the results with pvalue (t-test for marginal significance of each fixed effect with other fixed effects)
summary(mod0)

VarCorr(mod0)


#----------------------------------------------------------------------------------#

#Heterogeneous model variance (between-person)

mod1<-lme(fixed=LI ~ 1 + hand + task, random=list(id=pdDiag(form= ~ 0 + hand)),data=fTCD_mod_dat,na.action="na.exclude",method="REML")

summary(mod1)

VarCorr(mod1)


#----------------------------------------------------------------------------------#

#Likelihood ratio test

anova(mod0,mod1)

#----------------------------------------------------------------------------------#

#If we find no difference, we can check for heterogeneous within-person

#Heterogeneous model variance (within-person)

mod2<-lme(fixed=LI ~ 1 + hand + task, random = list(id=pdSymm(form = ~1 )), weights=varIdent(form=~1 | hand), data=fTCD_mod_dat, na.action=na.exclude, method="REML")

summary(mod2)

VarCorr(mod2)

#extract variances for hand and see if they are different.

#residual std deviation
summary(mod2)$sigma

#Residual variance of right hand (dependent on the order of factors)
(summary(mod2)$sigma*1)^2

#Residual variance of left hand
(summary(mod2)$sigma*coef(mod2$modelStruct$varStruct,uncons=FALSE))^2


#----------------------------------------------------------------------------------#

#Likelihood ratio test between homogeneous and heterogeneous withi-person

anova(mod0,mod2)

#----------------------------------------------------------------------------------#

#Hypothesis 3: Significant correlation between expressive and receptive task

#Test with Pearson's correlation

#Hypothesis 4: More variable relationship between expressive and receptive tasks in

#left handers than right handers

#Test by comparing variability of cook's distance between groups using Fligner-Killeen test

#----------------------------------------------------------------------------------#

#Pearson's correlation between expressive and receptive tasks

H3_results <- cor.test(fTCD_mod_dat_short$WordGen, fTCD_mod_dat_short$PPTT)

#Plot data
levels(fTCD_mod_dat_short$hand) <- c('Left', 'Right')
print(ggplot(fTCD_mod_dat_short,aes(y=PPTT,x=WordGen,colour=hand))+geom_point(size=2)+theme_bw()+ scale_color_manual(values=c("orange1", "royalblue2")))

#Fit a linear model to both handedness groups

mymod<-lm(PPTT~WordGen,data=fTCD_mod_dat_short)

#Calculate Cook's Distance for each participant
cooks<-cooks.distance(mymod)
fTCD_mod_dat_short$cooks<-cooks

#Run the Fligner-Killeen test
H4_p <- fligner.test(fTCD_mod_dat_short$cooks ~ fTCD_mod_dat_short$hand)$p.value


levels(fTCD_mod_dat$hand) <- c('Left', 'Right')
#plots

ggplot(fTCD_mod_dat,aes(x=LI,fill=hand))+geom_density(alpha=0.5) +theme_bw() + scale_fill_manual(values=c("orange1", "royalblue2"))

ggplot(fTCD_mod_dat,aes(x=LI,fill=task))+geom_density(alpha=0.5)+theme_bw() + scale_fill_manual(values=c("orange1", "royalblue2"))

ggplot(fTCD_mod_dat_short,aes(x=cooks))+geom_histogram()+facet_wrap(~hand)+theme_bw()+ scale_fill_manual(values=c("orange1", "royalblue2"))


# Pirate Plot
library('yarrr')

pirateplot(data=fTCD_mod_dat, LI~task*hand)
