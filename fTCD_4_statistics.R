#----------------------------------------------------------------------------------#
#
#   Model script to test Hypotheses 1 to 4
#
#   Created 26/11/2018.
#   Modified 27/11/2018.
#   Syntax error fixed 11/12/2018
#   Edited 09/04/2019 - adding figure 5 and other adjustments to plots.
# 
#----------------------------------------------------------------------------------#

#Following the example procedure described here: https://quantdev.ssri.psu.edu/sites/qdev/files/ILD_Ch06_2017_MLMwithHeterogeneousVariance.html

#WE have chosen to use nlme rather than lme4 as the specification of heterogeneous within-person error is more convenient in nlme (lme4 is only implemented via a messy solution).

#----------------------------------------------------------------------------------#

#Check for required R packages and install if not on system

list.of.packages <- c("nlme", "yarrr", "tidyverse")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(nlme)
library(yarrr)
library(tidyverse)
library(stringr)
library(reshape2)
library(ggplot2)

# Load in data
rootdir <- "/Users/zoe/OneDrive - Nexus365/github/DPhil_Chapter4_fTCD/"

wordgen <- read.csv(paste0(rootdir,'WordGen_results.csv'), stringsAsFactors=FALSE)
pptt <- read.csv(paste0(rootdir,'PPTT_results.csv'), stringsAsFactors=FALSE)     
demographics <- read.csv(paste0(rootdir,'Chpt4_fTCD_demographics.csv'), stringsAsFactors=FALSE)
wordgen_retest <- read.csv(paste0(rootdir,'WordGen_RETEST_results.csv'), stringsAsFactors=FALSE)
pptt_retest <- read.csv(paste0(rootdir,'PPTT_RETEST_results.csv'), stringsAsFactors=FALSE)

# Tidy data
wordgen$ID<- substr(wordgen$Filename, start = 1, stop = 3)
wordgen <- wordgen %>% select(ID, nFinal, LI, se, lat, odd, even, exclusions)
colnames(wordgen) <- c('ID','WG.n', 'WG.LI', 'WG.se', 'WG.lat', 'WG.odd', 'WG.even', 'WG.exclusions')

pptt$ID <- substr(pptt$Filename,start=1,stop=3)
pptt <- pptt %>% select(ID, nFinal, LI, se, lat, odd, even, exclusions)
colnames(pptt) <- c('ID','PPTT.n', 'PPTT.LI', 'PPTT.se', 'PPTT.lat', 'PPTT.odd', 'PPTT.even', 'PPTT.exclusions')

demographics$ID<- substr(demographics$ID,start=1,stop=3)

wordgen_retest$ID <- substr(wordgen_retest$Filename, start = 8, stop = 10)
wordgen_retest <- wordgen_retest %>% select(ID, LI)

pptt_retest$ID <- substr(pptt_retest$Filename, start = 8, stop = 10)
pptt_retest <- pptt_retest %>% select(ID, LI)

# Merge the wordgen, pptt and demographics data frames into fTCD_dat according to ID
fTCD_dat <- merge(demographics,wordgen,by="ID") 
fTCD_dat <- merge(fTCD_dat,pptt,by="ID")
#There are 154 participants with data on both tasks and also on demographics

# Will just delete those with exclusion in either task (WG.exclusions or PPTT.exclusions)
bothexclude<-fTCD_dat$WG.exclusions + fTCD_dat$PPTT.exclusions
fTCD_dat_short <- fTCD_dat[bothexclude==0, ] # Removes excluded participants
# This leaves 151 participants with useable data on both tasks

#fTCD_mod_dat<- melt(fTCD_dat[ , -c(6,7)]) #excluding EHI and handpref columns
#colnames(fTCD_mod_dat) <- c('id','Hand','exclude','Task','LI')

#----------------------------------------------------------------------------------#
# Participants
# Report demographic summary statistics - this is using the 151 participants with usable data

age_mean <- mean(fTCD_dat_short$age)
age_sd <- sd(fTCD_dat$age)

cat(paste('Average age =', floor(age_mean), 'years and', round(12 *(age_mean - floor(age_mean))), 'months\n', 
          'SD =', floor(age_sd), 'years and', round(12 *(age_sd - floor(age_sd))), 'months.'))

# Contingency table of left/right handedness and male/female sex.
# Hand 0=left, Hand 1=right
table(fTCD_dat_short$sex,fTCD_dat_short$hand_self_report)

# Count participants with retest data
cat(paste('Word Generation retest data was acquired from', sum(fTCD_dat_short$wordgen_retest), 'participants'))
cat(paste('Semantic Association retest data was acquired from', sum(fTCD_dat_short$PPTT_retest), 'participants'))

#----------------------------------------------------------------------------------#
# Data Quality

# Calculate the number of trials excluded for each task
wordgen_excluded_trials <- length(fTCD_dat_short$ID) * 23 - sum(fTCD_dat_short$WG.n)
wordgen_excluded_trials_pc <- wordgen_excluded_trials / (length(fTCD_dat_short$ID) * 23) * 100

pptt_excluded_trials <- length(fTCD_dat_short$ID) * 15 - sum(fTCD_dat_short$PPTT.n)
pptt_excluded_trials_pc <- pptt_excluded_trials / (length(fTCD_dat_short$ID) * 15) * 100

cat(paste('For word generation,', round(wordgen_excluded_trials_pc,2), '% of all trials were excluded,\n',
          'and for semantic association,', round(pptt_excluded_trials_pc,2), '% were excluded.'))

# Calculate split-half reliability (comparing LI from odd and even trials)
WG_splithalf <- cor.test(fTCD_dat_short$WG.odd, fTCD_dat_short$WG.even)
plot(fTCD_dat_short$WG.odd, fTCD_dat_short$WG.even)
PPTT_splithalf <- cor.test(fTCD_dat_short$PPTT.odd, fTCD_dat_short$PPTT.even)
plot(fTCD_dat_short$PPTT.odd, fTCD_dat_short$PPTT.even) #PPTT

# Join with retest data and calculate test-retest reliability
fTCD_dat_short$WG2.LI <- NA
fTCD_dat_short$PPTT2.LI <- NA

w < which(fTCD_dat_short$ID %in% wordgen_retest$ID)
fTCD_dat_short$WG2.LI[w] <- wordgen_retest$LI

w <- which(fTCD_dat_short$ID %in% pptt_retest$ID)
fTCD_dat_short$PPTT2.LI[w] <- pptt_retest$LI

WG_test_retest <- cor.test(fTCD_dat_short$WG.LI, fTCD_dat_short$WG2.LI)
plot(fTCD_dat_short$WG.LI, fTCD_dat_short$WG2.LI)

PPTT_test_retest <- cor.test(fTCD_dat_short$PPTT.LI, fTCD_dat_short$PPTT2.LI)
plot(fTCD_dat_short$PPTT.LI, fTCD_dat_short$PPTT2.LI)

#----------------------------------------------------------------------------------#
# Laterality Indices

# Shapiro-Wilks normality tests
shapiro.test(fTCD_dat_short$WG.LI[fTCD_dat_short$hand_self_report==1]) # WG, right handers
shapiro.test(fTCD_dat_short$WG.LI[fTCD_dat_short$hand_self_report==0]) # WG, left handers
shapiro.test(fTCD_dat_short$PPTT.LI[fTCD_dat_short$hand_self_report==1]) # PPTT, right handers
shapiro.test(fTCD_dat_short$PPTT.LI[fTCD_dat_short$hand_self_report==0]) # PPTT, left handers

# Report mean and sd LI values
LI_stats <- fTCD_dat_short %>% group_by(hand_self_report) %>% 
  summarise(WG.LI_mean = mean(WG.LI), WG.LI_sd = sd(WG.LI),
            PPTT.LI_mean = mean(PPTT.LI), PPTT.LI_sd = sd(PPTT.LI))

LI_stats$hand_self_report <- c('Left', 'Right')
print(LI_stats)

#----------------------------------------------------------------------------------#
##Figure 2: Pirate plot
fTCD_dat_long <- fTCD_dat_short %>% select(ID, hand_self_report, WG.LI, PPTT.LI) %>% 
  pivot_longer(cols = WG.LI:PPTT.LI, names_to = 'Task', values_to = 'LI')

colnames(fTCD_dat_long)[2] <-  'Hand' 
fTCD_dat_long$Hand <- as.factor(fTCD_dat_long$Hand)
fTCD_dat_long$Task <- as.factor(fTCD_dat_long$Task)

levels(fTCD_dat_long$Hand) <- c('Left', 'Right')
levels(fTCD_dat_long$Task)<-c("Word Gen","Semantic")
png(filename = 'Figure2.png')
pirateplot(data=fTCD_dat_long, LI~Task*Hand, bean.f.col = c("orange1", "royalblue2"),ylab="Laterality Index (LI)")
abline(h=0,lwd=2.5)
dev.off()

#----------------------------------------------------------------------------------#

#Hypothesis 1: stronger left lateralization for the expressive than the receptive task

#we fit the heterogenous and homogeneous models, chose the optimal fitting model and then interpret the main effect of task.

#Hypothesis 2: stronger left lateralization for right handers than left handers

#we fit the heterogenous and homogeneous models, chose the optimal fitting model and then interpret the main effect of handedness.

#----------------------------------------------------------------------------------#

# Model 1: Homogeneous variance model
# This model has fixed main effects of handedness and task: fixed=LI~1+Hand+Task
# It has a fixed slope, but the intercept is random, i.e. allowed to vary between participants: random=list(id=pdSymm(form=~1))
# (i.e. each participant can have a different strength of lateralisation, but the difference between tasks is the same)
# It has homogeneous variances, i.e. it assumes that the variance between left and right handed participants is the same

mod1<-lme(fixed=LI~1+Hand+Task, random=list(ID=pdSymm(form=~1)),data=fTCD_dat_long, 
          na.action="na.exclude", method="REML")

#to extract the results with pvalue (t-test for marginal significance of each fixed effect with other fixed effects)
summary(mod1)

VarCorr(mod1)


#----------------------------------------------------------------------------------#

# Model 2: Heterogeneous model variance (between-person)
# This model has fixed main effects of handedness and task (as before)
# It has a fixed intercept, but the slope is random, i.e. allowed to vary between participants (between-person variance)
# There is assumes that the slopes will be different between the left and right handed groups
# i.e. the between-persons variances are heterogeneous 
# We predict that the left handers will have more varied slopes than the right handers.

mod2<-lme(fixed=LI ~ 1 + Hand + Task, random=list(ID=pdDiag(form= ~ 0 + Hand)),data=fTCD_dat_long,na.action="na.exclude",method="REML")

summary(mod2)

VarCorr(mod2)


#----------------------------------------------------------------------------------#

#Likelihood ratio test, comparing Model 1 and Model 2

anova(mod1,mod2)

#----------------------------------------------------------------------------------#

#If we find no difference, we can check for heterogeneous within-person

#Heterogeneous model variance (within-person)

# mod3 <- lme(fixed=LI ~ 1 + Hand + Task, random = list(ID=pdSymm(form = ~1 )), weights=varIdent(form=~1 | Hand), data=fTCD_dat_long, na.action=na.exclude, method="REML")
# 
# summary(mod3)
# 
# VarCorr(mod3)
# 
# #extract variances for hand and see if they are different.
# 
# #residual std deviation
# summary(mod3)$sigma
# 
# #Residual variance of right hand (dependent on the order of factors)
# (summary(mod3)$sigma*1)^2
# 
# #Residual variance of left hand
# (summary(mod3)$sigma*coef(mod3$modelStruct$varStruct,uncons=FALSE))^2


#----------------------------------------------------------------------------------#

#Likelihood ratio test between Model 1 and Model 3

# anova(mod1,mod3)

#----------------------------------------------------------------------------------#

#Hypothesis 3: Significant correlation between expressive and receptive task

#Test with Pearson's correlation

#Hypothesis 4: More variable relationship between expressive and receptive tasks in

#left handers than right handers

#Test by comparing variability of cook's distance between groups using Fligner-Killeen test

#----------------------------------------------------------------------------------#

#Pearson's correlation between expressive and receptive tasks

H3_results <- cor.test(fTCD_dat_short$WG.LI, fTCD_dat_short$PPTT.LI)

#Plot data
#levels(fTCD_mod_dat_short$hand) <- c('Left', 'Right')
#ggplot(fTCD_mod_dat_short,aes(y=PPTT,x=WordGen,colour=hand))+geom_point(size=2)+theme_bw()+ scale_color_manual(values=c("orange1", "royalblue2"))

#Fit a linear model to both handedness groups

mymod<-lm(PPTT.LI~WG.LI,data=fTCD_dat_short)

#Calculate Cook's Distance for each participant
cooks <- cooks.distance(mymod)
fTCD_dat_short$cooks <- cooks
fTCD_dat_short$cooks_ind <- ifelse(cooks>=4*mean(cooks),"Outlier","Non-Outlier")
fTCD_dat_short$cooks_ind <- as.factor(fTCD_dat_short$cooks_ind)
#Run the Fligner-Killeen test
H4_p <- fligner.test(fTCD_dat_short$cooks ~ fTCD_dat_short$hand_self_report)$p.value


## Figure 3: Density histogram

levels(fTCD_mod_dat$Hand) <- c('Left', 'Right')
ggplot(fTCD_mod_dat,aes(x=LI,fill=Hand))+geom_density(alpha=0.5) +theme_bw() + scale_fill_manual(values=c("orange1", "royalblue2"))+ guides(fill=guide_legend(title="Handedness"))+xlab("Laterality Index (LI)")+ylab("Density")

#ggplot(fTCD_mod_dat,aes(x=LI,fill=task))+geom_density(alpha=0.5)+theme_bw() + scale_fill_manual(values=c("orange1", "royalblue2"))

#ggplot(fTCD_mod_dat_short,aes(x=cooks))+geom_histogram()+facet_wrap(~hand)+theme_bw()+ scale_fill_manual(values=c("orange1", "royalblue2"))



## Figure 4: Scatterplot of LI

levels(fTCD_mod_dat_short$Hand) <- c('Left', 'Right')
ggplot(fTCD_mod_dat_short,aes(y=PPTT,x=WordGen,colour=Hand,shape=cooks_ind))+geom_point(size=2,alpha=0.6)+theme_bw()+ scale_color_manual(values=c("orange1", "royalblue2"))+ylab("Semantic Decision LI")+xlab("Word Generation LI")+geom_hline(yintercept = 0)+geom_vline(xintercept=0) + guides(colour=guide_legend(title = "Handedness"),shape=guide_legend(title="Bivariate Outliers"))

# Bonus Figure: Line plots
tmpdata <- fTCD_mod_dat
tmpdata$Task <- as.numeric(c(rep(1,154), rep(2,154)))
tmpdata %>% 
  ggplot(aes(x=Task, y = LI, group = id, color = Hand)) + geom_line(lwd=1, alpha=0.5) + geom_point(size=3, alpha=0.8)


#----------------------------------------------------------------------------------#

##Exploratory analysis (handedness)

# Categorise each individual as LL, LR, RL or RR for laterality on each task
fTCD_mod_dat_short$cat2 <- 11
w<-which(fTCD_mod_dat_short$WordGen<0)
fTCD_mod_dat_short$cat2[w] <- 21
w<-which(fTCD_mod_dat_short$PPTT<0)
fTCD_mod_dat_short$cat2[w] <- fTCD_mod_dat_short$cat2[w]+1

# Chi square of binary handedness data and laterality categories
mytable <- table(fTCD_mod_dat_short$cat2,fTCD_mod_dat_short$Hand)
chisq.test(mytable) #v small sample for this, though it shows striking difference!

# Report EHI values for each category
myRHmeans <- fTCD_mod_dat_short %>% filter(Hand=='Right') %>% group_by(cat2) %>% 
  summarise(myn = n(), EHI_mean = mean(EHI), EHI_sd = sd(EHI),
            handpref_mean = mean(Handpref), handpref_sd = sd(Handpref))

# Try that for LHs only
myLHmeans <- fTCD_mod_dat_short %>% filter(Hand=='Left') %>% group_by(cat2) %>% 
  summarise(myn = n(), EHI_mean = mean(EHI), EHI_sd = sd(EHI),
            handpref_mean = mean(Handpref), handpref_sd = sd(Handpref))
  

# Prepare data for Figure 5
fTCD_dat2 <- fTCD_dat %>% select(id, EHI, Handpref)
fTCD_dat2_long <- melt(fTCD_dat2) 
colnames(fTCD_dat2_long) <- c('id','Measure','Handedness')
fTCD_mod_dat3<-base::merge(fTCD_mod_dat,fTCD_dat2_long,by=c("id"))
fTCD_mod_dat3$Measure <- factor(fTCD_mod_dat3$Measure, labels = c("Edinburgh Handedness Index", "Quantification of Hand Preference"))
levels(fTCD_mod_dat3$Task) <- c("Word Generation","Semantic Decision")

#Plot Figure 5
ggplot(fTCD_mod_dat3,aes(y=Handedness,x=LI,colour=Task)) + 
  geom_point(alpha=0.3,size=2)+theme_bw() + scale_color_manual(values=c("green", "magenta")) + 
  ylab("Handedness")+xlab("Laterality Index") + 
  geom_vline(xintercept=0) + 
  facet_grid(Measure~.,scales="free",labeller= label_value) + 
  theme(legend.position="top") + guides(colour=guide_legend(title="Task"))
