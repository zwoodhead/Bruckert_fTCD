#----------------------------------------------------------------------------------#
#
#   Model script to test Hypotheses 1 to 4
#
#   Created 26/11/2018.
#   Modified 27/11/2018.
#   Syntax error fixed 11/12/2018
#   Edited 09/04/2019 - adding figure 5 and other adjustments to plots.
#   Edited 11/08/2020 - making compatible with OSF data
#----------------------------------------------------------------------------------#

#Following the example procedure described here: https://quantdev.ssri.psu.edu/sites/qdev/files/ILD_Ch06_2017_MLMwithHeterogeneousVariance.html

#WE have chosen to use nlme rather than lme4 as the specification of heterogeneous within-person error is more convenient in nlme (lme4 is only implemented via a messy solution).

#----------------------------------------------------------------------------------#

#Check for required R packages and install if not on system

list.of.packages <- c("nlme", "yarrr", "tidyverse","ggplot2", "ggpubr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

require(nlme)
require(yarrr)
require(tidyverse)
require(ggplot2)
require(ggpubr) 

# Load in data
rootdir <- getwd()

WGfile <- paste0(rootdir,'/WordGen_results.csv')
PPTTfile <- paste0(rootdir,'/PPTT_results.csv')
demofile <- paste0(rootdir,'/Chpt4_fTCD_demographics.csv')
WGretestfile <- paste0(rootdir,'/WordGen_RETEST_results.csv')
PPTTretestfile <- paste0(rootdir,'/PPTT_RETEST_results.csv')

if (!file.exists(WGfile)) {osf_retrieve_file("https://osf.io/8msj5") %>% osf_download(conflicts = "skip")} # WordGen_results.csv
if (!file.exists(PPTTfile)) {osf_retrieve_file("https://osf.io/jh37g") %>% osf_download(conflicts = "skip")} # PPTT_results.csv
if (!file.exists(demofile)) {osf_retrieve_file("https://osf.io/x93w4/") %>% osf_download(conflicts = "skip")} # Chpt4_fTCD_demographics.csv
if (!file.exists(WGretestfile)) {osf_retrieve_file("https://osf.io/x4yts") %>% osf_download(conflicts = "skip")} # WordGen_RETEST_results.csv
if (!file.exists(PPTTretestfile)) {osf_retrieve_file("https://osf.io/4j5uy") %>% osf_download(conflicts = "skip")} # PPTT_RETEST_results.csv

wordgen <- read.csv(WGfile, stringsAsFactors=FALSE)
pptt <- read.csv(PPTTfile, stringsAsFactors=FALSE)     
demographics <- read.csv(demofile, stringsAsFactors=FALSE)
wordgen_retest <- read.csv(WGretestfile, stringsAsFactors=FALSE)
pptt_retest <- read.csv(PPTTretestfile, stringsAsFactors=FALSE)

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
cat(paste('\nWord Generation retest data was acquired from', sum(fTCD_dat_short$wordgen_retest), 'participants'))
cat(paste('\nSemantic Association retest data was acquired from', sum(fTCD_dat_short$PPTT_retest), 'participants'))

#----------------------------------------------------------------------------------#
# Data Quality

# Calculate the number of trials excluded for each task
wordgen_excluded_trials <- length(fTCD_dat_short$ID) * 23 - sum(fTCD_dat_short$WG.n)
wordgen_excluded_trials_pc <- wordgen_excluded_trials / (length(fTCD_dat_short$ID) * 23) * 100

pptt_excluded_trials <- length(fTCD_dat_short$ID) * 15 - sum(fTCD_dat_short$PPTT.n)
pptt_excluded_trials_pc <- pptt_excluded_trials / (length(fTCD_dat_short$ID) * 15) * 100

cat(paste('\nFor word generation,', round(wordgen_excluded_trials_pc,2), '% of all trials were excluded,\n',
          'and for semantic association,', round(pptt_excluded_trials_pc,2), '% were excluded.'))

# Calculate split-half reliability (comparing LI from odd and even trials)
WG_splithalf <- cor.test(fTCD_dat_short$WG.odd, fTCD_dat_short$WG.even)
plot(fTCD_dat_short$WG.odd, fTCD_dat_short$WG.even)
PPTT_splithalf <- cor.test(fTCD_dat_short$PPTT.odd, fTCD_dat_short$PPTT.even)
plot(fTCD_dat_short$PPTT.odd, fTCD_dat_short$PPTT.even) #PPTT

# Join with retest data and calculate test-retest reliability
fTCD_dat_short$WG2.LI <- NA
fTCD_dat_short$PPTT2.LI <- NA

w <- which(fTCD_dat_short$ID %in% wordgen_retest$ID)
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
levels(fTCD_dat_long$Task)<-c("Semantic", "Word Gen")
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
# Figure 3: Density Plots

# Word Generation
p1 <- ggplot(fTCD_dat_long[fTCD_dat_long$Task=='Word Gen',], aes(x=LI, fill=Hand)) +
  geom_density(alpha=0.4) +
  xlim(-3,6)+
  labs(title="Word Generation", x ="") + 
  theme_bw() + scale_fill_manual(values=c("orange1", "royalblue2"))
# Semantic Association
p2 <- ggplot(fTCD_dat_long[fTCD_dat_long$Task=='Semantic',], aes(x=LI, fill=Hand)) +
  geom_density(alpha=0.4)+
  xlim(-3,6)+
  labs(title="Semantic Association",
       x ="Laterality index")  + 
  theme_bw() + scale_fill_manual(values=c("orange1", "royalblue2"))
# Arrange into two panels
ggarrange(p1,p2, ncol = 1, nrow = 2,common.legend=T,legend="bottom")
# Save
ggsave(
  "Figure3.png",
  width = 5, height = 8,
  dpi = 300
)

#----------------------------------------------------------------------------------#
# Hypothesis 3: Correlation between tasks

#Pearson's correlation between expressive and receptive tasks
H3_results <- cor.test(fTCD_dat_short$WG.LI, fTCD_dat_short$PPTT.LI)
print(H3_results)

#----------------------------------------------------------------------------------#
# Figure 4: Scatter plot of LI values

#Fit a linear model to both handedness groups
mymod<-lm(PPTT.LI~WG.LI,data=fTCD_dat_short)

#Calculate Cook's Distance for each participant
cooks <- cooks.distance(mymod)
fTCD_dat_short$cooks <- cooks
fTCD_dat_short$cooks_ind <- ifelse(cooks>=4*mean(cooks),"Outlier","Non-Outlier")
fTCD_dat_short$cooks_ind <- as.factor(fTCD_dat_short$cooks_ind)

fTCD_dat_short$hand_self_report <- as.factor(fTCD_dat_short$hand_self_report)
levels(fTCD_dat_short$hand_self_report) <- c('Left', 'Right')

ggplot(fTCD_dat_short, aes(y=PPTT.LI, x=WG.LI,colour=hand_self_report, shape=cooks_ind)) +
  geom_point(size=2,alpha=0.8) + theme_bw() + scale_color_manual(values=c("orange1", "royalblue2")) +
  labs(title="Laterality Indices") + ylab("Semantic Decision")+xlab("Word Generation") +
  geom_hline(yintercept = 0) + geom_vline(xintercept=0) + 
  guides(colour=guide_legend(title = "Handedness"),shape=guide_legend(title="Outlier"))

ggsave(
  "Figure4.png",
  width = 5, height = 4,
  dpi = 300
)


#----------------------------------------------------------------------------------#
# Hypothesis 4: variability in left and right handers

#Run the Fligner-Killeen test
H4_p <- fligner.test(fTCD_dat_short$cooks ~ fTCD_dat_short$hand_self_report)$p.value

# Categorise each individual as LL, LR, RL or RR for laterality on each task
fTCD_dat_short$cat2 <- 11
w<-which(fTCD_dat_short$WG.LI<0)
fTCD_dat_short$cat2[w] <- 21
w<-which(fTCD_dat_short$PPTT.LI<0)
fTCD_dat_short$cat2[w] <- fTCD_dat_short$cat2[w]+1

# Chi square of binary handedness data and laterality categories
mytable <- table(fTCD_dat_short$cat2,fTCD_dat_short$hand_self_report)
print(mytable)
chisq.test(mytable) #v small sample for this, though it shows striking difference!


#----------------------------------------------------------------------------------#
# Exploratory analysis: continuous measures of handedness

# Report EHI values for each category
myEHImeans <- fTCD_dat_short %>% group_by(hand_self_report, cat2) %>% 
  summarise(myn = n(), EHI_mean = mean(hand_EHI_sum), EHI_sd = sd(hand_EHI_sum),
            handpref_mean = mean(hand_pref_bias), handpref_sd = sd(hand_pref_bias))

#----------------------------------------------------------------------------------#
# Figure 5: EHI and QHP data

# Prepare data for Figure 5
fTCD_handdat_long <- fTCD_dat_short %>% select(ID, hand_EHI_sum, hand_pref_bias) %>% 
  pivot_longer(cols = hand_EHI_sum:hand_pref_bias, names_to = 'Measure', values_to = 'Handedness')

fTCD_dat_long2<-base::merge(fTCD_dat_long,fTCD_handdat_long,by=c("ID"))

fTCD_dat_long2$Measure <- factor(fTCD_dat_long2$Measure, labels = c("Edinburgh Handedness Index", "Quantification of Hand Preference"))
levels(fTCD_dat_long2$Task) <- c("Semantic Decision", "Word Generation")

#Plot Figure 5
ggplot(fTCD_dat_long2,aes(y=Handedness,x=LI)) + 
  geom_point(alpha=0.3,size=1)+theme_bw() + 
  ylab("Handedness")+xlab("Laterality Index") + 
  geom_vline(xintercept=0) + 
  facet_grid(rows = vars(Measure), cols = vars(Task), scales="free",labeller= label_value)

ggsave(
  "Figure5.png",
  width = 5, height = 5,
  dpi = 300
)
