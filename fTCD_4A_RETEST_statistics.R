#----------------------------------------------------------------------------------#
#
#   Model script to test Hypotheses for retest data
#
# 
#----------------------------------------------------------------------------------#


#Check for required R packages and install if not on system

library(stringr)
library(reshape2)
library(ggplot2)

#Load in data and clean if necessary.

rootdir <- "H:/github/DPhil_Chapter4_fTCD/"

wordgen <- read.csv(paste0(rootdir,'WordGen_results.csv'), stringsAsFactors=FALSE) # The expressive task
wordgen_retest <- read.csv(paste0(rootdir,'WordGen_RETEST_results.csv'))
pptt <- read.csv(paste0(rootdir,'PPTT_results.csv'), stringsAsFactors=FALSE)       # The receptive task
pptt_retest <- read.csv(paste0(rootdir,'PPTT_RETEST_results.csv'), stringsAsFactors=FALSE)       # The receptive task

wordgen <- wordgen[ , c(1,7,12:14)]
wordgen_retest <- wordgen_retest[ , c(1,7,14)]
pptt <- pptt[ , c(1,7,12:14)]
pptt_retest <- pptt_retest[ , c(1,7,14)]

demographics <- read.csv(paste0(rootdir,'Chpt4_fTCD_demographics.csv'), stringsAsFactors=FALSE)
demographics <- demographics[, c(1,4)]
colnames(demographics) <- c('Filename','Hand')

#Now we have multiple data frames that we need to merge into alldat, but the filenames may be in a different order
#Word Gen
wg_demoindex <- vector(mode='integer', dim(wordgen_retest)[1])
wordgenindex <- vector(mode='integer', dim(wordgen_retest)[1])

for (i in 1:dim(wordgen_retest)[1]) # start by running through participants in wordgen_retest data
{shortname <- substr(wordgen_retest$Filename[i],8,14) # ignore final characters of name
  wg_demoindex[i] <- which(str_detect(demographics$Filename, shortname))
  wordgenindex[i] <- which(str_detect(wordgen$Filename, shortname))
 }

pptt_demoindex <- vector(mode='integer', dim(pptt_retest)[1])
ppttindex <- vector(mode='integer', dim(pptt_retest)[1])

for (i in 1:dim(pptt_retest)[1]) # start by running through participants in wordgen_retest data
{shortname <- substr(pptt_retest$Filename[i],8,13) # ignore final characters of name
pptt_demoindex[i] <- which(str_detect(demographics$Filename, shortname))
ppttindex[i] <- which(str_detect(pptt$Filename, shortname))
}

# Collate data
# WG
WG_retest_dat <- data.frame('id' = as.factor(wordgen$Filename[wordgenindex]),
                       'hand' = as.factor(demographics$Hand[wg_demoindex]),
                       'exclude' = as.factor(wordgen_retest$exclusions + wordgen$exclusions[wordgenindex]), # Participants have to have good data for BOTH tasks to be included
                       'Test1' = wordgen$LI[wordgenindex],
                       'Test2' = wordgen_retest$LI)

# PPTT
PPTT_retest_dat <- data.frame('id' = as.factor(pptt_retest$Filename),
                            'hand' = as.factor(demographics$Hand[pptt_demoindex]),
                            'exclude' = as.factor(pptt_retest$exclusions + pptt$exclusions[ppttindex]), # Participants have to have good data for BOTH tasks to be included
                            'Test1' = pptt$LI[ppttindex],
                            'Test2' = pptt_retest$LI)

# Remove excluded data
WG_retest_mod_dat_short <- WG_retest_dat[which(WG_retest_dat$exclude==0), ] # Removes excluded participants
PPTT_retest_mod_dat_short <- PPTT_retest_dat[which(PPTT_retest_dat$exclude==0), ]

# Melt to long format
WG_retest_mod_dat <- melt(WG_retest_mod_dat_short) 
colnames(WG_retest_mod_dat) <- c('id','hand','exclude','Test', 'LI')

PPTT_retest_mod_dat <- melt(PPTT_retest_mod_dat_short) 
colnames(PPTT_retest_mod_dat) <- c('id','hand','exclude','Test', 'LI')

# Normality tests
shapiro.test(WG_retest_mod_dat_short$Test1)
shapiro.test(WG_retest_mod_dat_short$Test2)
shapiro.test(PPTT_retest_mod_dat_short$Test1)
shapiro.test(PPTT_retest_mod_dat_short$Test2)

# Test-retest correlations
cor.test(WG_retest_mod_dat_short$Test1, WG_retest_mod_dat_short$Test2)
cor.test(WG_retest_mod_dat_short$Test1, WG_retest_mod_dat_short$Test2, method='spearman')
cor.test(PPTT_retest_mod_dat_short$Test1, PPTT_retest_mod_dat_short$Test2)
cor.test(PPTT_retest_mod_dat_short$Test1, PPTT_retest_mod_dat_short$Test2, method='spearman')


## Exploratory analysis: Split-half reliability
wordgen_mod <- wordgen[which(wordgen$exclusions==0), ]
wordgen_splithalf <- cor.test(wordgen_mod$odd, wordgen_mod$even)

pptt_mod <- pptt[which(pptt$exclusions==0), ]
pptt_splithalf <- cor.test(pptt_mod$odd, pptt_mod$even)



