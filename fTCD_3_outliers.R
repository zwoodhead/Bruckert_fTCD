#########################################################################################
# Script 3: Outliers
#########################################################################################
# This script runs after Script 2, which calculates LI values for all participants.
# Script 3 then looks for outlier values, based on the standard error of the LI values.

########################################################
# Install packages

list.of.packages <- c("tidyverse","osfr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

require(tidyverse)
require(osfr)

########################################################
# Specify task
task_switch <- as.integer(readline(prompt='Which task? 1=WordGen, 2=PPTT: '))
task <- 'WordGen'
mintrials <- 18 # Minimum trials is 18 for wordgen

if (task_switch == 2){ 
  task <- 'PPTT'
  mintrials <- 12} # Minimum trials is 12 for PPTT
  
# Specify directory and other variable parameters
rootdir <- getwd()
datadir <- paste0(rootdir,'/Chpt4_fTCD_',task,'_rawdata/')

resultsfile <- paste0(rootdir,"/",task,"_results.csv") # File lists analysis results

########################################################
# Read in results file

# Read in results file from OSF if it's not present in working directory

if (!file.exists(resultsfile)){
  if (task_switch == 1){
    osf_retrieve_file("https://osf.io/8msj5") %>% osf_download(conflicts = "skip") # WordGen_results.csv
  }
  if (task_switch == 2){
    osf_retrieve_file("https://osf.io/jh37g") %>% osf_download(conflicts = "skip") # PPTT_results.csv
  }
}

alldata <- read.csv(resultsfile)

nsub <- dim(alldata)[1]

exclusions  <- rep(0,nsub)


########################################################
# Outliers are defined based on the standard error across trials.
# LI values more than 2.2 times the difference between the first and third quartiles (2.2 * (Q3-Q1)) 
# above the third quartile values are classed as outliers
# (e.g: upper limit = Q3 + 2.2*(Q3-Q1)). 

## We used to do this by combining data from ALL tasks, but now I'm not sure that's such a good idea, 
## as one task has more trials than the other
# allse<-vector() # start with an empty vector
# for (t in 1:6){
#   allse<-c(allse,LI_data[,(t+14)])
# }

lower_quartile <- quantile(alldata$se, probs=0.25, na.rm="TRUE")
upper_quartile <- quantile(alldata$se, probs=0.75, na.rm="TRUE")
quartile_diff <- upper_quartile - lower_quartile

upper_limit <- upper_quartile + 2.2*quartile_diff

for (p in 1:(nsub)){
    if (is.na(alldata$se[p]) == 0       # If se is not NA
        & alldata$se[p] > upper_limit)  # And se is greater than the upper limit
    {exclusions[p] <- 1}         # Then set exclusion status to 1
  }

# Mark tasks with too few useable trials as excluded

for (p in 1:nsub){
   if (alldata$nFinal[p] < mintrials){ #If nFinal is less than mintrials
        exclusions[p] <- 2  # Then set exclusion status to 2
      }
    }

alldata$exclusions <- exclusions

write.csv(alldata, resultsfile, row.names=FALSE)