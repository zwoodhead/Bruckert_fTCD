#########################################################################################
# Script 1: View trials
#########################################################################################
# This script reads in fTCD raw data and allows you to manually mark trials with artifacts

########################################################
# Install packages

list.of.packages <- c("tidyverse","osfr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

require(tidyverse)
require(osfr)

########################################################
# Load raw data from OSF
# NB: it will not overwrite existing files

osf_retrieve_file("https://osf.io/5kq42") %>% osf_download(conflicts = "skip") # Chpt4_fTCD_WordGen_rawdata.zip
osf_retrieve_file("https://osf.io/6gn23") %>% osf_download(conflicts = "skip") # Chpt4_fTCD_PPTT_rawdata.zip
unzip ("Chpt4_fTCD_WordGen_rawdata.zip", exdir = ".", overwrite = FALSE)
unzip ("Chpt4_fTCD_PPTT_rawdata.zip", exdir = ".", overwrite = FALSE)

########################################################
# Specify task
task_switch <- as.integer(readline(prompt='Which task? 1=WordGen, 2=PPTT: '))
task <- 'WordGen'
if (task_switch == 2){ task <- 'PPTT' }

# Specify directory and other variable parameters
rootdir <- getwd()
datadir <- paste0(rootdir,'/Chpt4_fTCD_',task,'_rawdata/')

datafiles <- list.files(datadir, pattern='*.exp')
nfiles <- length(datafiles)

# Timings in seconds
intertrial_interval <- 50 # Should be 50 seconds between each marker for wordgen
if (task_switch == 2)
{ intertrial_interval <- 45}

epochstart_time <- -12
epochend_time <- 30   # Previously called postmarker
baselinestart_time <- -10
baselineend_time <- 0
poistart_time <- 8
poiend_time <- 20
if (task_switch == 2)
  { poiend_time <- 25}

# Timings in samples
samplingrate=25
epochstart_index <- epochstart_time * samplingrate
epochend_index <- epochend_time * samplingrate  # Previously called postpoints
baselinestart_index <- baselinestart_time * samplingrate
baselineend_index <- baselineend_time * samplingrate
poistart_index <- poistart_time * samplingrate
poiend_index <- poiend_time * samplingrate

trialsperrun=23
if (task_switch == 2)
  { trialsperrun = 15}

# Check if a trial inclusion file already exists, or if not, create a new one
all_trials_inclusions <- as.data.frame(matrix(data = NA, nrow = trialsperrun, ncol = nfiles))
if (file.exists(paste0(task,'_trial_inclusion.csv'))){
  all_trials_inclusions <- read.csv(paste0(task,'_trial_inclusion.csv'))}
colnames(all_trials_inclusions) <- datafiles

########################################################
# Start loop through datafiles

startdata <- as.integer(readline(prompt="Start from participant: "))
enddata <- as.integer(readline(prompt='End at participant: '))

for (mysub in startdata:enddata){ 
  mydatafile <- datafiles[mysub]
  mycomment <- "Comment: "
  
  # Read exp data
  mydata<-read.table(paste0(datadir,mydatafile), skip = 6,  header =FALSE, sep ='\t')
  
  wantcols = c(2,3,4,7) #sec, L, R,marker #select columns of interest to put in shortdat
  shortdat = data.frame(mydata[,wantcols])
  colnames(shortdat) = c ("csec","L","R","marker")
  
  # downsample to 25 Hz by taking every 4th point
  rawdata = filter(shortdat, row_number() %% 4 == 0) # downsample to 25 Hz by taking every 4th point
  allpts = nrow(rawdata) # total N points in long file
  rawdata[,1] = (seq(from=1,to=allpts*4,by=4)-1)/100 #create 1st column which is time in seconds from start
  colnames(rawdata) = c("sec","L","R","marker")
  
  #-------------------------------------------------------
  # Brief plot of 1500 pts to check all OK; range here is arbitrary
  #-------------------------------------------------------
  x=rawdata$sec[3000:5000]
  y=rawdata$L[3000:5000]
  z=rawdata$R[3000:5000]
  w=rawdata$marker[3000:5000]
  
  plot(x,y, type="n") #set up plot - doesn't actually plot anything
  lines(x,y,col="red")
  lines(x,z,col="blue")
  lines(x,w)
  cat("Press [enter] to continue")
  line <- readline()
  #This should show left (red) and right (blue) channels and some markers in black
  
  
  #-----------------------------------------------------------
  #Now find markers; place where 'marker' column goes from low to high value
  #-----------------------------------------------------------
  mylen = nrow(rawdata); # Number of timepoints in filtered data (rawdata)
  markerplus = c(0 ,rawdata$marker); # create vectors with offset of one
  markerchan = c(rawdata$marker,0); 
  markersub = markerchan - markerplus; # start of marker indicated by large difference between consecutive data points
  meanmarker <- mean(rawdata$marker) # We will identify big changes in marker value that are > 5 sds
  markersize <- meanmarker+4*sd(rawdata$marker)
  origmarkerlist = which(markersub>markersize)
  norigmarkers = length(origmarkerlist)
  markerlist<-origmarkerlist
  
  #If there is not a whole epoch of time after the final marker, add a comment
  if (origmarkerlist[norigmarkers] > (mylen - epochend_index)) 
  {myaddcomment<-paste('. Short last epoch in run')
  mycomment<-paste(mycomment,myaddcomment)
  } # indicates if there is a short last epoch; this may need disposing of
  
  #If there is not a whole baseline of time before the first marker, add a comment
  if ((origmarkerlist[1] + epochstart_index) < 0) 
  {myaddcomment<-paste('. Short first epoch in run')
  mycomment<-paste(mycomment,myaddcomment)
  } # indicates if there is a short last epoch; this may need disposing of
  
  # Check whether the number of markers matches the expected number of trials
  excessmarkers=norigmarkers-trialsperrun
  
  # If there are too few epochs, comment on this
  if (excessmarkers<0)
  {mycomment<-paste(mycomment,'. Fewer markers than expected')
  }
  
  # If thre are too many epochs, ignore the first one(s)
  if(excessmarkers>0)
  {myaddcomment<-paste(excessmarkers,'. Unexpected markers found in run 1, investigate')
  mycomment<-paste(mycomment, myaddcomment)
  markerlist<-origmarkerlist[(excessmarkers+1):(length(origmarkerlist))]
  }
  
  # Check that markers are at least 48s apart
  intervals=c(rawdata$sec[markerlist],10000)-c(0,rawdata$sec[markerlist])
  intervals=intervals[2:(length(intervals)-1)]
  # Ignore first and last values since these are arbitrary; other intervals should be around 30s 
  # but may be longer if recording interrupted. Shorter intervals indicate there have been spurious 
  # markers which will need dealing with
  if(min(intervals<(intertrial_interval - 1)))
  {myaddcomment<-paste('. Possible spurious markers')
  mycomment<-paste(mycomment,myaddcomment)
  }
  
  nmarkers = length(markerlist)
  
  # Echo comment
  cat(mycomment)
  
  
  #----------------------------------------------------------
  # epoch the trials into an array
  # This has 3 dimensions; trials, points, L/R
  #-----------------------------------------------------------
  # myepoched will be the full epoched trial
  myepoched <- array(0, dim=c(nmarkers, epochend_index - epochstart_index +1,2)) # Set up an empty matrix
  myinclude <- matrix(data=NA, nrow=trialsperrun)
  
  for(mym in 1:nmarkers) # for trials
  { 
    index1=markerlist[mym]+epochstart_index  # index1 is index of the timepoint at the start of the epoch
    index2=markerlist[mym]+epochend_index    # index2 is the index of the timepoint at the end of the epoch
    
    # If recording started late, the start of the epoch for trial 1 will be beyond the recorded range. 
    # If this doesn't affect the baseline period (ie, results will be unaffected), then replace with mean
    # and keep the trial
    if (index1 < 0 & (markerlist[mym]+baselinestart_index) > 0){
      cat("Recording started late, but before baseline. Padding start with mean", "\n")
      replacement_mean_left = mean(rawdata[0:index2,2]) # Left hemisphere mean
      replacement_mean_right = mean(rawdata[0:index2,3]) # Right hemisphere mean
      myepoched[mym, ,1] = c(rep(replacement_mean_left,index1*-1+1),rawdata[0:index2,2])
      myepoched[mym, ,2] = c(rep(replacement_mean_right,index1*-1+1),rawdata[0:index2,3])
    }
    
    # If recording started later than the start of the baseline period the trial should
    # be discarded
    if (index1 < 0 & (markerlist[mym]+baselinestart_index) < 0){
      cat("Recording started too late. Discard this trial", "\n")
      replacement_mean_left = mean(rawdata[0:index2,2]) # Left hemisphere mean
      replacement_mean_right = mean(rawdata[0:index2,3]) # Right hemisphere mean
      myepoched[mym, ,1] = c(rep(replacement_mean_left,index1*-1+1),rawdata[0:index2,2])
      myepoched[mym, ,2] = c(rep(replacement_mean_right,index1*-1+1),rawdata[0:index2,3])
    }
    
    if (index1 > 1){
      myepoched[mym,,1]=rawdata[index1:index2,2] #L side
      myepoched[mym,,2]=rawdata[index1:index2,3] #R side
    }
    
    #-------------------------------------------
    # See epoch-by-epoch plots of raw data
    #-------------------------------------------
    
    # X axis will be time of full epoch
    timeline = rawdata$sec[1:(epochend_index-epochstart_index+1)] #timeline used in all plots
    myplotbit <- myepoched[mym, , ]
    
    #first plot the old values with no correction
    myylim <- range(c(range(na.omit(myplotbit[,1])),range(na.omit(myplotbit[,2]))))
    
    plot(timeline+epochstart_time,myplotbit[,1],type="n",xlab='time (secs)',ylab='velocity',ylim=myylim)
    lines(timeline+epochstart_time,myplotbit[,1],col="red")
    lines(timeline+epochstart_time,myplotbit[,2],col="blue")
    
    #then overplot the baseline and POI markers
    abline(v=baselinestart_time, col='green')
    abline(v=baselineend_time, col='green')
    abline(v=poistart_time, col='purple')
    abline(v=poiend_time, col='purple')
    text(epochstart_time, max(myplotbit), pos=4, mycomment)
    
    mytitle=paste(mydatafile, 'Trial:', mym)
    title(mytitle)
    
    cat("Press 1 to include. Press 0 to exclude")
    myoverride <- as.integer(readline(prompt = ""))
    
    # If anything other than 0 is pressed (including no response), include trial
    if(is.na(myoverride))
    {myoverride = 1}
    
    if(myoverride != 0) 
    {myoverride = 1  
    myinclude[mym] = 1
    }               
    
    # If zero is pressed, exclude trial
    if(myoverride==0)
    {myinclude[mym]=0
    }
    
    
  } #next epoch
  
  all_trials_inclusions[ , mysub] <- myinclude
  
} #next subject
  
#Print trial inclusion file
write.csv(all_trials_inclusions, paste0(task,'_trial_inclusion.csv'), row.names=FALSE)
  
