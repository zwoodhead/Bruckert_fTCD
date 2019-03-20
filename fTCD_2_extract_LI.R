#########################################################################################
# Script 2: Extract LI
#########################################################################################
# Run this script after running Script 1, which identifies trials with artifacts.
# Script 2 reads in all data from one task (WordGen or PPTT, excluding artifact trials) 
# and calcuates LI values

########################################################
# Install packages

require(dplyr)

########################################################
# Specify task
task_switch <- as.integer(readline(prompt='Which task? 1=WordGen, 2=PPTT: '))
task <- 'WordGen'

trialsperrun=23
min_trials <- 18

if (task_switch == 2){ 
  task <- 'PPTT' 
  trialsperrun <- 15
  min_trials <- 12}

# Specify directory and other variable parameters
rootdir <- "H:/github/DPhil_Chapter4_fTCD/"
datadir <- paste0(rootdir,'Chpt4_fTCD_',task,'_rawdata/')
resultsfile <- paste0(rootdir,task,"_results.csv") # File lists analysis results

initialdatacheck1=0; # set to 1 to view epochs after normalisation
initialdatacheck2=0; #set to 1 to view epochs after heartbeat Correction
initialdatacheck3=0; # set to 1 to visualise after baseline correction
initialdatacheck4=0; # set to 1 to plot AND SAVE average for each subject

# Timings in seconds
intertrial_interval <- 50 # Should be 50 seconds between each marker
if (task_switch == 2)
{ intertrial_interval <- 45}

epochstart_time <- -12
epochend_time <- 30   # Previously called postmarker
baselinestart_time <- -10
baselineend_time <- 0
poistart_time <- 8
poiend_time <- 20
if (task_switch == 2){ poiend_time <- 25 }

# Timings in samples
samplingrate=25
epochstart_index <- epochstart_time * samplingrate
epochend_index <- epochend_time * samplingrate  # Previously called postpoints
baselinestart_index <- baselinestart_time * samplingrate
baselineend_index <- baselineend_time * samplingrate
poistart_index <- poistart_time * samplingrate
poiend_index <- poiend_time * samplingrate

extremehi=140 # define values for rejecting bad epochs 
extremelo=60 # (% above/below mean of 100 in normed/corrected data)
samplingrate=25
heartratemax = 125  # Maximum heartrate that would be expected, in bpm
peakdiffmin = 60/heartratemax * samplingrate # The minumum number of samples expected between heartbeats, based on heartratemax
baselinecorrect=1 # correct for baseline
interpolatebad=2  #set to 2 to replace brief dropout/spiking with mean value for that channel
                  #number specified here is max number of bad datapoints corrected for


########################################################
# Specify subject (mysubname)

# Read in existing inclusion data and identify datafiles with enough good trials (> 18 for wordgen, > 12 for PPTT)
all_trials_inclusions <- read.csv(paste0(rootdir,task,'_trial_inclusion.csv'))
datafiles <- substr(colnames(all_trials_inclusions), 2, 100) # List of all datafile names

all_trials_sum <- colSums(all_trials_inclusions, na.rm = 'TRUE') # Counts number of included trials per dataset

to_run <- which(all_trials_sum >= min_trials)

results <- read.csv(resultsfile, stringsAsFactors = FALSE)

for (mysub in (1:length(to_run))){ 
  
  mydatafile <- datafiles[to_run[mysub]]
  mysubname <- substr(mydatafile,1,nchar(mydatafile)-4) # Remove the .exp from the file name
  cat(mydatafile, "\n\n")
  
  # Read Results file
  results$Filename <- as.character(results$Filename)
  
  # Find row, if it exists
  results_row <- which(results$Filename == mydatafile)
  # If it doesn't exist, make a new row
  if(length(results_row)==0) {
    results_row <- length(results$Filename) + 1
    results[nrow(results)+1, ] <- list(rep('x', ncol(results)))
    results$Filename[results_row] <- mysubname
  }
    
  # Open relevant comment from results file
  mycomment<-as.character(results$Comment[results_row])
    
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
    
    
  #-----------------------------------------------------------
  #Now find markers; place where 'marker' column goes from low to high value
  #-----------------------------------------------------------
  mylen = nrow(rawdata); # Number of timepoints in filtered data (rawdata)
  markerplus = c(rawdata$marker[1] ,rawdata$marker); # create vectors with offset of one
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

  # Check whether the number of markers matches the expected number of trials
  excessmarkers=norigmarkers-trialsperrun

  # If there are too few epochs, comment on this
  if (excessmarkers<0)
  {mycomment<-paste(mycomment,'. Fewer markers than expected')
  }
  
  # If thre are too many epochs, ignore the first one(s)
  if(excessmarkers>0)
  {myaddcomment<-paste(excessmarkers,'. unexpected markers found in run 1, investigate')
  mycomment<-paste(mycomment, myaddcomment)
  markerlist<-origmarkerlist[(excessmarkers+1):(length(origmarkerlist))]
  }
  
  #If there is not a whole baseline of time before the first marker, add a comment
  if ((markerlist[1] + epochstart_index) < 0) 
  {myaddcomment<-paste('. Short first epoch in run')
  mycomment<-paste(mycomment,myaddcomment)
  } # indicates if there is a short last epoch; this may need disposing of
  
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
    
  results$nMarkers[results_row] <- nmarkers 
    
  #---------------------------------------------------------
  # Make vector indicating trials to be be included/excluded
  #---------------------------------------------------------
  myinclude = all_trials_inclusions[ ,to_run[mysub]]

  myremove = which(myinclude==0) # excluded trials
  myinclude2 = myinclude
  if(length(myremove)>0)
    {markerlist=markerlist[-myremove]
    myinclude2=myinclude[-myremove]
    }
  nmarkers = length(markerlist)
    
  #-----------------------------------------------------------
  # identify extreme values; can also check each epoch visually
  #------------------------------------------------------------
  #droprej and spikerej give lower and upper limits of signal for L and R channels
  droprej = rep(0,2);
  spikerej = droprej # initialise spikerej and droprej with zero
  mymax = max(rawdata[,2:3])
    
  droprej[1] = quantile(rawdata$L,.0001)
  droprej[2] = quantile(rawdata$R,.0001)
  spikerej[1] = quantile(rawdata$L,.9999)
  spikerej[2] = quantile(rawdata$R,.9999)
    
  for(i in 1:2) # For left and right sensors
    {if(droprej[i]<1)
      {droprej[i]=1 #value cannot be 0 or less! Lowest droprej value is 1
      }
    }
  #----------------------------------------------------------
  # epoch the accepted trials into an array
  # This has 4 dimensions; trials,points, L/R, raw/normalised/heartcorr/baselined
  #-----------------------------------------------------------
  # myepoched will be the full epoched trial
  myepoched <- array(0, dim=c(nmarkers,epochend_index-epochstart_index +1 ,2, 4)) # Set up an empty matrix
  # mybit will be the data we actually use, from start of baseline to end of POI
  mybit = matrix(data = NA, nrow = poiend_index-baselinestart_index, ncol = 2)
  timeline = rawdata$sec[1:(epochend_index-epochstart_index+1)] + epochstart_time #timeline used in all plots
    
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
      myepoched[mym, ,1,1] = c(rep(replacement_mean_left,index1*-1+1),rawdata[0:index2,2])
      myepoched[mym, ,2,1] = c(rep(replacement_mean_right,index1*-1+1),rawdata[0:index2,3])
      }
      
    # If recording started later than the start of the baseline period the trial should
    # be discarded
    if (index1 < 0 & (markerlist[mym]+baselinestart_index) < 0){
      cat("Recording started too late. Discard this trial", "\n")
      replacement_mean_left = mean(rawdata[0:index2,2]) # Left hemisphere mean
      replacement_mean_right = mean(rawdata[0:index2,3]) # Right hemisphere mean
      myepoched[mym, ,1,1] = c(rep(replacement_mean_left,index1*-1+1),rawdata[0:index2,2])
      myepoched[mym, ,2,1] = c(rep(replacement_mean_right,index1*-1+1),rawdata[0:index2,3])
      }
      
    if (index1 > 1){
      myepoched[mym,,1,1]=rawdata[index1:index2,2] #L side
      myepoched[mym,,2,1]=rawdata[index1:index2,3] #R side
      }
      
    # Looks for data points lower than droprej or higher than spikerej. Only look between beginning of baseline and end of POI
    for(i in 1:2) # for left and right sides
      {rejpoints <- numeric(0) #coerces vector rejpoints to zero length between iterations 
      mybit[,i]=myepoched[mym, c((baselinestart_index-epochstart_index):(poiend_index-epochstart_index-1)), i, 1]
      thisbit = mybit[,i]
      
      # rejpoints is a list of points where signal indicates dropout or spiking
      rejpoints=c(rejpoints, which(thisbit < droprej[i]) + baselinestart_index - epochstart_index -1); # identifies the indices of myepoched which will be marked as bad
      rejpoints=c(rejpoints, which(thisbit > spikerej[i]) + baselinestart_index - epochstart_index -1);
      rejpoints=c(rejpoints, which(is.na(thisbit))) #triggered if epoch too short
      
      # if there are more than 2 rejected points, the whole epoch is marked as bad
      if(length(rejpoints)>interpolatebad)
        {myinclude2[mym]=-1; #flag with -1; denotes drop this epoch; triggered by either channel
        }
      
      # if there are two or less (but more than zero)
      if(length(rejpoints)<=interpolatebad & length(rejpoints) > 0){
        for (p in 1:length(rejpoints)){
          myepoched[mym, rejpoints[p],i,1] = mean(myepoched[mym, c((baselinestart_index-epochstart_index):(poiend_index-epochstart_index-1)),i,1])
          }
        } # End of loop through rejpoints
      } # End of left / right loop
      
    if(mym==1)
      {badpoints=rejpoints
      }
      
    if(mym>1)
      {badpoints=c(badpoints,rejpoints)# keeps record of points with dropout/spiking between iterations
      }
    
    } # End of loop through mym (epochs)
  
  results$nSpikes[results_row]=length(badpoints) # add number of spiking/dropout points to table for saving
    
  #--------------------------------------------------------
  # Remove deleted epochs (originals in origdata; 
  # myepoched updated so only has retained epochs)
  #--------------------------------------------------------
    
  keepmarkers=which(myinclude2==1)
  origdata=myepoched #keep this so can reconstruct
  myepoched=myepoched[keepmarkers,,,] #file with only accepted epochs
  nmarkers2=length(keepmarkers)
    
  #---------------------------------------------------------
  # Normalise to mean of 100 (see Deppe et al, 2004)
  # Multiply by 100 and divide by overall mean value
  # ensures results are independent of angle of insonation
  #----------------------------------------------------------
  meanL=mean(myepoched[,,1,1], na.rm='TRUE')
  meanR=mean(myepoched[,,2,1], na.rm='TRUE')
  myepoched[,,1,2]=myepoched[,,1,1]/meanL * 100 #last dim of myepoched is 2 for the normalised data
  myepoched[,,2,2]=myepoched[,,2,1]/meanR * 100
    
  # Short final epochs have NAs in them. Replace with zero to avoid errors.
  for (x in 1:length(myepoched[nmarkers2,,1,2])){
    if (is.na(myepoched[nmarkers2,x,1,2])){
      cat("Recording ended early. Replacing NAs with zeros", "\n")
      myepoched[nmarkers2,x,1,2] = 0 # Left hemisphere
      myepoched[nmarkers2,x,2,2] = 0 # Right hemisphere
    }
  }
    
  #---------------------------------------------------------
  # See plots of normed epochs
  #---------------------------------------------------------
  if(initialdatacheck1==1)
    {for(mym in 1:nmarkers2)
      {plot(timeline,myepoched[mym,,1,2],type="n",xlab='time (secs)',ylab='velocity')
      lines(timeline,myepoched[mym,,1,2],col='red')
      lines(timeline,myepoched[mym,,2,2],col='blue')
      title('After normalization')
      cat("Press [enter] to continue")
      line <- readline()
      }
    }
    
  #----------------------------------------------------------
  # Find heart beat markers and put corrected values in col 3
  # of 4th dimension of myepoched
  #----------------------------------------------------------
  #Find peaks with moving window, looking for troughs in heartbeat
    
  for(mym in 1:nmarkers2)
    {peaklist=numeric(0)
     pdiff=numeric(0)
     badp=numeric(0)
     thisbit=myepoched[mym,,1,2]
     mypts=length(na.omit(thisbit))
    
     for(i in seq(6,mypts-6,2))
      {if(
       (thisbit[i] > thisbit[i-5])      # Check that ith value is greater than the value 5 back
    && (thisbit[i-1] > thisbit[i-5]) # Check that the previous value is greater than the value 5 back
    && (thisbit[i] > thisbit[i+5])   # Check that the ith value is greater than the value 5 ahead
    && (thisbit[i+1]>thisbit[i+5]))  # Check that the next value is greater than the value 5 ahead
       {peaklist=c(peaklist,i)
       }
      }
    
    # Check that the heartbeats are spaced by far enough!
    pdiff <- peaklist[2:length(peaklist)]-peaklist[1:(length(peaklist)-1)] # pdiff is a list of the number of samples between peaks
    badp<-which(pdiff<peakdiffmin) # badp is a list of the pdiff values that are less than peakdiffmin
    if (length(badp) != 0)
    {peaklist<-peaklist[-(badp+1)] # update peaklist, removing peaks identified by badp
    }
    peaklist=c(1,peaklist,mypts) #top and tail the list 
    
    peakn=length(peaklist)
    for (p in 1:(peakn-1))
      {myrange=seq(peaklist[p],peaklist[p+1]) # the indices where the heartbeat will be replaced
      thisheart1=mean(myepoched[mym,myrange,1,2]) # the new values that will be replaced
      thisheart2=mean(myepoched[mym,myrange,2,2])
      myepoched[mym,myrange,1,3]=thisheart1
      myepoched[mym,myrange,2,3]=thisheart2
      }
    } # End of loop through mym
    
  #----------------------------------------------------------
  # See plot after heartbeat correction
  #----------------------------------------------------------
  if (initialdatacheck2==1)
    {for(mym in 1:nmarkers2 )
      {plot(timeline,myepoched[mym,,1,3],type="n",xlab='time (secs)',ylab='velocity')
      lines(timeline,myepoched[mym,,1,3],col='red')
      lines(timeline,myepoched[mym,,2,3],col='blue')
      mytitle=paste('Trial after heart beat correction, trial =', mym)
      title(mytitle)
      cat ("Press [enter] to continue")
      line <- readline()
      }
    }
    
  #----------------------------------------------------------
  # Find mean for baseline and subtract this.
  # This amounts to baseline correction...
  #----------------------------------------------------------
  nepochbase=nmarkers2
  basepoints=(baselinestart_index-epochstart_index):(baselineend_index-epochstart_index) #all baseline points within epoch
  if (baselinecorrect==1)
    {for (mym in 1:nmarkers2)
      {basemeanL=mean(myepoched[mym,basepoints,1,3]) #last dim is 3, which is HB corrected
      basemeanR=mean(myepoched[mym,basepoints,2,3])
      myepoched[mym,,1,4]=100+myepoched[mym,,1,3]-basemeanL #last dim 4 is HB and baseline
      myepoched[mym,,2,4]=100+myepoched[mym,,2,3]-basemeanR
      }
    }
    
  #--------------------------------------------------------
  # Plot after HB correction and baseline correction
  #--------------------------------------------------------
  if(initialdatacheck3==1)
    {for(mym in 1:nmarkers2 )
      {plot(timeline,myepoched[mym,,1,4],type="n",xlab='time (secs)',ylab='velocity')
      lines(timeline,myepoched[mym,,1,4],col='red')
      lines(timeline,myepoched[mym,,2,4],col='blue')
      mytitle=paste('Trial after baseline correction, trial =',mym)
      title(mytitle)
      text(-5,110,'blue=R\n red=L\n',cex=.75)
      cat ("Press [enter] to continue")
      line <- readline()
      }
    }
    
  #--------------------------------------------------------
  # Find and exclude epochs with extreme values in period 
  # between start of baseline and end of POI
  #---------------------------------------------------------
  keepepoch=rep(1,nmarkers2) #initialise for inclusions
  for(mym in 1:nmarkers2)
    {extremerange=c(which(myepoched[mym,(baselinestart_index-epochstart_index):(poiend_index-epochstart_index),1:2,4]>extremehi),which(myepoched[mym,(baselinestart_index-epochstart_index):(poiend_index-epochstart_index),1:2,4]<extremelo))
    # If there are any extreme values, exclude the trial
    if(length(extremerange)>0 )  
      {keepepoch[mym]=0}
    # Keep count of the number of extreme values in allextreme
    if(mym==1)
      {allextreme=extremerange}
    if(mym>1)
      {allextreme=c(allextreme,extremerange)} #keeps record of extreme values across trials
    } # end of trials
  acceptableepochs=which(keepepoch==1)
  results$nExtreme[results_row] <- length(allextreme) #report number of extreme values across trials
    
  #--------------------------------------------------------
  # Get grand average and summary stats. 
  # Plot overall laterality curve.
  #--------------------------------------------------------
  finalepochs=myepoched[acceptableepochs,,,4]
  myN=dim(finalepochs)[1] #initialise vectors for laterality stats
  results$nFinal[results_row] <- myN
  Lmean <- apply(finalepochs[,,1], c(2), mean)
  Rmean <- apply(finalepochs[,,2], c(2), mean)
  LRdiff=Lmean-Rmean
  odds<-seq(from=1,to=myN,by=2)
  evens<-seq(from=2,to=myN,by=2)
  Lmeanodd<-apply(finalepochs[odds,,1],c(2),mean)
  Lmeaneven<-apply(finalepochs[evens,,1],c(2),mean)
  Rmeanodd<-apply(finalepochs[odds,,2],c(2),mean)
  Rmeaneven<-apply(finalepochs[evens,,2],c(2),mean)
  LRdiffodd<-Lmeanodd-Rmeanodd
  LRdiffeven<-Lmeaneven-Rmeaneven
    
  # Compute LI: the mean LRdiff within the POI
  rangestart <- poistart_index - epochstart_index
  rangeend   <- poiend_index - epochstart_index
  myLI <- mean(LRdiff[rangestart:rangeend])
  results$LI[results_row] <- round(myLI,3)
  
  results$odd[results_row] <- round(mean(LRdiffodd[rangestart:rangeend]),3)
  results$even[results_row] <- round(mean(LRdiffeven[rangestart:rangeend]),3)
    
  myside <- 1 # POSITIVE LIs are left lateralised, myside = 1
  if (myLI < 0){myside <- -1} # NEGATIVE LIs are right lateralised, myside = -1
    
  # Extract trial-by-trial data for SE
  indLI=numeric(0)
  for (m in 1:myN)
    {inddiff <- finalepochs[m,rangestart:rangeend,1] - finalepochs[m,rangestart:rangeend,2]
    indLI =c(indLI, mean(inddiff))
    }
  
  mysd <- sd(indLI)
  myse <- as.numeric(format(mysd/sqrt(myN), digits=3))
  results$se[results_row] <- myse
    
  # Other stats
  lowCI=as.numeric(myLI-myse*1.96,3)
  hiCI=as.numeric(myLI+myse*1.96,3)
  results$lowCI[results_row] <- round(lowCI,3)
  results$hiCI[results_row] <- round(hiCI,3)
    
  lateralised=myside # 1 for left lateralisation, -1 for right lateralisation
  if((myside*lowCI)<0) {lateralised=0} # if CI crosses zero, lateralisation is not significant (0)
    
  latdir=c("R","bilat","L")
  results$lat[results_row] <- latdir[lateralised+2]
    
  #----------------------------------------------------------
  #Plot and save overall  laterality curve
  #----------------------------------------------------------
  if (initialdatacheck4==1)
    {plot(timeline,Lmean, type="n",ylab='mean blood flow',xlab='time(s)',ylim=c(90,120)) #set up plot - doesn't actually plot anything
    lines(timeline,Lmean,col='red')
    lines(timeline,Rmean,col='blue')
    lines(timeline,(100+LRdiff),col='black')
    abline(v = poistart_time, lty = 2, col = 'green')
    abline(v = poiend_time, lty = 2, col = 'green')
    abline(v = baselinestart_time, lty = 2)
    abline(v = baselineend_time, lty = 2)
    text(-4,110,'blue=R\n red=L\n black=(L-R) +100',cex=.75)
    mytitle <- paste0(task, ', ', mysubname)
    title(mytitle)

    cat ("Press [enter] to continue")
    line <- readline()
    }
    
  png(filename=paste0(datadir,"LI_Plot_",mysubname,"_",task,".png"))
    
  plot(timeline,Lmean, type="n",ylab='mean blood flow',xlab='time(s)',ylim=c(90,120)) #set up plot - doesn't actually plot anything
  lines(timeline,Lmean,col='red')
  lines(timeline,Rmean,col='blue')
  lines(timeline,(100+LRdiff),col='black')
  abline(v = poistart_time, lty = 2, col = 'green')
  abline(v = poiend_time, lty = 2, col = 'green')
  abline(v = baselinestart_time, lty = 2)
  abline(v = baselineend_time, lty = 2)
  text(-4,110,'blue=R\n red=L\n black=(L-R) +100',cex=.75)
  mytitle <- paste0(task, ', ', mysubname)
  title(mytitle)
    
  dev.off()
    
  #------------------------------------------------------
  # Writes data to file.
  #------------------------------------------------------
  ## Print averaged epoch data to file
  if(length(keepmarkers)>=min_trials){
      mymeanLR<-data.frame(matrix(ncol=5,nrow=mypts)) ###TESTING WAS 801
      mymeanLR[,1]<-mysubname
      mymeanLR[,2]<-timeline
      mymeanLR[,3]<-Lmean
      mymeanLR[,4]<-Rmean
      mymeanLR[,5]<-LRdiff
      colnames(mymeanLR)<-c("ID", "time", "Lmean", "Rmean", "meanDiff")
      csvFile<-paste0(datadir,"Averaged_",mysubname,"_",task,".csv")
      write.csv(mymeanLR, csvFile, row.names=F)
    }
  
  results$Comment <- mycomment
  } # End loop through subjects

# Print results file
write.csv(results, resultsfile, row.names=FALSE)