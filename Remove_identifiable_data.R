
filelist <- list.files(getwd()) #Creates list of all files in directory
nfiles <- length(filelist)
id_data <- data.frame('Filename' = character(), #Creates empty dataframe for identifiable data
                      'Name1' = character(),
                      'Name2' = character(),
                      'DOB' = character(),
                      stringsAsFactors=FALSE)

for (i in 1:length(filelist)){ #Loops through all files in filelist
  
  tmp <- readLines(filelist[i]) #Reads in data from one file at a time
  
  #Record filename in id_data
  id_data[i,1] <- filelist[i]
  
  #Remove names (line 1)
  names <- tmp[1]
  tmp[1] <- "Patient Name: XX XXXXXXX"
  
  #Record first name in id_data
  id_data[i,2] <- gsub(" .*$", "", substring(names, 14, 100)) # Anything before the space
  #Record second name in id_data
  id_data[i,3] <- gsub(".* ", "", substring(names, 14, 100)) # Anything after the space
  
  #Remove DOB (line 2)
  dob <- tmp[2]
  tmp[2] <- "birthday:XX:XX:XXXX" #Changes second line of text to replace the DOB with x's
  
  #Record DOB in id_data
  id_data[i,4] <- substring(dob, 10, 100)
  
  #Overwrite original file with names and ID removed
  writeLines(tmp, filelist[i])
}