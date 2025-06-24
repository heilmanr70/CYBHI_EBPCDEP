NOMS_Ind_data <- read.csv("/Users/tristan/CWS Dropbox/Tristan Burgess/CYBHI Project/Rebecca and Tristan and Cristin/EInsight Data/Exports - Raw/Individual Item/NOMS_indiv_2025-06-17.csv",header=T,skip=1)

names <- read.csv("/Users/tristan/CWS Dropbox/Tristan Burgess/CYBHI Project/Rebecca and Tristan and Cristin/EInsight Data/Exports - Raw/Individual Item/NOMS_indiv_2025-06-17.csv",header=F)[1,]

# Delete Unwanted Columns
# NOTE: NOT ROBUST TO CHANGES IN UNDERLYING DATASET COLUMNS/STRUCTURE

drops <- c(4,8)
NOMS_Ind_data <- NOMS_Ind_data[,-drops]
names <- names[-drops]
newnames <- names

# Fix Header Names
list = rep(NA,length(newnames))
for(i in 7:length(newnames)){
  if(newnames[i]==""){
    newnames[i] <- paste("SCR.",names[i-1])
  }
  else{
    newnames[i] <- paste("RES.",names[i])
  }
}

#tab <- rbind(names,colnames(NOMS_Ind_data),newnames)
colnames(NOMS_Ind_data) <- newnames

# Test for valid assessment
NOMS_Ind_data$NOMSValid <- rep(0,dim(NOMS_Ind_data)[1])
NOMS_Ind_data$NOMSValid # <- 1 for a valid assessment, leave as zero if invalid

# Calculate NOMS Score
NOMS_Ind_data$NOMSRawScore <- rep(NA,dim(NOMS_Ind_data)[1])
NOMS_Ind_data$NOMSRawScore  <- # sum of individual scores according to scoring guide
NOMS_Ind_data$NOMSScore <- rep(NA,dim(NOMS_Ind_data)[1])
NOMS_Ind_data$NOMSScore[NOMS_Ind_data$NOMSValid==1] <- NOMS_Ind_data$NOMSRawScore

# Calculate Positive Change (Y/N) 
# Next step will be to connect initial and follow-up scores by client to check positive change calculations