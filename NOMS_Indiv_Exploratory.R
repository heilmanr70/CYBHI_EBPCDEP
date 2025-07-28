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

NOMS_Ind_clean <- NOMS_Ind_data[,c(1:6,37,39,41,7,9:22,43:48,23,24,49:54,25:36)] # Cristin will hate this line... :) Ideally would be replaced by robust searches for each column in order.

# Calculate putative NOMS Score
NOMS_Ind_clean$NOMSRawScore <- rep(NA,dim(NOMS_Ind_data)[1])
Scorecols <- grepl( "SCR. ", names(NOMS_Ind_clean), fixed = TRUE)
tab2 <- rbind(names(NOMS_Ind_clean),Scorecols) # view this table to check correct columns selected

NOMS_Ind_clean[,Scorecols]<-lapply(NOMS_Ind_clean[,Scorecols],as.numeric)
NOMS_Ind_clean$NOMSRawScore  <- rowSums(NOMS_Ind_clean[,Scorecols]) # sum of individual scores

# Test for valid assessment
NOMS_Ind_clean$NOMSValid <- rep(0,dim(NOMS_Ind_clean)[1])
NOMS_Ind_clean$NOMSValid[!is.na(rowSums(NOMS_Ind_clean[,Scorecols]))] <- 1 # <- 1 for a valid assessment, leave as zero if invalid - NB: Steve excluded any assessment with a single missing item.

NOMS_Ind_clean$NOMSScore <- rep(NA,dim(NOMS_Ind_clean)[1])
NOMS_Ind_clean$NOMSScore[NOMS_Ind_clean$NOMSValid==1] <- NOMS_Ind_clean$NOMSRawScore[NOMS_Ind_clean$NOMSValid==1] # NOTE: Check response distribution -> no bullshit numbers, only valid ones.

# Calculate Positive Change (Y/N) 
# Next step will be to connect initial and follow-up scores by client to check positive change calculations

Valid_INIT <- rep(1,dim(NOMS_Ind_clean)[1])# Select all lines which constitute a valid NOMS Initial score
Valid_INIT[which(is.na(NOMS_Ind_clean$NOMSScore))] <- 0
Valid_INIT[which(NOMS_Ind_clean$Collection!="NOMS Self-Report Version Initial")] <- 0

CIDS <- NOMS_Ind_clean$`Client ID`[Valid_INIT]
NOMS_POS <- data.frame(Client_ID=CIDS) # SEEMS plausible, has not been checked yet

# For each client ID in this list, gather the other valid scores
# If there are none - NA
# If there is at least one, select the latest in time
# Compare the numerical change and report by client ID