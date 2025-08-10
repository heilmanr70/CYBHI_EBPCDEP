NOMS_Ind_data <- read.csv("/Users/tristan/CWS Dropbox/Tristan Burgess/CYBHI Project/Rebecca and Tristan and Cristin/EInsight Data/Exports - Raw/Individual Item/NOMS_indiv_2025-08-07.csv",header=T,skip=1)
newnames <- read.csv("/Users/tristan/CWS Dropbox/Tristan Burgess/CYBHI Project/Rebecca and Tristan and Cristin/EInsight Data/Exports - Raw/Individual Item/NOMS_indiv_2025-08-07.csv",header=F)[1,]

tab <- rbind(newnames,colnames(NOMS_Ind_data))

# Hardly a work of genius, but this clunky approach will be insensitive to column order and will throw helpful errors if the variable names change...

NOMS_Ind_clean<- data.frame('Provider' = NOMS_Ind_data$Provider)
NOMS_Ind_clean$Provider.ID <- NOMS_Ind_data$Provider.ID
NOMS_Ind_clean$System.ID <- NOMS_Ind_data$System.ID
NOMS_Ind_clean$Unique.Identifier <- NOMS_Ind_data$Unique.Identifier
NOMS_Ind_clean$Collection <- NOMS_Ind_data$Collection
NOMS_Ind_clean$Noms.ID <- NOMS_Ind_data$Noms.ID
NOMS_Ind_clean$Date.Completed <- NOMS_Ind_data$Date.Completed
NOMS_Ind_clean$Participant.Name <- NOMS_Ind_data$Participant.Name
NOMS_Ind_clean$Participant.ID <- NOMS_Ind_data$Response.16
NOMS_Ind_clean$Assessment.Type <- NOMS_Ind_data$Response.17
NOMS_Ind_clean$Timeline <- NOMS_Ind_data$Response.18

ncols <- dim(NOMS_Ind_clean)[2]
qlist <- c("Q1","2a","2b","2c","2d","2e","2f","2g","3a","3b","3c","3d","3e","3f","3g","4a","4b","4c","4d","4e","4f")

for (i in 1:length(qlist)){
  NOMS_Ind_clean[,dim(NOMS_Ind_clean)[2]+1] <- NOMS_Ind_data[,which(grepl(qlist[i],newnames,fixed=T))]
  # print(qlist[i])
  # print(NOMS_Ind_data[1:10,which(grepl(qlist[i],newnames,fixed=T))])
  colnames(NOMS_Ind_clean)[dim(NOMS_Ind_clean)[2]] <- paste("RES.",qlist[i],sep="")
  NOMS_Ind_clean[,dim(NOMS_Ind_clean)[2]+1] <- NOMS_Ind_data[,(which(grepl(qlist[i],newnames,fixed=T))+1)]
  colnames(NOMS_Ind_clean)[dim(NOMS_Ind_clean)[2]] <- paste("SCR.",qlist[i],sep="")
}

# Calculate putative NOMS Score
NOMS_Ind_clean$NOMSRawScore <- rep(NA,dim(NOMS_Ind_data)[1])
Scorecols <- grepl( "SCR.", names(NOMS_Ind_clean), fixed = TRUE)
tab2 <- rbind(names(NOMS_Ind_clean),Scorecols) # view this table to check correct columns selected

NOMS_Ind_clean[,Scorecols]<-lapply(NOMS_Ind_clean[,Scorecols],as.numeric)
NOMS_Ind_clean$NOMSRawScore  <- rowSums(NOMS_Ind_clean[,Scorecols],na.rm=T) # sum of individual scores

# Test for valid assessment
NOMS_Ind_clean$NOMSValid <- rep(0,dim(NOMS_Ind_clean)[1])
NOMS_Ind_clean$NOMSValid[!is.na(rowSums(NOMS_Ind_clean[,Scorecols]))] <- 1 # <- 1 for a valid assessment, leave as zero if invalid - NB: eInsight excluded any assessment with a single missing item.

NOMS_Ind_clean$NOMSScore <- rep(NA,dim(NOMS_Ind_clean)[1])
NOMS_Ind_clean$NOMSScore[NOMS_Ind_clean$NOMSValid==1] <- NOMS_Ind_clean$NOMSRawScore[NOMS_Ind_clean$NOMSValid==1] 

# Check response distribution -> no bullshit numbers, only valid ones.
hist(NOMS_Ind_clean$NOMSScore)

# Calculate Positive Change (Y/N) 
# Connect initial and follow-up scores by client to check positive change calculations

NOMS_Ind_clean$Valid_INIT <- rep(1,dim(NOMS_Ind_clean)[1])# Select all lines which constitute a valid NOMS Initial score
NOMS_Ind_clean$Valid_INIT[which(is.na(NOMS_Ind_clean$NOMSScore))] <- 0
NOMS_Ind_clean$Valid_INIT[which(NOMS_Ind_clean$Collection!="Initial")] <- 0







#### Need to check code below this point still works








NOMS_POS <- data.frame(System.ID=NOMS_Ind_clean$System.ID[NOMS_Ind_clean$Valid_INIT==1])
NOMS_POS$init_score <- NOMS_Ind_clean$NOMSScore[NOMS_Ind_clean$Valid_INIT==1] # Initial scores
NOMS_POS$followup <- NA

# For each client ID in this list, gather the other valid scores
#for (i in 1:length(NOMS_POS$System.ID)){
for (i in 1:2){
  subset_i <- NOMS_Ind_clean[which(NOMS_Ind_clean$System.ID==NOMS_POS$System.ID[i]),]
  print(subset_i[,3:10,10])
  print(max(subset_i$Date))
  subset_i <- subset_i[subset_i$NOMSValid==1,]
  last_i <- subset_i[which(subset_i$Date == max(subset_i$Date)),]
  print(last_i[,3:6])
  NOMS_POS$followup[i] <- last_i$NOMSScore
}

# If there are none - NA
# If there is at least one, select the latest in time
# Compare the numerical change and report by client ID
# Add the e-insight pos change value
# Compare the two scores for agreement