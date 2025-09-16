library(openxlsx)
library(dplyr)

discharge.data <- read.xlsx("/Users/tristan/CWS Dropbox/Tristan Burgess/CYBHI Project/Rebecca and Tristan and Cristin/EInsight Data/Exports - Raw/May13_Report_Data/Dishcharge_Report.xlsx")

#outcomes.data <- read_xlsx("~/Dropbox/Rebecca and Tristan and Cristin/EInsight Data/Exports - Raw/2025_04/Discharge/CYBHI_EInsights Export_Discharge Summary Report_Export 02.14.2025.xlsx")

# [TB] NOTE - AS FAR AS I CAN TELL, THE ONLY ADDITIONAL USEFUL PIECE OF DATA IN THIS FILE IS "COMPLETED" - PRESUMABLY THAT IS MODEL COMPLETION?
clinical.data <- read.xlsx("/Users/tristan/CWS Dropbox/Tristan Burgess/CYBHI Project/Rebecca and Tristan and Cristin/EInsight Data/Exports - Raw/May13_Report_Data/Clinical_Summary.xlsx")

demo.data <- read.xlsx("/Users/tristan/CWS Dropbox/Tristan Burgess/CYBHI Project/Rebecca and Tristan and Cristin/EInsight Data/Exports - Raw/May13_Report_Data/Client_Demographics.xlsx")

# Detect and remove entries where the Provider ID contains the text "DEMO" as these are not real data
text <- demo.data$Provider.ID
contains_DEMO <- grepl("DEMO", text)
print(contains_DEMO)
demo.data <- demo.data[which(!contains_DEMO),]



# Data Joining
join.data <- merge(discharge.data,demo.data, by="Client.ID",all.x=TRUE)
length(discharge.data$`Client.ID`)
length(demo.data$`Client.ID`)
length(join.data$`Client.ID`)
length(unique(join.data$Client.ID))

# [TB] 7 additional rows indicate that 7 clients had multiple entries in demo.data. Will need to deal with these once the reason is determined. Could be mistakes, but they seem quite consistent. Suspect that same client ID is being used for multiple members of a family group. All instances of multiple entries in the demo data sheet share the same 11 providers.

#are there unique client IDs? 
# [TB] Test fails in part because the data are assessment-level not client level
length(unique(join.data$`Client ID`)) == nrow(join.data)

#which IDs have been repeated and how many times? 
join.data %>% 
  group_by(`Client.ID`) %>% 
  summarise(n=n()) %>% 
  filter(n>1)

# Question 2.01 Did successful transition rates out of EBP/CDEP services differ by location?
# THREE inclusion criteria: must have initial time point data (i.e., be enrolled in an intervention), must have a valid reason for discharge selected at time of discharge (i.e., excludes those with "no reason selected"), and client must be attached to a specific grantee location
#Make a table of participation and completion rates by location: 
# % reported Mutually agreed upon cessation of Tx/successful completion at Discharge by location
# % of those who met Tx goals (% successful completion + improved score) by location"

join.data$Pre.Score[join.data$Pre.Score=="-"] <- NA
join.data$Post.Score[join.data$Post.Score=="-"] <- NA
join.data$Reason.For.Discharge[join.data$Reason.For.Discharge=="-"] <- NA

data201 <- filter(join.data,  !is.na(Location))
data201 <- filter(data201,  !is.na(Pre.Score))
data201 <- filter(data201,  !is.na(Post.Score))
data201 <- filter(data201,  !is.na(Reason.For.Discharge))
data201$Pre.Score <- as.numeric(data201$Pre.Score)
data201$Post.Score <- as.numeric(data201$Post.Score)
data201$Reason.For.Discharge <- as.factor(data201$Reason.For.Discharge)

out <- table(data201$Location,data201$Reason.For.Discharge)
write.xlsx(out,"/Users/tristan/CWS Dropbox/Tristan Burgess/CYBHI Project/Rebecca and Tristan and Cristin/Results/201-1.xlsx")

data201$MTG <- data201$Positive.Outcomes
data201$Positive.Outcomes[which(data201$Reason.For.Discharge!="Mutually agreed cessation of treatment (Successful Completion)")] <- "No"

out <- table(data201$Location,data201$MTG)
write.xlsx(out,"/Users/tristan/CWS Dropbox/Tristan Burgess/CYBHI Project/Rebecca and Tristan and Cristin/Results/201-2.xlsx")