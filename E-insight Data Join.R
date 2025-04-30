library(openxlsx)
library(dplyr)


discharge.data <- read.xlsx("/Users/tristan/CWS Dropbox/Tristan Burgess/CYBHI Project/Rebecca and Tristan and Cristin/EInsight Data/Exports - Raw/2025_04/CYBHI_EInsights Export_Discharge Summary Report_Export 04.07.2025.xlsx")
#outcomes.data <- read_xlsx("~/Dropbox/Rebecca and Tristan and Cristin/EInsight Data/Exports - Raw/2025_04/Discharge/CYBHI_EInsights Export_Discharge Summary Report_Export 02.14.2025.xlsx")

# [TB] NOTE - AS FAR AS I CAN TELL, THE ONLY ADDITIONAL USEFUL PIECE OF DATA IN THIS FILE IS "COMPLETED" - PRESUMABLY THAT IS MODEL COMPLETION?
clinical.data <- read.xlsx("/Users/tristan/CWS Dropbox/Tristan Burgess/CYBHI Project/Rebecca and Tristan and Cristin/EInsight Data/Exports - Raw/2025_04/EInsight_Clinical Summary_04.07.2025.xlsx")

demo.data <- read.xlsx("/Users/tristan/CWS Dropbox/Tristan Burgess/CYBHI Project/Rebecca and Tristan and Cristin/EInsight Data/Exports - Raw/2025_04/EInsight_Demographics_04.07.2025.xlsx")

# Detect and remove entries where the Provide ID contains the text "DEMO" as these are not real data
text <- demo.data$Provider.ID
contains_DEMO <- grepl("DEMO", text)
print(contains_DEMO)
demo.data <- demo.data[which(!contains_DEMO),]



# Data Joining
join.data <- merge(discharge.data,demo.data, by="Client.ID",all.x=TRUE)
length(discharge.data$`Client.ID`)
length(demo.data$`Client.ID`)
length(join.data$`Client.ID`)
# [TB] 7 additional rows indicate that 7 clients had multiple entries in demo.data. Will need to deal with these once the reason is determined. Could be mistakes, but they seem quite consistent. Suspect that same client ID is being used for multiple members of a family group. All instances of multiple entries in the demo data sheet share the same 11 providers.

#are there unique client IDs? 
# [TB] Test fails in part becuase this is assessment-level not client level
length(unique(join.data$`Client ID`)) == nrow(join.data)

#which IDs have been repeated and how many times? 
join.data %>% 
  group_by(`Client.ID`) %>% 
  summarise(n=n()) %>% 
  filter(n>1)
