library(openxlsx)

#outcomes.data <- read.xlsx("/Users/tristan/CWS Dropbox/Tristan Burgess/CYBHI Project/Rebecca and Tristan and Cristin/EInsight Data/Exports - Raw/2025_02/Discharge/CYBHI_EInsights Export_Discharge Summary Report_Export 02.14.2025.xlsx")
outcomes.data <- read_xlsx("~/Dropbox/Rebecca and Tristan and Cristin/EInsight Data/Exports - Raw/2025_02/Discharge/CYBHI_EInsights Export_Discharge Summary Report_Export 02.14.2025.xlsx")

#demo.data <- read.xlsx("/Users/tristan/CWS Dropbox/Tristan Burgess/CYBHI Project/Rebecca and Tristan and Cristin/EInsight Data/Exports - Raw/2025_02/Discharge/MOCKUP CYBHI_EInsights Export_Demographics Summary Report_Export 02.14.2025.xlsx")
demo.data <- read_xlsx("~/Dropbox/Rebecca and Tristan and Cristin/EInsight Data/Exports - Raw/2025_02/Discharge/MOCKUP CYBHI_EInsights Export_Demographics Summary Report_Export 02.14.2025.xlsx")

join.data <- merge(outcomes.data,demo.data, by="Client ID")

length(outcomes.data$`Client ID`)
length(demo.data$`Client ID`)

test <- join.data$TestNum/join.data$`Client ID`
max(test)
min(test)

#are there unique client IDs? 
length(unique(join.data$`Client ID`)) == nrow(join.data)

#which IDs have been repeated and how many times? 
join.data %>% 
  group_by(`Client ID`) %>% 
  summarise(n=n()) %>% 
  filter(n>1)

#NOW we have this!