library(openxlsx)
date.data <- read.xlsx("/Users/tristan/CWS Dropbox/Tristan Burgess/CYBHI Project/Rebecca and Tristan and Cristin/Date Comparisons/Round 2 Client Clinical Summary Report_06.30.2025.xlsx")

date.data$LOC_start <- as.Date(date.data$Level.Of.Care.Start)
summary(date.data$LOC_start)

date.data$INT_start <- as.Date(date.data$Intervention.Start)
summary(date.data$INT_start)

comp.date.data <- date.data[!is.na(date.data$LOC_start),]
comp.date.data <- comp.date.data[!is.na(comp.date.data$INT_start),]

summary(comp.date.data$LOC_start)
summary(comp.date.data$INT_start)

hist(summary(comp.date.data$LOC_start),breaks=10)

# Exlcude implausible dates
dim(comp.date.data)
comp.date.data <- comp.date.data[(comp.date.data$LOC_start>"2023-12-31") & (comp.date.data$LOC_start<"2026-01-01") & (comp.date.data$INT_start>"2023-12-31") & (comp.date.data$INT_start<"2026-01-01"),]
dim(comp.date.data)

hist(summary(comp.date.data$LOC_start),breaks=10,xlim="2024-01-01")

comp.date.data$diff <- as.numeric(abs(comp.date.data$LOC_start-comp.date.data$INT_start))
intervals <- cut(comp.date.data$diff,breaks=c(-.5,0.5,1,7,30,1000))
summary(intervals)
