#First download needed csv's and libraries
setwd("C:/Users/rickard/Documents/2024 Nutrient and TSS analysis")
library(readxl)
library(ggplot2)
TSS<-read_xlsx("~/2024 Nutrient and TSS analysis/TSS_allyears.xlsx",sheet=3)
sample_locations<-read_xlsx("~/2024 Nutrient and TSS analysis/TSS_allyears.xlsx",sheet=4)

#remove negative values
TSS<-TSS[TSS$`TSS (mg/L)`>0 & TSS$`TSS (mg/L)`!="see above",]

#Check the notes and delete bad data
unique(TSS$NOTES)
TSS<-TSS[TSS$`Keepfor analysis`=="Y",]

#Merge with location and date date
sample_locations
data<-merge(TSS,sample_locations,by="Sample Number",all.x=TRUE)
data
#Look for any columns with missing data

getwd()

install.packages("usethis")
