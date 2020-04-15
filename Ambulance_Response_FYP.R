# Final Year Project
# Carl O'Beirne
# Analysis of Ambulance Responses

# Packages
install.packages("data.table")
library(data.table)

# Read Datasets
DFB_EMS_Data <- fread("Data/DFB_EMS_Data.csv", sep = ",", stringsAsFactors = T)
NYC_EMS_Data <- fread("Data/NYC_EMS_Data_Desc.csv", sep = ",", stringsAsFactors = T)

# Checking for NA values
sapply(DFB_EMS_Data, function(x) sum(is.na(x)))
sapply(NYC_EMS_Data, function(x) sum(is.na(x)))

