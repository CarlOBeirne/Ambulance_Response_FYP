# Final Year Project
# Carl O'Beirne
# Analysis of Ambulance Responses

# Packages
library("data.table")
library("zipcode")
library("caret")
library("randomForest")
library("ROSE")
library("leaflet")
library("tidyverse")

# Read Datasets
DFB_EMS_Data <- fread("Data/DFB_EMS_Data.csv", sep = ",", stringsAsFactors = T)
NYC_EMS_Data <- fread("Data/NYC_EMS_Data.csv", sep = ",", stringsAsFactors = T)

# Functions
# Obtain Latitude & Longitude from Zip Code
NYC_EMS_MapSample <- sample(1:nrow(NYC_EMS_Data), 80000, replace = F)
NYC_EMS_MapSample <- NYC_EMS_Data[NYC_EMS_MapSample, ]

data("zipcode")

for (i in 1:nrow(NYC_EMS_MapSample)){
    if(length(zipcode$zip[NYC_EMS_MapSample$ZIPCODE[i] == zipcode$zip]) == 1){
        NYC_EMS_MapSample$Latitude[i] <- zipcode$latitude[NYC_EMS_MapSample$ZIPCODE[i] == zipcode$zip]
        NYC_EMS_MapSample$Longitude[i] <- zipcode$longitude[NYC_EMS_MapSample$ZIPCODE[i] == zipcode$zip]
        print(paste("Row: ",i, "Zip Code: ",NYC_EMS_MapSample$ZIPCODE[i],NYC_EMS_MapSample$Latitude[i],NYC_EMS_MapSample$Longitude[i], "Status: ", TRUE))
    }else{
        NYC_EMS_MapSample$Latitude[i] <- NA
        NYC_EMS_MapSample$Longitude[i] <- NA
        print(paste("Row: ",i, "Zip Code: ",NYC_EMS_MapSample$ZIPCODE[i],NYC_EMS_MapSample$Latitude[i],NYC_EMS_MapSample$Longitude[i], "Status: ", FALSE))
    }
}

fwrite(NYC_EMS_MapSample, "Data/NYC_EMS_MapData.csv", row.names = F)# Saving the file

rm(i)
rm(zipcode)

# Data Types
str(DFB_EMS_Data)
str(NYC_EMS_Data)

# Data Cleansing DFB
DFB_EMS_Data[DFB_EMS_Data == ''] <- NA # Changing unrecognized Blanks to NA Values

DFB_EMS_Data <- data.frame(DFB_EMS_Data) # Convert back to Data Frame

# Checking for NA values
sapply(DFB_EMS_Data, function(x) sum(is.na(x)))

# Removing Obs with NA values in particular variables
DFB_EMS_Data <- DFB_EMS_Data[complete.cases(DFB_EMS_Data[, c(5,7,11:13,15)]), ] # Removing obs with blanks

# Data Cleansing NYC
# Changing T/F values to Y/N
NYC_EMS_Data$VALID_DISPATCH_RSPNS_TIME_INDC <- gsub('false', 'N', NYC_EMS_Data$VALID_DISPATCH_RSPNS_TIME_INDC)
NYC_EMS_Data$VALID_DISPATCH_RSPNS_TIME_INDC <- gsub('true', 'Y', NYC_EMS_Data$VALID_DISPATCH_RSPNS_TIME_INDC)
NYC_EMS_Data$VALID_DISPATCH_RSPNS_TIME_INDC <- as.factor(NYC_EMS_Data$VALID_DISPATCH_RSPNS_TIME_INDC)

NYC_EMS_Data$VALID_INCIDENT_RSPNS_TIME_INDC <- gsub('false', 'N', NYC_EMS_Data$VALID_INCIDENT_RSPNS_TIME_INDC)
NYC_EMS_Data$VALID_INCIDENT_RSPNS_TIME_INDC <- gsub('true', 'Y', NYC_EMS_Data$VALID_INCIDENT_RSPNS_TIME_INDC)
NYC_EMS_Data$VALID_INCIDENT_RSPNS_TIME_INDC <- as.factor(NYC_EMS_Data$VALID_INCIDENT_RSPNS_TIME_INDC)

NYC_EMS_Data$REOPEN_INDICATOR <- gsub('false', 'N', NYC_EMS_Data$REOPEN_INDICATOR)
NYC_EMS_Data$REOPEN_INDICATOR <- gsub('true', 'Y', NYC_EMS_Data$REOPEN_INDICATOR)
NYC_EMS_Data$REOPEN_INDICATOR <- as.factor(NYC_EMS_Data$REOPEN_INDICATOR)

NYC_EMS_Data$SPECIAL_EVENT_INDICATOR <- gsub('false', 'N', NYC_EMS_Data$SPECIAL_EVENT_INDICATOR)
NYC_EMS_Data$SPECIAL_EVENT_INDICATOR <- gsub('true', 'Y', NYC_EMS_Data$SPECIAL_EVENT_INDICATOR)
NYC_EMS_Data$SPECIAL_EVENT_INDICATOR <- as.factor(NYC_EMS_Data$SPECIAL_EVENT_INDICATOR)

NYC_EMS_Data$STANDBY_INDICATOR <- gsub('false', 'N', NYC_EMS_Data$STANDBY_INDICATOR)
NYC_EMS_Data$STANDBY_INDICATOR <- gsub('true', 'Y', NYC_EMS_Data$STANDBY_INDICATOR)
NYC_EMS_Data$STANDBY_INDICATOR <- as.factor(NYC_EMS_Data$STANDBY_INDICATOR)

NYC_EMS_Data$TRANSFER_INDICATOR <- gsub('false', 'N', NYC_EMS_Data$TRANSFER_INDICATOR)
NYC_EMS_Data$TRANSFER_INDICATOR <- gsub('true', 'Y', NYC_EMS_Data$TRANSFER_INDICATOR)
NYC_EMS_Data$TRANSFER_INDICATOR <- as.factor(NYC_EMS_Data$TRANSFER_INDICATOR)

#Creating NA values where they are not detected
NYC_EMS_Data[NYC_EMS_Data == ''] <- NA # Changing unrecognized Blanks to NA Values

NYC_EMS_Data <- data.frame(NYC_EMS_Data)

NYC_EMS_Data$INCIDENT_DATETIME <- as.POSIXct(NYC_EMS_Data$INCIDENT_DATETIME, format = "%m/%d/%Y %I:%M:%S %p", tx = "UTC")

NYC_EMS_Data <- NYC_EMS_Data[ NYC_EMS_Data$INCIDENT_DATETIME >= as.POSIXct("2017-01-01") & NYC_EMS_Data$INCIDENT_DATETIME <= as.POSIXct("2018-12-31"), ]
#Checking for NA Values
sapply(NYC_EMS_Data, function(x) sum(is.na(x)))

NYC_EMS_Data <- NYC_EMS_Data[complete.cases(NYC_EMS_Data[, c(7,10,11,13:17,19,22:27)]), ] # Removing obs with blanks

# Saving Cleaned File for quicker reading
fwrite(NYC_EMS_Data, "Data/NYC_EMS_Data.csv", row.names = F)
NYC_EMS_Data <- fread("Data/NYC_EMS_Data.csv", header = T, sep = ",", stringsAsFactors = T)
#Analysis

# Model 1 - RF to Predict whether the incident will be Reopened in NYC

#NYC RF Sample
set.seed(16326186) # Reproducability
index <- sample(1:nrow(NYC_EMS_Data), 20000, replace = F)
NYC_EMS_RFSample <- NYC_EMS_Data[index, ]

NYC_EMS_RFSample$BOROUGH <- droplevels(NYC_EMS_RFSample$BOROUGH, 6)

sapply(NYC_EMS_RFSample, function(x) sum(is.na(x)))

str(NYC_EMS_RFSample)
NYC_EMS_RFSample <- NYC_EMS_RFSample[, -c(1:5,7,10,11,15:17,22)]

NYC_EMS_RFSample$POLICEPRECINCT <- as.factor(NYC_EMS_RFSample$POLICEPRECINCT)

# Create Train & Test Data
set.seed(16326186) # Reproducability
index <- sample(1:nrow(NYC_EMS_RFSample), 0.75*nrow(NYC_EMS_RFSample), replace = F )
nycTrain <- NYC_EMS_RFSample[index, ]
nycTest <- NYC_EMS_RFSample[-index, ]

rm(index)


nyc_rf_model <- randomForest(POLICEPRECINCT ~., nycTrain)

varImpPlot(nyc_rf_model)

nyc_rf_pred <- predict(nyc_rf_model, nycTest)

table(nycTest$TRANSFER_INDICATOR)
table(nycTrain$TRANSFER_INDICATOR)


confusionMatrix(nyc_rf_pred, nycTest$BOROUGH)

# SMOTE
reopen <- sample(2, nrow(NYC_EMS_RFSample), replace = T, prob = c(0.7,0.3))
train <- NYC_EMS_RFSample[reopen==1, ]
test <- NYC_EMS_RFSample[reopen==2, ]

table(train$REOPEN_INDICATOR)
prop.table(table(train$REOPEN_INDICATOR))
summary(train$REOPEN_INDICATOR)

#Creating Model
rfTrain <- randomForest(FINAL_SEVERITY_LEVEL_CODE ~., train)
rfOver <- randomForest(REOPEN_INDICATOR ~., over)

#Evaluate Model with Test Data
confusionMatrix(predict(rfOver, test), test$REOPEN_INDICATOR)

# ROSE
table(train$REOPEN_INDICATOR)


over <- ovun.sample(REOPEN_INDICATOR ~., data=train, method = "over", N = 138898)$data

table(over$REOPEN_INDICATOR)

rfModel <- randomForest(REOPEN_INDICATOR ~., over)
rfPred <- predict(rfModel, test) 
str()

# Mapping
NYC_EMS_MapData <- fread("Data/NYC_EMS_MapData.csv", header = T, sep = ",")

NYC_EMS_MapData_Sample <- sample(1:nrow(NYC_EMS_MapData), 4000, replace = F)
NYC_EMS_MapData_Sample <- NYC_EMS_MapData[NYC_EMS_MapData_Sample, ]

NYC_Call_Map <- leaflet() %>%
    addTiles() %>%
    addMarkers(lat = NYC_EMS_MapData_Sample$Latitude, lng = NYC_EMS_MapData_Sample$Longitude, popup = NYC_EMS_MapData_Sample$FINAL_CALL_TYPE)

rm(NYC_EMS_MapData_Sample)
rm(NYC_EMS_MapData)

# Analysis of Call types & Call duration
NYC_CallSev_Times <- NYC_EMS_Data[, c(6,14)]
DFB_CallSev_Times <- DFB_EMS_Data[, c(17,18,36, 42)]

index <- sample(1:nrow(NYC_CallSev_Times), 15000, replace = F)
NYC_CallSev_Times <- NYC_CallSev_Times[index, ]
index <- sample(1:nrow(DFB_CallSev_Times), 15000, replace = F)
DFB_CallSev_Times <- DFB_CallSev_Times[index, ]

rm(index)

fwrite(NYC_CallSev_Times, "Data/NYC_CallSev_Times.csv", row.names = F)
fwrite(DFB_CallSev_Times, "Data/DFB_CallSev_Times.csv", row.names = F)


mean(DFB_EMS_Data$TOC_IA_Mins)
median(DFB_EMS_Data$TOC_IA_Mins)
which.max(tabulate(DFB_EMS_Data$TOC_IA_Mins))

max(DFB_EMS_Data$TOC_CD_Mins)/60

findMode <- function(x){
    uniqueVals <- unique(x)
    uniqueVals[which.max(tabulate(match(x, uniqueVals)))]
}

findMode(DFB_EMS_Data$Criticality_Code)

DFB_CallSev_Times <- subset(DFB_CallSev_Times, TOC_CD_Mins < 750)
