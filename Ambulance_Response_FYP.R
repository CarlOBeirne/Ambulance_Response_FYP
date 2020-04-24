# Final Year Project
# Carl O'Beirne
# Analysis of Ambulance Responses

###########

########### Packages ###########
library("data.table")
library("zipcode")
library("caret")
library("randomForest")
library("ROSE")
library("leaflet")
library("tidyverse")
library("ggthemes")

########### Read Datasets ###########
DFB_EMS_Data <- fread("Data/DFB_EMS_Data.csv", sep = ",", stringsAsFactors = T, na.strings= "")
NYC_EMS_Data <- fread("Data/EMS_Incident_Dispatch_Data.csv", sep = ",", stringsAsFactors = T) # No longer required to be read

############################################ 

########### Data Cleansing DFB ###########
str(DFB_EMS_Data)

DFB_EMS_Data$Date <- as.POSIXct(DFB_EMS_Data$Date, format = "%d/%m/%Y") # Changning data format to readable R date format

DFB_EMS_Data <- DFB_EMS_Data[, -c(2,19,20,21,23,25,27,29,31,33,35,37,39,41)]

# Checking for NA values
sapply(DFB_EMS_Data, function(x) sum(is.na(x)))

# Removing Obs with NA values in particular variables
DFB_EMS_Data <- DFB_EMS_Data[complete.cases(DFB_EMS_Data[, c(4,6,10:12,14)]), ] # Removing obs with blanks

max(DFB_EMS_Data$TOC_IA_Mins)

DFB_EMS_Data <- subset(DFB_EMS_Data, TOC_CD_Mins <= 360 & TOC_IA_Mins <= 360) # Removing data where call length > 6 hours
############################################

########### Data Cleansing NYC ###########
str(NYC_EMS_Data)

# Dropping level in borough - set as unknown and there are only 5 boroughs
NYC_EMS_Data$BOROUGH <- droplevels(NYC_EMS_Data$BOROUGH, "UNKNOWN")

# Removing final call type unknown

NYC_EMS_Data <- subset(NYC_EMS_Data, FINAL_CALL_TYPE != 'UNKNOW')

# Changing date format
NYC_EMS_Data$INCIDENT_DATETIME <- as.POSIXct(NYC_EMS_Data$INCIDENT_DATETIME, format = "%m/%d/%Y %I:%M:%S %p", tx = "UTC")

# Reducing data to just 2017/2018
NYC_EMS_Data <- NYC_EMS_Data[ NYC_EMS_Data$INCIDENT_DATETIME >= as.POSIXct("2017-01-01") & NYC_EMS_Data$INCIDENT_DATETIME <= as.POSIXct("2018-12-31"), ]

# Changing T/F values to Y/N for consistency
NYC_EMS_Data$HELD_INDICATOR <- gsub('false', 'N', NYC_EMS_Data$HELD_INDICATOR)
NYC_EMS_Data$HELD_INDICATOR <- gsub('true', 'Y', NYC_EMS_Data$HELD_INDICATOR)
NYC_EMS_Data$HELD_INDICATOR <- as.factor(NYC_EMS_Data$HELD_INDICATOR)

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

#Checking for NA Values
sapply(NYC_EMS_Data, function(x) sum(is.na(x)))

NYC_EMS_Data <- NYC_EMS_Data[complete.cases(NYC_EMS_Data[, c(13,14,18,19,23:27)]), ] # Removing obs with blanks

# Removing irrelevant columns
NYC_EMS_Data <- NYC_EMS_Data[, -c(8,12)]

# Saving Cleaned File for quicker reading
fwrite(NYC_EMS_Data, "Data/NYC_EMS_Data.csv", row.names = F)
fwrite(DFB_EMS_Data, "Data/NEW_DFB_EMS_Data.csv", row.names = F)
;############################################

########### Functions ###########
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

# Function to find the mode for any data
findMode <- function(x){
    uniqueVals <- unique(x)
    uniqueVals[which.max(tabulate(match(x, uniqueVals)))]
}

############################################

########### Reading Cleaned NYC Data ###########
NYC_EMS_Data <- fread("Data/NYC_EMS_Data.csv", header = T, sep = ",", stringsAsFactors = T)
NYC_EMS_MapSample <- fread("Data/NYC_EMS_MapData.csv", header = T, sep = ",", stringsAsFactors = T)

########### Start of Analysis ###########

########### Model 1 - RF to Predict whether the incident will be Reopened in NYC ###########

#NYC RF Sample
set.seed(16326186) # Reproducability
index <- sample(1:nrow(NYC_EMS_Data), 300000, replace = F)
NYC_EMS_RFSample <- NYC_EMS_Data[index, ]

sapply(NYC_EMS_RFSample, function(x) sum(is.na(x)))

str(NYC_EMS_RFSample)
NYC_EMS_RFSample <- NYC_EMS_RFSample[, -c(1:5,7:9,10,13:15)]

# Create Train & Test Data
set.seed(16326186) # Reproducability
index <- sample(1:nrow(NYC_EMS_RFSample), 0.75*nrow(NYC_EMS_RFSample), replace = F )
nycTrain <- NYC_EMS_RFSample[index, ]
nycTest <- NYC_EMS_RFSample[-index, ]

rm(index)

nyc_rf_model <- randomForest(HELD_INDICATOR ~., nycTrain)

varImpPlot(nyc_rf_model)

nyc_rf_pred <- predict(nyc_rf_model, nycTest)

heldupCM <- confusionMatrix(nyc_rf_pred, nycTest$HELD_INDICATOR, positive = 'Y')

rm(nyc_rf_pred)
rm(nycTest)
rm(nycTrain)
rm(NYC_EMS_RFSample)
# ROSE (Random Over Sampling Examples)
# Oversampling with Rose
index <- sample(2, nrow(NYC_EMS_RFSample), replace = T, prob = c(0.75,0.25))
roseTrain <- NYC_EMS_RFSample[index == 1, ]
roseTest <- NYC_EMS_RFSample[index == 2, ]

rm(index)

table(nycTrain$HELD_INDICATOR)

heldup <- ovun.sample(HELD_INDICATOR ~., data=roseTrain, method = "over", N = 429574)$data

table(heldup$HELD_INDICATOR)

#Creating Model
rfTrainRose <- randomForest(HELD_INDICATOR ~., heldup)

#Evaluate Model with Test Data
rosePred <- predict(rfTrainRose, roseTest)

roseHeldupCM <- confusionMatrix(rosePred, roseTest$HELD_INDICATOR, positive = 'Y')

rm(rosePred)
rm(roseTest)
rm(roseTrain)
rm(rfTrainRose)
rm(heldup)

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
DFB_CallSev_Times <- DFB_EMS_Data[, c(1, 16,17, 25, 28)]

index <- sample(1:nrow(NYC_CallSev_Times), 15000, replace = F)
NYC_CallSev_Times <- NYC_CallSev_Times[index, ]

index <- sample(1:nrow(DFB_CallSev_Times), 5000, replace = F)
DFB_CallSev_Times <- DFB_CallSev_Times[index, ]

rm(index)

table(DFB_CallSev_Times$Criticality_Code)

fwrite(NYC_CallSev_Times, "Data/NYC_CallSev_Times.csv", row.names = F)
fwrite(DFB_CallSev_Times, "Data/DFB_CallSev_Times.csv", row.names = F)

DFB_2017 <- DFB_EMS_Data
DFB_2017 <- DFB_EMS_Data[ DFB_2017$Date >= as.POSIXct("2017-01-01") & DFB_2017$Date <= as.POSIXct("2017-12-31"), ]

sd(DFB_CallSev_Times[DFB_CallSev_Times$Criticality_Code == 'E', ]$TOC_IA_Mins)
########### Analysis Results ###########
# Held up RF model
varImpPlot(nyc_rf_model)
heldupCM

############################################

# Top 10 Areas DFB

DFB_Top_10_Areas <- DFB_EMS_Data %>%
    group_by(District) %>%
    summarise(count = n()) %>%
    top_n(n = 10, wt = count)

ggplot(DFB_Top_10_Areas, aes(x = District, y = count)) +
    geom_col(col = rainbow(10), 
             bg = rainbow(10)) +
    ggtitle(label = "Top 10 Regions",
            subtitle = "by No. of Calls")+
    theme_economist_white()
    
# Time Series Testing
TOC_TimeSeries_DFB <- data.frame(DFB_EMS_Data$Date, DFB_EMS_Data$TOC, DFB_EMS_Data$Criticality_Code)
TOC_TimeSeries_DFB$DateTimeTOC <- paste(TOC_TimeSeries_DFB$DFB_EMS_Data.Date, TOC_TimeSeries_DFB$DFB_EMS_Data.TOC)

str(TOC_TimeSeries_DFB)

TOC_TimeSeries_DFB$DateTimeTOC <- as.POSIXct(TOC_TimeSeries_DFB$DateTimeTOC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

TOC_TimeSeries_DFB <- subset(TOC_TimeSeries_DFB, DateTimeTOC >= "2017-12-25 00:00:00" & DateTimeTOC <= "2017-12-31 23:59:59")

plot.ts(TOC_TimeSeries_DFB$DateTimeTOC)

plot(ts(TOC_TimeSeries_DFB, frequency = 1))
