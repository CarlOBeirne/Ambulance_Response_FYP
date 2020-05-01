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
library("leaflet.extras")
library("magrittr")
library("e1071")
library("mlbench")

########### Read Datasets ########### 
DFB_EMS_Data <- fread("Data/DFB_EMS_Data.csv", sep = ",", stringsAsFactors = T, na.strings= "")
NYC_EMS_Data <- fread("Data/EMS_Incident_Dispatch_Data.csv", sep = ",", stringsAsFactors = T, na.strings = "") # No longer required to be read
###############################################

########### Reading Cleaned NYC Data ###########
NYC_EMS_Data <- read.csv("Data/NYC_EMS_Data.csv", header = T, sep = ",", stringsAsFactors = T, na.strings = NA)
DFB_EMS_Data <- read.csv("Data/NEW_DFB_EMS_Data.csv", header = T, sep = ",", stringsAsFactors = T, na.strings = NA)
NYC_EMS_MapSample <- fread("Data/NYC_EMS_MapData.csv", header = T, sep = ",", stringsAsFactors = T, na.strings = NA)

############################################ 

########### Data Cleansing DFB ###########
str(DFB_EMS_Data)

DFB_EMS_Data$Date <- as.POSIXct(DFB_EMS_Data$Date, format = "%d/%m/%Y") # Changning data format to readable R date format

DFB_EMS_Data <- DFB_EMS_Data[, -c(2,19,20,21,23,25,27,29,31,33,35,37,39,41)]

# Checking for NA values
sapply(DFB_EMS_Data, function(x) sum(is.na(x))) # Make sure there are no NA values

# Removing Obs with NA values in particular variables
DFB_EMS_Data <- DFB_EMS_Data[complete.cases(DFB_EMS_Data[, c(4,6,10:12,14)]), ] # Removing obs with blanks

DFB_EMS_Data$Date <- as.POSIXct(paste(DFB_EMS_Data$Date,DFB_EMS_Data$TOC))

DFB_EMS_Data <- subset(DFB_EMS_Data, TOC_CD_Mins <= 360 & TOC_IA_Mins <= 360 & AH_MAV_Mins <= 360) # Removing data where call length > 6 hours

# Saving cleaned file for faster readoing
write.csv(DFB_EMS_Data, "Data/NEW_DFB_EMS_Data.csv", row.names = F)

############################################

########### Data Cleansing NYC ###########
str(NYC_EMS_Data)

# Dropping level in borough - set as unknown and there are only 5 boroughs
NYC_EMS_Data$BOROUGH <- droplevels(NYC_EMS_Data$BOROUGH, "UNKNOWN")

# Removing final call type unknown

NYC_EMS_Data <- subset(NYC_EMS_Data, FINAL_CALL_TYPE != 'UNKNOW')

# Changing date format
NYC_EMS_Data$INCIDENT_DATETIME <- as.POSIXct(NYC_EMS_Data$INCIDENT_DATETIME, format = "%m/%d/%Y %I:%M:%S %p")

# NYC2019 <- NYC_EMS_Data[ NYC_EMS_Data$INCIDENT_DATETIME >= as.POSIXct("2019-01-01 00:00:00") & NYC_EMS_Data$INCIDENT_DATETIME <= as.POSIXct("2019-12-31 23:59:59"), ] # Only created to get the number of calls made in 2019 for results in report

# Reducing data to just 2017/2018
NYC_EMS_Data <- NYC_EMS_Data[ NYC_EMS_Data$INCIDENT_DATETIME >= as.POSIXct("2017-01-01 00:00:00") & NYC_EMS_Data$INCIDENT_DATETIME <= as.POSIXct("2018-12-31 23:59:59"), ]

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

#Checking for NA Values
sapply(NYC_EMS_Data, function(x) sum(is.na(x))) # Make sure there are no NA values

NYC_EMS_Data <- NYC_EMS_Data[complete.cases(NYC_EMS_Data[, c(7,10,11,13:17,19,20,22:27)]), ] # Removing obs with blanks

# Removing irrelevant columns
NYC_EMS_Data <- NYC_EMS_Data[, -c(8,12)]

# Saving Cleaned File for quicker reading
write.csv(NYC_EMS_Data, "Data/NYC_EMS_Data.csv", row.names = F)

############################################

########### Functions ###########
# Obtain Latitude & Longitude from Zip Code
NYC_EMS_MapSample <- NYC_EMS_Data

data("zipcode")

for (i in 1:nrow(NYC_EMS_MapSample)){
    if(length(zipcode$zip[NYC_EMS_MapSample$ZIPCODE[i] == zipcode$zip]) == 1){
        NYC_EMS_MapSample$Latitude[i] <- zipcode$latitude[NYC_EMS_MapSample$ZIPCODE[i] == zipcode$zip]
        NYC_EMS_MapSample$Longitude[i] <- zipcode$longitude[NYC_EMS_MapSample$ZIPCODE[i] == zipcode$zip]
        print(paste("Row: ",i, "Zip Code: ",NYC_EMS_MapSample$ZIPCODE[i],NYC_EMS_MapSample$Latitude[i],NYC_EMS_MapSample$Longitude[i], "Status: ", TRUE, Sys.time()))
    }else{
        NYC_EMS_MapSample$Latitude[i] <- NA
        NYC_EMS_MapSample$Longitude[i] <- NA
        print(paste("Row: ",i, "Zip Code: ",NYC_EMS_MapSample$ZIPCODE[i],NYC_EMS_MapSample$Latitude[i],NYC_EMS_MapSample$Longitude[i], "Status: ", FALSE, Sys.time()))
        
    }
}

fwrite(NYC_EMS_MapSample, "Data/NYC_EMS_MapData.csv", row.names = F)# Saving the file
Sys.time()
rm(i)
rm(zipcode)

# Function to find the mode for any data
findMode <- function(x){
    uniqueVals <- unique(x)
    uniqueVals[which.max(tabulate(match(x, uniqueVals)))]
}

############################################

########### Start of Analysis ###########

########### Model 1 - RF to Predict whether the incident will be HELDUP in NYC ###########

#NYC RF Sample
set.seed(546513) # Reproducability
index <- sample(1:nrow(NYC_EMS_Data), 300000, replace = F)
NYC_EMS_RFSample <- NYC_EMS_Data[index, ]

sapply(NYC_EMS_RFSample, function(x) sum(is.na(x))) # Make sure there are no NA values


str(NYC_EMS_RFSample)

NYC_EMS_RFSample <- NYC_EMS_RFSample[, -c(1:5,7:10,13:15,20)] # Removing variables with factors > 53 levels & unrelated variables

# Create Train & Test Data
set.seed(325146) # Reproducability

index <- sample(1:nrow(NYC_EMS_RFSample), 0.75*nrow(NYC_EMS_RFSample), replace = F )
nycTrainRF <- NYC_EMS_RFSample[index, ]
nycTestRF <- NYC_EMS_RFSample[-index, -4]

actualHeldUpRF <- NYC_EMS_RFSample[-index, 4]

rm(index)

nyc_rf_model <- randomForest(HELD_INDICATOR ~., nycTrainRF)

varImpPlot(nyc_rf_model)

nyc_rf_pred <- predict(nyc_rf_model, nycTestRF)

heldupCMRF <- confusionMatrix(nyc_rf_pred, actualHeldUpRF, positive = "Y")

heldupCMRF

rm(nyc_rf_pred)
rm(nycTestRF)
rm(nycTrainRF)
rm(nyc_rf_model)
rm(actualHeldUpRF)

# ROSE (Random Over Sampling Examples)
# Oversampling with Rose
set.seed(69745)
index <- sample(2, nrow(NYC_EMS_RFSample), replace = T, prob = c(0.75,0.25))
roseTrain <- NYC_EMS_RFSample[index == 1, ]
roseTest <- NYC_EMS_RFSample[index == 2, -4 ]

actualHeldUpROSE <- NYC_EMS_RFSample[index == 2, 4]

rm(index)

table(roseTrain$HELD_INDICATOR) # See how many obs there are for each class

heldup <- ovun.sample(HELD_INDICATOR ~., data=roseTrain, method = "over", N = 428594)$data # Oversample the most frequent 

table(heldup$HELD_INDICATOR)

#Creating Model
rfTrainRose <- randomForest(HELD_INDICATOR ~., heldup)

#Evaluate Model with Test Data
rosePred <- predict(rfTrainRose, roseTest)

heldupRoseCM <- confusionMatrix(rosePred, actualHeldUpROSE, positive = "Y")

heldupRoseCM

rm(rosePred)
rm(roseTest)
rm(roseTrain)
rm(rfTrainRose)
rm(heldup)
rm(actualHeldUpROSE)
rm(NYC_EMS_RFSample)

########### ML Model 2 - NB to Predict whether the incident will be held up ###########
# NYC NB Sample
set.seed(215145) # Reproducability
index <- sample(1:nrow(NYC_EMS_Data), 300000, replace = F)
NYC_EMS_NBSample <- NYC_EMS_Data[index, ]

sapply(NYC_EMS_NBSample, function(x) sum(is.na(x))) # Make sure there are no NA values

str(NYC_EMS_NBSample)
NYC_EMS_NBSample <- NYC_EMS_NBSample[, -c(1:5,7:9,10,13:15)]

# Creating Train & Test Set
set.seed(234158) # Reproducability

index <- sample(1:nrow(NYC_EMS_NBSample), 0.75*nrow(NYC_EMS_NBSample), replace = F )
nycTrainNB <- NYC_EMS_NBSample[index, ]
nycTestNB <- NYC_EMS_NBSample[-index, -4]

actualHeldUpNB <- NYC_EMS_NBSample[-index, 4]

rm(index)

nb_model <- naiveBayes(HELD_INDICATOR ~., nycTrainNB)

nb_pred <- predict(nb_model, nycTestNB)

heldupNBCM <- confusionMatrix(nb_pred, actualHeldUpNB, positive = "Y")

heldupNBCM

rm(nb_pred)
rm(actualHeldUpNB)
rm(nycTestNB)
rm(nycTrainNB)
rm(NYC_EMS_NBSample)
rm(nb_model)

##################################################################################

########### ML Model 3 - SVM to Predict whether the incident will be held up ###########
NYC_EMS_SVMSample <- NYC_EMS_Data

sapply(NYC_EMS_SVMSample, function(x) sum(is.na(x))) # Make sure there are no NA values
# NYC SVM Sample
set.seed(65451) # Reproducability
index <- sample(1:nrow(NYC_EMS_Data), 20000, replace = F)
NYC_EMS_SVMSample <- NYC_EMS_Data[index, ]

rm(index)

str(NYC_EMS_SVMSample)
NYC_EMS_SVMSample_Numbers <- NYC_EMS_SVMSample[, c(6,8,11,12,16,17,20:25)] # Keeping Just Numeric Columns for SVM

rm(NYC_EMS_SVMSample)

str(NYC_EMS_SVMSample_Numbers)
sapply(NYC_EMS_SVMSample_Numbers, function(x) sum(is.na(x)))

set.seed(65451)
index <- createDataPartition(
    NYC_EMS_SVMSample_Numbers$HELD_INDICATOR,
    p = .75,
    list = F
)
svm_train <- NYC_EMS_SVMSample_Numbers[index, ]
svm_test <- NYC_EMS_SVMSample_Numbers[-index, ]

rm(index)

# Linear Based SVM with Tuning Grid #
Cost <- 2^c(1:8)
Cost

set.seed(65451)
svm_gt_control <- trainControl(
    method = "cv",
    number = 10,
    summaryFunction = defaultSummary
)
set.seed(65451)
svm_linear_grid <- expand.grid(
    C = Cost
)
set.seed(65451)
svm_lgt_model1 <- train(
    HELD_INDICATOR ~.,
    data = svm_train,
    method = "svmLinear",
    trControl = svm_gt_control,
    preProc = c("center", "scale", "nzv"),
    verbose = F,
    tuneGrid = svm_linear_grid
)
Sys.time()

svm_lgt_model1

Cost = 2^seq(0,2,0.1)
Cost

set.seed(65451)
svm_linear_grid2 <- expand.grid(
    C = Cost
)
set.seed(65451)
svm_lgt_model2 <- train(
    HELD_INDICATOR ~.,
    data = svm_train,
    method = "svmLinear",
    trControl = svm_gt_control,
    preProc = c("center", "scale", "nzv"),
    verbose = F,
    tuneGrid = svm_linear_grid2
)
svm_lgt_pred

set.seed(65451)
svm_lgt_pred <- predict(
    svm_lgt_model2,
    svm_test[, -5]
)

svm_lgt_pred <- confusionMatrix(
    data = svm_pred,
    reference = svm_test[, 5],
    positive = "Y"
)

svm_cm

# Radial Based SVM with Tuning Grid #
set.seed(65451)
svm_rbf_grid <- expand.grid(
    C = 2^seq(3,5,0.1),
    sigma = 2^c(-25,-20,-15,-1,-5,0)
)
set.seed(65451)
sigma_svm_model <- train(
    HELD_INDICATOR ~.,
    data = svm_train,
    method = "svmRadial",
    trControl = svm_gt_control,
    preProc = c("center", "scale", "nzv"),
    verbose = F,
    tuneGrid = svm_rbf_grid
)

sigma_svm_model$bestTune

set.seed(65451)
svm_rbf_pred <- predict(
    sigma_svm_model,
    svm_test[, -5]
)

svm_rbf_cm <- confusionMatrix(
    svm_rbf_pred,
    svm_test[, 5],
    positive = "Y"
)

svm_rbf_cm

# Radial Based SVM with Random Tuned #
set.seed(65451)
svm_rndm_control <- trainControl(
    method = "cv",
    number = 10,
    summaryFunction = defaultSummary,
    search = "random"
)

set.seed(65451)
svm_rndm_model <- train(
    HELD_INDICATOR ~.,
    data = svm_train,
    method = "svmRadial",
    trControl = svm_rndm_control,
    preProc = c("center", "scale", "nzv"),
    verbose = F,
    tuneLenght = 60
)

svm_rndm_model$bestTune

set.seed(65451)
svm_rndm_pred <- predict(
    svm_rndm_model,
    svm_test[, -5]
)

svm_rndm_cm <- confusionMatrix(
    svm_rndm_pred,
    svm_test[, 5],
    positive = "Y"
)

svm_rndm_cm


rm(svm_rndm_control)
rm(svm_rndm_pred)
rm(svm_rndm_model)
rm(svm_rbf_grid)
rm(svm_rbf_pred)
rm(sigma_svm_model)
rm(svm.control)
rm(svm_train)
rm(svm_test)
rm(svm_linear_grid)
rm(svm_linear_grid2)
rm(svm_model1)
rm(svm_model2)
rm(svm_pred)
rm(Cost)
rm(NYC_EMS_SVMSample_Numbers)

##################################################################################

# Mapping
NYC_EMS_MapData.1 <- fread("Data/NYC_EMS_MapData.csv", header = T, sep = ",", stringsAsFactors = T, na.strings = "")

NYC_EMS_MapData_Sample <- sample(1:nrow(NYC_EMS_MapData), 10000, replace = F)
NYC_EMS_MapData_Sample <- NYC_EMS_MapData[NYC_EMS_MapData_Sample, ]

# FDNY EMS Calls in Cluster Map
leaflet() %>%
    addTiles() %>%
    addCircleMarkers( lat = NYC_EMS_MapData_Sample$Latitude, 
                      lng = NYC_EMS_MapData_Sample$Longitude,
                      popup = NYC_EMS_MapData_Sample$FINAL_CALL_TYPE,
                      clusterOptions = markerClusterOptions())

# FDNY EMS Calls Map
leaflet() %>%
    addTiles() %>%
    addMarkers( lat = NYC_EMS_MapData_Sample$Latitude, 
                      lng = NYC_EMS_MapData_Sample$Longitude,
                      popup = NYC_EMS_MapData_Sample$FINAL_CALL_TYPE)

leaflet() %>%
    addTiles() %>%
    addHeatmap( lat = NYC_EMS_MapData$Latitude, 
                lng = NYC_EMS_MapData$Longitude,
                blur = 25,
                radius = 15)

rm(NYC_EMS_MapSample)
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

############################################

# Top 5 Final Call Types by Borough FDNY EMS

NYC_TOP_5_CALL <- NYC_EMS_Data %>%
    group_by(BOROUGH, FINAL_CALL_TYPE) %>%
    summarise(count = n()) %>%
    top_n(n = 5, wt = count)

ggplot(NYC_TOP_5_CALL, aes(x = FINAL_CALL_TYPE, y = count)) +
    geom_col() +
    ggtitle(label = "Top 5 Call Categories",
            subtitle = "by Borough") +
    facet_grid(~BOROUGH, scales = "free")+
    theme_economist_white()


# NYC Response Times
NYC_Response_Times <- data.frame(NYC_EMS_Data$INCIDENT_DATETIME,NYC_EMS_Data$FINAL_SEVERITY_LEVEL_CODE, NYC_EMS_Data$INCIDENT_RESPONSE_SECONDS_QY, NYC_EMS_Data$INCIDENT_TRAVEL_TM_SECONDS_QY)

NYC_Response_Times$NYC_EMS_Data.INCIDENT_RESPONSE_SECONDS_QY <- round(NYC_Response_Times$NYC_EMS_Data.INCIDENT_RESPONSE_SECONDS_QY / 60, 2)

NYC_Response_Times$NYC_EMS_Data.INCIDENT_TRAVEL_TM_SECONDS_QY <- round(NYC_Response_Times$NYC_EMS_Data.INCIDENT_TRAVEL_TM_SECONDS_QY / 60, 2)

summary(NYC_Response_Times)

table(NYC_Response_Times$NYC_EMS_Data.FINAL_SEVERITY_LEVEL_CODE)
mean(NYC_Response_Times$NYC_EMS_Data.INCIDENT_RESPONSE_SECONDS_QY)

NYC_Response_Times <- subset(NYC_Response_Times, NYC_EMS_Data.INCIDENT_RESPONSE_SECONDS_QY <= 360)

write.csv(NYC_Response_Times, "Data/NYC_Response_Times.csv", row.names = F)

########### Analysis Results ###########
# Confusion Matrices
# Random Forest
heldupCMRF

# Random Forest w/ Rose
heldupRoseCM

# Naive Bayes
heldupNBCM

# SVM Linear Grid Tuned
svm_cm

# SVM Radial Grid Tuned
svm_rbf_cm

# SVM Radial Random Tuned
svm_rndm_cm
