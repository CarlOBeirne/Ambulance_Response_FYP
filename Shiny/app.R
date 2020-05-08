library(rsconnect)
library(shiny)
library(rlang)
library(shinydashboard)
library(leaflet)
library(data.table)
library(DT)

NYC_EMS_MapData <- fread("https://raw.githubusercontent.com/CarlOBeirne/Ambulance_Response_FYP/master/NYC_EMS_MapData.csv", header = T, sep = ",")

NYC_EMS_MapData$PopUp <- as.character(paste("Call ID: ", NYC_EMS_MapData$CAD_INCIDENT_ID, " | Call Reason: ", NYC_EMS_MapData$FINAL_CALL_TYPE, " | Borough: ", NYC_EMS_MapData$BOROUGH))

NYC_EMS_Disp_Code <- fread("https://raw.githubusercontent.com/CarlOBeirne/Ambulance_Response_FYP/master/NYC_EMS_Disposition_Desc.csv", header = T, sep = ",")
NYC_EMS_Call_Desc <- fread("https://raw.githubusercontent.com/CarlOBeirne/Ambulance_Response_FYP/master/NYC_EMS_Call_Desc.csv", header = T, sep = ",")

NYC_EMS_MapData_Sample <- sample(1:nrow(NYC_EMS_MapData), 2000, replace = F)
NYC_EMS_MapData_Sample <- NYC_EMS_MapData[NYC_EMS_MapData_Sample, ]

NYC_EMS_MapData_Table <- NYC_EMS_MapData_Sample[, c(1,2,5,6,17,18,20,30,31)]
colnames(NYC_EMS_MapData_Table)[1] <- "Incident_ID"
colnames(NYC_EMS_MapData_Table)[2] <- "Incident_Date"
colnames(NYC_EMS_MapData_Table)[3] <- "Incident_Reason"
colnames(NYC_EMS_MapData_Table)[4] <- "Incident_Severity"
colnames(NYC_EMS_MapData_Table)[5] <- "Disposition_Code"
colnames(NYC_EMS_MapData_Table)[6] <- "Borough"
colnames(NYC_EMS_MapData_Table)[7] <- "Zipcode"
colnames(NYC_EMS_MapData_Table)[8] <- "Latitude"
colnames(NYC_EMS_MapData_Table)[9] <- "Longitude"


# Define UI for application
ui <- dashboardPage(
    skin = "red",
    # Application title
    dashboardHeader(title = "EMS Calls NYC"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Interactive Map", tabName = "NYCMap"),
            menuItem("Data Descriptions", tabName = "DataDescs")
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(
                tabName = "NYCMap",
                fluidRow(
                    box(
                        width = 12,
                        leafletOutput(
                            outputId = "nycMap"
                        )
                    )
                    
                ),
                fluidRow(
                    box(
                        width = 12,
                        dataTableOutput(
                            outputId = "SummaryTable"
                        )
                    )
                )
            ),
            tabItem(
                tabName = "DataDescs",
                fluidRow(
                    box(
                        titlePanel(
                            h2("Disposition Code Descriptions", align = "center")
                        ),
                        width = 6,
                        dataTableOutput(
                            outputId = "CallReasonDesc"
                        )
                    ),
                    box(
                        titlePanel(
                            h2("Call Type Descriptions", align = "center")
                        ),
                        width = 6,
                        dataTableOutput(
                            outputId = "CallDispDesc"
                            
                        )
                    )
                )
            )
        )
    )
)

# Server function to produce leaflet map and tables using DT
server <- function(input, output) {
    output$nycMap <- renderLeaflet(
        leaflet() %>%
            addProviderTiles(providers$OpenStreetMap) %>%
            addMarkers( lat = NYC_EMS_MapData_Sample$Latitude, 
                        lng = NYC_EMS_MapData_Sample$Longitude,
                        popup = NYC_EMS_MapData_Sample$PopUp,
            )
    )
    
    output$SummaryTable <- renderDataTable(
        NYC_EMS_MapData_Table
    )
    
    output$CallReasonDesc <- renderDataTable(
        NYC_EMS_Disp_Code
    )
    
    output$CallDispDesc <- renderDataTable(
        NYC_EMS_Call_Desc
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
