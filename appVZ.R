#setwd("C:/Users/USER/Desktop/COLD_CHAIN/Data Dashboard")
source("global.R")

## --- USER INTERFACE ---------------------------------------------------------
ui <- navbarPage("Cold Chain Capacity Monitoring App",
                 
                 # Include an external CSS file for additional styling (adjust the path as needed)
                 header = includeCSS("www/styles2.css"),
                 
                 ### TAB 1: MAP (Map fills the entire tab)
                 tabPanel("Map",
                          fluidPage(
                            leafletOutput("map", width = "100%", height = "100vh")
                          )
                 ),
                 
                 ### TAB 2: VISUALIZATION & REPORTS
                 tabPanel("Visualization & Reports",
                          fluidPage(
                            fluidRow(
                              column(4,
                                     h4("Select a variable to update the plot"),
                                     # Radio buttons for variable selection
                                     radioButtons("selectedVar", "Available Variables:",
                                                  choices = c("available_functional_capacity", 
                                                              "required_functional_capacity", 
                                                              "unutilized_capacity"),
                                                  selected = "unutilized_capacity"),
                                     hr(),
                                     h4("Filter Facilities"),
                                     selectInput("filterCounty", "Select County", 
                                                 choices = c("All", sort(unique(facilities$county))), 
                                                 selected = "All"),
                                     selectInput("filterSubcounty", "Select Subcounty", 
                                                 choices = c("All", sort(unique(facilities$subcounty))), 
                                                 selected = "All"),
                                     hr(),
                                     selectInput("plotType", "Select Plot Type",
                                                 choices = c("Bar Plot" = "bar", 
                                                             "Scatter Plot" = "scatter", 
                                                             "Boxplot" = "box")),
                                     actionButton("reportBtn", "Generate Report")
                              ),
                              column(8,
                                     plotOutput("plotOutput"),
                                     br(),
                                     DTOutput("dataTable")
                              )
                            )
                          )
                 ),
                 
                 ### TAB 3: DATA UPLOAD & CALIBRATION
                 tabPanel("Data Upload & Calibration",
                          sidebarLayout(
                            sidebarPanel(
                              fileInput("uploadExcel", "Upload Excel File with 3 Sheets",
                                        accept = c(".xlsx", ".xls")),
                              actionButton("uploadBtn", "Process Uploaded Data")
                            ),
                            mainPanel(
                              verbatimTextOutput("uploadStatus")
                            )
                          )
                 )
)

## --- SERVER LOGIC -----------------------------------------------------------
server <- function(input, output, session) {
  
  ## Simulate pulling data from a database (dummy facility data)
  facilityData <- facilities
  
  ## Reactive values for parameters (dummy parameters for capacity calculations)
  params <- reactiveValues(
    population = 100,
    dose = 1,
    vial_volume = 1
  )
  
  ## Filter facility data based on County/Subcounty selections, then compute capacities.
  computedData <- reactive({
    data <- facilityData
    
    # Filter by County if a specific county is selected.
    if (input$filterCounty != "All") {
      data <- data %>% filter(county == input$filterCounty)
    }
    # Filter by Subcounty if a specific subcounty is selected.
    if (input$filterSubcounty != "All") {
      data <- data %>% filter(subcounty == input$filterSubcounty)
    }
    
    # Compute required and unutilized capacity.
    req_capacity <- params$population * (params$dose * params$vial_volume)
    data %>%
      mutate(required_functional_capacity = req_capacity,
             unutilized_capacity = available_functional_capacity - req_capacity)
  })
  
  ## --- MAP TAB: Render a full-page Leaflet map with a search bar -----------
  output$map <- renderLeaflet({
    data <- computedData()
    markerColors <- ifelse(data$unutilized_capacity >= 0, "blue", "red")
    
    leaflet(data) %>%
      addTiles(group = "Base Map") %>%
      addCircleMarkers(
        ~longitude, ~latitude,
        popup = ~paste0("<strong>", facility_name, "</strong><br>",
                        "Available: ", available_functional_capacity, "<br>",
                        "Required: ", required_functional_capacity, "<br>",
                        "Unutilized: ", unutilized_capacity),
        radius = 8,
        color = markerColors,
        fillOpacity = 0.8,
        group = "Facilities"
      ) %>%
      addLayersControl(
        baseGroups = c("Base Map"),
        overlayGroups = c("Facilities"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      addSearchFeatures(
        targetGroups = "Facilities",
        options = searchFeaturesOptions(zoom = 12, openPopup = TRUE, hideMarkerOnCollapse = TRUE)
      )
  })
  
  ## Update the map markers when the filtered data changes.
  observe({
    leafletProxy("map", data = computedData()) %>%
      clearMarkers() %>%
      addCircleMarkers(
        lng = ~longitude, lat = ~latitude,
        popup = ~paste0("<strong>", facility_name, "</strong><br>",
                        "Available: ", available_functional_capacity, "<br>",
                        "Required: ", required_functional_capacity, "<br>",
                        "Unutilized: ", unutilized_capacity),
        radius = 8,
        color = ifelse(computedData()$unutilized_capacity >= 0, "blue", "red"),
        fillOpacity = 0.8,
        group = "Facilities"
      )
  })
  
  ## --- VISUALIZATION & REPORTS TAB: Plot and Data Table -------------------
  # Use the selected variable from the radio buttons.
  selectedVar <- reactive({
    input$selectedVar
  })
  
  output$plotOutput <- renderPlot({
    data <- computedData()
    varName <- selectedVar()
    
    if (input$plotType == "bar") {
      ggplot(data, aes_string(x = "facility_name", y = varName, fill = "facility_name")) +
        geom_bar(stat = "identity") +
        labs(title = paste("Bar Plot of", varName),
             x = "Facility", y = varName) +
        theme_minimal()
    } else if (input$plotType == "scatter") {
      ggplot(data, aes_string(x = "available_functional_capacity", y = varName, color = "facility_name")) +
        geom_point(size = 3) +
        labs(title = paste("Scatter Plot of", varName, "vs Available Capacity"),
             x = "Available Functional Capacity", y = varName) +
        theme_minimal()
    } else if (input$plotType == "box") {
      ggplot(data, aes_string(x = "facility_name", y = varName, fill = "facility_name")) +
        geom_boxplot() +
        labs(title = paste("Boxplot of", varName, "by Facility"),
             x = "Facility", y = varName) +
        theme_minimal()
    }
  })
  
  output$dataTable <- renderDT({
    datatable(computedData(), filter = "top")
  })
  
  ## Generate Report: (Modal dialog remains from the previous version.)
  observeEvent(input$reportBtn, {
    showModal(modalDialog(
      title = "Report Parameters",
      textInput("reportTitle", "Report Title", value = "My Report"),
      selectInput("report_scope", "Select Report Scope",
                  choices = c("All Facilities", "Per County", "Per Subcounty", "Individual Facility"),
                  selected = "All Facilities"),
      conditionalPanel(
        condition = "input.report_scope == 'Per County'",
        selectInput("report_county", "Select County", choices = c("All", sort(unique(facilityData$county))))
      ),
      conditionalPanel(
        condition = "input.report_scope == 'Per Subcounty'",
        selectInput("report_subcounty", "Select Subcounty", choices = c("All", sort(unique(facilityData$subcounty))))
      ),
      conditionalPanel(
        condition = "input.report_scope == 'Individual Facility'",
        selectInput("report_facility", "Select Facility", choices = unique(facilityData$facility_name))
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirmReport", "Generate")
      )
    ))
  })
  
  observeEvent(input$confirmReport, {
    removeModal()
    reportScope <- input$report_scope
    details <- switch(reportScope,
                      "All Facilities" = "All Facilities Selected",
                      "Per County" = paste("County:", input$report_county),
                      "Per Subcounty" = paste("Subcounty:", input$report_subcounty),
                      "Individual Facility" = paste("Facility:", input$report_facility))
    showNotification(paste("Report generated with title:", input$reportTitle, "-", details),
                     type = "message")
  })
  
  ## --- DATA UPLOAD & CALIBRATION TAB -------------------------------------
  observeEvent(input$uploadBtn, {
    req(input$uploadExcel)
    
    # Read the three sheets from the uploaded Excel file.
    targetPop <- read_excel(input$uploadExcel$datapath, sheet = "Target population for each facility")
    vialQty   <- read_excel(input$uploadExcel$datapath, sheet = "Number of vial per vaccine per facility")
    vialVolume <- read_excel(input$uploadExcel$datapath, sheet = "Vol per vial per vaccine")
    
    uploadMessage <- paste("Uploaded Excel file:",
                           "\nTarget Population sheet has", nrow(targetPop), "rows.",
                           "\nVial Quantity sheet has", nrow(vialQty), "rows.",
                           "\nVial Volume sheet has", nrow(vialVolume), "rows.")
    
    output$uploadStatus <- renderText({ uploadMessage })
  })
}

## --- RUN THE APP ------------------------------------------------------------
shinyApp(ui, server)
