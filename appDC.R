source("global.R") 
# Define UI
ui <- fluidPage(
  titlePanel("Cold Chain Capacity Data Collection"),
  
  fluidRow(
    column(12, 
           wellPanel(
             # Primary selection fields
             selectInput("county", "Select County *", 
                         choices = c("", sort(unique(locations$County))), 
                         selected = "", width = "100%"),
             
             selectInput("subcounty", "Select SubCounty *", 
                         choices = c(""), selected = "", width = "100%"),
             
             selectInput("facility", "Select Facility *", 
                         choices = c("", "Other"), selected = "", width = "100%"),
             conditionalPanel(
               condition = "input.facility == 'Other'",
               textInput("facility_other", "Enter Facility Name *", width = "100%"),
               textInput("facility_lat", "Enter Facility Latitude *", width = "100%"),
               textInput("facility_long", "Enter Facility Longitude *", width = "100%")
             ),
             tags$hr(),
             
             # Equipment Entries Container
             div(
               id = "equipment_container",
               
               # First Equipment Entry (always shown)
               div(
                 id = "equipment_entry_1",
                 tags$hr(),
                 selectInput("equip_make_1", "Equipment Make *", 
                             choices = c("", equipment_makes, "Other"), 
                             selected = "", width = "100%"),
                 conditionalPanel(
                   condition = "input.equip_make_1 == 'Other'",
                   textInput("equip_make_other_1", "Enter Equipment Make *", width = "100%")
                 ),
                 
                 selectInput("equip_model_1", "Equipment Model *", 
                             choices = c("", equipment_models, "Other"), 
                             selected = "", width = "100%"),
                 conditionalPanel(
                   condition = "input.equip_model_1 == 'Other'",
                   textInput("equip_model_other_1", "Enter Equipment Model *", width = "100%")
                 ),
                 
                 textInput("serial_no_1", "Serial Number *", width = "100%"),
                 
                 # Use numericInput for year-only selection:
                 numericInput("year_manufacture_1", "Year of Manufacture *", 
                              value = as.numeric(format(Sys.Date(), "%Y")), min = 1900, max = 2100, width = "100%"),
                 
                 numericInput("year_installation_1", "Year of Installation *", 
                              value = as.numeric(format(Sys.Date(), "%Y")), min = 1900, max = 2100, width = "100%"),
                 
                 radioButtons("functional_status_1", "Functional Status *", 
                              choices = c("Yes", "No"), selected = "Yes", inline = TRUE),
                 
                 radioButtons("repair_needed_1", "Repair Needed *", 
                              choices = c("Yes", "No"), selected = "No", inline = TRUE),
                 
                 conditionalPanel(
                   condition = "input.repair_needed_1 == 'Yes'",
                   selectInput("repair_type_1", "What Repairs Are Needed?", 
                               choices = c("", repair_options, "Other"), 
                               selected = "", width = "100%"),
                   conditionalPanel(
                     condition = "input.repair_type_1 == 'Other'",
                     textInput("repair_type_other_1", "Enter Repair Type:", width = "100%")
                   ),
                   selectInput("spare_parts_1", "What Spare Parts Are Needed?", 
                               choices = c("", spare_parts_options, "Other"), 
                               selected = "", width = "100%"),
                   conditionalPanel(
                     condition = "input.spare_parts_1 == 'Other'",
                     textInput("spare_parts_other_1", "Enter Spare Parts:", width = "100%")
                   )
                 )
               )  # end of first equipment entry
             ),  # end of equipment_container
             
             # Buttons for adding and removing equipment entries
             actionButton("add_equipment", "Add Another Equipment", class = "btn-secondary"),
             actionButton("remove_equipment", "Remove Equipment", class = "btn-danger"),
             tags$hr(),
             actionButton("submit", "Submit Data", class = "btn-primary btn-lg btn-block")
           ),
           textOutput("success_message"),
           tags$style("#success_message {text-align: center; font-size: 20px; font-weight: bold; color: green; margin-top: 20px;}")
    )
  )
)

# Define server
server <- function(input, output, session) {
  
  # Verify access to Google Sheet
  test_access <- tryCatch({
    read_sheet(sheet_id)
    TRUE
  }, error = function(e) {
    FALSE
  })
  
  if (!test_access) {
    stop("Error: Cannot access Google Sheet. Check permissions.")
  }
  
  # Update dependent dropdowns for County, SubCounty, and Facility
  observeEvent(input$county, {
    subcounty_choices <- locations |>
      filter(County == input$county) |>
      pull(Sub.county) |>
      unique() |>
      sort()
    updateSelectInput(session, "subcounty", choices = c("", subcounty_choices))
  })
  
  observeEvent(input$subcounty, {
    facility_choices <- locations |>
      filter(County == input$county, Sub.county == input$subcounty) |>
      pull(Facility_Name) |>
      unique() |>
      sort()
    facility_choices <- c("", facility_choices, "Other")
    updateSelectInput(session, "facility", choices = facility_choices)
  })
  
  observeEvent(input$facility, {
    if (input$facility == "Other") {
      updateTextInput(session, "facility_other", value = "")
    }
  })
  
  # Track the number of equipment entries
  equipment_count <- reactiveVal(1)
  
  # Add new equipment entry dynamically
  observeEvent(input$add_equipment, {
    new_count <- equipment_count() + 1
    equipment_count(new_count)
    
    insertUI(
      selector = "#equipment_container",
      where = "beforeEnd",
      ui = div(
        id = paste0("equipment_entry_", new_count),
        tags$hr(),
        selectInput(paste0("equip_make_", new_count), "Equipment Make *", 
                    choices = c("", equipment_makes, "Other"), selected = "", width = "100%"),
        conditionalPanel(
          condition = paste0("input.equip_make_", new_count, " == 'Other'"),
          textInput(paste0("equip_make_other_", new_count), "Enter Equipment Make *", width = "100%")
        ),
        selectInput(paste0("equip_model_", new_count), "Equipment Model *", 
                    choices = c("", equipment_models, "Other"), selected = "", width = "100%"),
        conditionalPanel(
          condition = paste0("input.equip_model_", new_count, " == 'Other'"),
          textInput(paste0("equip_model_other_", new_count), "Enter Equipment Model *", width = "100%")
        ),
        textInput(paste0("serial_no_", new_count), "Serial Number *", width = "100%"),
        numericInput(paste0("year_manufacture_", new_count), "Year of Manufacture *", 
                     value = as.numeric(format(Sys.Date(), "%Y")), min = 1900, max = 2100, width = "100%"),
        numericInput(paste0("year_installation_", new_count), "Year of Installation *", 
                     value = as.numeric(format(Sys.Date(), "%Y")), min = 1900, max = 2100, width = "100%"),
        radioButtons(paste0("functional_status_", new_count), "Functional Status *", 
                     choices = c("Yes", "No"), selected = "Yes", inline = TRUE),
        radioButtons(paste0("repair_needed_", new_count), "Repair Needed *", 
                     choices = c("Yes", "No"), selected = "No", inline = TRUE),
        conditionalPanel(
          condition = paste0("input.repair_needed_", new_count, " == 'Yes'"),
          selectInput(paste0("repair_type_", new_count), "What Repairs Are Needed?", 
                      choices = c("", repair_options, "Other"), 
                      selected = "", width = "100%"),
          conditionalPanel(
            condition = paste0("input.repair_type_", new_count, " == 'Other'"),
            textInput(paste0("repair_type_other_", new_count), "Enter Repair Type:", width = "100%")
          ),
          selectInput(paste0("spare_parts_", new_count), "What Spare Parts Are Needed?", 
                      choices = c("", spare_parts_options, "Other"), 
                      selected = "", width = "100%"),
          conditionalPanel(
            condition = paste0("input.spare_parts_", new_count, " == 'Other'"),
            textInput(paste0("spare_parts_other_", new_count), "Enter Spare Parts:", width = "100%")
          )
        )
      )
    )
  })
  
  # Remove the last equipment entry (only if more than one exists)
  observeEvent(input$remove_equipment, {
    if (equipment_count() > 1) {
      removeUI(selector = paste0("#equipment_entry_", equipment_count()))
      equipment_count(equipment_count() - 1)
    } else {
      showNotification("At least one equipment entry is required.", type = "error")
    }
  })
  
  # Handle form submission with validation for all equipment entries
  observeEvent(input$submit, {
    # Validate primary fields (County, SubCounty, Facility)
    if (input$county == "" || input$subcounty == "" || input$facility == "" ||
        (input$facility == "Other" && (input$facility_other == "" || input$facility_lat == "" || input$facility_long == ""))) {
      showNotification("Please fill all mandatory fields for County/SubCounty/Facility and coordinates.", type = "error")
      return()
    }
    
    # Loop over each equipment entry to validate mandatory fields
    for(i in 1:equipment_count()) {
      if (is.null(input[[paste0("equip_make_", i)]]) || input[[paste0("equip_make_", i)]] == "" ||
          (input[[paste0("equip_make_", i)]] == "Other" && 
           (is.null(input[[paste0("equip_make_other_", i)]]) || input[[paste0("equip_make_other_", i)]] == ""))) {
        showNotification(paste("Please fill all mandatory fields for Equipment Make in entry", i), type = "error")
        return()
      }
      
      if (is.null(input[[paste0("equip_model_", i)]]) || input[[paste0("equip_model_", i)]] == "" ||
          (input[[paste0("equip_model_", i)]] == "Other" && 
           (is.null(input[[paste0("equip_model_other_", i)]]) || input[[paste0("equip_model_other_", i)]] == ""))) {
        showNotification(paste("Please fill all mandatory fields for Equipment Model in entry", i), type = "error")
        return()
      }
      
      if (is.null(input[[paste0("serial_no_", i)]]) || input[[paste0("serial_no_", i)]] == "") {
        showNotification(paste("Please enter a Serial Number for entry", i), type = "error")
        return()
      }
      
      if (is.null(input[[paste0("year_manufacture_", i)]]) ||
          is.na(input[[paste0("year_manufacture_", i)]])) {
        showNotification(paste("Please select a Year of Manufacture for entry", i), type = "error")
        return()
      }
      
      if (is.null(input[[paste0("year_installation_", i)]]) ||
          is.na(input[[paste0("year_installation_", i)]])) {
        showNotification(paste("Please select a Year of Installation for entry", i), type = "error")
        return()
      }
      # Radio buttons always have a value.
    }
    
    # --- DETERMINE FACILITY COORDINATES ---
    # If a facility is selected (not "Other"), look up coordinates from the preloaded 'locations' data.
    # Otherwise, use the manually entered coordinates.
    if (input$facility != "Other") {
      coords <- locations |>
        filter(Facility_Name == input$facility) |>
        slice(1)
      if (nrow(coords) == 0) {
        facility_lat <- NA
        facility_lon <- NA
      } else {
        facility_lat <- coords$latitude  # using lowercase as in the preloaded data
        facility_lon <- coords$longitude # using lowercase as in the preloaded data
      }
    } else {
      facility_lat <- input$facility_lat
      facility_lon <- input$facility_long
    }
    
    # Helper function to safely retrieve inputs (substitute "" if NULL)
    safe_input <- function(x) {
      if (is.null(x)) "" else x
    }
    
    # Collect the data for each equipment entry.
    records <- lapply(1:equipment_count(), function(i) {
      # For text inputs, use safe_input to ensure length 1.
      equip_make <- if (is.null(input[[paste0("equip_make_", i)]])) "" else input[[paste0("equip_make_", i)]]
      if (equip_make == "Other") {
        equip_make <- if (is.null(input[[paste0("equip_make_other_", i)]])) "" else input[[paste0("equip_make_other_", i)]]
      }
      
      equip_model <- if (is.null(input[[paste0("equip_model_", i)]])) "" else input[[paste0("equip_model_", i)]]
      if (equip_model == "Other") {
        equip_model <- if (is.null(input[[paste0("equip_model_other_", i)]])) "" else input[[paste0("equip_model_other_", i)]]
      }
      
      serial_no <- safe_input(input[[paste0("serial_no_", i)]])
      
      # For numeric inputs, we assume they return a number.
      year_manufacture <- input[[paste0("year_manufacture_", i)]]
      year_installation <- input[[paste0("year_installation_", i)]]
      
      func_status <- safe_input(input[[paste0("functional_status_", i)]])
      repair_needed <- safe_input(input[[paste0("repair_needed_", i)]])
      
      repair_type <- NA
      if (repair_needed == "Yes") {
        repair_type <- safe_input(input[[paste0("repair_type_", i)]])
        if (repair_type == "Other") {
          repair_type <- safe_input(input[[paste0("repair_type_other_", i)]])
        }
      }
      
      spare_parts <- NA
      if (repair_needed == "Yes") {
        spare_parts <- safe_input(input[[paste0("spare_parts_", i)]])
        if (spare_parts == "Other") {
          spare_parts <- safe_input(input[[paste0("spare_parts_other_", i)]])
        }
      }
      
      data.frame(
        County = safe_input(input$county),
        SubCounty = safe_input(input$subcounty),
        Facility = if (input$facility == "Other") safe_input(input$facility_other) else safe_input(input$facility),
        Latitude = facility_lat,   # Column name changed to Latitude
        Longitude = facility_lon,  # Column name changed to Longitude
        EquipmentMake = equip_make,
        EquipmentModel = equip_model,
        SerialNo = serial_no,
        YearManufacture = year_manufacture,
        YearInstallation = year_installation,
        FunctionalStatus = func_status,
        RepairNeeded = repair_needed,
        RepairType = repair_type,
        SpareParts = spare_parts,
        stringsAsFactors = FALSE
      )
    })
    
    all_records <- do.call(rbind, records)
    
    # Write data to the Google Sheet.
    sheet_append(sheet_id, all_records)
    
    showNotification("Data submitted successfully!", type = "message")
    
    # --- CLEAR THE FORM AFTER SUBMISSION ---
    # Remove additional equipment entries (if any) so that only the first entry remains.
    if (equipment_count() > 1) {
      for (i in seq(2, equipment_count())) {
        removeUI(selector = paste0("#equipment_entry_", i))
      }
      equipment_count(1)
    }
    
    # Reset primary inputs
    updateSelectInput(session, "county", selected = "")
    updateSelectInput(session, "subcounty", selected = "")
    updateSelectInput(session, "facility", selected = "")
    updateTextInput(session, "facility_other", value = "")
    updateTextInput(session, "facility_lat", value = "")
    updateTextInput(session, "facility_long", value = "")
    
    # Reset first equipment entry inputs
    updateSelectInput(session, "equip_make_1", selected = "")
    updateTextInput(session, "equip_make_other_1", value = "")
    updateSelectInput(session, "equip_model_1", selected = "")
    updateTextInput(session, "equip_model_other_1", value = "")
    updateTextInput(session, "serial_no_1", value = "")
    updateNumericInput(session, "year_manufacture_1", value = as.numeric(format(Sys.Date(), "%Y")))
    updateNumericInput(session, "year_installation_1", value = as.numeric(format(Sys.Date(), "%Y")))
    updateRadioButtons(session, "functional_status_1", selected = "Yes")
    updateRadioButtons(session, "repair_needed_1", selected = "No")
  })
}

shinyApp(ui, server)

# Optionally, confirm the working directory:
#getwd()
#setwd("C:\\Users\\USER\\Desktop\\COLD_CHAIN\\Data Collection App")
