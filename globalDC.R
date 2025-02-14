pacman::p_load(shiny, dplyr, googlesheets4, googledrive)

# Authenticate using the service account
gs4_auth(path = "D:\\COLD_CHAIN\\Data Collection App\\cold-chain-450001-b619f39f74cf.json",
         scopes = c("https://www.googleapis.com/auth/spreadsheets",
                    "https://www.googleapis.com/auth/drive"))

# Define Google Sheet ID
sheet_id <- "1ocWdlAOscZ17mYZjPvjJ_47CnRJ8zfKBI8bI9BJ-wAQ"

# Load and prepare data
locations <- read.csv("D:\\COLD_CHAIN\\Data Collection App\\Turkana_Central_With_Capacity_11.csv", stringsAsFactors = FALSE)
equipment_makes <- read.csv("D:\\COLD_CHAIN\\Data Collection App\\makes.csv", stringsAsFactors = FALSE)[[1]]
equipment_models <- read.csv("D:\\COLD_CHAIN\\Data Collection App\\models.csv", stringsAsFactors = FALSE)[[1]]
repair_options <- read.csv("D:\\COLD_CHAIN\\Data Collection App\\repairoptions.csv", stringsAsFactors = FALSE)[[1]]
spare_parts_options <- read.csv("D:\\COLD_CHAIN\\Data Collection App\\sparePartsOptions.csv", stringsAsFactors = FALSE)[[1]]

# Include custom CSS
tags$head(
  tags$link(rel = "stylesheet", type = "text/css", href = "www/styles.css"),
  tags$link(rel = "stylesheet", type = "text/css", href = "www/styles2.css")
)
