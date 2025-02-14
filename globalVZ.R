library(shiny)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(ggplot2)
library(DT)
library(shinyjqui)
library(readxl)

## --- DUMMY DATA (simulate preloaded CSV data) -----------------------------
facilities <- data.frame(
  facility_id = 1:5,
  facility_name = c("Health Center A", "Health Center B", "Hospital C", "Clinic D", "Facility E"),
  county = c("County1", "County1", "County2", "County2", "County3"),
  subcounty = c("Sub1", "Sub2", "Sub3", "Sub3", "Sub4"),
  latitude = c(3.23658,  3.200236, 3.15853, 3.226301, 3.10973),
  longitude = c(35.6164, 35.670362, 35.99968, 35.744417, 35.60969),
  model = c("Model1", "Model2", "Model1", "Model3", "Model2"),
  make = c("MakeA", "MakeB", "MakeA", "MakeC", "MakeB"),
  available_functional_capacity = c(100, 200, 150, 120, 180),
  stringsAsFactors = FALSE
)