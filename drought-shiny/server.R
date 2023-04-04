library(shiny)
library(tmap)
tmap_options(check.and.fix = TRUE)
library(tidyverse)
library(sf)
library(DT)
# Data Read in ----
# # # Read data
# hydro_summer_20203 <- readRDS(file = "./data/hydro_summer_20203.RDS")
# hydro_summer_11502 <- readRDS(file = "./data/hydro_summer_11502.RDS")
# hydro_summer_10304 <- readRDS(file = "./data/hydro_summer_10304.RDS")
# hydro_winter_20203 <- readRDS(file = "./data/hydro_winter_20203.RDS")
# hydro_winter_11502 <- readRDS(file = "./data/hydro_winter_11502.RDS")
# hydro_winter_10304 <- readRDS(file = "./data/hydro_winter_10304.RDS")
# hydro_total <- readRDS(file = "./data/hydro_total.RDS")
# pegel_prop <- readRDS(file = "./data/pegel_prop.RDS")
# # Add rescaled soilwater
# hydro_summer_20203$soilwater100 <- hydro_summer_20203$soilwater * 100
# hydro_summer_11502$soilwater100 <- hydro_summer_11502$soilwater * 100
# hydro_summer_10304$soilwater100 <- hydro_summer_10304$soilwater * 100
# hydro_winter_20203$soilwater100 <- hydro_winter_20203$soilwater * 100
# hydro_winter_11502$soilwater100 <- hydro_winter_11502$soilwater * 100
# hydro_winter_10304$soilwater100 <- hydro_winter_10304$soilwater * 100
# hydro_bavaria <- read_sf("./data/Geo-Daten_Uebersicht/shapefile/EZG_OHNE_Reservoir_UTM32.shp")
# 
# # Quelle Data https://hub.arcgis.com/datasets/esri-de-content::bundesl%C3%A4nder-2021-mit-einwohnerzahl/explore?location=51.947250%2C15.358806%2C5.85&showTable=true
# admin_bavaria <- read_sf("./added_data/LAN_ew_21.shp") %>% filter(GEN %in% c("Bayern", "Bayern (Bodensee)"))
# admin_bavaria <- st_as_sf(admin_bavaria, crs = 4326)
# pegel_prop_sf <- st_as_sf(pegel_prop, coords = c("longitude", "latitude"), crs = 4326) # crs Code for WGS 84
# 
# pegel_prop_sf$ID <- as.factor(pegel_prop_sf$ID)
# 
# # https://geoportal.bafg.de/CSWView/od.xhtml
# waterways <- read_sf("./added_data/germany-waterways-shape/waterways.shp")
# 
# waterways_three <- waterways %>%
#     filter(name %in% c("Isar",
#                        "Donau",
#                        "Fränkische Saale",
#                        "Main",
#                        "Iller"))
# 
# # Add Pegel props
# hydro_bavaria_20203 <- hydro_bavaria %>% filter(gridcode == 20203) %>%
#     left_join(pegel_prop, by = c("gridcode" = "ID"))
# hydro_bavaria_11502 <- hydro_bavaria %>% filter(gridcode == 11502)%>%
#     left_join(pegel_prop, by = c("gridcode" = "ID"))
# hydro_bavaria_10304 <- hydro_bavaria %>% filter(gridcode == 10304)%>%
#     left_join(pegel_prop, by = c("gridcode" = "ID"))
# hydro_bavaria_three <- hydro_bavaria %>% filter(gridcode %in% c(10304, 11502, 20203))%>%
#     left_join(pegel_prop, by = c("gridcode" = "ID"))
# qpr_hydro_summer_10304 <- readRDS("./added_data/tables/extreme_values/quantile_percents_ranges_hydro_summer_10304.RDS")
# qpr_hydro_summer_11502 <- readRDS("./added_data/tables/extreme_values/quantile_percents_ranges_hydro_summer_11502.RDS")
# qpr_hydro_summer_20203 <- readRDS("./added_data/tables/extreme_values/quantile_percents_ranges_hydro_summer_20203.RDS")
# 
# qpr_hydro_winter_10304 <- readRDS("./added_data/tables/extreme_values/quantile_percents_ranges_hydro_winter_10304.RDS")
# qpr_hydro_winter_11502 <- readRDS("./added_data/tables/extreme_values/quantile_percents_ranges_hydro_winter_11502.RDS")
# qpr_hydro_winter_20203 <- readRDS("./added_data/tables/extreme_values/quantile_percents_ranges_hydro_winter_20203.RDS")

# Server ---- 
shinyServer(function(input, output) {
    output$three_catchment_map <- renderTmap(
        tm_shape(hydro_bavaria)+
            tm_polygons(id = "NameString")+
            tm_shape(waterways_three)+
            tm_lines(col = "blue")+
            tm_shape(pegel_prop_sf)+
            tm_markers(size = 0.3)
    )
    
    pick_qpr_summer_table <-  reactive({
      if (input$extreme_value_catchment %in% "Fränkische Saale / Salz") {
        return(datatable(qpr_hydro_summer_10304, rownames = TRUE, options = list(scrollX = TRUE,
                                                                          scrollY = TRUE,
                                                                          autoWidth = TRUE)))
      }
      if (input$extreme_value_catchment %in% "Iller Kempten") {
        return(datatable(qpr_hydro_summer_11502, rownames = TRUE, options = list(scrollX = TRUE,
                                                                                 scrollY = TRUE,
                                                                                 autoWidth = TRUE)))
      }
      if (input$extreme_value_catchment %in% "Isar Mittenwald") {
        return(datatable(qpr_hydro_summer_20203, rownames = TRUE, options = list(scrollX = TRUE,
                                                                                 scrollY = TRUE,
                                                                                 autoWidth = TRUE)))
      }
    }
    )
    
    pick_qpr_winter_table <-  reactive({
      if (input$extreme_value_catchment %in% "Fränkische Saale / Salz") {
        return(datatable(qpr_hydro_winter_10304, rownames = TRUE, options = list(scrollX = TRUE,
                                                                                 scrollY = TRUE,
                                                                                 autoWidth = TRUE)))
      }
      if (input$extreme_value_catchment %in% "Iller Kempten") {
        return(datatable(qpr_hydro_winter_11502, rownames = TRUE, options = list(scrollX = TRUE,
                                                                                 scrollY = TRUE,
                                                                                 autoWidth = TRUE)))
      }
      if (input$extreme_value_catchment %in% "Isar Mittenwald") {
        return(datatable(qpr_hydro_winter_20203, rownames = TRUE, options = list(scrollX = TRUE,
                                                                                 scrollY = TRUE,
                                                                                 autoWidth = TRUE)))
      }
    }
    )
    
    output$qpr_hydro_summer <- renderDataTable(pick_qpr_summer_table())
    output$qpr_hydro_winter <- renderDataTable(pick_qpr_winter_table())
})
