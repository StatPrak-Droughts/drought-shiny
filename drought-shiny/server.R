library(shiny)
library(tmap)
tmap_options(check.and.fix = TRUE)
library(tidyverse)
library(sf)
library(DT)
library(mgcv)
library(sjPlot)
library(slickR)
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
# load(file= "./added_data/models/gam_all_summer_11502.Rdata")
# load(file= "./added_data/models/gam_all_winter_11502.Rdata")
# load(file= "./added_data/models/gam_trimmed_summer_11502.Rdata")
# load(file= "./added_data/models/gam_trimmed_winter_11502.Rdata")
# load(file= "./added_data/models/gam_interactions_summer_11502.Rdata")
# load(file= "./added_data/models/gam_interactions_winter_11502.Rdata")



# Server ---- 
shinyServer(function(input, output) {
    # Catchment Map ----
    output$three_catchment_map <- renderTmap(
        tm_shape(hydro_bavaria)+
            tm_polygons(id = "NameString")+
            tm_shape(waterways_three)+
            tm_lines(col = "blue")+
            tm_shape(pegel_prop_sf)+
            tm_markers(size = 0.3)
    )
    # QPR Tab ----
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
    # Model tables
    output$model_tab_summer <- renderUI({
      if (input$model_catchment %in% "Fränkische Saale / Salz") {
        if (input$model_selection %in% "Full Model") {
          h2("Table of Model")
        }
        if (input$model_selection %in% "Trimmed Model") {
          h2("Table of Model")
        }
        if (input$model_selection %in% "Interactions") {
          h2("Table of Model")
        }
      }
      if (input$model_catchment %in% "Iller Kempten") {
        if (input$model_selection %in% "Full Model") {
          return(HTML(as.character(tab_model(gam_all_summer_11502))[4][1]))
        }
        if (input$model_selection %in% "Trimmed Model") {
          return(HTML(as.character(tab_model(gam_trimmed_summer_11502))[4][1]))
        }
        if (input$model_selection %in% "Interactions") {
          return(HTML(as.character(tab_model(gam_interactions_summer_11502))[4][1]))
        }
      }
      if (input$model_catchment %in% "Isar Mittenwald") {
        if (input$model_selection %in% "Full Model") {
          h2("Table of Model")
        }
        if (input$model_selection %in% "Trimmed Model") {
          h2("Table of Model")
        }
        
        if (input$model_selection %in% "Interactions") {
          h2("Table of Model")
        }
      }
    })
    
    # Model Summaries ----
    output$model_summary_summer <- renderPrint({
      req(input$model_summary)
      if (input$model_catchment %in% "Fränkische Saale / Salz") {
        if (input$model_selection %in% "Full Model") {
          return(summary(gam_all_summer_11502))
        }
        if (input$model_selection %in% "Trimmed Model") {
          return(summary(gam_trimmed_summer_11502))
        }
        if (input$model_selection %in% "Interactions") {
          return(gam_interactions_summer_11502)
        }
      }
      if (input$model_catchment %in% "Iller Kempten") {
        if (input$model_selection %in% "Full Model") {
          return(summary(gam_all_summer_11502))
        }
        if (input$model_selection %in% "Trimmed Model") {
          return(summary(gam_trimmed_summer_11502))
        }
        if (input$model_selection %in% "Interactions") {
          return(summary(gam_interactions_summer_11502))
        }
      }
      if (input$model_catchment %in% "Isar Mittenwald") {
        if (input$model_selection %in% "Full Model") {
          return(summary(gam_all_summer_11502))
        }
        if (input$model_selection %in% "Trimmed Model") {
          return(summary(gam_trimmed_summer_11502))
        }
        
        if (input$model_selection %in% "Interactions") {
          return(summary(gam_interactions_summer_11502))
        }
      }
    })
    output$model_summary_winter <- renderPrint({
      req(input$model_summary)
      if (input$model_catchment %in% "Fränkische Saale / Salz") {
        if (input$model_selection %in% "Full Model") {
          return(summary(gam_all_winter_11502))
        }
        if (input$model_selection %in% "Trimmed Model") {
          return(summary(gam_trimmed_winter_11502))
        }
        if (input$model_selection %in% "Interactions") {
          return(gam_interactions_winter_11502)
        }
      }
      if (input$model_catchment %in% "Iller Kempten") {
        if (input$model_selection %in% "Full Model") {
          return(summary(gam_all_winter_11502))
        }
        if (input$model_selection %in% "Trimmed Model") {
          return(summary(gam_trimmed_winter_11502))
        }
        if (input$model_selection %in% "Interactions") {
          return(summary(gam_interactions_winter_11502))
        }
      }
      if (input$model_catchment %in% "Isar Mittenwald") {
        if (input$model_selection %in% "Full Model") {
          return(summary(gam_all_winter_11502))
        }
        if (input$model_selection %in% "Trimmed Model") {
          return(summary(gam_trimmed_winter_11502))
        }
        
        if (input$model_selection %in% "Interactions") {
          return(summary(gam_interactions_winter_11502))
        }
      }
    })
    
    # Model Effects ----
    ## Summer ----
    #### Effect Plot 1 ----
    output$model_effect_summer_1 <- renderPlot({
      req(input$effect_plots)
      ###### Full Model ----
      if (input$model_selection %in% "Full Model") {
        if(input$model_catchment %in% "Fränkische Saale / Salz"){
          plot(1:10)
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_all_summer_11502, select = 1, ylim = c(-200, 50))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(1:10)
        }}
      ##### Trimmed Model ----
      if (input$model_selection %in% "Trimmed Model") {
        if(input$model_catchment %in% "Fränkische Saale / Salz"){
          plot(1:10)
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_trimmed_summer_11502, select = 1, ylim = c(-200, 50))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(1:10)
        }
      }
      ##### Interactions ----
      if (input$model_selection %in% "Interactions") {
        if(input$model_catchment %in% "Fränkische Saale / Salz"){
          plot(1:10)
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_interactions_summer_11502, select = 1, ylim = c(-200, 50))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(1:10)
        }
      }
    })
    #### Effect Plot 2 ----
    output$model_effect_summer_2 <- renderPlot({
      req(input$effect_plots)
      ###### Full Model ----
      if (input$model_selection %in% "Full Model") {
        if(input$model_catchment %in% "Fränkische Saale / Salz"){
          plot(1:10)
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_all_summer_11502, select = 2, ylim = c(-20, 15))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(1:10)
        }}
      ##### Trimmed Model ----
      if (input$model_selection %in% "Trimmed Model") {
        if(input$model_catchment %in% "Fränkische Saale / Salz"){
          plot(1:10)
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_trimmed_summer_11502, select = 2, ylim = c(-20, 15))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(1:10)
        }
      }
      ##### Interactions ----
      if (input$model_selection %in% "Interactions") {
        if(input$model_catchment %in% "Fränkische Saale / Salz"){
          plot(1:10)
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_interactions_summer_11502, select = 2, ylim = c(-20, 15))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(1:10)
        }
      }
    })
    #### Effect Plot 3 ----
    output$model_effect_summer_3 <- renderPlot({
      req(input$effect_plots)
      ###### Full Model ----
      if (input$model_selection %in% "Full Model") {
        if(input$model_catchment %in% "Fränkische Saale / Salz"){
          plot(1:10)
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_all_summer_11502, select = 3, ylim = c(-15, 15))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(1:10)
        }}
      ##### Trimmed Model ----
      if (input$model_selection %in% "Trimmed Model") {
        if(input$model_catchment %in% "Fränkische Saale / Salz"){
          plot(1:10)
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_trimmed_summer_11502, select = 3, ylim = c(-15, 15))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(1:10)
        }
      }
      ##### Interactions ----
      if (input$model_selection %in% "Interactions") {
        if(input$model_catchment %in% "Fränkische Saale / Salz"){
          plot(1:10)
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_interactions_summer_11502, select = 3, ylim = c(-15, 15))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(1:10)
        }
      }
    })
    #### Effect Plot 4 ----
    output$model_effect_summer_4 <- renderPlot({
      req(input$effect_plots)
      ###### Full Model ----
      if (input$model_selection %in% "Full Model") {
        if(input$model_catchment %in% "Fränkische Saale / Salz"){
          plot(1:10)
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_all_summer_11502, select = 4, ylim = c(-10, 10))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(1:10)
        }}
      ##### Trimmed Model ----
      if (input$model_selection %in% "Trimmed Model") {
        if(input$model_catchment %in% "Fränkische Saale / Salz"){
          plot(1:10)
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_trimmed_summer_11502, select = 4, ylim = c(-10, 15))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(1:10)
        }
      }
      ##### Interactions ----
      if (input$model_selection %in% "Interactions") {
        if(input$model_catchment %in% "Fränkische Saale / Salz"){
          plot(1:10)
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_interactions_summer_11502, select = 4, ylim = c(-10, 15))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(1:10)
        }
      }
    })
    #### Effect Plot 5 ----
    output$model_effect_summer_5 <- renderPlot({
      req(input$effect_plots)
      ###### Full Model ----
      if (input$model_selection %in% "Full Model") {
        if(input$model_catchment %in% "Fränkische Saale / Salz"){
          plot(1:10)
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_all_summer_11502, select = 5, ylim = c(-20, 20))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(1:10)
        }}
      ##### Trimmed Model ----
      if (input$model_selection %in% "Trimmed Model") {
        if(input$model_catchment %in% "Fränkische Saale / Salz"){
          plot(1:10)
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_trimmed_summer_11502, select = 5, ylim = c(-30, 20))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(1:10)
        }
      }
      ##### Interactions ----
      if (input$model_selection %in% "Interactions") {
        if(input$model_catchment %in% "Fränkische Saale / Salz"){
          plot(1:10)
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_interactions_summer_11502, select = 5, ylim = c(-30, 20))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(1:10)
        }
      }
    })
    #### Effect Plot 6 ----
    output$model_effect_summer_6 <- renderPlot({
      req(input$effect_plots)
      ###### Full Model ----
      if (input$model_selection %in% "Full Model") {
        if(input$model_catchment %in% "Fränkische Saale / Salz"){
          plot(1:10)
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_all_summer_11502, select = 6, ylim = c(-500, 30))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(1:10)
        }}
      ##### Trimmed Model ----
      if (input$model_selection %in% "Trimmed Model") {
        if(input$model_catchment %in% "Fränkische Saale / Salz"){
          plot(1:10)
        }
        if (input$model_catchment %in% "Iller Kempten") {
          
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(1:10)
        }
      }
      ##### Interactions ----
      if (input$model_selection %in% "Interactions") {
        if(input$model_catchment %in% "Fränkische Saale / Salz"){
          plot(1:10)
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_interactions_summer_11502, select = 6, ylim = c(-10, 10))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(1:10)
        }
      }
    })
    #### Effect Plot 7 ----
    output$model_effect_summer_7 <- renderPlot({
      req(input$effect_plots)
      ###### Full Model ----
      if (input$model_selection %in% "Full Model") {
        if(input$model_catchment %in% "Fränkische Saale / Salz"){
          plot(1:10)
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_all_summer_11502, select = 7, ylim = c(-50, 15))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(1:10)
        }}
      ##### Trimmed Model ----
      if (input$model_selection %in% "Trimmed Model") {
        if(input$model_catchment %in% "Fränkische Saale / Salz"){
          plot(1:10)
        }
        if (input$model_catchment %in% "Iller Kempten") {
          
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(1:10)
        }
      }
      ##### Interactions ----
      if (input$model_selection %in% "Interactions") {
        if(input$model_catchment %in% "Fränkische Saale / Salz"){
          plot(1:10)
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_interactions_summer_11502, select = 7, ylim = c(-120, 15))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(1:10)
        }
      }
    })
    #### Effect Plot 8 ----
    output$model_effect_summer_8 <- renderPlot({
      req(input$effect_plots)
      ###### Full Model ----
      if (input$model_selection %in% "Full Model") {
        if(input$model_catchment %in% "Fränkische Saale / Salz"){
          plot(1:10)
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_all_summer_11502, select = 8, ylim = c(-50, 15))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(1:10)
        }}
      ##### Trimmed Model ----
      if (input$model_selection %in% "Trimmed Model") {
        if(input$model_catchment %in% "Fränkische Saale / Salz"){
          plot(1:10)
        }
        if (input$model_catchment %in% "Iller Kempten") {
          
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(1:10)
        }
      }
      ##### Interactions ----
      if (input$model_selection %in% "Interactions") {
        if(input$model_catchment %in% "Fränkische Saale / Salz"){
          plot(1:10)
        }
        if (input$model_catchment %in% "Iller Kempten") {
          
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(1:10)
        }
      }
    })
    #### Effect Plot 9 ----
    output$model_effect_summer_9 <- renderPlot({
      req(input$effect_plots)
      ###### Full Model ----
      if (input$model_selection %in% "Full Model") {
        if(input$model_catchment %in% "Fränkische Saale / Salz"){
          plot(1:10)
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_all_summer_11502, select = 9, ylim = c(-15, 15))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(1:10)
        }}
      ##### Trimmed Model ----
      if (input$model_selection %in% "Trimmed Model") {
        if(input$model_catchment %in% "Fränkische Saale / Salz"){
          plot(1:10)
        }
        if (input$model_catchment %in% "Iller Kempten") {
          
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(1:10)
        }
      }
      ##### Interactions ----
      if (input$model_selection %in% "Interactions") {
        if(input$model_catchment %in% "Fränkische Saale / Salz"){
          plot(1:10)
        }
        if (input$model_catchment %in% "Iller Kempten") {

        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(1:10)
        }
      }
    })
    #### Effect Plot 10 ----
    output$model_effect_summer_10 <- renderPlot({
      req(input$effect_plots)
    ###### Full Model ----
      if (input$model_selection %in% "Full Model") {
        if(input$model_catchment %in% "Fränkische Saale / Salz"){
          plot(1:10)
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_all_summer_11502, select = 10, ylim = c(-150, 100))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(1:10)
        }}
    ##### Trimmed Model ----
      if (input$model_selection %in% "Trimmed Model") {
        if(input$model_catchment %in% "Fränkische Saale / Salz"){
          plot(1:10)
        }
        if (input$model_catchment %in% "Iller Kempten") {
          
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(1:10)
        }
      }
    ##### Interactions ----
      if (input$model_selection %in% "Interactions") {
        if(input$model_catchment %in% "Fränkische Saale / Salz"){
          plot(1:10)
        }
        if (input$model_catchment %in% "Iller Kempten") {

        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(1:10)
        }
      }
      })
    ## Winter ----
    #### Effect Plot 1 ----
    output$model_effect_winter_1 <- renderPlot({
      req(input$effect_plots)
      ###### Full Model ----
      if (input$model_selection %in% "Full Model") {
        if(input$model_catchment %in% "Fränkische Saale / Salz"){
          plot(1:10)
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_all_winter_11502, select = 1, ylim = c(-50, 20))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(1:10)
        }}
      ##### Trimmed Model ----
      if (input$model_selection %in% "Trimmed Model") {
        if(input$model_catchment %in% "Fränkische Saale / Salz"){
          plot(1:10)
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_trimmed_winter_11502, select = 1, ylim = c(-30, 10))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(1:10)
        }
      }
      ##### Interactions ----
      if (input$model_selection %in% "Interactions") {
        if(input$model_catchment %in% "Fränkische Saale / Salz"){
          plot(1:10)
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_interactions_winter_11502, select = 1, ylim = c(-50, 20))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(1:10)
        }
      }
    })
    #### Effect Plot 2 ----
    output$model_effect_winter_2 <- renderPlot({
      req(input$effect_plots)
      ###### Full Model ----
      if (input$model_selection %in% "Full Model") {
        if(input$model_catchment %in% "Fränkische Saale / Salz"){
          plot(1:10)
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_all_winter_11502, select = 2, ylim = c(-10, 15))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(1:10)
        }}
      ##### Trimmed Model ----
      if (input$model_selection %in% "Trimmed Model") {
        if(input$model_catchment %in% "Fränkische Saale / Salz"){
          plot(1:10)
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_trimmed_winter_11502, select = 2, ylim = c(-10, 15))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(1:10)
        }
      }
      ##### Interactions ----
      if (input$model_selection %in% "Interactions") {
        if(input$model_catchment %in% "Fränkische Saale / Salz"){
          plot(1:10)
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_interactions_winter_11502, select = 2, ylim = c(-10, 15))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(1:10)
        }
      }
    })
    #### Effect Plot 3 ----
    output$model_effect_winter_3 <- renderPlot({
      req(input$effect_plots)
      ###### Full Model ----
      if (input$model_selection %in% "Full Model") {
        if(input$model_catchment %in% "Fränkische Saale / Salz"){
          plot(1:10)
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_all_winter_11502, select = 3, ylim = c(-15, 15))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(1:10)
        }}
      ##### Trimmed Model ----
      if (input$model_selection %in% "Trimmed Model") {
        if(input$model_catchment %in% "Fränkische Saale / Salz"){
          plot(1:10)
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_trimmed_winter_11502, select = 3, ylim = c(-15, 15))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(1:10)
        }
      }
      ##### Interactions ----
      if (input$model_selection %in% "Interactions") {
        if(input$model_catchment %in% "Fränkische Saale / Salz"){
          plot(1:10)
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_interactions_winter_11502, select = 3, ylim = c(-15, 15))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(1:10)
        }
      }
    })
    #### Effect Plot 4 ----
    output$model_effect_winter_4 <- renderPlot({
      req(input$effect_plots)
      ###### Full Model ----
      if (input$model_selection %in% "Full Model") {
        if(input$model_catchment %in% "Fränkische Saale / Salz"){
          plot(1:10)
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_all_winter_11502, select = 4, ylim = c(-10, 10))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(1:10)
        }}
      ##### Trimmed Model ----
      if (input$model_selection %in% "Trimmed Model") {
        if(input$model_catchment %in% "Fränkische Saale / Salz"){
          plot(1:10)
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_trimmed_winter_11502, select = 4, ylim = c(-10, 15))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(1:10)
        }
      }
      ##### Interactions ----
      if (input$model_selection %in% "Interactions") {
        if(input$model_catchment %in% "Fränkische Saale / Salz"){
          plot(1:10)
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_interactions_winter_11502, select = 4, ylim = c(-10, 15))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(1:10)
        }
      }
    })
    #### Effect Plot 5 ----
    output$model_effect_winter_5 <- renderPlot({
      req(input$effect_plots)
      ###### Full Model ----
      if (input$model_selection %in% "Full Model") {
        if(input$model_catchment %in% "Fränkische Saale / Salz"){
          plot(1:10)
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_all_winter_11502, select = 5, ylim = c(-20, 20))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(1:10)
        }}
      ##### Trimmed Model ----
      if (input$model_selection %in% "Trimmed Model") {
        if(input$model_catchment %in% "Fränkische Saale / Salz"){
          plot(1:10)
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_trimmed_winter_11502, select = 5, ylim = c(-20, 20))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(1:10)
        }
      }
      ##### Interactions ----
      if (input$model_selection %in% "Interactions") {
        if(input$model_catchment %in% "Fränkische Saale / Salz"){
          plot(1:10)
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_interactions_winter_11502, select = 5, ylim = c(-20, 20))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(1:10)
        }
      }
    })
    #### Effect Plot 6 ----
    output$model_effect_winter_6 <- renderPlot({
      req(input$effect_plots)
      ###### Full Model ----
      if (input$model_selection %in% "Full Model") {
        if(input$model_catchment %in% "Fränkische Saale / Salz"){
          plot(1:10)
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_all_winter_11502, select = 6, ylim = c(-500, 30))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(1:10)
        }}
      ##### Trimmed Model ----
      if (input$model_selection %in% "Trimmed Model") {
        if(input$model_catchment %in% "Fränkische Saale / Salz"){
          plot(1:10)
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_trimmed_winter_11502, select = 6, ylim = c(-500, 30))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(1:10)
        }
      }
      ##### Interactions ----
      if (input$model_selection %in% "Interactions") {
        if(input$model_catchment %in% "Fränkische Saale / Salz"){
          plot(1:10)
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_interactions_summer_11502, select = 6, ylim = c(-10, 10))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(1:10)
        }
      }
    })
    #### Effect Plot 7 ----
    output$model_effect_winter_7 <- renderPlot({
      req(input$effect_plots)
      ###### Full Model ----
      if (input$model_selection %in% "Full Model") {
        if(input$model_catchment %in% "Fränkische Saale / Salz"){
          plot(1:10)
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_all_winter_11502, select = 7, ylim = c(-50, 15))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(1:10)
        }}
      ##### Trimmed Model ----
      if (input$model_selection %in% "Trimmed Model") {
        if(input$model_catchment %in% "Fränkische Saale / Salz"){
          plot(1:10)
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_trimmed_winter_11502, select = 7, ylim = c(-50, 15))
          
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(1:10)
        }
      }
      ##### Interactions ----
      if (input$model_selection %in% "Interactions") {
        if(input$model_catchment %in% "Fränkische Saale / Salz"){
          plot(1:10)
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_interactions_winter_11502, select = 7, ylim = c(-60, 15))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(1:10)
        }
      }
    })
    #### Effect Plot 8 ----
    output$model_effect_winter_8 <- renderPlot({
      req(input$effect_plots)
      ###### Full Model ----
      if (input$model_selection %in% "Full Model") {
        if(input$model_catchment %in% "Fränkische Saale / Salz"){
          plot(1:10)
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_all_winter_11502, select = 8, ylim = c(-60, 15))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(1:10)
        }}
      ##### Trimmed Model ----
      if (input$model_selection %in% "Trimmed Model") {
        if(input$model_catchment %in% "Fränkische Saale / Salz"){
          plot(1:10)
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_trimmed_winter_11502, select = 8, ylim = c(-60, 15))
          
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(1:10)
        }
      }
      ##### Interactions ----
      if (input$model_selection %in% "Interactions") {
        if(input$model_catchment %in% "Fränkische Saale / Salz"){
          plot(1:10)
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_interactions_winter_11502, select = 8, ylim = c(-60, 15))
          
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(1:10)
        }
      }
    })
    #### Effect Plot 9 ----
    output$model_effect_winter_9 <- renderPlot({
      req(input$effect_plots)
      ###### Full Model ----
      if (input$model_selection %in% "Full Model") {
        if(input$model_catchment %in% "Fränkische Saale / Salz"){
          plot(1:10)
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_all_winter_11502, select = 9, ylim = c(-15, 15))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(1:10)
        }}
      ##### Trimmed Model ----
      if (input$model_selection %in% "Trimmed Model") {
        if(input$model_catchment %in% "Fränkische Saale / Salz"){
          plot(1:10)
        }
        if (input$model_catchment %in% "Iller Kempten") {
          
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(1:10)
        }
      }
      ##### Interactions ----
      if (input$model_selection %in% "Interactions") {
        if(input$model_catchment %in% "Fränkische Saale / Salz"){
          plot(1:10)
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_interactions_winter_11502, select = 9, ylim = c(-60, 15))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(1:10)
        }
      }
    })
    #### Effect Plot 10 ----
    output$model_effect_winter_10 <- renderPlot({
      req(input$effect_plots)
      ###### Full Model ----
      if (input$model_selection %in% "Full Model") {
        if(input$model_catchment %in% "Fränkische Saale / Salz"){
          plot(1:10)
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_all_winter_11502, select = 10, ylim = c(-150, 100))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(1:10)
        }}
      ##### Trimmed Model ----
      if (input$model_selection %in% "Trimmed Model") {
        if(input$model_catchment %in% "Fränkische Saale / Salz"){
          plot(1:10)
        }
        if (input$model_catchment %in% "Iller Kempten") {
          
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(1:10)
        }
      }
      ##### Interactions ----
      if (input$model_selection %in% "Interactions") {
        if(input$model_catchment %in% "Fränkische Saale / Salz"){
          plot(1:10)
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_interactions_winter_11502, select = 10, ylim = c(-10, 10))
          
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(1:10)
        }
      }
    })
})
