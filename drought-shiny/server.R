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
# Read data
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
# # Descriptive Data
# # Load tables
# table_yearly_avg_min_groundwaterdepth <-
#   readRDS("added_data/tables/driver_analysis/table_yearly_avg_min_groundwaterdepth.RDS")
# table_yearly_avg_max_groundwaterdepth <-
#   readRDS("added_data/tables/driver_analysis/table_yearly_avg_max_groundwaterdepth.RDS")
# table_yearly_avg_min_soilwater <-
#   readRDS("added_data/tables/driver_analysis/table_yearly_avg_min_soilwater.RDS")
# table_yearly_avg_max_soilwater <-
#   readRDS("added_data/tables/driver_analysis/table_yearly_avg_max_soilwater.RDS")
# table_yearly_avg_min_snowstorage <-
#   readRDS("added_data/tables/driver_analysis/table_yearly_avg_min_snowstorage.RDS")
# table_yearly_avg_max_snowstorage <-
#   readRDS("added_data/tables/driver_analysis/table_yearly_avg_max_snowstorage.RDS")
# table_yearly_avg_min_airtmp <-
#   readRDS("added_data/tables/driver_analysis/table_yearly_avg_min_airtmp.RDS")
# table_yearly_avg_max_airtmp <-
#   readRDS("added_data/tables/driver_analysis/table_yearly_avg_max_airtmp.RDS")
# table_yearly_avg_max_precip <-
#   readRDS("added_data/tables/driver_analysis/table_yearly_avg_max_precip.RDS")
# table_yearly_avg_min_precip <-
#   readRDS("added_data/tables/driver_analysis/table_yearly_avg_min_precip.RDS")
# table_yearly_avg_max_glorad <-
#   readRDS("added_data/tables/driver_analysis/table_yearly_avg_max_glorad.RDS")
# table_yearly_avg_min_glorad <-
#   readRDS("added_data/tables/driver_analysis/table_yearly_avg_min_glorad.RDS")
# table_yearly_avg_max_relhum <-
#   readRDS("added_data/tables/driver_analysis/table_yearly_avg_max_relhum.RDS")
# table_yearly_avg_min_relhum <-
#   readRDS("added_data/tables/driver_analysis/table_yearly_avg_min_relhum.RDS")
# table_yearly_avg_max_infiltration <-
#   readRDS("added_data/tables/driver_analysis/table_yearly_avg_max_infiltration.RDS")
# table_yearly_avg_min_infiltration <-
#   readRDS("added_data/tables/driver_analysis/table_yearly_avg_min_infiltration.RDS")
# # QPR Data
# qpr_hydro_summer_10304 <- readRDS("./added_data/tables/extreme_values/quantile_percents_ranges_hydro_summer_10304.RDS")
# qpr_hydro_summer_11502 <- readRDS("./added_data/tables/extreme_values/quantile_percents_ranges_hydro_summer_11502.RDS")
# qpr_hydro_summer_20203 <- readRDS("./added_data/tables/extreme_values/quantile_percents_ranges_hydro_summer_20203.RDS")
# 
# qpr_hydro_winter_10304 <- readRDS("./added_data/tables/extreme_values/quantile_percents_ranges_hydro_winter_10304.RDS")
# qpr_hydro_winter_11502 <- readRDS("./added_data/tables/extreme_values/quantile_percents_ranges_hydro_winter_11502.RDS")
# qpr_hydro_winter_20203 <- readRDS("./added_data/tables/extreme_values/quantile_percents_ranges_hydro_winter_20203.RDS")
# # 11502 Models
# load(file= "./added_data/models/11502/gam_all_summer_11502.Rdata")
# load(file= "./added_data/models/11502/gam_all_winter_11502.Rdata")
# load(file= "./added_data/models/11502/gam_trimmed_summer_11502.Rdata")
# load(file= "./added_data/models/11502/gam_trimmed_winter_11502.Rdata")
# load(file= "./added_data/models/11502/gam_interactions_summer_11502.Rdata")
# load(file= "./added_data/models/11502/gam_interactions_winter_11502.Rdata")
# # 10304 Models
# load(file = "./added_data/models/10304/gam_all_summer_10304.Rdata")
# load(file = "./added_data/models/10304/gam_all_winter_10304.Rdata")
# load(file = "./added_data/models/10304/gam_selected_summer_10304.Rdata")
# load(file = "./added_data/models/10304/gam_selected_winter_10304.Rdata")
# load(file = "./added_data/models/10304/gam_selected_interac_summer_10304.Rdata")
# load(file = "./added_data/models/10304/gam_selected_interac_winter_10304.Rdata")
# # 20203 Models
# load(file = "./added_data/models/20203/gam2_all_summer_20203.Rdata")
# load(file = "./added_data/models/20203/gam2_all_winter_20203.Rdata")
# load(file = "./added_data/models/20203/gam_selected_summer_20203.Rdata")
# load(file = "./added_data/models/20203/gam_selected_winter_20203.Rdata")
# load(file = "./added_data/models/20203/gam_selected_interact_summer_20203.Rdata")
# load(file = "./added_data/models/20203/gam_selected_interact_winter_20203.Rdata")

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
   # Descriptive Min Max Plots ----
   ## Minima ----
    output$min_plot <- renderPlot({
      ### Groundwaterdepth ----
      if (input$variable_min_max_plot %in% "groundwaterdepth") {
        if (input$facet_hydro_year){
          min_plot_groundwaterdepth_facet <- ggplot(data = table_yearly_avg_min_groundwaterdepth, mapping = aes(x = YY, y = avg_min_groundwaterdepth, color = waterlevel)) +
            geom_line() +
            geom_point() +
            labs(title = "Jährliches durchschnittliches Minimum des Grundwasserstandes \nje Jahreszeit im Zeitverlauf (aller Member)",
                 y = "Jährliches durchschnittliches Minimum des täglichem Grundwasserstandes \n(in m unter Oberfläche)",
                 x = "Datum") +
            guides(color=guide_legend(title="Pegel")) +
            theme(text = element_text(size = 20),
                  legend.position="bottom") +
            scale_color_brewer(palette = "Dark2") +
            facet_wrap(vars(hydro_year), labeller = as_labeller(c(`summer` = "Sommer", `winter` = "Winter")))
          return(min_plot_groundwaterdepth_facet)
        } else {
           min_plot_groundwaterdepth <- ggplot(data = table_yearly_avg_min_groundwaterdepth, mapping = aes(x = YY, y = avg_min_groundwaterdepth, color = waterlevel)) +
            geom_line() +
            geom_point() +
            labs(title = "Jährliches durchschnittliches Minimum des Grundwasserstandes \nje Jahreszeit im Zeitverlauf (aller Member)",
                 y = "Jährliches durchschnittliches Minimum des täglichem Grundwasserstandes \n(in m unter Oberfläche)",
                 x = "Datum") +
            guides(color=guide_legend(title="Pegel")) +
            theme(text = element_text(size = 20),
                  legend.position="bottom") +
            scale_color_brewer(palette = "Dark2")
          return(min_plot_groundwaterdepth)
        }
      }
      ### Soilwater ----
      if (input$variable_min_max_plot %in% "soilwater") {
        if (input$facet_hydro_year){
          min_plot_soilwater_facet <- ggplot(data = table_yearly_avg_min_soilwater, mapping = aes(x = YY, y = avg_min_soilwater, color = waterlevel)) +
            geom_line() +
            geom_point() +
            labs(title = "Jährliches durchschnittliches Minimum der täglichen oberflächennahen \nrelativen Bodenfeuchte je Jahreszeit im Zeitverlauf (aller Member)",
                 y = "Jährliches durchschnittliches Minimum der täglichen oberflächennahen \nrelativen Bodenfeuchte (in Prozentpunkten)",
                 x = "Datum") +
            guides(color=guide_legend(title="Pegel")) +
            theme(text = element_text(size = 20),
                  legend.position="bottom") +
            scale_color_brewer(palette = "Dark2") +
            facet_wrap(vars(hydro_year), labeller = as_labeller(c(`summer` = "Sommer", `winter` = "Winter")))
          return(min_plot_soilwater_facet)
        } else {
          min_plot_soilwater <- ggplot(data = table_yearly_avg_min_soilwater, mapping = aes(x = YY, y = avg_min_soilwater, color = waterlevel)) +
            geom_line() +
            geom_point() +
            labs(title = "Jährliches durchschnittliches Minimum der täglichen oberflächennahen \nrelativen Bodenfeuchte je Jahreszeit im Zeitverlauf (aller Member)",
                 y = "Jährliches durchschnittliches Minimum der täglichen oberflächennahen \nrelativen Bodenfeuchte (in Prozentpunkten)",
                 x = "Datum") +
            guides(color=guide_legend(title="Pegel")) +
            theme(text = element_text(size = 20),
                  legend.position="bottom") +
            scale_color_brewer(palette = "Dark2")
          return(min_plot_soilwater)
        }
      }
      ### Snowstorage ----
      if (input$variable_min_max_plot %in% "snowstorage") {
        if (input$facet_hydro_year){
          min_plot_snowstorage_facet <- ggplot(data = table_yearly_avg_min_snowstorage, mapping = aes(x = YY, y = avg_min_snowstorage, color = waterlevel)) +
            geom_line() +
            geom_point() +
            labs(title = "Jährliches durchschnittliches Minimum an täglichem Schneespeicher \nje Jahreszeit im Zeitverlauf (aller Member)",
                 y = "Jährliches durchschnittliches Minimum an täglichem Schneespeicher \n(in mm)",
                 x = "Datum") +
            guides(color=guide_legend(title="Pegel")) +
            theme(text = element_text(size = 20),
                  legend.position="bottom") +
            scale_color_brewer(palette = "Dark2") +
            facet_wrap(vars(hydro_year), labeller = as_labeller(c(`summer` = "Sommer", `winter` = "Winter")))
          return(min_plot_snowstorage_facet)
        } else {
          min_plot_snowstorage <- ggplot(data = table_yearly_avg_min_snowstorage, mapping = aes(x = YY, y = avg_min_snowstorage, color = waterlevel)) +
            geom_line() +
            geom_point() +
            labs(title = "Jährliches durchschnittliches Minimum an täglichem Schneespeicher \nje Jahreszeit im Zeitverlauf (aller Member)",
                 y = "Jährliches durchschnittliches Minimum an täglichem Schneespeicher \n(in mm)",
                 x = "Datum") +
            guides(color=guide_legend(title="Pegel")) +
            theme(text = element_text(size = 20),
                  legend.position="bottom") +
            scale_color_brewer(palette = "Dark2")
            return(min_plot_snowstorage)
        }
      }
      ### airtmp ----
      if (input$variable_min_max_plot %in% "airtmp") {
        if (input$facet_hydro_year){
          min_plot_airtmp_facet <- ggplot(data = table_yearly_avg_min_airtmp, mapping = aes(x = YY, y = avg_min_airtmp, color = waterlevel)) +
            geom_line() +
            geom_point() +
            labs(title = "Jährliches durchschnittliches Minimum an täglicher Lufttemperatur \nje Jahreszeit im Zeitverlauf (aller Member)",
                 y = "Jährliches durchschnittliches Minimum an täglicher Lufttemperatur \n(in °C)",
                 x = "Datum") +
            guides(color=guide_legend(title="Pegel")) +
            theme(text = element_text(size = 20),
                  legend.position="bottom") +
            scale_color_brewer(palette = "Dark2") +
            facet_wrap(vars(hydro_year), labeller = as_labeller(c(`summer` = "Sommer", `winter` = "Winter")))
          return(min_plot_airtmp_facet)
        } else {
          min_plot_airtmp <- ggplot(data = table_yearly_avg_min_airtmp, mapping = aes(x = YY, y = avg_min_airtmp, color = waterlevel)) +
            geom_line() +
            geom_point() +
            labs(title = "Jährliches durchschnittliches Minimum an täglicher Lufttemperatur \nje Jahreszeit im Zeitverlauf (aller Member)",
                 y = "Jährliches durchschnittliches Minimum an täglicher Lufttemperatur \n(in °C)",
                 x = "Datum") +
            guides(color=guide_legend(title="Pegel")) +
            theme(text = element_text(size = 20),
                  legend.position="bottom") +
            scale_color_brewer(palette = "Dark2")
          return(min_plot_airtmp)
        }
      }
      
      ### precip ----
      if (input$variable_min_max_plot %in% "precipitation") {
        if (input$facet_hydro_year){
          min_plot_precip_facet <- ggplot(data = table_yearly_avg_min_precip, mapping = aes(x = YY, y = avg_min_precip, color = waterlevel)) +
            geom_line() +
            geom_point() +
            labs(title = "Jährliches durchschnittliches Minimum an täglichem Niederschlag \nje Jahreszeit im Zeitverlauf (aller Member)",
                 y = "Jährliches durchschnittliches Minimum an täglichem Niederschlag \n(in mm/24h)",
                 x = "Datum") +
            guides(color=guide_legend(title="Pegel")) +
            theme(text = element_text(size = 20),
                  legend.position="bottom") +
            scale_color_brewer(palette = "Dark2") +
            facet_wrap(vars(hydro_year), labeller = as_labeller(c(`summer` = "Sommer", `winter` = "Winter")))
          return(min_plot_precip_facet)
        } else {
          min_plot_precip <- ggplot(data = table_yearly_avg_min_precip, mapping = aes(x = YY, y = avg_min_precip, color = waterlevel)) +
            geom_line() +
            geom_point() +
            labs(title = "Jährliches durchschnittliches Minimum an täglichem Niederschlag \nje Jahreszeit im Zeitverlauf (aller Member)",
                 y = "Jährliches durchschnittliches Minimum an täglichem Niederschlag \n(in mm/24h)",
                 x = "Datum") +
            guides(color=guide_legend(title="Pegel")) +
            theme(text = element_text(size = 20),
                  legend.position="bottom") +
            scale_color_brewer(palette = "Dark2")
          return(min_plot_precip)
        }
      }
      
      ### glorad ----
      if (input$variable_min_max_plot %in% "glorad") {
        if (input$facet_hydro_year){
          min_plot_glorad_facet <- ggplot(data = table_yearly_avg_min_glorad, mapping = aes(x = YY, y = avg_min_glorad, color = waterlevel)) +
            geom_line() +
            geom_point() +
            labs(title = "Jährliches durchschnittliches Miniumum an täglich einfallender \nkurzwelligen Strahlung je Jahreszeit im Zeitverlauf (aller Member)",
                 y = "Jährliches durchschnittliches Miniumum an täglich einfallender \nkurzwelligen Strahlung (in Wh/m²)",
                 x = "Datum") +
            guides(color=guide_legend(title="Pegel")) +
            theme(text = element_text(size = 20),
                  legend.position="bottom") +
            scale_color_brewer(palette = "Dark2") +
            facet_wrap(vars(hydro_year), labeller = as_labeller(c(`summer` = "Sommer", `winter` = "Winter")))
          return(min_plot_glorad_facet)
        } else {
          min_plot_glorad <- ggplot(data = table_yearly_avg_min_glorad, mapping = aes(x = YY, y = avg_min_glorad, color = waterlevel)) +
            geom_line() +
            geom_point() +
            labs(title = "Jährliches durchschnittliches Miniumum an täglich einfallender \nkurzwelligen Strahlung je Jahreszeit im Zeitverlauf (aller Member)",
                 y = "Jährliches durchschnittliches Miniumum an täglich einfallender \nkurzwelligen Strahlung (in Wh/m²)",
                 x = "Datum") +
            guides(color=guide_legend(title="Pegel")) +
            theme(text = element_text(size = 20),
                  legend.position="bottom") +
            scale_color_brewer(palette = "Dark2")
          return(min_plot_glorad)
        }
      }
      
      ### infiltration ----
      if (input$variable_min_max_plot %in% "infiltration") {
        if (input$facet_hydro_year){
          min_plot_infiltration_facet <- ggplot(data = table_yearly_avg_min_infiltration, mapping = aes(x = YY, y = avg_min_infiltration, color = waterlevel)) +
            geom_line() +
            geom_point() +
            labs(title = "Jährliches durchschnittliches Minimum an täglicher Wasserleitfähigkeit \nje Jahreszeit im Zeitverlauf (aller Member)",
                 y = "Jährliches durchschnittliches Minimum an täglicher Wasserleitfähigkeit \n(in mm/24h)",
                 x = "Datum") +
            guides(color=guide_legend(title="Pegel")) +
            theme(text = element_text(size = 20),
                  legend.position="bottom") +
            scale_color_brewer(palette = "Dark2") +
            facet_wrap(vars(hydro_year), labeller = as_labeller(c(`summer` = "Sommer", `winter` = "Winter")))
          return(min_plot_infiltration_facet)
        } else {
          min_plot_infiltration <- ggplot(data = table_yearly_avg_min_infiltration, mapping = aes(x = YY, y = avg_min_infiltration, color = waterlevel)) +
            geom_line() +
            geom_point() +
            labs(title = "Jährliches durchschnittliches Minimum an täglicher Wasserleitfähigkeit \nje Jahreszeit im Zeitverlauf (aller Member)",
                 y = "Jährliches durchschnittliches Minimum an täglicher Wasserleitfähigkeit \n(in mm/24h)",
                 x = "Datum") +
            guides(color=guide_legend(title="Pegel")) +
            theme(text = element_text(size = 20),
                  legend.position="bottom") +
            scale_color_brewer(palette = "Dark2")
          return(min_plot_infiltration)
        }
      }
      
    })
    ## Maxima
    output$max_plot <- renderPlot({
      ### Groundwaterdepth ----
      if (input$variable_min_max_plot %in% "groundwaterdepth") {
        if (input$facet_hydro_year){
          max_plot_groundwaterdepth_facet <- ggplot(data = table_yearly_avg_max_groundwaterdepth, mapping = aes(x = YY, y = avg_max_groundwaterdepth, color = waterlevel)) +
            geom_line() +
            geom_point() +
            labs(title = "Jährliches durchschnittliches Maximum des Grundwasserstandes \nje Jahreszeit im Zeitverlauf (aller Member)",
                 y = "Jährliches durchschnittliches Maximum des täglichem Grundwasserstandes \n(in m unter Oberfläche)",
                 x = "Datum") +
            guides(color=guide_legend(title="Pegel")) +
            theme(text = element_text(size = 20),
                  legend.position="bottom") +
            scale_color_brewer(palette = "Dark2") +
            facet_wrap(vars(hydro_year), labeller = as_labeller(c(`summer` = "Sommer", `winter` = "Winter")))
          return(max_plot_groundwaterdepth_facet)
        } else {
          max_plot_groundwaterdepth <- ggplot(data = table_yearly_avg_max_groundwaterdepth, mapping = aes(x = YY, y = avg_max_groundwaterdepth, color = waterlevel)) +
            geom_line() +
            geom_point() +
            labs(title = "Jährliches durchschnittliches Maximum des Grundwasserstandes \nje Jahreszeit im Zeitverlauf (aller Member)",
                 y = "Jährliches durchschnittliches Maximum des täglichem Grundwasserstandes \n(in m unter Oberfläche)",
                 x = "Datum") +
            guides(color=guide_legend(title="Pegel")) +
            theme(text = element_text(size = 20),
                  legend.position="bottom") +
            scale_color_brewer(palette = "Dark2")
          return(max_plot_groundwaterdepth)
        }
      }
      ### Soilwater ----
      if (input$variable_min_max_plot %in% "soilwater") {
        if (input$facet_hydro_year){
          max_plot_soilwater_facet <- ggplot(data = table_yearly_avg_max_soilwater, mapping = aes(x = YY, y = avg_max_soilwater, color = waterlevel)) +
            geom_line() +
            geom_point() +
            labs(title = "Jährliches durchschnittliches Maximum der täglichen oberflächennahen \nrelativen Bodenfeuchte je Jahreszeit im Zeitverlauf (aller Member)",
                 y = "Jährliches durchschnittliches Maximum der täglichen oberflächennahen \nrelativen Bodenfeuchte (in Prozentpunkten)",
                 x = "Datum") +
            guides(color=guide_legend(title="Pegel")) +
            theme(text = element_text(size = 20),
                  legend.position="bottom") +
            scale_color_brewer(palette = "Dark2") +
            facet_wrap(vars(hydro_year), labeller = as_labeller(c(`summer` = "Sommer", `winter` = "Winter")))
          return(max_plot_soilwater_facet)
        } else {
          max_plot_soilwater <- ggplot(data = table_yearly_avg_max_soilwater, mapping = aes(x = YY, y = avg_max_soilwater, color = waterlevel)) +
            geom_line() +
            geom_point() +
            labs(title = "Jährliches durchschnittliches Maximum der täglichen oberflächennahen \nrelativen Bodenfeuchte je Jahreszeit im Zeitverlauf (aller Member)",
                 y = "Jährliches durchschnittliches Maximum der täglichen oberflächennahen \nrelativen Bodenfeuchte (in Prozentpunkten)",
                 x = "Datum") +
            guides(color=guide_legend(title="Pegel")) +
            theme(text = element_text(size = 20),
                  legend.position="bottom") +
            scale_color_brewer(palette = "Dark2")
          return(max_plot_soilwater)
        }
      }
      ### Snowstorage ----
      if (input$variable_min_max_plot %in% "snowstorage") {
        if (input$facet_hydro_year){
          max_plot_snowstorage_facet <- ggplot(data = table_yearly_avg_max_snowstorage, mapping = aes(x = YY, y = avg_max_snowstorage, color = waterlevel)) +
            geom_line() +
            geom_point() +
            labs(title = "Jährliches durchschnittliches Maximum an täglichem Schneespeicher \nje Jahreszeit im Zeitverlauf (aller Member)",
                 y = "Jährliches durchschnittliches Maximum an täglichem Schneespeicher \n(in mm)",
                 x = "Datum") +
            guides(color=guide_legend(title="Pegel")) +
            theme(text = element_text(size = 20),
                  legend.position="bottom") +
            scale_color_brewer(palette = "Dark2") +
            facet_wrap(vars(hydro_year), labeller = as_labeller(c(`summer` = "Sommer", `winter` = "Winter")))
          return(max_plot_snowstorage_facet)
        } else {
          max_plot_snowstorage <- ggplot(data = table_yearly_avg_max_snowstorage, mapping = aes(x = YY, y = avg_max_snowstorage, color = waterlevel)) +
            geom_line() +
            geom_point() +
            labs(title = "Jährliches durchschnittliches Maximum an täglichem Schneespeicher \nje Jahreszeit im Zeitverlauf (aller Member)",
                 y = "Jährliches durchschnittliches Maximum an täglichem Schneespeicher \n(in mm)",
                 x = "Datum") +
            guides(color=guide_legend(title="Pegel")) +
            theme(text = element_text(size = 20),
                  legend.position="bottom") +
            scale_color_brewer(palette = "Dark2")
          return(max_plot_snowstorage)
        }
      }
      ### airtmp ----
      if (input$variable_min_max_plot %in% "airtmp") {
        if (input$facet_hydro_year){
          max_plot_airtmp_facet <- ggplot(data = table_yearly_avg_max_airtmp, mapping = aes(x = YY, y = avg_max_airtmp, color = waterlevel)) +
            geom_line() +
            geom_point() +
            labs(title = "Jährliches durchschnittliches Maximum an täglicher Lufttemperatur \nje Jahreszeit im Zeitverlauf (aller Member)",
                 y = "Jährliches durchschnittliches Maximum an täglicher Lufttemperatur \n(in °C)",
                 x = "Datum") +
            guides(color=guide_legend(title="Pegel")) +
            theme(text = element_text(size = 20),
                  legend.position="bottom") +
            scale_color_brewer(palette = "Dark2") +
            facet_wrap(vars(hydro_year), labeller = as_labeller(c(`summer` = "Sommer", `winter` = "Winter")))
          return(max_plot_airtmp_facet)
        } else {
          max_plot_airtmp <- ggplot(data = table_yearly_avg_max_airtmp, mapping = aes(x = YY, y = avg_max_airtmp, color = waterlevel)) +
            geom_line() +
            geom_point() +
            labs(title = "Jährliches durchschnittliches Maximum an täglicher Lufttemperatur \nje Jahreszeit im Zeitverlauf (aller Member)",
                 y = "Jährliches durchschnittliches Maximum an täglicher Lufttemperatur \n(in °C)",
                 x = "Datum") +
            guides(color=guide_legend(title="Pegel")) +
            theme(text = element_text(size = 20),
                  legend.position="bottom") +
            scale_color_brewer(palette = "Dark2")
          return(max_plot_airtmp)
        }
      }
      
      ### precip ----
      if (input$variable_min_max_plot %in% "precipitation") {
        if (input$facet_hydro_year){
          max_plot_precip_facet <- ggplot(data = table_yearly_avg_max_precip, mapping = aes(x = YY, y = avg_max_precip, color = waterlevel)) +
            geom_line() +
            geom_point() +
            labs(title = "Jährliches durchschnittliches Maximum an täglichem Niederschlag \nje Jahreszeit im Zeitverlauf (aller Member)",
                 y = "Jährliches durchschnittliches Maximum an täglichem Niederschlag \n(in mm/24h)",
                 x = "Datum") +
            guides(color=guide_legend(title="Pegel")) +
            theme(text = element_text(size = 20),
                  legend.position="bottom") +
            scale_color_brewer(palette = "Dark2") +
            facet_wrap(vars(hydro_year), labeller = as_labeller(c(`summer` = "Sommer", `winter` = "Winter")))
          return(max_plot_precip_facet)
        } else {
          max_plot_precip <- ggplot(data = table_yearly_avg_max_precip, mapping = aes(x = YY, y = avg_max_precip, color = waterlevel)) +
            geom_line() +
            geom_point() +
            labs(title = "Jährliches durchschnittliches Maximum an täglichem Niederschlag \nje Jahreszeit im Zeitverlauf (aller Member)",
                 y = "Jährliches durchschnittliches Maximum an täglichem Niederschlag \n(in mm/24h)",
                 x = "Datum") +
            guides(color=guide_legend(title="Pegel")) +
            theme(text = element_text(size = 20),
                  legend.position="bottom") +
            scale_color_brewer(palette = "Dark2")
          return(max_plot_precip)
        }
      }
      
      ### glorad ----
      if (input$variable_min_max_plot %in% "glorad") {
        if (input$facet_hydro_year){
          max_plot_glorad_facet <- ggplot(data = table_yearly_avg_max_glorad, mapping = aes(x = YY, y = avg_max_glorad, color = waterlevel)) +
            geom_line() +
            geom_point() +
            labs(title = "Jährliches durchschnittliches Maximum an täglich einfallender \nkurzwelligen Strahlung je Jahreszeit im Zeitverlauf (aller Member)",
                 y = "Jährliches durchschnittliches Maximum an täglich einfallender \nkurzwelligen Strahlung (in Wh/m²)",
                 x = "Datum") +
            guides(color=guide_legend(title="Pegel")) +
            theme(text = element_text(size = 20),
                  legend.position="bottom") +
            scale_color_brewer(palette = "Dark2") +
            facet_wrap(vars(hydro_year), labeller = as_labeller(c(`summer` = "Sommer", `winter` = "Winter")))
          return(max_plot_glorad_facet)
        } else {
          max_plot_glorad <- ggplot(data = table_yearly_avg_max_glorad, mapping = aes(x = YY, y = avg_max_glorad, color = waterlevel)) +
            geom_line() +
            geom_point() +
            labs(title = "Jährliches durchschnittliches Maximum an täglich einfallender \nkurzwelligen Strahlung je Jahreszeit im Zeitverlauf (aller Member)",
                 y = "Jährliches durchschnittliches Maximum an täglich einfallender \nkurzwelligen Strahlung (in Wh/m²)",
                 x = "Datum") +
            guides(color=guide_legend(title="Pegel")) +
            theme(text = element_text(size = 20),
                  legend.position="bottom") +
            scale_color_brewer(palette = "Dark2")
          return(max_plot_glorad)
        }
      }
      
      ### infiltration ----
      if (input$variable_min_max_plot %in% "infiltration") {
        if (input$facet_hydro_year){
          max_plot_infiltration_facet <- ggplot(data = table_yearly_avg_max_infiltration, mapping = aes(x = YY, y = avg_max_infiltration, color = waterlevel)) +
            geom_line() +
            geom_point() +
            labs(title = "Jährliches durchschnittliches Maximum an täglicher Wasserleitfähigkeit \nje Jahreszeit im Zeitverlauf (aller Member)",
                 y = "Jährliches durchschnittliches Maximum an täglicher Wasserleitfähigkeit \n(in mm/24h)",
                 x = "Datum") +
            guides(color=guide_legend(title="Pegel")) +
            theme(text = element_text(size = 20),
                  legend.position="bottom") +
            scale_color_brewer(palette = "Dark2") +
            facet_wrap(vars(hydro_year), labeller = as_labeller(c(`summer` = "Sommer", `winter` = "Winter")))
          return(max_plot_infiltration_facet)
        } else {
          max_plot_infiltration <- ggplot(data = table_yearly_avg_max_infiltration, mapping = aes(x = YY, y = avg_max_infiltration, color = waterlevel)) +
            geom_line() +
            geom_point() +
            labs(title = "Jährliches durchschnittliches Maximum an täglicher Wasserleitfähigkeit \nje Jahreszeit im Zeitverlauf (aller Member)",
                 y = "Jährliches durchschnittliches Maximum an täglicher Wasserleitfähigkeit \n(in mm/24h)",
                 x = "Datum") +
            guides(color=guide_legend(title="Pegel")) +
            theme(text = element_text(size = 20),
                  legend.position="bottom") +
            scale_color_brewer(palette = "Dark2")
          return(max_plot_infiltration)
        }
      }
      
    })
    
    # QPR Tab ----
    pick_qpr_summer_table <-  reactive({
      if (input$extreme_value_catchment %in% "Fränkische Saale Salz") {
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
      if (input$extreme_value_catchment %in% "Fränkische Saale Salz") {
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
    
    # Model Summaries ----
    output$model_summary_summer <- renderPrint({
      req(input$model_summary)
      
      if (input$model_catchment %in% "Fränkische Saale Salz") {
        if (input$model_selection %in% "Full Model") {
          return(summary(gam2_all_summer_20203))
        }
        if (input$model_selection %in% "Trimmed Model") {
          return(summary(gam_selected_summer_20203))
        }
        if (input$model_selection %in% "Interactions") {
          return(summary(gam_selected_interact_summer_20203))
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
          return(summary(gam_all_summer_10304))
        }
        if (input$model_selection %in% "Trimmed Model") {
          return(summary(gam_selected_summer_10304))
        }
        
        if (input$model_selection %in% "Interactions") {
          return(summary(gam_selected_interac_summer_10304))
        }
      }
    })
    output$model_summary_winter <- renderPrint({
      req(input$model_summary)
      if (input$model_catchment %in% "Fränkische Saale Salz") {
        if (input$model_selection %in% "Full Model") {
          return(summary(gam2_all_winter_20203))
        }
        if (input$model_selection %in% "Trimmed Model") {
          return(summary(gam_selected_winter_20203))
        }
        if (input$model_selection %in% "Interactions") {
          return(summary(gam_selected_interact_winter_20203))
        }
      }
      if (input$model_catchment %in% "Iller Kempten") {
        if (input$model_selection %in% "Full Model") {
          return(summary(gam_all_winter_10304))
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
          return(summary(gam_all_winter_10304))
        }
        if (input$model_selection %in% "Trimmed Model") {
          return(summary(gam_selected_winter_10304))
        }
        
        if (input$model_selection %in% "Interactions") {
          return(summary(gam_selected_interac_winter_10304))
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
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam2_all_summer_20203, select = 1, ylim = c(-20, 20))
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_all_summer_11502, select = 1, ylim = c(-200, 50))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_all_summer_10304, select = 1, ylim = c(-200, 50))
        }}
      ##### Trimmed Model ----
      if (input$model_selection %in% "Trimmed Model") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_selected_summer_20203, select = 1, ylim = c(-10, 10))
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_trimmed_summer_11502, select = 1, ylim = c(-200, 50))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_selected_summer_10304, select = 1, ylim = c(-200, 50))
        }
      }
      ##### Interactions ----
      if (input$model_selection %in% "Interactions") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_selected_interact_summer_20203, select = 1, ylim = c(-20, 20))
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_interactions_summer_11502, select = 1, ylim = c(-200, 50))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_selected_interac_summer_10304, select = 1, ylim = c(-200, 50))
        }
      }
    })
    #### Effect Plot 2 ----
    output$model_effect_summer_2 <- renderPlot({
      req(input$effect_plots)
      ###### Full Model ----
      if (input$model_selection %in% "Full Model") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam2_all_summer_20203, select = 2, ylim = c(-20, 15))
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_all_summer_11502, select = 2, ylim = c(-20, 15))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_all_summer_10304, select = 2, ylim = c(-20, 15))
        }}
      ##### Trimmed Model ----
      if (input$model_selection %in% "Trimmed Model") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_selected_interact_summer_20203, select = 2, ylim = c(-20, 15))
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_trimmed_summer_11502, select = 2, ylim = c(-20, 15))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_selected_interac_summer_10304, select = 2, ylim = c(-20, 15))
        }
      }
      ##### Interactions ----
      if (input$model_selection %in% "Interactions") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_selected_interact_summer_20203, select = 2, ylim = c(-20, 15))
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_interactions_summer_11502, select = 2, ylim = c(-20, 15))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_selected_interac_summer_10304, select = 2, ylim = c(-20, 15))
        }
      }
    })
    #### Effect Plot 3 ----
    output$model_effect_summer_3 <- renderPlot({
      req(input$effect_plots)
      ###### Full Model ----
      if (input$model_selection %in% "Full Model") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam2_all_summer_20203, select = 3, ylim = c(-15, 15))
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_all_summer_11502, select = 3, ylim = c(-15, 15))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_all_summer_10304, select = 3, ylim = c(-15, 15))
        }}
      ##### Trimmed Model ----
      if (input$model_selection %in% "Trimmed Model") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_selected_summer_20203, select = 3, ylim = c(-15, 15))
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_trimmed_summer_11502, select = 3, ylim = c(-15, 15))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_selected_summer_10304, select = 3, ylim = c(-15, 15))
        }
      }
      ##### Interactions ----
      if (input$model_selection %in% "Interactions") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_selected_interact_summer_20203, select = 3, ylim = c(-15, 15))
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_interactions_summer_11502, select = 3, ylim = c(-15, 15))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_selected_interac_summer_10304, select = 3, ylim = c(-15, 15))
        }
      }
    })
    #### Effect Plot 4 ----
    output$model_effect_summer_4 <- renderPlot({
      req(input$effect_plots)
      ###### Full Model ----
      if (input$model_selection %in% "Full Model") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam2_all_summer_20203, select = 4, ylim = c(-100, 40))
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_all_summer_11502, select = 4, ylim = c(-10, 10))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_all_summer_10304, select = 4, ylim = c(-10, 10))
        }}
      ##### Trimmed Model ----
      if (input$model_selection %in% "Trimmed Model") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_selected_summer_20203, select = 4, ylim = c(-100, 40))
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_trimmed_summer_11502, select = 4, ylim = c(-10, 15))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_selected_summer_10304, select = 4, ylim = c(-10, 15))
        }
      }
      ##### Interactions ----
      if (input$model_selection %in% "Interactions") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_selected_interact_summer_20203, select = 4, ylim = c(-100, 40))
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_interactions_summer_11502, select = 4, ylim = c(-10, 15))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_selected_interac_summer_10304, select = 4, ylim = c(-10, 15))
        }
      }
    })
    #### Effect Plot 5 ----
    output$model_effect_summer_5 <- renderPlot({
      req(input$effect_plots)
      ###### Full Model ----
      if (input$model_selection %in% "Full Model") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam2_all_summer_20203, select = 5, ylim = c(-20, 20))
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_all_summer_11502, select = 5, ylim = c(-20, 20))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_all_summer_10304, select = 5, ylim = c(-20, 20))
        }}
      ##### Trimmed Model ----
      if (input$model_selection %in% "Trimmed Model") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_selected_summer_20203, select = 5, ylim = c(-50000, 100))
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_trimmed_summer_11502, select = 5, ylim = c(-800, 20))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_selected_summer_10304, select = 5, ylim = c(-30, 20))
        }
      }
      ##### Interactions ----
      if (input$model_selection %in% "Interactions") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_selected_interact_summer_20203, select = 5, ylim = c(-50000, 100))
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_interactions_summer_11502, select = 5, ylim = c(-30, 20))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_selected_interac_summer_10304, select = 5, ylim = c(-30, 20))
        }
      }
    })
    #### Effect Plot 6 ----
    output$model_effect_summer_6 <- renderPlot({
      req(input$effect_plots)
      ###### Full Model ----
      if (input$model_selection %in% "Full Model") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam2_all_summer_20203, select = 6, ylim = c(-100, 50))
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_all_summer_11502, select = 6, ylim = c(-500, 30))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_all_summer_10304, select = 6, ylim = c(-500, 30))
        }}
      ##### Trimmed Model ----
      if (input$model_selection %in% "Trimmed Model") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_selected_summer_20203, select = 6, ylim = c(-10, 10))        
          }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_trimmed_summer_11502, select = 6, ylim = c(-40, 20))        
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
     
        }
      }
      ##### Interactions ----
      if (input$model_selection %in% "Interactions") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_selected_interact_summer_20203, select = 6, ylim = c(-10, 10))
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_interactions_summer_11502, select = 6, ylim = c(-20, 10))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_selected_interac_summer_10304, select = 6, ylim = c(-40, 40))
        }
      }
    })
    #### Effect Plot 7 ----
    output$model_effect_summer_7 <- renderPlot({
      req(input$effect_plots)
      ###### Full Model ----
      if (input$model_selection %in% "Full Model") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_all_summer_11502, select = 7, ylim = c(-50, 15))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_all_summer_10304, select = 7, ylim = c(-50, 15))
        }}
      ##### Trimmed Model ----
      if (input$model_selection %in% "Trimmed Model") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_trimmed_summer_11502, select = 7, ylim = c(-50, 15))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
        }
      }
      ##### Interactions ----
      if (input$model_selection %in% "Interactions") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_selected_interact_summer_20203, select = 7, ylim = c(-50, 15))
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_interactions_summer_11502, select = 7, ylim = c(-120, 15))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_selected_interac_summer_10304, select = 7, ylim = c(-30, 15))
        }
      }
    })
    #### Effect Plot 8 ----
    output$model_effect_summer_8 <- renderPlot({
      req(input$effect_plots)
      ###### Full Model ----
      if (input$model_selection %in% "Full Model") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){

        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_all_summer_11502, select = 8, ylim = c(-50, 15))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_all_summer_10304, select = 8, ylim = c(-50, 15))
        }}
      ##### Trimmed Model ----
      if (input$model_selection %in% "Trimmed Model") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_trimmed_summer_11502, select = 8, ylim = c(-30, 15))
          
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
        }
      }
      ##### Interactions ----
      if (input$model_selection %in% "Interactions") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_selected_interact_summer_20203, select = 8, ylim = c(-20, 20))
          
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_interactions_summer_11502, select = 8, ylim = c(-20, 20))
          
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
        }
      }
    })
    #### Effect Plot 9 ----
    output$model_effect_summer_9 <- renderPlot({
      req(input$effect_plots)
      ###### Full Model ----
      if (input$model_selection %in% "Full Model") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_all_summer_11502, select = 9, ylim = c(-15, 15))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_all_summer_10304, select = 9, ylim = c(-15, 15))
        }}
      ##### Trimmed Model ----
      if (input$model_selection %in% "Trimmed Model") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
        }
        if (input$model_catchment %in% "Iller Kempten") {
          
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
        }
      }
      ##### Interactions ----
      if (input$model_selection %in% "Interactions") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_interactions_summer_11502, select = 9, ylim = c(-90, 20))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          
        }
      }
    })
    #### Effect Plot 10 ----
    output$model_effect_summer_10 <- renderPlot({
      req(input$effect_plots)
    ###### Full Model ----
      if (input$model_selection %in% "Full Model") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_all_summer_11502, select = 10, ylim = c(-150, 100))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_all_summer_10304, select = 10, ylim = c(-150, 100))
        }}
    ##### Trimmed Model ----
      if (input$model_selection %in% "Trimmed Model") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
        }
        if (input$model_catchment %in% "Iller Kempten") {
          
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
        }
      }
    ##### Interactions ----
      if (input$model_selection %in% "Interactions") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
        }
        if (input$model_catchment %in% "Iller Kempten") {

        }
        if (input$model_catchment %in% "Isar Mittenwald") {
        }
      }
      })
    ## Winter ----
    # Selected Saale 6 Isar 6 Iller
    #### Effect Plot 1 ----
    output$model_effect_winter_1 <- renderPlot({
      req(input$effect_plots)
      ###### Full Model ----
      if (input$model_selection %in% "Full Model") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam2_all_winter_20203, select = 1, ylim = c(-20, 20))
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_all_winter_11502, select = 1, ylim = c(-50, 20))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_all_winter_10304, select = 1, ylim = c(-20, 20))
        }}
      ##### Trimmed Model ----
      if (input$model_selection %in% "Trimmed Model") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_selected_winter_20203, select = 1, ylim = c(-10, 10))
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_trimmed_winter_11502, select = 1, ylim = c(-30, 10))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_selected_summer_10304, select = 1, ylim = c(-240, 10))
        }
      }
      ##### Interactions ----
      if (input$model_selection %in% "Interactions") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_selected_interact_winter_20203, select = 1, ylim = c(-50, 20))
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_interactions_winter_11502, select = 1, ylim = c(-50, 20))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_selected_interac_winter_10304, select = 1, ylim = c(-50, 20))
        }
      }
    })
    #### Effect Plot 2 ----
    output$model_effect_winter_2 <- renderPlot({
      req(input$effect_plots)
      ###### Full Model ----
      if (input$model_selection %in% "Full Model") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam2_all_winter_20203, select = 2, ylim = c(-10, 15))
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_all_winter_11502, select = 2, ylim = c(-10, 15))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_all_winter_10304, select = 2, ylim = c(-10, 15))
        }}
      ##### Trimmed Model ----
      if (input$model_selection %in% "Trimmed Model") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_selected_winter_20203, select = 2, ylim = c(-20, 20))
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_trimmed_winter_11502, select = 2, ylim = c(-10, 15))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_selected_summer_10304, select = 2, ylim = c(-10, 10))
        }
      }
      ##### Interactions ----
      if (input$model_selection %in% "Interactions") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_selected_interact_winter_20203, select = 2, ylim = c(-50, 20))
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_interactions_winter_11502, select = 2, ylim = c(-10, 15))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_selected_interac_winter_10304, select = 2, ylim = c(-500, 1000))
        }
      }
    })
    #### Effect Plot 3 ----
    output$model_effect_winter_3 <- renderPlot({
      req(input$effect_plots)
      ###### Full Model ----
      if (input$model_selection %in% "Full Model") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam2_all_winter_20203, select = 3, ylim = c(-15, 30))
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_all_winter_11502, select = 3, ylim = c(-15, 15))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_all_winter_10304, select = 3, ylim = c(-15, 15))
        }}
      ##### Trimmed Model ----
      if (input$model_selection %in% "Trimmed Model") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_selected_winter_20203, select = 3, ylim = c(-20, 30))
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_trimmed_winter_11502, select = 3, ylim = c(-15, 15))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_selected_winter_10304, select = 3, ylim = c(-20, 10))
        }
      }
      ##### Interactions ----
      if (input$model_selection %in% "Interactions") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_selected_interact_winter_20203, select = 3, ylim = c(-50, 50))
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_interactions_winter_11502, select = 3, ylim = c(-15, 15))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_selected_interac_winter_10304, select = 3, ylim = c(-50, 50))
        }
      }
    })
    #### Effect Plot 4 ----
    output$model_effect_winter_4 <- renderPlot({
      req(input$effect_plots)
      ###### Full Model ----
      if (input$model_selection %in% "Full Model") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam2_all_winter_20203, select = 4, ylim = c(-20, 30))
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_all_winter_11502, select = 4, ylim = c(-10, 10))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_all_winter_10304, select = 4, ylim = c(-10, 10))
        }}
      ##### Trimmed Model ----
      if (input$model_selection %in% "Trimmed Model") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_selected_winter_20203, select = 4, ylim = c(-30, 30))
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_trimmed_winter_11502, select = 4, ylim = c(-10, 15))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_selected_winter_10304, select = 4, ylim = c(-10, 10))
        }
      }
      ##### Interactions ----
      if (input$model_selection %in% "Interactions") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_selected_interact_winter_20203, select = 4, ylim = c(-1700, 1000))
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_interactions_winter_11502, select = 4, ylim = c(-10, 15))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_selected_interac_winter_10304, select = 4, ylim = c(-3200, 1000))
        }
      }
    })
    #### Effect Plot 5 ----
    output$model_effect_winter_5 <- renderPlot({
      req(input$effect_plots)
      ###### Full Model ----
      if (input$model_selection %in% "Full Model") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam2_all_winter_20203, select = 5, ylim = c(-900, 500))
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_all_winter_11502, select = 5, ylim = c(-20, 20))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_all_winter_10304, select = 5, ylim = c(-40, 20))
        }}
      ##### Trimmed Model ----
      if (input$model_selection %in% "Trimmed Model") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_selected_winter_20203, select = 5, ylim = c(-800, 50))
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_trimmed_winter_11502, select = 5, ylim = c(-20, 20))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_selected_winter_10304, select = 5, ylim = c(-30, 20))
        }
      }
      ##### Interactions ----
      if (input$model_selection %in% "Interactions") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_selected_interact_winter_20203, select = 5, ylim = c(-10, 10))
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_interactions_winter_11502, select = 5, ylim = c(-20, 20))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_selected_interac_winter_10304, select = 5, ylim = c(-2500, 100))
        }
      }
    })
    #### Effect Plot 6 ----
    output$model_effect_winter_6 <- renderPlot({
      req(input$effect_plots)
      ###### Full Model ----
      if (input$model_selection %in% "Full Model") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam2_all_winter_20203, select = 6, ylim = c(-30, 10))
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_all_winter_11502, select = 6, ylim = c(-500, 30))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_all_winter_10304, select = 6, ylim = c(-10, 10))
        }}
      ##### Trimmed Model ----
      if (input$model_selection %in% "Trimmed Model") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_selected_winter_20203, select = 6, ylim = c(-20, 10))
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_trimmed_winter_11502, select = 6, ylim = c(-500, 30))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_selected_winter_10304, select = 6, ylim = c(-200, 60))
        }
      }
      ##### Interactions ----
      if (input$model_selection %in% "Interactions") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_selected_interact_winter_20203, select = 6, ylim = c(-10, 10))
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_interactions_summer_11502, select = 6, ylim = c(-10, 10))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_selected_interac_winter_10304, select = 6, ylim = c(-200, 250))
        }
      }
    })
    #### Effect Plot 7 ----
    output$model_effect_winter_7 <- renderPlot({
      req(input$effect_plots)
      ###### Full Model ----
      if (input$model_selection %in% "Full Model") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam2_all_winter_20203, select = 7, ylim = c(-10, 10))
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_all_winter_11502, select = 7, ylim = c(-50, 15))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_all_winter_10304, select = 7, ylim = c(-50, 15))
        }}
      ##### Trimmed Model ----
      if (input$model_selection %in% "Trimmed Model") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_trimmed_winter_11502, select = 7, ylim = c(-50, 15))
          
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
        }
      }
      ##### Interactions ----
      if (input$model_selection %in% "Interactions") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_selected_interact_winter_20203, select = 7, ylim = c(-700, 100))
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_interactions_winter_11502, select = 7, ylim = c(-60, 15))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_selected_interac_winter_10304, select = 7, ylim = c(-500, 250))
        }
      }
    })
    #### Effect Plot 8 ----
    output$model_effect_winter_8 <- renderPlot({
      req(input$effect_plots)
      ###### Full Model ----
      if (input$model_selection %in% "Full Model") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_all_winter_11502, select = 8, ylim = c(-60, 15))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_all_winter_10304, select = 8, ylim = c(-150, 40))
        }}
      ##### Trimmed Model ----
      if (input$model_selection %in% "Trimmed Model") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_trimmed_winter_11502, select = 8, ylim = c(-60, 15))
          
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
        }
      }
      ##### Interactions ----
      if (input$model_selection %in% "Interactions") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_selected_interact_winter_20203, select = 8, ylim = c(-100, 100))
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_interactions_winter_11502, select = 8, ylim = c(-60, 15))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_selected_interac_winter_10304, select = 8, ylim = c(-150, 80))
        }
      }
    })
    #### Effect Plot 9 ----
    output$model_effect_winter_9 <- renderPlot({
      req(input$effect_plots)
      ###### Full Model ----
      if (input$model_selection %in% "Full Model") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_all_winter_11502, select = 9, ylim = c(-15, 15))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_all_winter_10304, select = 9, ylim = c(-15, 15))
        }}
      ##### Trimmed Model ----
      if (input$model_selection %in% "Trimmed Model") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
        }
        if (input$model_catchment %in% "Iller Kempten") {
          
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
        }
      }
      ##### Interactions ----
      if (input$model_selection %in% "Interactions") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_interactions_winter_11502, select = 9, ylim = c(-60, 15))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
        }
      }
    })
    #### Effect Plot 10 ----
    output$model_effect_winter_10 <- renderPlot({
      req(input$effect_plots)
      ###### Full Model ----
      if (input$model_selection %in% "Full Model") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_all_winter_11502, select = 10, ylim = c(-150, 100))
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_all_winter_10304, select = 10, ylim = c(-400, 100))
        }}
      ##### Trimmed Model ----
      if (input$model_selection %in% "Trimmed Model") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
        }
        if (input$model_catchment %in% "Iller Kempten") {
          
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
        }
      }
      ##### Interactions ----
      if (input$model_selection %in% "Interactions") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_interactions_winter_11502, select = 10, ylim = c(-10, 10))
          
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
        }
      }
    })
    
    
    ## Saale Salz ----
    output$map_saale_salz <- renderTmap(
      tm_shape(hydro_bavaria)+
        tm_polygons(id = "NameString")+
        tm_shape(waterways_three)+
        tm_lines(col = "blue")+
        tm_shape(pegel_prop_sf[3,])+
        tm_markers(size = 0.3)
    )
    ## Iller Kempten ----
    output$map_iller_kempten <- renderTmap(
      tm_shape(hydro_bavaria)+
        tm_polygons(id = "NameString")+
        tm_shape(waterways_three)+
        tm_lines(col = "blue")+
        tm_shape(pegel_prop_sf[2,])+
        tm_markers(size = 0.3)
    )
    ## Isar Mittenwald ----
    output$map_isar_mittenwald <- renderTmap(
      tm_shape(hydro_bavaria)+
        tm_polygons(id = "NameString")+
        tm_shape(waterways_three)+
        tm_lines(col = "blue")+
        tm_shape(pegel_prop_sf[1,])+
        tm_markers(size = 0.3)
    )
    # Model tables
    output$model_tab_summer <- renderUI({
      if (input$model_catchment %in% "Fränkische Saale Salz") {
        if (input$model_selection %in% "Full Model") {
          return(HTML(as.character(tab_model(gam2_all_summer_20203, show.aic = TRUE))[4][1]))
        }
        if (input$model_selection %in% "Trimmed Model") {
          return(HTML(as.character(tab_model(gam_selected_summer_20203, show.aic = TRUE))[4][1]))
        }
        if (input$model_selection %in% "Interactions") {
          return(HTML(as.character(tab_model(gam_selected_interact_summer_20203, show.aic = TRUE))[4][1]))
        }
      }
      if (input$model_catchment %in% "Iller Kempten") {
        if (input$model_selection %in% "Full Model") {
          return(HTML(as.character(tab_model(gam_all_summer_11502, show.aic = TRUE))[4][1]))
        }
        if (input$model_selection %in% "Trimmed Model") {
          return(HTML(as.character(tab_model(gam_trimmed_summer_11502, show.aic = TRUE))[4][1]))
        }
        if (input$model_selection %in% "Interactions") {
          return(HTML(as.character(tab_model(gam_interactions_summer_11502, show.aic = TRUE))[4][1]))
        }
      }
      if (input$model_catchment %in% "Isar Mittenwald") {
        if (input$model_selection %in% "Full Model") {
          return(HTML(as.character(tab_model(gam_all_summer_10304, show.aic = TRUE))[4][1]))
        }
        if (input$model_selection %in% "Trimmed Model") {
          return(HTML(as.character(tab_model(gam_selected_summer_10304, show.aic = TRUE))[4][1]))
        }
        
        if (input$model_selection %in% "Interactions") {
          return(HTML(as.character(tab_model(gam_selected_interac_summer_10304, show.aic = TRUE))[4][1]))
        }
      }
    })
    output$model_tab_winter <- renderUI({
      if (input$model_catchment %in% "Fränkische Saale Salz") {
        if (input$model_selection %in% "Full Model") {
          return(HTML(as.character(tab_model(gam2_all_winter_20203, show.aic = TRUE))[4][1]))
        }
        if (input$model_selection %in% "Trimmed Model") {
          return(HTML(as.character(tab_model(gam_selected_winter_20203, show.aic = TRUE))[4][1]))
        }
        if (input$model_selection %in% "Interactions") {
          return(HTML(as.character(tab_model(gam_selected_interact_winter_20203, show.aic = TRUE))[4][1]))
        }
      }
      if (input$model_catchment %in% "Iller Kempten") {
        if (input$model_selection %in% "Full Model") {
          return(HTML(as.character(tab_model(gam_all_winter_11502, show.aic = TRUE))[4][1]))
        }
        if (input$model_selection %in% "Trimmed Model") {
          return(HTML(as.character(tab_model(gam_trimmed_winter_11502, show.aic = TRUE))[4][1]))
        }
        if (input$model_selection %in% "Interactions") {
          return(HTML(as.character(tab_model(gam_interactions_winter_11502, show.aic = TRUE))[4][1]))
        }
      }
      if (input$model_catchment %in% "Isar Mittenwald") {
        if (input$model_selection %in% "Full Model") {
          return(HTML(as.character(tab_model(gam_all_winter_10304, show.aic = TRUE))[4][1]))
        }
        if (input$model_selection %in% "Trimmed Model") {
          return(HTML(as.character(tab_model(gam_selected_winter_10304, show.aic = TRUE))[4][1]))
        }
        
        if (input$model_selection %in% "Interactions") {
          return(HTML(as.character(tab_model(gam_selected_interac_winter_10304, show.aic = TRUE))[4][1]))
        }
      }
    })
})
