library(shiny)
library(tmap)
tmap_options(check.and.fix = TRUE)
library(tidyverse)
library(sf)
library(DT)
library(mgcv)
library(sjPlot)
library(slickR)
library(corrplot)
library(verification)
# Data Read in ----
source("data_read.R")
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
                 y = "Jährliches durchschnittliches Minimum \ndes täglichem Grundwasserstandes \n(in m unter Oberfläche)",
                 x = "Datum") +
            guides(color=guide_legend(title="Pegel")) +
            theme(text = element_text(size = 15),
                  legend.position="bottom") +
            scale_color_brewer(palette = "Dark2") +
            facet_wrap(vars(hydro_year), labeller = as_labeller(c(`summer` = "Sommer", `winter` = "Winter")))
          return(min_plot_groundwaterdepth_facet)
        } else {
           min_plot_groundwaterdepth <- ggplot(data = table_yearly_avg_min_groundwaterdepth, mapping = aes(x = YY, y = avg_min_groundwaterdepth, color = waterlevel)) +
            geom_line() +
            geom_point() +
            labs(title = "Jährliches durchschnittliches Minimum des Grundwasserstandes \nje Jahreszeit im Zeitverlauf (aller Member)",
                 y = "Jährliches durchschnittliches Minimum \ndes täglichem Grundwasserstandes \n(in m unter Oberfläche)",
                 x = "Datum") +
            guides(color=guide_legend(title="Pegel")) +
            theme(text = element_text(size = 15),
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
                 y = "Jährliches durchschnittliches Minimum \nder täglichen oberflächennahen \nrelativen Bodenfeuchte (in Prozentpunkten)",
                 x = "Datum") +
            guides(color=guide_legend(title="Pegel")) +
            theme(text = element_text(size = 15),
                  legend.position="bottom") +
            scale_color_brewer(palette = "Dark2") +
            facet_wrap(vars(hydro_year), labeller = as_labeller(c(`summer` = "Sommer", `winter` = "Winter")))
          return(min_plot_soilwater_facet)
        } else {
          min_plot_soilwater <- ggplot(data = table_yearly_avg_min_soilwater, mapping = aes(x = YY, y = avg_min_soilwater, color = waterlevel)) +
            geom_line() +
            geom_point() +
            labs(title = "Jährliches durchschnittliches Minimum der täglichen oberflächennahen \nrelativen Bodenfeuchte je Jahreszeit im Zeitverlauf (aller Member)",
                 y = "Jährliches durchschnittliches Minimum \nder täglichen oberflächennahen \nrelativen Bodenfeuchte (in Prozentpunkten)",
                 x = "Datum") +
            guides(color=guide_legend(title="Pegel")) +
            theme(text = element_text(size = 15),
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
                 y = "Jährliches durchschnittliches Minimum \nan täglichem Schneespeicher \n(in mm)",
                 x = "Datum") +
            guides(color=guide_legend(title="Pegel")) +
            theme(text = element_text(size = 15),
                  legend.position="bottom") +
            scale_color_brewer(palette = "Dark2") +
            facet_wrap(vars(hydro_year), labeller = as_labeller(c(`summer` = "Sommer", `winter` = "Winter")))
          return(min_plot_snowstorage_facet)
        } else {
          min_plot_snowstorage <- ggplot(data = table_yearly_avg_min_snowstorage, mapping = aes(x = YY, y = avg_min_snowstorage, color = waterlevel)) +
            geom_line() +
            geom_point() +
            labs(title = "Jährliches durchschnittliches Minimum an täglichem Schneespeicher \nje Jahreszeit im Zeitverlauf (aller Member)",
                 y = "Jährliches durchschnittliches Minimum \nan täglichem Schneespeicher \n(in mm)",
                 x = "Datum") +
            guides(color=guide_legend(title="Pegel")) +
            theme(text = element_text(size = 15),
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
                 y = "Jährliches durchschnittliches Minimum \nan täglicher Lufttemperatur \n(in °C)",
                 x = "Datum") +
            guides(color=guide_legend(title="Pegel")) +
            theme(text = element_text(size = 15),
                  legend.position="bottom") +
            scale_color_brewer(palette = "Dark2") +
            facet_wrap(vars(hydro_year), labeller = as_labeller(c(`summer` = "Sommer", `winter` = "Winter")))
          return(min_plot_airtmp_facet)
        } else {
          min_plot_airtmp <- ggplot(data = table_yearly_avg_min_airtmp, mapping = aes(x = YY, y = avg_min_airtmp, color = waterlevel)) +
            geom_line() +
            geom_point() +
            labs(title = "Jährliches durchschnittliches Minimum an täglicher Lufttemperatur \nje Jahreszeit im Zeitverlauf (aller Member)",
                 y = "Jährliches durchschnittliches Minimum \nan täglicher Lufttemperatur \n(in °C)",
                 x = "Datum") +
            guides(color=guide_legend(title="Pegel")) +
            theme(text = element_text(size = 15),
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
                 y = "Jährliches durchschnittliches Minimum \nan täglichem Niederschlag \n(in mm/24h)",
                 x = "Datum") +
            guides(color=guide_legend(title="Pegel")) +
            theme(text = element_text(size = 15),
                  legend.position="bottom") +
            scale_color_brewer(palette = "Dark2") +
            facet_wrap(vars(hydro_year), labeller = as_labeller(c(`summer` = "Sommer", `winter` = "Winter")))
          return(min_plot_precip_facet)
        } else {
          min_plot_precip <- ggplot(data = table_yearly_avg_min_precip, mapping = aes(x = YY, y = avg_min_precip, color = waterlevel)) +
            geom_line() +
            geom_point() +
            labs(title = "Jährliches durchschnittliches Minimum an täglichem Niederschlag \nje Jahreszeit im Zeitverlauf (aller Member)",
                 y = "Jährliches durchschnittliches Minimum \nan täglichem Niederschlag \n(in mm/24h)",
                 x = "Datum") +
            guides(color=guide_legend(title="Pegel")) +
            theme(text = element_text(size = 15),
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
                 y = "Jährliches durchschnittliches Miniumum \nan täglich einfallender \nkurzwelligen Strahlung (in Wh/m²)",
                 x = "Datum") +
            guides(color=guide_legend(title="Pegel")) +
            theme(text = element_text(size = 15),
                  legend.position="bottom") +
            scale_color_brewer(palette = "Dark2") +
            facet_wrap(vars(hydro_year), labeller = as_labeller(c(`summer` = "Sommer", `winter` = "Winter")))
          return(min_plot_glorad_facet)
        } else {
          min_plot_glorad <- ggplot(data = table_yearly_avg_min_glorad, mapping = aes(x = YY, y = avg_min_glorad, color = waterlevel)) +
            geom_line() +
            geom_point() +
            labs(title = "Jährliches durchschnittliches Miniumum an täglich einfallender \nkurzwelligen Strahlung je Jahreszeit im Zeitverlauf (aller Member)",
                 y = "Jährliches durchschnittliches Miniumum \nan täglich einfallender \nkurzwelligen Strahlung (in Wh/m²)",
                 x = "Datum") +
            guides(color=guide_legend(title="Pegel")) +
            theme(text = element_text(size = 15),
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
            labs(title = "Jährliches durchschnittliches Minimum an täglicher Sickerung \nje Jahreszeit im Zeitverlauf (aller Member)",
                 y = "Jährliches durchschnittliches Minimum \nan täglicher Sickerung \n(in mm/24h)",
                 x = "Datum") +
            guides(color=guide_legend(title="Pegel")) +
            theme(text = element_text(size = 15),
                  legend.position="bottom") +
            scale_color_brewer(palette = "Dark2") +
            facet_wrap(vars(hydro_year), labeller = as_labeller(c(`summer` = "Sommer", `winter` = "Winter")))
          return(min_plot_infiltration_facet)
        } else {
          min_plot_infiltration <- ggplot(data = table_yearly_avg_min_infiltration, mapping = aes(x = YY, y = avg_min_infiltration, color = waterlevel)) +
            geom_line() +
            geom_point() +
            labs(title = "Jährliches durchschnittliches Minimum an täglicher Sickerung \nje Jahreszeit im Zeitverlauf (aller Member)",
                 y = "Jährliches durchschnittliches Minimum \nan täglicher Sickerung \n(in mm/24h)",
                 x = "Datum") +
            guides(color=guide_legend(title="Pegel")) +
            theme(text = element_text(size = 15),
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
                 y = "Jährliches durchschnittliches Maximum \ndes täglichem Grundwasserstandes \n(in m unter Oberfläche)",
                 x = "Datum") +
            guides(color=guide_legend(title="Pegel")) +
            theme(text = element_text(size = 15),
                  legend.position="bottom") +
            scale_color_brewer(palette = "Dark2") +
            facet_wrap(vars(hydro_year), labeller = as_labeller(c(`summer` = "Sommer", `winter` = "Winter")))
          return(max_plot_groundwaterdepth_facet)
        } else {
          max_plot_groundwaterdepth <- ggplot(data = table_yearly_avg_max_groundwaterdepth, mapping = aes(x = YY, y = avg_max_groundwaterdepth, color = waterlevel)) +
            geom_line() +
            geom_point() +
            labs(title = "Jährliches durchschnittliches Maximum des Grundwasserstandes \nje Jahreszeit im Zeitverlauf (aller Member)",
                 y = "Jährliches durchschnittliches Maximum \ndes täglichem Grundwasserstandes \n(in m unter Oberfläche)",
                 x = "Datum") +
            guides(color=guide_legend(title="Pegel")) +
            theme(text = element_text(size = 15),
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
                 y = "Jährliches durchschnittliches Maximum \nder täglichen oberflächennahen \nrelativen Bodenfeuchte (in Prozentpunkten)",
                 x = "Datum") +
            guides(color=guide_legend(title="Pegel")) +
            theme(text = element_text(size = 15),
                  legend.position="bottom") +
            scale_color_brewer(palette = "Dark2") +
            facet_wrap(vars(hydro_year), labeller = as_labeller(c(`summer` = "Sommer", `winter` = "Winter")))
          return(max_plot_soilwater_facet)
        } else {
          max_plot_soilwater <- ggplot(data = table_yearly_avg_max_soilwater, mapping = aes(x = YY, y = avg_max_soilwater, color = waterlevel)) +
            geom_line() +
            geom_point() +
            labs(title = "Jährliches durchschnittliches Maximum der täglichen oberflächennahen \nrelativen Bodenfeuchte je Jahreszeit im Zeitverlauf (aller Member)",
                 y = "Jährliches durchschnittliches Maximum \nder täglichen oberflächennahen \nrelativen Bodenfeuchte (in Prozentpunkten)",
                 x = "Datum") +
            guides(color=guide_legend(title="Pegel")) +
            theme(text = element_text(size = 15),
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
                 y = "Jährliches durchschnittliches Maximum \nan täglichem Schneespeicher \n(in mm)",
                 x = "Datum") +
            guides(color=guide_legend(title="Pegel")) +
            theme(text = element_text(size = 15),
                  legend.position="bottom") +
            scale_color_brewer(palette = "Dark2") +
            facet_wrap(vars(hydro_year), labeller = as_labeller(c(`summer` = "Sommer", `winter` = "Winter")))
          return(max_plot_snowstorage_facet)
        } else {
          max_plot_snowstorage <- ggplot(data = table_yearly_avg_max_snowstorage, mapping = aes(x = YY, y = avg_max_snowstorage, color = waterlevel)) +
            geom_line() +
            geom_point() +
            labs(title = "Jährliches durchschnittliches Maximum an täglichem Schneespeicher \nje Jahreszeit im Zeitverlauf (aller Member)",
                 y = "Jährliches durchschnittliches Maximum \nan täglichem Schneespeicher \n(in mm)",
                 x = "Datum") +
            guides(color=guide_legend(title="Pegel")) +
            theme(text = element_text(size = 15),
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
                 y = "Jährliches durchschnittliches Maximum \nan täglicher Lufttemperatur \n(in °C)",
                 x = "Datum") +
            guides(color=guide_legend(title="Pegel")) +
            theme(text = element_text(size = 15),
                  legend.position="bottom") +
            scale_color_brewer(palette = "Dark2") +
            facet_wrap(vars(hydro_year), labeller = as_labeller(c(`summer` = "Sommer", `winter` = "Winter")))
          return(max_plot_airtmp_facet)
        } else {
          max_plot_airtmp <- ggplot(data = table_yearly_avg_max_airtmp, mapping = aes(x = YY, y = avg_max_airtmp, color = waterlevel)) +
            geom_line() +
            geom_point() +
            labs(title = "Jährliches durchschnittliches Maximum an täglicher Lufttemperatur \nje Jahreszeit im Zeitverlauf (aller Member)",
                 y = "Jährliches durchschnittliches Maximum \nan täglicher Lufttemperatur \n(in °C)",
                 x = "Datum") +
            guides(color=guide_legend(title="Pegel")) +
            theme(text = element_text(size = 15),
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
                 y = "Jährliches durchschnittliches Maximum \nan täglichem Niederschlag \n(in mm/24h)",
                 x = "Datum") +
            guides(color=guide_legend(title="Pegel")) +
            theme(text = element_text(size = 15),
                  legend.position="bottom") +
            scale_color_brewer(palette = "Dark2") +
            facet_wrap(vars(hydro_year), labeller = as_labeller(c(`summer` = "Sommer", `winter` = "Winter")))
          return(max_plot_precip_facet)
        } else {
          max_plot_precip <- ggplot(data = table_yearly_avg_max_precip, mapping = aes(x = YY, y = avg_max_precip, color = waterlevel)) +
            geom_line() +
            geom_point() +
            labs(title = "Jährliches durchschnittliches Maximum an täglichem Niederschlag \nje Jahreszeit im Zeitverlauf (aller Member)",
                 y = "Jährliches durchschnittliches Maximum \nan täglichem Niederschlag \n(in mm/24h)",
                 x = "Datum") +
            guides(color=guide_legend(title="Pegel")) +
            theme(text = element_text(size = 15),
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
                 y = "Jährliches durchschnittliches Maximum \nan täglich einfallender \nkurzwelligen Strahlung (in Wh/m²)",
                 x = "Datum") +
            guides(color=guide_legend(title="Pegel")) +
            theme(text = element_text(size = 15),
                  legend.position="bottom") +
            scale_color_brewer(palette = "Dark2") +
            facet_wrap(vars(hydro_year), labeller = as_labeller(c(`summer` = "Sommer", `winter` = "Winter")))
          return(max_plot_glorad_facet)
        } else {
          max_plot_glorad <- ggplot(data = table_yearly_avg_max_glorad, mapping = aes(x = YY, y = avg_max_glorad, color = waterlevel)) +
            geom_line() +
            geom_point() +
            labs(title = "Jährliches durchschnittliches Maximum an täglich einfallender \nkurzwelligen Strahlung je Jahreszeit im Zeitverlauf (aller Member)",
                 y = "Jährliches durchschnittliches Maximum \nan täglich einfallender \nkurzwelligen Strahlung (in Wh/m²)",
                 x = "Datum") +
            guides(color=guide_legend(title="Pegel")) +
            theme(text = element_text(size = 15),
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
            labs(title = "Jährliches durchschnittliches Maximum an täglicher Sickerung \nje Jahreszeit im Zeitverlauf (aller Member)",
                 y = "Jährliches durchschnittliches Maximum \nan täglicher Sickerung \n(in mm/24h)",
                 x = "Datum") +
            guides(color=guide_legend(title="Pegel")) +
            theme(text = element_text(size = 15),
                  legend.position="bottom") +
            scale_color_brewer(palette = "Dark2") +
            facet_wrap(vars(hydro_year), labeller = as_labeller(c(`summer` = "Sommer", `winter` = "Winter")))
          return(max_plot_infiltration_facet)
        } else {
          max_plot_infiltration <- ggplot(data = table_yearly_avg_max_infiltration, mapping = aes(x = YY, y = avg_max_infiltration, color = waterlevel)) +
            geom_line() +
            geom_point() +
            labs(title = "Jährliches durchschnittliches Maximum an täglicher Sickerung \nje Jahreszeit im Zeitverlauf (aller Member)",
                 y = "Jährliches durchschnittliches Maximum \nan täglicher Sickerung \n(in mm/24h)",
                 x = "Datum") +
            guides(color=guide_legend(title="Pegel")) +
            theme(text = element_text(size = 15),
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
          return(summary(gam_all_summer_20203))
        }
        if (input$model_selection %in% "Selected Model 1") {
          return(summary(gam_uni_selected_summer_20203))
        }
        if (input$model_selection %in% "Selected Model 2") {
          return(summary(gam2_uni_selected_summer_20203))
        }
        if (input$model_selection %in% "Interactions") {
          return(summary(gam_uni_selected_interac_summer_20203))
        }
      }
      if (input$model_catchment %in% "Iller Kempten") {
        if (input$model_selection %in% "Full Model") {
          return(summary(gam_all_summer_11502))
        }
        if (input$model_selection %in% "Selected Model 1") {
          return(summary(gam_uni_selected_summer_11502))
        }
        if (input$model_selection %in% "Selected Model 2") {
          return(summary(gam2_uni_selected_summer_11502))
        }
        if (input$model_selection %in% "Interactions") {
          return(summary(gam_uni_selected_interac_summer_11502))
        }
      }
      if (input$model_catchment %in% "Isar Mittenwald") {
        if (input$model_selection %in% "Full Model") {
          return(summary(gam_all_summer_10304))
        }
        if (input$model_selection %in% "Selected Model 1") {
          return(summary(gam_uni_selected_summer_10304))
        }
        if (input$model_selection %in% "Selected Model 2") {
          return(summary(gam2_uni_selected_summer_10304))
        }
        if (input$model_selection %in% "Interactions") {
          return(summary(gam_uni_selected_interac_summer_10304))
        }
      }
    })
    output$model_summary_winter <- renderPrint({
      req(input$model_summary)
      if (input$model_catchment %in% "Fränkische Saale Salz") {
        if (input$model_selection %in% "Full Model") {
          return(summary(gam_all_winter_20203))
        }
        if (input$model_selection %in% "Selected Model 1") {
          return(summary(gam_uni_selected_winter_20203))
        }
        if (input$model_selection %in% "Selected Model 2") {
          return(summary(gam2_uni_selected_winter_20203))
        }
        if (input$model_selection %in% "Interactions") {
          return(summary(gam_uni_selected_interac_winter_20203))
        }
      }
      if (input$model_catchment %in% "Iller Kempten") {
        if (input$model_selection %in% "Full Model") {
          return(summary(gam_all_winter_11502))
        }
        if (input$model_selection %in% "Selected Model 1") {
          return(summary(gam_uni_selected_winter_11502))
        }
        if (input$model_selection %in% "Selected Model 2") {
          return(summary(gam2_uni_selected_winter_11502))
        }
        if (input$model_selection %in% "Interactions") {
          return(summary(gam_uni_selected_interac_winter_11502))
        }
      }
      if (input$model_catchment %in% "Isar Mittenwald") {
        if (input$model_selection %in% "Full Model") {
          return(summary(gam_all_winter_10304))
        }
        if (input$model_selection %in% "Selected Model 1") {
          return(summary(gam_uni_selected_winter_10304))
        }
        if (input$model_selection %in% "Selected Model 2") {
          return(summary(gam2_uni_selected_winter_10304))
        }
        if (input$model_selection %in% "Interactions") {
          return(summary(gam_uni_selected_interac_winter_10304))
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
          plot(gam_all_summer_20203, select = 1, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_all_summer_11502, select = 1, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_all_summer_10304, select = 1, ylim = input$model_range, ylab = "log. Odds")
        }}
      ##### Trimmed Model ----
      if (input$model_selection %in% "Selected Model 1") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_uni_selected_summer_20203, select = 1, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_uni_selected_summer_11502, select = 1, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_uni_selected_summer_10304, select = 1, ylim = input$model_range, ylab = "log. Odds")
        }
      }
      ##### Trimmed Model 2 ----
      if (input$model_selection %in% "Selected Model 2") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam2_uni_selected_summer_20203, select = 1, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam2_uni_selected_summer_11502, select = 1, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam2_uni_selected_summer_10304, select = 1, ylim = input$model_range, ylab = "log. Odds")
        }
      }
      ##### Interactions ----
      if (input$model_selection %in% "Interactions") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_uni_selected_interac_summer_20203, select = 1, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_uni_selected_interac_summer_11502, select = 1, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_uni_selected_interac_summer_10304, select = 1, ylim = input$model_range, ylab = "log. Odds")
        }
      }
    })
    #### Effect Plot 2 ----
    output$model_effect_summer_2 <- renderPlot({
      req(input$effect_plots)
      ###### Full Model ----
      if (input$model_selection %in% "Full Model") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_all_summer_20203, select = 2, ylim = input$model_range, ylab = "log. Odds")        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_all_summer_11502, select = 2, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_all_summer_10304, select = 2, ylim = input$model_range, ylab = "log. Odds")
        }}
      ##### Trimmed Model ----
      if (input$model_selection %in% "Selected Model 1") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_uni_selected_summer_20203, select = 2, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_uni_selected_summer_11502, select = 2, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_uni_selected_summer_10304, select = 2, ylim = input$model_range, ylab = "log. Odds")
        }
      }
      ##### Trimmed Model 2 ----
      if (input$model_selection %in% "Selected Model 2") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam2_uni_selected_summer_20203, select = 2, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam2_uni_selected_summer_11502, select = 2, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam2_uni_selected_summer_10304, select = 2, ylim = input$model_range, ylab = "log. Odds")
        }
      }
      ##### Interactions ----
      if (input$model_selection %in% "Interactions") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_uni_selected_interac_summer_20203, select = 2, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_uni_selected_interac_summer_11502, select = 2, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_uni_selected_interac_summer_10304, select = 2, ylim = input$model_range, ylab = "log. Odds")
        }
      }
    })
    #### Effect Plot 3 ----
    output$model_effect_summer_3 <- renderPlot({
      req(input$effect_plots)
      ###### Full Model ----
      if (input$model_selection %in% "Full Model") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_all_summer_20203, select = 3, ylim = input$model_range, ylab = "log. Odds")        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_all_summer_11502, select = 3, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_all_summer_10304, select = 3, ylim = input$model_range, ylab = "log. Odds")
        }}
      ##### Trimmed Model ----
      if (input$model_selection %in% "Selected Model 1") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_uni_selected_summer_20203, select = 3, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_uni_selected_summer_11502, select = 3, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_uni_selected_summer_10304, select = 3, ylim = input$model_range, ylab = "log. Odds")
        }
      }
      ##### Trimmed Model 2 ----
      if (input$model_selection %in% "Selected Model 2") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam2_uni_selected_summer_20203, select = 3, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam2_uni_selected_summer_11502, select = 3, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam2_uni_selected_summer_10304, select = 3, ylim = input$model_range, ylab = "log. Odds")
        }
      }
      ##### Interactions ----
      if (input$model_selection %in% "Interactions") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_uni_selected_interac_summer_20203, select = 3, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_uni_selected_interac_summer_11502, select = 3, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_uni_selected_interac_summer_10304, select = 3, ylim = input$model_range, ylab = "log. Odds")
        }
      }
    })
    #### Effect Plot 4 ----
    output$model_effect_summer_4 <- renderPlot({
      req(input$effect_plots)
      ###### Full Model ----
      if (input$model_selection %in% "Full Model") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_all_summer_20203, select = 4, ylim = input$model_range, ylab = "log. Odds")        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_all_summer_11502, select = 4, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_all_summer_10304, select = 4, ylim = input$model_range, ylab = "log. Odds")
        }}
      ##### Trimmed Model ----
      if (input$model_selection %in% "Selected Model 1") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_uni_selected_summer_20203, select = 4, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_uni_selected_summer_11502, select = 4, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_uni_selected_summer_10304, select = 4, ylim = input$model_range, ylab = "log. Odds")
        }
      }
      ##### Trimmed Model 2 ----
      if (input$model_selection %in% "Selected Model 2") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam2_uni_selected_summer_20203, select = 4, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam2_uni_selected_summer_11502, select = 4, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam2_uni_selected_summer_10304, select = 4, ylim = input$model_range, ylab = "log. Odds")
        }
      }
      ##### Interactions ----
      if (input$model_selection %in% "Interactions") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_uni_selected_interac_summer_20203, select = 4, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_uni_selected_interac_summer_11502, select = 4, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_uni_selected_interac_summer_10304, select = 4, ylim = input$model_range, ylab = "log. Odds")
        }
      }
    })
    #### Effect Plot 5 ----
    output$model_effect_summer_5 <- renderPlot({
      req(input$effect_plots)
      ###### Full Model ----
      if (input$model_selection %in% "Full Model") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_all_summer_20203, select = 5, ylim = input$model_range, ylab = "log. Odds")        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_all_summer_11502, select = 5, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_all_summer_10304, select = 5, ylim = input$model_range, ylab = "log. Odds")
        }}
      ##### Trimmed Model ----
      if (input$model_selection %in% "Selected Model 1") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_uni_selected_summer_20203, select = 5, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_uni_selected_summer_11502, select = 5, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_uni_selected_summer_10304, select = 5, ylim = input$model_range, ylab = "log. Odds")
        }
      }
      ##### Trimmed Model 2 ----
      if (input$model_selection %in% "Selected Model 2") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam2_uni_selected_summer_20203, select = 5, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam2_uni_selected_summer_11502, select = 5, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam2_uni_selected_summer_10304, select = 5, ylim = input$model_range, ylab = "log. Odds")
        }
      }
      ##### Interactions ----
      if (input$model_selection %in% "Interactions") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_uni_selected_interac_summer_20203, select = 5, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_uni_selected_interac_summer_11502, select = 5, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_uni_selected_interac_summer_10304, select = 5, ylim = input$model_range, ylab = "log. Odds")
        }
      }
    })
    #### Effect Plot 6 ----
    output$model_effect_summer_6 <- renderPlot({
      req(input$effect_plots)
      ###### Full Model ----
      if (input$model_selection %in% "Full Model") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_all_summer_20203, select = 6, ylim = input$model_range, ylab = "log. Odds")        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_all_summer_11502, select = 6, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_all_summer_10304, select = 6, ylim = input$model_range, ylab = "log. Odds")
        }}
      ##### Trimmed Model ----
      if (input$model_selection %in% "Selected Model 1") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_uni_selected_summer_20203, select = 6, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_uni_selected_summer_11502, select = 6, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_uni_selected_summer_10304, select = 6, ylim = input$model_range, ylab = "log. Odds")
        }
      }
      ##### Trimmed Model 2 ----
      if (input$model_selection %in% "Selected Model 2") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam2_uni_selected_summer_20203, select = 6, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam2_uni_selected_summer_11502, select = 6, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam2_uni_selected_summer_10304, select = 6, ylim = input$model_range, ylab = "log. Odds")
        }
      }
      ##### Interactions ----
      if (input$model_selection %in% "Interactions") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_uni_selected_interac_summer_20203, select = 6, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_uni_selected_interac_summer_11502, select = 6, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_uni_selected_interac_summer_10304, select = 6, ylim = input$model_range, ylab = "log. Odds")
        }
      }
    })
    #### Effect Plot 7 ----
    output$model_effect_summer_7 <- renderPlot({
      req(input$effect_plots)
      ###### Full Model ----
      if (input$model_selection %in% "Full Model") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_all_summer_20203, select = 7, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_all_summer_11502, select = 7, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_all_summer_10304, select = 7, ylim = input$model_range, ylab = "log. Odds")
        }}
      ##### Trimmed Model ----
      if (input$model_selection %in% "Selected Model 1") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_uni_selected_summer_20203, select = 7, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_uni_selected_summer_11502, select = 7, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_uni_selected_summer_10304, select = 7, ylim = input$model_range, ylab = "log. Odds")
        }
      }
      ##### Trimmed Model 2 ----
      if (input$model_selection %in% "Selected Model 2") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam2_uni_selected_summer_20203, select = 7, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam2_uni_selected_summer_11502, select = 7, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam2_uni_selected_summer_10304, select = 7, ylim = input$model_range, ylab = "log. Odds")
        }
      }
      ##### Interactions ----
      if (input$model_selection %in% "Interactions") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_uni_selected_interac_summer_20203, select = 7, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_uni_selected_interac_summer_11502, select = 7, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_uni_selected_interac_summer_10304, select = 7, ylim = input$model_range, ylab = "log. Odds")
        }
      }
    })
    #### Effect Plot 8 ----
    output$model_effect_summer_8 <- renderPlot({
      req(input$effect_plots)
      ###### Full Model ----
      if (input$model_selection %in% "Full Model") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_all_summer_20203, select = 8, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_all_summer_11502, select = 8, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_all_summer_10304, select = 8, ylim = input$model_range, ylab = "log. Odds")
        }}
      ##### Trimmed Model ----
      if (input$model_selection %in% "Selected Model 1") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_uni_selected_summer_20203, select = 8, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_uni_selected_summer_11502, select = 8, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_uni_selected_summer_10304, select = 8, ylim = input$model_range, ylab = "log. Odds")
        }
      }
      ##### Trimmed Model 2 ----
      if (input$model_selection %in% "Selected Model 2") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam2_uni_selected_summer_20203, select = 8, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam2_uni_selected_summer_11502, select = 8, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam2_uni_selected_summer_10304, select = 8, ylim = input$model_range, ylab = "log. Odds")
        }
      }
      ##### Interactions ----
      if (input$model_selection %in% "Interactions") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_uni_selected_interac_summer_20203, select = 8, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_uni_selected_interac_summer_11502, select = 8, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_uni_selected_interac_summer_10304, select = 8, ylim = input$model_range, ylab = "log. Odds")
        }
      }
    })
    #### Effect Plot 9 ----
    output$model_effect_summer_9 <- renderPlot({
      req(input$effect_plots)
      ###### Full Model ----
      if (input$model_selection %in% "Full Model") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_all_summer_20203, select = 9, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_all_summer_11502, select = 9, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_all_summer_10304, select = 9, ylim = input$model_range, ylab = "log. Odds")
        }}
      ##### Trimmed Model ----
      if (input$model_selection %in% "Selected Model 1") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_uni_selected_summer_20203, select = 9, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_uni_selected_summer_11502, select = 9, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_uni_selected_summer_10304, select = 9, ylim = input$model_range, ylab = "log. Odds")
        }
      }
      ##### Trimmed Model 2 ----
      if (input$model_selection %in% "Selected Model 2") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam2_uni_selected_summer_20203, select = 9, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam2_uni_selected_summer_11502, select = 9, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam2_uni_selected_summer_10304, select = 9, ylim = input$model_range, ylab = "log. Odds")
        }
      }
      ##### Interactions ----
      if (input$model_selection %in% "Interactions") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_uni_selected_interac_summer_20203, select = 9, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_uni_selected_interac_summer_11502, select = 9, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_uni_selected_interac_summer_10304, select = 9, ylim = input$model_range, ylab = "log. Odds")
        }
      }
    })
    #### Effect Plot 10 ----
    output$model_effect_summer_10 <- renderPlot({
      req(input$effect_plots)
    ###### Full Model ----
      if (input$model_selection %in% "Full Model") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_all_summer_20203, select = 10, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_all_summer_11502, select = 10, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_all_summer_10304, select = 10, ylim = input$model_range, ylab = "log. Odds")
        }}
    ##### Trimmed Model ----
      if (input$model_selection %in% "Selected Model 1") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_uni_selected_summer_20203, select = 10, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_uni_selected_summer_11502, select = 10, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_uni_selected_summer_10304, select = 10, ylim = input$model_range, ylab = "log. Odds")
        }
      }
    ##### Trimmed Model 2 ----
      if (input$model_selection %in% "Selected Model 2") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam2_uni_selected_summer_20203, select = 10, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam2_uni_selected_summer_11502, select = 10, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam2_uni_selected_summer_10304, select = 10, ylim = input$model_range, ylab = "log. Odds")
        }
      }
    ##### Interactions ----
      if (input$model_selection %in% "Interactions") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_uni_selected_interac_summer_20203, select = 10, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_uni_selected_interac_summer_11502, select = 10, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_uni_selected_interac_summer_10304, select = 10, ylim = input$model_range, ylab = "log. Odds")
        }
      }
      })
    ## Winter ----
    output$slider_ui <- renderUI({
      req(input$effect_plots)
      sliderInput("model_range", label = "Wähle Skalierung für Effektplots y-Achse", min = -50, max = 50,  value = c(-7,7))
    })
    # Selected Saale 6 Isar 6 Iller
    #### Effect Plot 1 ----
    output$model_effect_winter_1 <- renderPlot({
      req(input$effect_plots)
      ###### Full Model ----
      if (input$model_selection %in% "Full Model") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_all_winter_20203, select = 1, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_all_winter_11502, select = 1, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_all_winter_10304, select = 1, ylim = input$model_range, ylab = "log. Odds")
        }}
      ##### Trimmed Model ----
      if (input$model_selection %in% "Selected Model 1") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_uni_selected_winter_20203, select = 1, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_uni_selected_winter_11502, select = 1, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_uni_selected_winter_10304, select = 1, ylim = input$model_range, ylab = "log. Odds")
        }
      }
      ##### Trimmed Model 2 ----
      if (input$model_selection %in% "Selected Model 2") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam2_uni_selected_winter_20203, select = 1, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam2_uni_selected_winter_11502, select = 1, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam2_uni_selected_winter_10304, select = 1, ylim = input$model_range, ylab = "log. Odds")
        }
      }
      ##### Interactions ----
      if (input$model_selection %in% "Interactions") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_uni_selected_interac_winter_20203, select = 1, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_uni_selected_interac_winter_11502, select = 1, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_uni_selected_interac_winter_10304, select = 1, ylim = input$model_range, ylab = "log. Odds")
        }
      }
    })
    #### Effect Plot 2 ----
    output$model_effect_winter_2 <- renderPlot({
      req(input$effect_plots)
      ###### Full Model ----
      if (input$model_selection %in% "Full Model") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_all_winter_20203, select = 2, ylim = input$model_range, ylab = "log. Odds")        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_all_winter_11502, select = 2, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_all_winter_10304, select = 2, ylim = input$model_range, ylab = "log. Odds")
        }}
      ##### Trimmed Model ----
      if (input$model_selection %in% "Selected Model 1") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_uni_selected_winter_20203, select = 2, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_uni_selected_winter_11502, select = 2, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_uni_selected_winter_10304, select = 2, ylim = input$model_range, ylab = "log. Odds")
        }
      }
      ##### Trimmed Model 2 ----
      if (input$model_selection %in% "Selected Model 2") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam2_uni_selected_winter_20203, select = 2, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam2_uni_selected_winter_11502, select = 2, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam2_uni_selected_winter_10304, select = 2, ylim = input$model_range, ylab = "log. Odds")
        }
      }
      ##### Interactions ----
      if (input$model_selection %in% "Interactions") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_uni_selected_interac_winter_20203, select = 2, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_uni_selected_interac_winter_11502, select = 2, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_uni_selected_interac_winter_10304, select = 2, ylim = input$model_range, ylab = "log. Odds")
        }
      }
    })
    #### Effect Plot 3 ----
    output$model_effect_winter_3 <- renderPlot({
      req(input$effect_plots)
      ###### Full Model ----
      if (input$model_selection %in% "Full Model") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_all_winter_20203, select = 3, ylim = input$model_range, ylab = "log. Odds")        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_all_winter_11502, select = 3, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_all_winter_10304, select = 3, ylim = input$model_range, ylab = "log. Odds")
        }}
      ##### Trimmed Model ----
      if (input$model_selection %in% "Selected Model 1") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_uni_selected_winter_20203, select = 3, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_uni_selected_winter_11502, select = 3, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_uni_selected_winter_10304, select = 3, ylim = input$model_range, ylab = "log. Odds")
        }
      }
      ##### Trimmed Model 2 ----
      if (input$model_selection %in% "Selected Model 2") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam2_uni_selected_winter_20203, select = 3, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam2_uni_selected_winter_11502, select = 3, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam2_uni_selected_winter_10304, select = 3, ylim = input$model_range, ylab = "log. Odds")
        }
      }
      ##### Interactions ----
      if (input$model_selection %in% "Interactions") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_uni_selected_interac_winter_20203, select = 3, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_uni_selected_interac_winter_11502, select = 3, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_uni_selected_interac_winter_10304, select = 3, ylim = input$model_range, ylab = "log. Odds")
        }
      }
    })
    #### Effect Plot 4 ----
    output$model_effect_winter_4 <- renderPlot({
      req(input$effect_plots)
      ###### Full Model ----
      if (input$model_selection %in% "Full Model") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_all_winter_20203, select = 4, ylim = input$model_range, ylab = "log. Odds")        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_all_winter_11502, select = 4, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_all_winter_10304, select = 4, ylim = input$model_range, ylab = "log. Odds")
        }}
      ##### Trimmed Model ----
      if (input$model_selection %in% "Selected Model 1") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_uni_selected_winter_20203, select = 4, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_uni_selected_winter_11502, select = 4, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_uni_selected_winter_10304, select = 4, ylim = input$model_range, ylab = "log. Odds")
        }
      }
      ##### Trimmed Model 2 ----
      if (input$model_selection %in% "Selected Model 2") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam2_uni_selected_winter_20203, select = 4, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam2_uni_selected_winter_11502, select = 4, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam2_uni_selected_winter_10304, select = 4, ylim = input$model_range, ylab = "log. Odds")
        }
      }
      ##### Interactions ----
      if (input$model_selection %in% "Interactions") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_uni_selected_interac_winter_20203, select = 4, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_uni_selected_interac_winter_11502, select = 4, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_uni_selected_interac_winter_10304, select = 4, ylim = input$model_range, ylab = "log. Odds")
        }
      }
    })
    #### Effect Plot 5 ----
    output$model_effect_winter_5 <- renderPlot({
      req(input$effect_plots)
      ###### Full Model ----
      if (input$model_selection %in% "Full Model") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_all_winter_20203, select = 5, ylim = input$model_range, ylab = "log. Odds")        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_all_winter_11502, select = 5, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_all_winter_10304, select = 5, ylim = input$model_range, ylab = "log. Odds")
        }}
      ##### Trimmed Model ----
      if (input$model_selection %in% "Selected Model 1") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_uni_selected_winter_20203, select = 5, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_uni_selected_winter_11502, select = 5, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_uni_selected_winter_10304, select = 5, ylim = input$model_range, ylab = "log. Odds")
        }
      }
      ##### Trimmed Model 2 ----
      if (input$model_selection %in% "Selected Model 2") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam2_uni_selected_winter_20203, select = 5, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam2_uni_selected_winter_11502, select = 5, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam2_uni_selected_winter_10304, select = 5, ylim = input$model_range, ylab = "log. Odds")
        }
      }
      ##### Interactions ----
      if (input$model_selection %in% "Interactions") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_uni_selected_interac_winter_20203, select = 5, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_uni_selected_interac_winter_11502, select = 5, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_uni_selected_interac_winter_10304, select = 5, ylim = input$model_range, ylab = "log. Odds")
        }
      }
    })
    #### Effect Plot 6 ----
    output$model_effect_winter_6 <- renderPlot({
      req(input$effect_plots)
      ###### Full Model ----
      if (input$model_selection %in% "Full Model") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_all_winter_20203, select = 6, ylim = input$model_range, ylab = "log. Odds")        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_all_winter_11502, select = 6, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_all_winter_10304, select = 6, ylim = input$model_range, ylab = "log. Odds")
        }}
      ##### Trimmed Model ----
      if (input$model_selection %in% "Selected Model 1") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_uni_selected_winter_20203, select = 6, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_uni_selected_winter_11502, select = 6, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_uni_selected_winter_10304, select = 6, ylim = input$model_range, ylab = "log. Odds")
        }
      }
      ##### Trimmed Model 2 ----
      if (input$model_selection %in% "Selected Model 2") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam2_uni_selected_winter_20203, select = 6, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam2_uni_selected_winter_11502, select = 6, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam2_uni_selected_winter_10304, select = 6, ylim = input$model_range, ylab = "log. Odds")
        }
      }
      ##### Interactions ----
      if (input$model_selection %in% "Interactions") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_uni_selected_interac_winter_20203, select = 6, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_uni_selected_interac_winter_11502, select = 6, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_uni_selected_interac_winter_10304, select = 6, ylim = input$model_range, ylab = "log. Odds")
        }
      }    })
    #### Effect Plot 7 ----
    output$model_effect_winter_7 <- renderPlot({
      req(input$effect_plots)
      ###### Full Model ----
      if (input$model_selection %in% "Full Model") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_all_winter_20203, select = 7, ylim = input$model_range, ylab = "log. Odds")        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_all_winter_11502, select = 7, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_all_winter_10304, select = 7, ylim = input$model_range, ylab = "log. Odds")
        }}
      ##### Trimmed Model ----
      if (input$model_selection %in% "Selected Model 1") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_uni_selected_winter_20203, select = 7, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_uni_selected_winter_11502, select = 7, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_uni_selected_winter_10304, select = 7, ylim = input$model_range, ylab = "log. Odds")
        }
      }
      ##### Trimmed Model 2 ----
      if (input$model_selection %in% "Selected Model 2") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam2_uni_selected_winter_20203, select = 7, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam2_uni_selected_winter_11502, select = 7, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam2_uni_selected_winter_10304, select = 7, ylim = input$model_range, ylab = "log. Odds")
        }
      }
      ##### Interactions ----
      if (input$model_selection %in% "Interactions") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_uni_selected_interac_winter_20203, select = 7, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_uni_selected_interac_winter_11502, select = 7, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_uni_selected_interac_winter_10304, select = 7, ylim = input$model_range, ylab = "log. Odds")
        }
      }    })
    #### Effect Plot 8 ----
    output$model_effect_winter_8 <- renderPlot({
      req(input$effect_plots)
      ###### Full Model ----
      if (input$model_selection %in% "Full Model") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_all_winter_20203, select = 8, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_all_winter_11502, select = 8, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_all_winter_10304, select = 8, ylim = input$model_range, ylab = "log. Odds")
        }}
      ##### Trimmed Model ----
      if (input$model_selection %in% "Selected Model 1") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_uni_selected_winter_20203, select = 8, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_uni_selected_winter_11502, select = 8, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_uni_selected_winter_10304, select = 8, ylim = input$model_range, ylab = "log. Odds")
        }
      }
      ##### Trimmed Model 2 ----
      if (input$model_selection %in% "Selected Model 2") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam2_uni_selected_winter_20203, select = 8, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam2_uni_selected_winter_11502, select = 8, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam2_uni_selected_winter_10304, select = 8, ylim = input$model_range, ylab = "log. Odds")
        }
      }
      ##### Interactions ----
      if (input$model_selection %in% "Interactions") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_uni_selected_interac_winter_20203, select = 8, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_uni_selected_interac_winter_11502, select = 8, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_uni_selected_interac_winter_10304, select = 8, ylim = input$model_range, ylab = "log. Odds")
        }
      }    })
    #### Effect Plot 9 ----
    output$model_effect_winter_9 <- renderPlot({
      req(input$effect_plots)
      ###### Full Model ----
      if (input$model_selection %in% "Full Model") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_all_winter_20203, select = 9, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_all_winter_11502, select = 9, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_all_winter_10304, select = 9, ylim = input$model_range, ylab = "log. Odds")
        }}
      ##### Trimmed Model ----
      if (input$model_selection %in% "Selected Model 1") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_uni_selected_winter_20203, select = 9, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_uni_selected_winter_11502, select = 9, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_uni_selected_winter_10304, select = 9, ylim = input$model_range, ylab = "log. Odds")
        }
      }
      ##### Trimmed Model 2 ----
      if (input$model_selection %in% "Selected Model 2") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam2_uni_selected_winter_20203, select = 9, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam2_uni_selected_winter_11502, select = 9, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam2_uni_selected_winter_10304, select = 9, ylim = input$model_range, ylab = "log. Odds")
        }
      }
      ##### Interactions ----
      if (input$model_selection %in% "Interactions") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_uni_selected_interac_winter_20203, select = 9, ylim = input$model_range)
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_uni_selected_interac_winter_11502, select = 9, ylim = input$model_range)
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_uni_selected_interac_winter_10304, select = 9, ylim = input$model_range)
        }
      }    })
    #### Effect Plot 10 ----
    output$model_effect_winter_10 <- renderPlot({
      req(input$effect_plots)
      ###### Full Model ----
      if (input$model_selection %in% "Full Model") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_all_winter_20203, select = 10, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_all_winter_11502, select = 10, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_all_winter_10304, select = 10, ylim = input$model_range, ylab = "log. Odds")
        }}
      ##### Trimmed Model ----
      if (input$model_selection %in% "Selected Model 1") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_uni_selected_winter_20203, select = 10, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_uni_selected_winter_11502, select = 10, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_uni_selected_winter_10304, select = 10, ylim = input$model_range, ylab = "log. Odds")
        }
      }
      ##### Trimmed Model 2 ----
      if (input$model_selection %in% "Selected Model 2") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam2_uni_selected_winter_20203, select = 10, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam2_uni_selected_winter_11502, select = 10, ylim = input$model_range, ylab = "log. Odds")
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam2_uni_selected_winter_10304, select = 10, ylim = input$model_range, ylab = "log. Odds")
        }
      }
      ##### Interactions ----
      if (input$model_selection %in% "Interactions") {
        if(input$model_catchment %in% "Fränkische Saale Salz"){
          plot(gam_uni_selected_interac_winter_20203, select = 10, ylim = input$model_range)
        }
        if (input$model_catchment %in% "Iller Kempten") {
          plot(gam_uni_selected_interac_winter_11502, select = 10, ylim = input$model_range)
        }
        if (input$model_catchment %in% "Isar Mittenwald") {
          plot(gam_uni_selected_interac_winter_10304, select = 10, ylim = input$model_range)
        }
      }    })
    
    # Maps for Catchmenttab ----
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
    # Model tables ----
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
    # Corr Plot ----
    ## Summer ----
    output$corr_hydro_summer <- renderPlot({
      if (input$corr_catchment %in% "Fränkische Saale Salz") {
        if (input$corr_corr %in% "Spearman") {
          correlationm_spearman_summer_20203 <- cor(subset_hydro_summer_20203, method = "spearman", use = "complete.obs")
          corrplot(correlationm_spearman_summer_20203, method = "number", type = "lower", tl.col = "black")
        }
        if (input$corr_corr %in% "Pearson") {
          correlationm_pearson_summer_20203 <- cor(subset_hydro_summer_20203, method = "pearson", use = "complete.obs")
          corrplot(correlationm_pearson_summer_20203, method = "number", type = "lower", tl.col = "black")
          
        }
      }
      
      if (input$corr_catchment %in% "Iller Kempten") {
        if (input$corr_corr %in% "Spearman") {
          correlationm_spearman_summer_11502 <- cor(subset_hydro_summer_11502, method = "spearman", use = "complete.obs")
          corrplot(correlationm_spearman_summer_11502, method = "number", type = "lower", tl.col = "black")
        }
        if (input$corr_corr %in% "Pearson") {
          correlationm_pearson_summer_11502 <- cor(subset_hydro_summer_11502, method = "pearson", use = "complete.obs")
          corrplot(correlationm_pearson_summer_11502, method = "number", type = "lower", tl.col = "black")
        }
      }

      if (input$corr_catchment %in% "Isar Mittenwald") {
        if (input$corr_corr %in% "Spearman") {
          correlationm_spearman_summer_10304 <- cor(subset_hydro_summer_10304, method = "spearman", use = "complete.obs")
          corrplot(correlationm_spearman_summer_10304, method = "number", type = "lower", tl.col = "black")
        }
        
        if (input$corr_corr %in% "Pearson") {
          correlationm_pearson_summer_10304 <- cor(subset_hydro_summer_10304, method = "pearson", use = "complete.obs")
          corrplot(correlationm_pearson_summer_10304, method = "number", type = "lower", tl.col = "black")
        }
      }
    })
    
    ## Winter ----
    output$corr_hydro_winter <- renderPlot({
      if (input$corr_catchment %in% "Fränkische Saale Salz") {
        if (input$corr_corr %in% "Spearman") {
          correlationm_spearman_winter_10304 <- cor(subset_hydro_winter_10304, method = "spearman", use = "complete.obs")
          corrplot(correlationm_spearman_winter_10304, method = "number", type = "lower", tl.col = "black")
        }
        if (input$corr_corr %in% "Pearson") {
          correlationm_pearson_winter_20203 <- cor(subset_hydro_winter_20203, method = "pearson", use = "complete.obs")
          corrplot(correlationm_pearson_winter_20203, method = "number", type = "lower", tl.col = "black")
        }
      }
      
      if (input$corr_catchment %in% "Iller Kempten") {
        if (input$corr_corr %in% "Spearman") {
          correlationm_spearman_winter_11502 <- cor(subset_hydro_winter_11502, method = "spearman", use = "complete.obs")
          corrplot(correlationm_spearman_winter_11502, method = "number", type = "lower", tl.col = "black")
        }
        if (input$corr_corr %in% "Pearson") {
          correlationm_pearson_winter_11502 <- cor(subset_hydro_winter_11502, method = "pearson", use = "complete.obs")
          corrplot(correlationm_pearson_winter_11502, method = "number", type = "lower", tl.col = "black")
        }
      }
      
      if (input$corr_catchment %in% "Isar Mittenwald") {
        if (input$corr_corr %in% "Spearman") {
          correlationm_spearman_winter_10304 <- cor(subset_hydro_winter_10304, method = "spearman", use = "complete.obs")
          corrplot(correlationm_spearman_winter_10304, method = "number", type = "lower", tl.col = "black")
        }
        
        if (input$corr_corr %in% "Pearson") {
          correlationm_pearson_winter_20203 <- cor(subset_hydro_winter_20203, method = "pearson", use = "complete.obs")
          corrplot(correlationm_pearson_winter_20203, method = "number", type = "lower", tl.col = "black")
        }
      }
    })
    
    # ROC Plots ----
    ## Summmer ----
    output$roc_summer <- renderPlot({
      if(input$roc_catchment %in% "Fränkische Saale Salz"){
        hydro_summer_20203_pred <- predict(gam_uni_selected_interac_summer_20203, hydro_summer_20203_test, type = "response")
        roc.plot(hydro_summer_20203_test$lowlevel, hydro_summer_20203_pred, xlab = "1 - Spezifität", ylab = "Sensitivität")
      }
      if (input$roc_catchment %in% "Iller Kempten") {
        hydro_summer_11502_pred <- predict(gam_uni_selected_interac_summer_11502, hydro_summer_11502_test, type = "response")
        roc.plot(hydro_summer_11502_test$lowlevel, hydro_summer_11502_pred, xlab = "1 - Spezifität", ylab = "Sensitivität")
      }
      if (input$roc_catchment %in% "Isar Mittenwald") {
        hydro_summer_10304_pred <- predict(gam_uni_selected_interac_summer_10304, hydro_summer_10304_test, type = "response")
        roc.plot(hydro_summer_10304_test$lowlevel, hydro_summer_10304_pred, xlab = "1 - Spezifität", ylab = "Sensitivität")
      }
    })
    
    ## Winter ----
    output$roc_winter <- renderPlot({
      if(input$roc_catchment %in% "Fränkische Saale Salz"){
        hydro_winter_20203_pred <- predict(gam_uni_selected_interac_winter_20203, hydro_winter_20203_test, type = "response")
        roc.plot(hydro_winter_20203_test$lowlevel, hydro_winter_20203_pred, xlab = "1 - Spezifität", ylab = "Sensitivität")
      }
      if (input$roc_catchment %in% "Iller Kempten") {
        hydro_winter_11502_pred <- predict(gam_uni_selected_interac_winter_11502, hydro_winter_11502_test, type = "response")
        roc.plot(hydro_winter_11502_test$lowlevel, hydro_winter_11502_pred, xlab = "1 - Spezifität", ylab = "Sensitivität")
        
      }
      if (input$roc_catchment %in% "Isar Mittenwald") {
        hydro_winter_10304_pred <- predict(gam_uni_selected_interac_winter_10304, hydro_winter_10304_test, type = "response")
        roc.plot(hydro_winter_10304_test$lowlevel, hydro_winter_10304_pred, xlab = "1 - Spezifität", ylab = "Sensitivität")
      }
    })
})
