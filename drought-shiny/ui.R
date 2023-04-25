library(shiny)
library(shinydashboard)
library(tmap)
library(shiny)
library(sf)
library(shinyBS)
library(shinythemes)
library(shinycssloaders)
library(tmap)
library(tmaptools)
library(mgcv)
library(tidyverse)
library(corrplot)
library(modelsummary)
library(verification)
source("data_read.R")
button_color_css <- "
#DivCompClear, #FinderClear, #EnterTimes{
/* Change the background color of the update button
to blue. */
background: DodgerBlue;
/* Change the text size to 15 pixels. */
font-size: 15px;
}"
tags$style(HTML("
      .box-header {
        padding: 0 10px 0 0;
      }
      .box-header h3 {
        width: 100%;
        padding: 10px;
      }"))


shinyUI(fluidPage(
  navbarPage(id = "navbar", title = div(img(src='icon.png', style="background-color: transparent; margin-top: -10px;", height = 35), tags$a(href= "https://github.com/StatPrak-Droughts", "Niedrigwasser BY")),
               theme = shinytheme("lumen"),
               navbarMenu("Pegel", icon = icon("globe"),
                          tabPanel("Überblick", fluid = TRUE, icon = icon("map"),
                                   tags$style(button_color_css),
                                   tags$style(HTML(".datepicker {z-index:99999 !important;}")),
                                       mainPanel(
                                           titlePanel("Karten Überblick - Hydrologisches Bayern"),
                                           tmapOutput("three_catchment_map")
                                       )
                                   ),
                          tabPanel("Fränkische Saale Salz", icon = icon("water"),
                                       mainPanel(
                                           h1("Fränkische Saale Salz"),
                                           br(),
                                           fluidRow(splitLayout(cellWidths = c("80%", "50%"), img(src = "f-saale_salz.jpg", height = 200, width = 400), tmapOutput("map_saale_salz", height = 200))),
                                           br(),
                                           fluidRow(p("Der Pegel Fränkische Saale Salz in Bayern trägt die ID-Nummer 20203 und befindet sich auf einer Höhe von 225.6 Metern über dem Meeresspiegel. Die umgebende Landschaft des Pegels ist vorwiegend von einer landwirtschaftlichen Nutzung geprägt, wobei die Landnutzungskategorie auf 18 festgelegt ist. Der Hang rund um den Pegel weist eine Steigung von 3.554 auf und der Boden hat eine Beschaffenheit von 58.

Die Exposition des Pegels beträgt 114.5, was bedeutet, dass er einer östlichen Ausrichtung ausgesetzt ist. Diese Faktoren tragen zur Genauigkeit der Messung des Wasserstands bei und machen den Pegel Fränkische Saale Salz zu einem wichtigen Instrument zur Überwachung der Gewässerqualität in der Region. Darüber hinaus ermöglicht der Pegel eine präzise Beobachtung der Wasserstände, die wiederum bei der Planung von Hochwasserschutzmaßnahmen und anderen wasserbezogenen Aktivitäten hilft."))
                                       )
                           ),
                          tabPanel("Iller Kempten", icon = icon("water"),
                                   mainPanel(
                                   h1("Iller Kempten"),
                                   br(),
                                   fluidRow(splitLayout(cellWidths = c("80%", "50%"), img(src = "iller_kempten.jpg", height = 200, width = 400), tmapOutput("map_iller_kempten", height = 200))),
                                   br(),
                                   p("Der Pegel Iller Kempten in Bayern wird durch die ID 11502 identifiziert und befindet sich auf einer Höhe von 662.6 Metern über dem Meeresspiegel. Die Umgebung des Pegels ist hauptsächlich durch eine Waldnutzung gekennzeichnet, wobei die landuse-Kategorie auf 1 festgelegt ist. Der Hang rund um den Pegel hat eine Steigung von 1.769 und der Boden weist eine Beschaffenheit von 57 auf.

Die Exposition des Pegels beträgt 290.6, was bedeutet, dass er einer nordwestlichen Ausrichtung ausgesetzt ist. Diese Faktoren tragen zur Genauigkeit der Messung des Wasserstands bei, wodurch der Pegel Iller Kempten ein wichtiges Instrument für die Überwachung der Gewässerqualität in der Region darstellt. Darüber hinaus ermöglicht der Pegel eine genaue Beobachtung der Wasserstände und unterstützt damit eine bessere Planung von Hochwasserschutzmaßnahmen und anderer wasserbezogener Aktivitäten.")
                          )),
                          tabPanel("Isar Mittenwald", icon = icon("water"),
                                       mainPanel(
                                           h1("Isar Mittenwald"),
                                           br(),
                                           fluidRow(splitLayout(cellWidths = c("80%", "50%"), img(src = "isar_mittenwald.JPG", height = 200, width = 400), tmapOutput("map_isar_mittenwald", height = 200))),
                                           br(),
                                           p("Der Pegel Isar Mittenwald in Bayern hat die ID 10304 und liegt auf einer Höhe von 909.3 Metern über dem Meeresspiegel. Die Umgebung des Pegels ist geprägt von einer landwirtschaftlichen Nutzung mit einer landuse-Kategorie von 2. Die Landschaft rund um den Pegel weist einen Hang mit einer Steigung von 4.029 auf und der Boden hat eine Beschaffenheit von 54.

Des Weiteren zeigt der Pegel eine Exposition von 275.7, was bedeutet, dass er einer südwestlichen Ausrichtung ausgesetzt ist. All diese Faktoren tragen dazu bei, dass der Pegel Isar Mittenwald ein wichtiges Instrument für die Messung von Wasserständen in der Region darstellt und einen wesentlichen Beitrag zur Überwachung der Gewässerqualität leistet.")
                                       ))
                          ),
               navbarMenu("Deskriptive Analyse", icon = icon("chart-simple"),
                          tabPanel("Treibervariablen", fluid = TRUE, icon = icon("chart-simple"),
                                   tags$style(button_color_css),
                                   sidebarLayout(
                                       sidebarPanel(
                                           h1("Eingaben"),
                                           selectInput("variable_min_max_plot", label = "Wähle Variable", choices = c("groundwaterdepth", "soilwater", "snowstorage", "airtmp", "precipitation", "glorad", "relhum", "infiltration"), multiple = FALSE),
                                           checkboxInput("facet_hydro_year", label = "Aufteilung nach hydrolog. Halbjahr?", FALSE)
                                       ),
                                       mainPanel(h1("Minima"), plotOutput("min_plot"), br(), h1("Maxima"), plotOutput("max_plot")
                                       )
                                       )
                                   ),
                          tabPanel("Konditionale Verteilung von Niedrigwasserevents", icon = icon("chart-simple"),
                                   sidebarLayout(
                                       sidebarPanel(
                                           h1("Eingaben"),
                                           selectInput("extreme_value_catchment", label = "Wähle Pegel", 
                                                       choices = c("Fränkische Saale Salz", "Iller Kempten", "Isar Mittenwald"))
                                       ),
                                       mainPanel(
                                           tabsetPanel(type = "tabs",
                                                       tabPanel("Sommer", h1("Sommer"), h2("Erklärung"),
                                                                p("Datengrundlage: Daten aller 3 Pegel, bedingt auf 'Niedrigwasserevent eingetreten'"),
                                                                p("Zeilenindex: Dezil der Treibervariable"),
                                                                p("Spaltenname: Treibervariable"),
                                                                p("Zellenwert: Anteil der Niedrigwasserevents im jeweiligen Dezil (10% der Treibervariable)"),
                                                                br(),
                                                                p(strong("Beispiel")),
                                                                p("Zellenwert = 1: 100% der Niedrigwasserevents liegen in diesem Dezil"),
                                                                p("Gesamte Spalte = 0.1: Gleichmäßige Verteilung der Niedrigwasserevents"), DT::dataTableOutput("qpr_hydro_summer")),
                                                       tabPanel("Winter", h1("Winter"), h2("Erklärung"),
                                                                p("Zeilenindex: Dezil der Treibervariable"),
                                                                br(),
                                                                p("Spaltenname: Treibervariable"),
                                                                p("Zellenwert: Anteil der Niedrigwasserevents im jeweiligen Dezil (10% der Treibervariable)"),
                                                                br(),
                                                                p(strong("Beispiel")),
                                                                br(),
                                                                p("Zellenwert = 1: 100% der Niedrigwasserevents liegen in diesem Dezil"),
                                                                br(),
                                                                p("Gesamte Spalte = 0.1: Gleichmäßige Verteilung der Niedrigwasserevents"), DT::dataTableOutput("qpr_hydro_winter"))
                                           )
                                       )
                                   )
                               ),
                          tabPanel("Korrelationsanalyse", icon = icon("chart-simple"),
                                   sidebarLayout(
                                     sidebarPanel(
                                       h1("Eingaben"),
                                       selectInput("corr_catchment", label = "Wähle Pegel", 
                                                   choices = c("Fränkische Saale Salz", "Iller Kempten", "Isar Mittenwald")),
                                       selectInput("corr_corr", label = "Wähle Korrelationskoeffizienten",
                                                   choices = c("Spearman", "Pearson"))
                                     ),
                                     mainPanel(
                                       tabsetPanel(type = "tabs",
                                                   tabPanel("Sommer", h1("Sommer"), plotOutput("corr_hydro_summer", height = 650)),
                                                   tabPanel("Winter", h1("Winter"), plotOutput("corr_hydro_winter", height = 650))
                                       )
                                     )
                                   )
                          )
                          ),
               navbarMenu("Model", icon = icon("chart-simple"),
                          tabPanel("Overview / Effects", icon = icon("chart-simple"),
                                   sidebarLayout(
                                       sidebarPanel(
                                           h1("Eingaben"),
                                           selectInput("model_catchment", label = "Wähle Pegel",  choices = c("Fränkische Saale Salz", "Iller Kempten", "Isar Mittenwald"), selected = "Iller Kempten"),
                                           selectInput("model_selection", label = "Wähle Modell", choices = c("Full Model", "Selected Model 1", "Selected Model 2", "Interactions" = "Interactions")),
                                           checkboxInput("model_odds", label = "Zeige Odds Ratio anstatt log. Odds", FALSE),
                                           checkboxInput("model_summary", label = "Zeige Modell Zusammenfassung?", FALSE),
                                           checkboxInput("effect_plots", label = "Zeige Effekt Plots?", FALSE),
                                           uiOutput("slider_ui")
                                       ),
                                       mainPanel(
                                           tabsetPanel(type = "tabs",
                                                       tabPanel("Sommer",
                                                                h1("Sommer"),
                                                                uiOutput("model_tab_summer"),
                                                                p(strong("* p = 0.05 ** p = 0.01 *** p = 0.001 + p > 0.1 ")),
                                                                p(strong("Hinweis:\n"), "Die Odds Ratios können bei den vergleichbaren Modellen (Full & Selected 1/2) bei unterschiedlichen Pegeln sehr extreme Werte annehmen. Eine Odds Ratio von 0 ist meistens durch Artefakte in den Daten zu erklären. Man sollte deshalb stets die Effekt und Rugplots betrachten. \n
                                             Wir haben uns dafür entschlossen die Variablen dennoch nicht zu skalieren, da bestimmte Treiber wie 'Snowstorage' im Norden auf der gegebenen Skala sehr kleine log. Odss annehmen, im Süden jedoch bei gleicher Skala eine sinnvolle Interpretation gewährleisten."),
                                                                verbatimTextOutput("model_summary_summer"),
                                                                plotOutput("model_effect_summer_1"),
                                                                plotOutput("model_effect_summer_2"),
                                                                plotOutput("model_effect_summer_3"),
                                                                plotOutput("model_effect_summer_4"),
                                                                plotOutput("model_effect_summer_5"),
                                                                plotOutput("model_effect_summer_6"),
                                                                plotOutput("model_effect_summer_7"),
                                                                plotOutput("model_effect_summer_8"),
                                                                plotOutput("model_effect_summer_9"),
                                                                plotOutput("model_effect_summer_10")
                                                                ),
                                                       tabPanel("Winter", 
                                                                h1("Winter"),
                                                                uiOutput("model_tab_winter"),
                                                                p(strong("* p = 0.05 ** p = 0.01 *** p = 0.001 + p > 0.1 ")),
                                                                p(strong("Hinweis:\n"), "Die Odds Ratios können bei den vergleichbaren Modellen (Full & Selected 1/2) bei unterschiedlichen Pegeln sehr extreme Werte annehmen. Eine Odds Ratio von 0 ist meistens durch Artefakte in den Daten zu erklären. Man sollte deshalb stets die Effekt und Rugplots betrachten. \n
                                             Wir haben uns dafür entschlossen die Variablen dennoch nicht zu skalieren, da bestimmte Treiber wie 'Snowstorage' im Norden auf der gegebenen Skala sehr kleine log. Odss annehmen, im Süden jedoch bei gleicher Skala eine sinnvolle Interpretation gewährleisten."),
                                                                verbatimTextOutput("model_summary_winter"),
                                                                plotOutput("model_effect_winter_1"),
                                                                plotOutput("model_effect_winter_2"),
                                                                plotOutput("model_effect_winter_3"),
                                                                plotOutput("model_effect_winter_4"),
                                                                plotOutput("model_effect_winter_5"),
                                                                plotOutput("model_effect_winter_6"),
                                                                plotOutput("model_effect_winter_7"),
                                                                plotOutput("model_effect_winter_8"),
                                                                plotOutput("model_effect_winter_9"),
                                                                plotOutput("model_effect_winter_10")
                                                                )
                                           )
                                       )
                                   )
                                   ),
                          tabPanel("ROC Analysis", icon = icon("chart-simple"),
                                   sidebarLayout(
                                     sidebarPanel(
                                       h1("Eingaben"),
                                       selectInput("roc_catchment", label = "Wähle Pegel", 
                                                   choices = c("Fränkische Saale Salz", "Iller Kempten", "Isar Mittenwald"), selected = "Isar Mittenwald"),
                                       selectInput("roc_model", label = "Wähle Modell",
                                                   choices = c("Full Model", "Selected Model 1", "Selected Model 2", "Interactions" = "Interactions"))
                                     ),
                                     mainPanel(
                                       tabsetPanel(type = "tabs",
                                                   tabPanel("Sommer",
                                                            h1("Sommer"),
                                                            plotOutput("roc_summer")
                                                   ),
                                                   tabPanel("Winter", 
                                                            h1("Winter"),
                                                            plotOutput("roc_winter")
                                                   )
                                       )
                                     )
                                   )
                          )
                          ),
               navbarMenu("Mehr", icon = icon("info"),
                          tabPanel("Über das Projekt", icon = icon("circle-question"),
                                   fluidRow(
                                       h1("Über das Projekt"),
                                       p("Dieses Datenanalyse-Projekt beschäftigt sich mit der Vorhersage von Niedrigwasser für drei Pegel (Isar, Iller und Fränkische Saale) in Bayern. Das Projekt wurde als Weiterführung eines statistischen Consulting-Projekts von Theresa Meier und Nikita Paschan im Rahmen des statistischen Praktikums bearbeitet. Das Ziel des Projekts ist es, die Treiber von Niedrigwasser zu verstehen und Vorhersagen mit bestimmten Treibervariablen zu treffen. Im Consulting-Projekt wurde ganz Bayern mit deutlich mehreren Pegeln modelliert und vorgestellt. Die Daten stammen aus dem ClimEx-Projekt, einer Zusammenarbeit zwischen Bayern und Québec, das ein hydrologisches Simulationsmodell verwendet, um den Einfluss des Klimawandels auf meteorologische und hydrologische Extremereignisse in Bayern zu untersuchen. Es wurden 50 verschiedene Sätze von simulierten Daten (Member) mit unterschiedlichen Startbedingungen verwendet, um die natürliche Variabilität zu berücksichtigen. Das Projekt hat somit wichtige Implikationen für das Wasserressourcenmanagement in Bayern."),
                                       fluidRow(img(src = "Climex.png", height = 100, width = 250), img(src = "stablab.jpg", height = 150, width = 150))
                                       )       
                          ),
                          tabPanel("Modelle Leitfaden", icon = icon("lightbulb"),
                                   fluidRow(
                                     h1("Modelle Leitfaden"),
                                     h2("Einführung in Generalisierte Lineare Modelle (GAMs)"),
                                     img(src='GAM_Intro.png', align = "center", height = 450),
                                     br(),
                                     h2("Interpretation der Koeffizienten"),
                                     img(src='interpretation.png', align = "center", height = 450),
                                     br(),
                                     h2("ROC Kurven und Prädiktion"),
                                     img(src='ROC.png', align = "center", height = 430),
                                     br(),
                                     h2("AIC - Akaikes Informationskriterium"),
                                     img(src='AIC.png', align = "center", height = 220),
                                     br())))
    )
)
)