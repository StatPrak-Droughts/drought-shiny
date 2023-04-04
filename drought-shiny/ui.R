library(shiny)
library(shinydashboard)
library(tmap)
library(shiny)
library(shinythemes)
library(shinycssloaders)
library(tmap)
library(tmaptools)
library(plotly)

button_color_css <- "
#DivCompClear, #FinderClear, #EnterTimes{
/* Change the background color of the update button
to blue. */
background: DodgerBlue;
/* Change the text size to 15 pixels. */
font-size: 15px;
}"

shinyUI(fluidPage(
    navbarPage(id = "navbar", title = div(img(src='icon.png', style="background-color: transparent; margin-top: -10px;", height = 35), tags$a(href= "https://github.com/StatPrak-Droughts", "Niedrigwasser BY")),
               theme = shinytheme("lumen"),
               navbarMenu("Catchments", icon = icon("globe"),
                          tabPanel("Overview", fluid = TRUE, icon = icon("map"),
                                   tags$style(button_color_css),
                                   tags$style(HTML(".datepicker {z-index:99999 !important;}")),
                                       mainPanel(
                                           titlePanel("Map Overview"),
                                           tmapOutput("three_catchment_map")
                                       )
                                   ),
                          tabPanel("Fränkische Saale Salz", icon = icon("water"),
                                       mainPanel(
                                           h1("Hello")
                                       )
                           ),
                          tabPanel("Iller Kempten", icon = icon("water"),
                                       mainPanel(
                                           h1("hello")
                                       )),
                          tabPanel("Isar Mittenwald", icon = icon("water"),
                                       mainPanel(
                                           h1("Hello")
                                       ))
                          ),
               navbarMenu("Descriptive Analysis", icon = icon("earth-europe"),
                          tabPanel("Overall", fluid = TRUE, icon = icon("map"),
                                   tags$style(button_color_css),
                                   sidebarLayout(
                                       sidebarPanel(
                                           h1("Inputs")
                                       ),
                                       mainPanel(
                                           tabsetPanel(type = "tabs",
                                                       tabPanel("Summer", h1("Summer Plot")),
                                                       tabPanel("Winter", h1("Winter Plot"))
                                           )
                                       )
                                   )),
                          tabPanel("Extreme Value Analysis", icon = icon("chart-simple"),
                                   sidebarLayout(
                                       sidebarPanel(
                                           h1("Inputs"),
                                           selectInput("extreme_value_catchment", label = "Select Catchment", 
                                                       choices = c("Fränkische Saale / Salz", "Iller Kempten", "Isar Mittenwald"))
                                       ),
                                       mainPanel(
                                           tabsetPanel(type = "tabs",
                                                       tabPanel("Summer", h1("Summer"), dataTableOutput("qpr_hydro_summer")),
                                                       tabPanel("Winter", h1("Winter"), dataTableOutput("qpr_hydro_winter"))
                                           )
                                       )
                                   )
                               )
                          ),
               navbarMenu("Model", icon = icon("chart-simple"),
                          tabPanel("Analysis", icon = icon("chart-simple"),
                                   sidebarLayout(
                                       sidebarPanel(
                                           h1("Inputs"),
                                           selectInput("model_catchment", label = "Select Catchment", 
                                                       choices = c("Fränkische Saale / Salz", "Iller Kempten", "Isar Mittenwald")),
                                           selectInput("model_selection", label = "Select model", choices = c("Full Model", "Trimmed Model", "Interactions")),
                                           checkboxInput("model_summary", label = "Show model summary?", FALSE),
                                           checkboxInput("model_effects", label = "Show effect plots?", FALSE)
                                       ),
                                       mainPanel(
                                           tabsetPanel(type = "tabs",
                                                       tabPanel("Summer",
                                                                h1("Summer"),
                                                                verbatimTextOutput("model_summary_summer")
                                                                ),
                                                       tabPanel("Winter", 
                                                                h1("Winter")
                                                                
                                                                )
                                           )
                                       )
                                   )
                                   )
                          ),
               navbarMenu("More", icon = icon("info"),
                          tabPanel("About", icon = icon("circle-question"),
                                   fluidRow(
                                       h1("About")
                                   )
                          ))
    )
)
)