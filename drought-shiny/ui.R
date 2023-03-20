library(shiny)
library(shinydashboard)
library(tmap)
header <- dashboardHeader(
    title = "Low Level Water Events in Bavaria"
)

body <- dashboardBody(
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    h1("Landing Page & Info about the Project, How to use this app"),
    tabItems(
        tabItem("map_overview",
                fluidRow(
                         box(title = "Catchments of Interest", solidHeader = TRUE, collapsible = TRUE,
                             tmapOutput("three_catchment_map"))
                         )
                ),
        tabItem("descriptive",
                fluidRow(box(title = "Descriptive Analysis"))
            ),
        tabItem("models",
                fluidRow(box(title = "Models"))
            ),
        tabItem("about",
                fluidRow(box(title = "About & Info"))
            )
        )
)


sidebar <-  dashboardSidebar(
    sidebarMenu(
        menuItem("Map Overview", tabName = "map_overview", icon = icon("map")),
        menuItem("Descriptive Analysis", tabName = "descriptive", icon = icon("th")),
        menuItem("Models", tabName = "models", icon = icon("chart-simple")),
        menuItem("About & Credits", tabName = "about", icon = icon("info"))
    )
)

dashboardPage(
    skin = "black",
    header,
    sidebar,
    body
)
