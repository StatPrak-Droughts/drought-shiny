# Low-level Water Events in Bavaria - Shiny Application
<p align="center">
<img src="drought-shiny/www/icon.png" width="100" height="100" class="center">
</p>
This R shiny App let's you explore and deep dive into the analysis of the low-level water events in Bavaria analysis.
## App Structure
The App is structured into four drop-down menus covering the whole analysis of our project.

### Pegel (Catchment tab)
This tab introduces each catchment and also includes the overview and landing page of the app. Time constant variables can be checked here and used for interpretation of the results.

### Deskriptive Analyse (Descriptive Analysis tab)
The submenus are:
* **Driver variables**: Overview of independent variables over time
* **Conditional distribution of low-level water events**: Overview of extreme driver variables leading to low-level events
* **Correlation analysis**: Correlation matrices of each catchment

### Model (Model tab)
The submenus are:
* **Analysis**: Presenting each model with different options to  control the output
* **ROC Analysis**: Using the models to predict low level events and evaluating via ROC Curves

### Mehr (More tab)
The submenus are:
* **About**: Introducing the project
* **Model guidance and help**: Introducing and explaining our modelling approaches for everyone that wants to understand or revise GAMs.

## App Link
https://group-projects.shinyapps.io/drought-shiny/ 
(needs time to boot up)

## Contributing, reproducibility and setup
You will need access to the Climex-II data which is not publicly avaiable. Access can be granted by the Geography Department from the LMU.
### Setup
First of all you will need to generate the data used from the main repositoy which can be found [here](https://github.com/StatPrak-Droughts/Drought-Project) or under the organization's repos.

1. Follow the instructions under the "Setup" section.
2. Copy the folders `added_data` and `data` as they are into the folder drought-shiny. (The folders should be at the same level as `ui.R` and `server.R`

After that simply call `renv::restore` and you are set to go run the Shiny application.



