library(report)
library(parameters)
library(tidyverse)
library(mgcv)
library(performance)
# Read before Modelling
#Penalised Splines below (1.5 EDF will be modelled as linear)
# Please double check my selections 
# Please name the model le_NAME_OF_SPLINE_MODEL, example see below
# The variable in brackets should have have EDF below 1.5

# Work Assignments
# Some models need more work then others, hope it's ok like this otherwise @Max wil do more :)
# Lisa: Winter/Sommer Interactions
# Jonas: Sommer Selected 2
# Max : Winter/Sommer Full Model, Winter/Sommer Selected 1
# Winter Selected 2 already Done


# Winter
# Full Model ----
# 20203 (Precip, infiltration, snowdrain)
summary(gam_all_winter_20203)
# 11502 (precip, relhum, max_precip, glorad)
summary(gam_all_winter_11502)
# 10304 (airtmp, infiltration)
summary(gam_all_winter_10304)
# Selected 1 ----
# 20203 (NONE)
summary(gam_uni_selected_winter_20203)
le_gam_uni_selected_winter_20203 <- gam_uni_selected_winter_20203
# 11502 (avg_glorad)
summary(gam_uni_selected_winter_11502)
# 10304 (NONE)
summary(gam_uni_selected_winter_10304)
le_gam_uni_selected_winter_10304 <- gam_uni_selected_winter_10304
# Selected 2 DONE ----
# 20203 (None)
summary(gam2_uni_selected_winter_20203)
le_gam2_uni_selected_winter_20203 <- gam2_uni_selected_winter_20203
# 11502 (None)
summary(gam2_uni_selected_winter_11502)
le_gam2_uni_selected_winter_11502 <- gam2_uni_selected_winter_11502
# 10304 (None)
summary(gam2_uni_selected_winter_10304)
le_gam2_uni_selected_winter_10304 <- gam2_uni_selected_winter_10304

# Interactions ----
# 20203 (airtmp, snowstorage, infiltration)
summary(gam_uni_selected_interac_winter_20203)
# 11502 (relhum, precip)
summary(gam_uni_selected_interac_winter_11502)
# 10304 (groundwaterdepth)
summary(gam_uni_selected_interac_winter_10304)

# Summer ----
# Full Model ----
# 20203 (Precip, snowstorage, snowdrain)
summary(gam_all_summer_20203)

# 11502 (precip, snowstorage, snowstorage_drain)
summary(gam_all_summer_11502)

# 10304 (snowstorage, infiltration, max_precip, snowdrain)
summary(gam_all_summer_10304)

# Selected 1 (Nota copy paste error snowstorage always linear) ---- 
# 20203 (snowstorage) 
summary(gam_uni_selected_summer_20203)

# 11502 (snowstorage)
summary(gam_uni_selected_summer_11502)

# 10304 (snowstorage)
summary(gam_uni_selected_summer_10304)

# Selected 2 ----
# 20203 (airtmp snowstorage)
summary(gam2_uni_selected_summer_20203)

# 11502 (snowstorage)
summary(gam2_uni_selected_summer_11502)

# 10304 (relhum, snowstorage)
summary(gam2_uni_selected_summer_10304)

# Interactions ----
# 20203 (infiltration)
summary(gam_uni_selected_interac_summer_20203)
# 11502 (relhum, snowstorage)
summary(gam_uni_selected_interac_summer_11502)

# 10304 (groundwaterdepth, snowstorage, relhum)
summary(gam_uni_selected_interac_summer_10304)
