library(report)
library(parameters)
library(tidyverse)
library(mgcv)
library(performance)
# Read before Modelling
#Penalised Splines below (1.5 EDF will be modelled as linear)
source("data_read.R")


# Winter
# Full Model ----
# 20203 (Precip, infiltration, snowdrain)
summary(gam_all_winter_20203)
le_gam_all_winter_20203 <- gam(lowlevel ~ YY + avg_precip + s(avg_airtmp, bs = "ps") + s(avg_glorad, bs = "ps") + s(avg_relhum, bs = "ps") + s(avg_soilwater,bs = "ps") + s(avg_snowstorage, bs = "ps") + s(groundwaterdepth, bs = "ps") + avg_infiltration + s(max_precip,bs = "ps") + avg_snowstorage_drain,
                               family = binomial(), data = hydro_winter_20203_kbe, method = "REML")
summary(le_gam_all_winter_20203)
# 11502 (precip, relhum, max_precip, glorad)
summary(gam_all_winter_11502)
le_gam_all_winter_11502 <- gam(lowlevel ~ YY + avg_precip+ s(avg_airtmp, bs = "ps") + avg_glorad + avg_relhum + s(avg_soilwater,bs = "ps") + s(avg_snowstorage, bs = "ps") + s(groundwaterdepth, bs = "ps") + s(avg_infiltration, bs = "ps") + max_precip + s(avg_snowstorage_drain, bs = "ps"),
                               family = binomial(), data = hydro_winter_11502_kbe, method = "REML")
summary(le_gam_all_winter_11502)
# 10304 (airtmp, infiltration)
summary(gam_all_winter_10304)
le_gam_all_winter_10304 <- gam(lowlevel ~ YY + s(avg_precip, bs = "ps") + avg_airtmp + s(avg_glorad, bs = "ps") + s(avg_relhum, bs = "ps") + s(avg_soilwater,bs = "ps") + s(avg_snowstorage, bs = "ps") + s(groundwaterdepth, bs = "ps") + avg_infiltration + s(max_precip,bs = "ps") + s(avg_snowstorage_drain, bs = "ps"),
                               family = binomial(), data = hydro_winter_10304_kbe, method = "REML")
summary(le_gam_all_winter_10304)
# Selected 1 ----
# 20203 (NONE)
summary(gam_uni_selected_winter_20203)
le_gam_uni_selected_winter_20203 <- gam_uni_selected_winter_20203
# 11502 (avg_glorad, when I remove glorad precip gets linear)
summary(gam_uni_selected_winter_11502)
le_gam_uni_selected_winter_11502 <- gam(formula = lowlevel ~ YY + avg_precip + s(avg_airtmp, bs = "ps") + avg_glorad + s(avg_soilwater, bs = "ps") + s(avg_snowstorage, bs = "ps") + s(groundwaterdepth, bs = "ps"), family = binomial(), data = hydro_winter_11502_kbe, method = "REML")
summary(le_gam_uni_selected_winter_11502)
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
le_gam_uni_selected_interac_winter_20203 <- gam(formula = lowlevel ~ YY  + avg_airtmp + s(avg_relhum, bs = "ps") + s(avg_soilwater, bs = "ps") + avg_snowstorage + s(groundwaterdepth, bs = "ps") + avg_infiltration + s(avg_snowstorage_drain, bs = "ps") + ti(avg_airtmp, groundwaterdepth, bs = "ps") + ti(avg_soilwater, avg_snowstorage, bs = "ps"), family = binomial(), data = hydro_winter_20203_kbe, method = "REML")
summary(le_gam_uni_selected_interac_winter_20203)
# 11502 (relhum, precip)
summary(gam_uni_selected_interac_winter_11502)
le_gam_uni_selected_interac_winter_11502 <- gam(formula = lowlevel ~ YY + avg_precip + s(avg_airtmp, bs = "ps") + s(avg_glorad, bs = "ps") + avg_relhum + s(avg_soilwater, bs = "ps") + s(avg_snowstorage, bs = "ps") + s(groundwaterdepth, bs = "ps") + s(avg_infiltration, bs = "ps") + s(avg_snowstorage_drain, bs = "ps") + ti(avg_snowstorage, groundwaterdepth, bs = "ps"), family = binomial(), data = hydro_winter_11502_kbe, method = "REML")# 10304 (groundwaterdepth)
summary(le_gam_uni_selected_interac_winter_11502)

# 10304 (groundwaterdepth)
summary(gam_uni_selected_interac_winter_10304)
le_gam_uni_selected_interac_winter_10304 <- gam(formula = lowlevel ~ YY + s(avg_precip, bs = "ps") + s(avg_glorad, bs = "ps") + s(avg_relhum, bs = "ps") + s(avg_soilwater, bs = "ps") + s(avg_snowstorage, bs = "ps") + groundwaterdepth + s(avg_infiltration, bs = "ps") + s(avg_snowstorage_drain, bs = "ps") + ti(avg_glorad, avg_snowstorage, bs = "ps") + ti(avg_soilwater, groundwaterdepth, bs = "ps"), family = binomial(), data = hydro_winter_10304_kbe, method = "REML")
summary(le_gam_uni_selected_interac_winter_10304)

# Summer ----
# Full Model ----
# 20203 (Precip, snowstorage, snowdrain)
summary(gam_all_summer_20203)
le_gam_all_summer_20203 <- gam(formula = lowlevel ~ YY + avg_precip + s(avg_airtmp, bs = "ps") + s(avg_glorad, bs = "ps") + s(avg_relhum, bs = "ps") + s(avg_soilwater, bs = "ps") + avg_snowstorage + s(groundwaterdepth, bs = "ps") + s(avg_infiltration, bs = "ps") + s(max_precip, bs = "ps") + avg_snowstorage_drain, family = binomial(), data = hydro_summer_20203_kbe, method = "REML")
summary(le_gam_all_summer_20203)
# 11502 (precip, snowstorage, snowstorage_drain)
summary(gam_all_summer_11502)
le_gam_all_summer_11502 <- gam(formula = lowlevel ~ YY + avg_precip + s(avg_airtmp, bs = "ps") + s(avg_glorad, bs = "ps") + s(avg_relhum, bs = "ps") + s(avg_soilwater, bs = "ps") + avg_snowstorage + s(groundwaterdepth, bs = "ps") + s(avg_infiltration, bs = "ps") + s(max_precip, bs = "ps") + avg_snowstorage_drain, family = binomial(), data = hydro_summer_11502_kbe, method = "REML")
summary(le_gam_all_summer_11502)
# 10304 (snowstorage, infiltration, max_precip, snowdrain)
summary(gam_all_summer_10304)
le_gam_all_summer_10304 <- gam(formula = lowlevel ~ YY + s(avg_precip, bs = "ps") + s(avg_airtmp, bs = "ps") + s(avg_glorad, bs = "ps") + s(avg_relhum, bs = "ps") + s(avg_soilwater, bs = "ps") + avg_snowstorage + s(groundwaterdepth, bs = "ps") + avg_infiltration + max_precip + avg_snowstorage_drain, family = binomial(), data = hydro_summer_10304_kbe, method = "REML")
summary(le_gam_all_summer_10304)

# Selected 1 (Nota copy paste error snowstorage always linear) ---- 
# 20203 (snowstorage) 
summary(gam_uni_selected_summer_20203)
le_gam_uni_selected_summer_20203 <- gam(formula = lowlevel ~ YY + s(avg_precip, bs = "ps") + s(avg_airtmp, bs = "ps") + s(avg_glorad, bs = "ps") + s(avg_soilwater, bs = "ps") + avg_snowstorage + s(groundwaterdepth, bs = "ps"), family = binomial(), data = hydro_summer_20203_kbe, method = "REML")
summary(le_gam_uni_selected_summer_20203)
# 11502 (snowstorage)
summary(gam_uni_selected_summer_11502)
le_gam_uni_selected_summer_11502 <- gam(formula = lowlevel ~ YY + s(avg_precip, bs = "ps") + s(avg_airtmp, bs = "ps") + s(avg_glorad, bs = "ps") + s(avg_soilwater, bs = "ps") + avg_snowstorage + s(groundwaterdepth, bs = "ps"), family = binomial(), data = hydro_summer_11502_kbe, method = "REML")
summary(le_gam_uni_selected_summer_11502)
# 10304 (snowstorage)
summary(gam_uni_selected_summer_10304)
le_gam_uni_selected_summer_10304 <- gam(formula = lowlevel ~ YY + s(avg_precip, bs = "ps") + s(avg_airtmp, bs = "ps") + s(avg_glorad, bs = "ps") + s(avg_soilwater, bs = "ps") + avg_snowstorage + s(groundwaterdepth, bs = "ps"), family = binomial(), data = hydro_summer_10304_kbe, method = "REML")
summary(le_gam_uni_selected_summer_10304)

# Selected 2 ----
# 20203 (airtmp snowstorage)
summary(gam2_uni_selected_summer_20203)
le_gam2_uni_selected_summer_20203 <- gam(formula = lowlevel ~ YY + avg_airtmp + s(avg_relhum, bs = "ps") + s(avg_soilwater, bs = "ps") + avg_snowstorage + s(groundwaterdepth, bs = "ps") + s(avg_infiltration, bs = "ps"), family = binomial(), data = hydro_summer_20203_kbe, method = "REML")
summary(le_gam2_uni_selected_summer_20203)
# 11502 (snowstorage)
summary(gam2_uni_selected_summer_11502)
le_gam2_uni_selected_summer_11502 <- gam(formula = lowlevel ~ YY + s(avg_airtmp, bs = "ps") + s(avg_relhum, bs = "ps") + s(avg_soilwater, bs = "ps") + avg_snowstorage + s(groundwaterdepth, bs = "ps") + s(avg_infiltration, bs = "ps"), family = binomial(), data = hydro_summer_11502_kbe, method = "REML")
summary(le_gam2_uni_selected_summer_11502)
# 10304 (relhum, snowstorage)
summary(gam2_uni_selected_summer_10304)
le_gam2_uni_selected_summer_10304 <- gam(formula = lowlevel ~ YY + s(avg_airtmp, bs = "ps") + avg_relhum + s(avg_soilwater, bs = "ps") + avg_snowstorage + s(groundwaterdepth, bs = "ps") + s(avg_infiltration, bs = "ps"), family = binomial(), data = hydro_summer_10304_kbe, method = "REML")
summary(le_gam2_uni_selected_summer_10304)
# Interactions ----
# 20203 (infiltration)
summary(gam_uni_selected_interac_summer_20203)
le_gam_uni_selected_interac_summer_20203 <- gam(formula = lowlevel ~ YY  + s(avg_airtmp, bs = "ps") + s(avg_relhum, bs = "ps") + s(avg_soilwater, bs = "ps") + s(groundwaterdepth, bs = "ps") + avg_infiltration  + ti(avg_soilwater, groundwaterdepth, bs = "ps") + ti(avg_airtmp, avg_infiltration, bs = "ps"), family = binomial(), data = hydro_summer_20203_kbe, method = "REML")
summary(le_gam_uni_selected_interac_summer_20203)
# 11502 (relhum, snowstorage)
summary(gam_uni_selected_interac_summer_11502)
le_gam_uni_selected_interac_summer_11502 <- gam(formula = lowlevel ~ YY  + s(avg_airtmp, bs = "ps") + s(avg_glorad, bs = "ps") + avg_relhum + s(avg_soilwater, bs = "ps") + avg_snowstorage + s(groundwaterdepth, bs = "ps") + s(avg_infiltration, bs = "ps") + ti(avg_soilwater, groundwaterdepth, bs = "ps") + ti(avg_airtmp, avg_infiltration, bs = "ps"), family = binomial(), data = hydro_summer_11502_kbe, method = "REML")
summary(le_gam_uni_selected_interac_summer_11502)
# 10304 (groundwaterdepth, snowstorage, relhum)
summary(gam_uni_selected_interac_summer_10304)
le_gam_uni_selected_interac_summer_10304 <- gam(formula = lowlevel ~ YY + s(avg_airtmp, bs = "ps") + s(avg_glorad, bs = "ps") + avg_relhum + s(avg_soilwater, bs = "ps") + avg_snowstorage + groundwaterdepth + s(avg_infiltration, bs = "ps") + ti(avg_soilwater, groundwaterdepth, bs = "ps") + ti(avg_airtmp, avg_infiltration, bs = "ps"), family = binomial(), data = hydro_summer_10304_kbe, method = "REML")
summary(le_gam_uni_selected_interac_summer_10304)


tab_model(le_gam_all_summer_10304, show.intercept = FALSE, show.aic = T , digits = 3, show.est = T, transform = NULL)
modelsummary(list("Estimate [CI]" = le_gam_all_summer_10304), estimate = "{estimate} [{conf.low}, {conf.high}] {stars}", statistic = NULL, exponentiate = T, coef_omit = "Intercept", gof_map = c("aic", "r.squared"), align = "ll", output = "html")


le_gam_all_summer_10304 %>% model_parameters(exponentiate = T, drop = "Intercept")

save()




# +=.1, *=.05, **=.01, ***=0.001



