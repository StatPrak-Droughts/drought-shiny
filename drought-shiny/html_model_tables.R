library(report)
library(parameters)
library(tidyverse)
library(mgcv)
library(performance)
# Penalised Splines below (1.5 EDF will be modelled as linear)
# Full Model ----
# Winter ----
# 20203 (Precip, infiltration, snowdrain)

# 11502 (precip, relhum, max_precip, glorad)

# 10304 (airtmp, infiltration)

# Selected 1 ----
# 20203 (NONE)

# 11502 (avg_glorad)

# 10304 (NONE)

# Selected 2 ----
# 20203 (airtmp, snowstorage, infiltration) 

# 11502 (relhum, precip)

# 10304 (groundwaterdepth)


# Interactions ----
# 20203 (airtmp, snowstorage, infiltration)

# 11502 (relhum, precip)

# 10304 (groundwaterdepth)

# Summer ----
# Full Model ----
# 20203 (Precip, infiltration, snowdrain)

# 11502 (precip, relhum, max_precip, glorad

# 10304 (airtmp, infiltration)

# Selected 1 (Nota copy paste error snowstorage always linear) ---- 
# 20203 (snowstorage) 

# 11502 (snowstorage)

# 10304 (snowstorage)

# Selected 2 ----
# 20203 (airtmp snowstorage)

# 11502 (snowstorage)

# 10304 (avg_relhum, snowstorage)

# Interactions ----
# 20203 (infiltration)

# 11502 (relhum, snowstorage)

# 10304 (groundwaterdepth, snowstorage, relhum)