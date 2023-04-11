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

# Plots (min and max):

# TODO: Entfernen von "facet_wrap(vars(hydro_year), labeller = as_labeller(c(`summer` = "Sommer", `winter` = "Winter")))"
# mit zusätzlichem Kasten mit Haken: "Nach hydrologischem Halbjahr aufteilen?"

# groundwaterdepth ----
ggplot(data = table_yearly_avg_min_groundwaterdepth, mapping = aes(x = YY, y = avg_min_groundwaterdepth, color = waterlevel)) +
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

ggplot(data = table_yearly_avg_max_groundwaterdepth, mapping = aes(x = YY, y = avg_max_groundwaterdepth, color = waterlevel)) +
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

# soilwater----
ggplot(data = table_yearly_avg_min_soilwater, mapping = aes(x = YY, y = avg_min_soilwater, color = waterlevel)) +
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

ggplot(data = table_yearly_avg_max_soilwater, mapping = aes(x = YY, y = avg_max_soilwater, color = waterlevel)) +
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

# snowstorage ----
ggplot(data = table_yearly_avg_min_snowstorage, mapping = aes(x = YY, y = avg_min_snowstorage, color = waterlevel)) +
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

ggplot(data = table_yearly_avg_max_snowstorage, mapping = aes(x = YY, y = avg_max_snowstorage, color = waterlevel)) +
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

# airtmp ----
ggplot(data = table_yearly_avg_min_airtmp, mapping = aes(x = YY, y = avg_min_airtmp, color = waterlevel)) +
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

ggplot(data = table_yearly_avg_max_airtmp, mapping = aes(x = YY, y = avg_max_airtmp, color = waterlevel)) +
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

# precip ----
ggplot(data = table_yearly_avg_min_precip, mapping = aes(x = YY, y = avg_min_precip, color = waterlevel)) +
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

ggplot(data = table_yearly_avg_max_precip, mapping = aes(x = YY, y = avg_max_precip, color = waterlevel)) +
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

# glorad ----
ggplot(data = table_yearly_avg_min_glorad, mapping = aes(x = YY, y = avg_min_glorad, color = waterlevel)) +
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

ggplot(data = table_yearly_avg_max_glorad, mapping = aes(x = YY, y = avg_max_glorad, color = waterlevel)) +
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

# infiltration
ggplot(data = table_yearly_avg_min_infiltration, mapping = aes(x = YY, y = avg_min_infiltration, color = waterlevel)) +
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

ggplot(data = table_yearly_avg_max_infiltration, mapping = aes(x = YY, y = avg_max_infiltration, color = waterlevel)) +
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