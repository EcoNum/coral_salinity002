# nutrient exploration

# Package and function ----
SciViews::R
library(econum)
source("R/function.R")

# Import data ----

nu_cs <- read("data/nutrient.rds")

# Exploration ----

# Exploratory step A0, A1, A2 -----
## Phosphorus
nu_cs %>.%
  filter(., respi == "A0" |respi == "A1" |respi == "A2",
         Ptot_conc != "NA") %>.%
  pc_plot(data = ., y = "PO4_conc", x = "number_day", factor = "respi",
          rect_start = 29.83 + 0:7, rect_end = (29.83 + 0:7) + 0.5)
nu_cs %>.%
  filter(., respi == "A0" |respi == "A1" |respi == "A2",
         Ptot_conc != "NA") %>.%
  pc_plot(data = ., y = "Ptot_conc", x = "number_day", factor = "respi",
          rect_start = 29.83 + 0:7, rect_end = (29.83 + 0:7) + 0.5)

## Nitrogen
nu_cs %>.%
  filter(., respi == "A0" |respi == "A1" |respi == "A2" ) %>.%
  pc_plot(data = ., y = "NOx_conc", x = "number_day", factor = "respi",
          rect_start = 29.83 + 0:7, rect_end = (29.83 + 0:7) + 0.5)
nu_cs %>.%
  filter(., respi == "A0" |respi == "A1" |respi == "A2" ) %>.%
  pc_plot(data = ., y = "NO3_conc", x = "number_day", factor = "respi",
          rect_start = 29.83 + 0:7, rect_end = (29.83 + 0:7) + 0.5)
nu_cs %>.%
  filter(., respi == "A0" |respi == "A1" |respi == "A2" ) %>.%
  pc_plot(data = ., y = "NO2_conc", x = "number_day", factor = "respi",
          rect_start = 29.83 + 0:7, rect_end = (29.83 + 0:7) + 0.5)
nu_cs %>.%
  filter(., respi == "A0" |respi == "A1" |respi == "A2" ) %>.%
  pc_plot(data = ., y = "Ntot_conc", x = "number_day", factor = "respi",
          rect_start = 29.83 + 0:7, rect_end = (29.83 + 0:7) + 0.5)
nu_cs %>.%
  filter(., respi == "A0" |respi == "A1" |respi == "A2" ) %>.%
  pc_plot(data = ., y = "Ptot_conc", x = "number_day", factor = "respi",
          rect_start = 29.83 + 0:7, rect_end = (29.83 + 0:7) + 0.5)

# Respirometry analysis ----
nu_cs %>.%
  filter(., ! respi  %in% c("A0", "A1", "A2")) %>.%
  arrange(., respi, sample_date) -> nu_respi

nu_respi%>.%
pc_plot(data = nu_respi, y = "Ptot_conc", x = "number_day", factor = "respi",
        rect_start = 29.83 + 0:7, rect_end = (29.83 + 0:7) + 0.5)
