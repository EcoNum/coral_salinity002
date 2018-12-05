# nutrient analysis
# packages ----
SciViews::R
library(econum)

# import data inorganique ----
direction <- "data/cs002/aa3/inorganique/"

# create a list with all names
all_data <- dir(direction, full.names = TRUE)

# load data
repos_load(all_data[1])
nu <- EcoNumData_aa3

# create a loop to combine all dataset
for(f in all_data[-1]){
  repos_load(f)
  nu1 <- EcoNumData_aa3
  nu <- dplyr::bind_rows(nu, nu1)
  remove(nu1)
}
remove(all_data, direction, f, EcoNumData_aa3)
# filtor to keep only the data of the coralsalinity project
nu %>.%
  filter(., project == "coral_salinity002") -> nu_ino_cs

# minor corrections
nu_ino_cs[13, 18] <- "O"
nu_ino_cs[13, 15] <- " R5-C7-O"

# import data organique -----

direction <- "data/cs002/aa3/organique/"

# create a list with all names
all_data <- dir(direction, full.names = TRUE)

# load data
repos_load(all_data[1])
nu <- EcoNumData_aa3

# create a loop to combine all dataset
for(f in all_data[-1]){
  repos_load(f)
  nu1 <- EcoNumData_aa3
  nu <- dplyr::bind_rows(nu, nu1)
  remove(nu1)
}
remove(all_data, direction, f, EcoNumData_aa3)
# filtor to keep only the data of the coralsalinity project
nu %>.%
  filter(., project == "coral_salinity002") -> nu_o_cs

# combine datasets
nu_cs <- left_join(nu_ino_cs, nu_o_cs, by = c("sample_id","sample", "sample_date"))
rm(nu_ino_cs, nu_o_cs, nu)

nu_cs %>.%
  select(., contains("conc"), sample, sample_date) %>.%
  rename(., NOx_conc = NO3_conc) %>.%
  separate(., sample,
           into = c("respi", "cycle", "condi"), sep = "-") %>.%
  mutate(., number_day = as.numeric(round(difftime(sample_date, as.POSIXct("2018-04-12 00:00:00")), digits = 2)))-> nu_cs

# modify the negative value by 0 in NO2
nu_cs$NO2_conc[nu_cs$NO2_conc < 0] <- 0

nu_cs %>.%
  mutate(., NO3_conc = NOx_conc - NO2_conc) -> nu_cs

# modify the negative value by 0 in NO3
nu_cs$NO3_conc[nu_cs$NO3_conc < 0] <- 0
# correcetion " R5" into "R5"
nu_cs$respi[nu_cs$respi == " R5"] <- "R5"
nu_cs$respi <- as.factor(nu_cs$respi)
levels(nu_cs$respi)


position <- tibble(respi = c(paste0("R", 1:8), "A0", "A1", "A2"),
                   loc = c("A0", "A0", rep("A1", times = 3), rep("A2", times = 3) , "A0", "A1", "A2"))

nu_cs <- left_join(x = nu_cs,
                   y = tibble(respi =
                                c(paste0("R", 1:8), "A0", "A1", "A2"),
                              loc = c("A0", "A0",
                                      rep("A1", times = 3),
                                      rep("A2", times = 3),
                                      "A0", "A1", "A2"),
                              weight = c(6.224, 5.758, 5.480, 5.272,
                                         5.977, 5.209, 5.879, 4.594, NA, NA, NA)),  by = "respi")

nu_cs <-labelise(nu_cs,self = FALSE,
              label = list(PO4_conc = "Orthophosphates dissous",
                           NOx_conc = "Nitrates et Nitrites dissous",
                           NH4_conc = "Ammoniums dissous",
                           Ptot_conc = "Phosphore totale dissous",
                           Ntot_conc = "Azote totale dissous",
                           NO2_conc = "Nitrite dissous",
                           respi = "Positions",
                           cycle = "Cycle",
                           condi = "Ouvert/Fermé",
                           sample_date = "Date",
                           number_day = "Temps",
                           NO3_conc = "Nitrates dissous",
                           loc = "Conditions",
                           weight = "Masse des boutures"),
              units = list(PO4_conc = "µmol/L",
                           NOx_conc = "µmol/L",
                           NH4_conc = "µmol/L",
                           Ptot_conc = "µmol/L",
                           Ntot_conc = "µmol/L",
                           NO2_conc = "µmol/L",
                           respi = NA,
                           cycle = NA,
                           condi = NA,
                           sample_date = NA,
                           number_day = "Nombre de jours",
                           NO3_conc = "µmolmol/L",
                           loc = NA,
                           weight = "g"),
              as_labelled = TRUE)

# nu_cs$respi <- factor(nu_cs$respi,
#                       levels = c("A0", "A1", "A2", "R1", "R2",
#                                  "R3", "R4", "R5", "R6", "R7", "R8"),
#                       labels = c("Contrôle", "Hyposalin", "Hypersalin",
#                                  "R1-contrôle", "R2-contrôle",
#                                  "R3-hyposalin", "R4-hyposalin", "R5-hyposalin",
#                                  "R6-hypersalin", "R7-hypersalin", "R8-hypersalin"))
#
# nu_cs$cycle <- as.factor(nu_cs$cycle)
#
# nu_cs$condi <- as.character(nu_cs$condi)
# nu_cs$condi[nu_cs$condi == "NA"] <- "aqua"
# nu_cs$condi <- as.factor(nu_cs$condi)

# Exploratory step and correction

source("R/function.R")

pc_plot(data = nu_cs, y = "PO4_conc", x = "number_day", factor = "respi",
          rect_start = 29.83 + 0:7, rect_end = (29.83 + 0:7) + 0.5)

pc_plot(data = nu_cs, y = "NO3_conc", x = "number_day", factor = "respi",
        rect_start = 29.83 + 0:7, rect_end = (29.83 + 0:7) + 0.5)

pc_plot(data = nu_cs, y = "NH4_conc", x = "number_day", factor = "respi",
        rect_start = 29.83 + 0:7, rect_end = (29.83 + 0:7) + 0.5)

pc_plot(data = nu_cs, y = "NO2_conc", x = "number_day", factor = "respi",
        rect_start = 29.83 + 0:7, rect_end = (29.83 + 0:7) + 0.5)

pc_plot(data = nu_cs, y = "Ntot_conc", x = "number_day", factor = "respi",
        rect_start = 29.83 + 0:7, rect_end = (29.83 + 0:7) + 0.5)

pc_plot(data = nu_cs, y = "Ptot_conc", x = "number_day", factor = "respi",
        rect_start = 29.83 + 0:7, rect_end = (29.83 + 0:7) + 0.5)

nu_cs$NO3_conc[nu_cs$NO3_conc == 56.972] <- NA
nu_cs$NOx_conc[nu_cs$NOx_conc == 56.972] <- NA
nu_cs$Ntot_conc[nu_cs$Ntot_conc < 10] <- NA
nu_cs$Ptot_conc[nu_cs$Ptot_conc < 0.0] <- NA

data.io::write(nu_cs, type = "rds",  compress = "xz", file = "data/nutrient.rds")
