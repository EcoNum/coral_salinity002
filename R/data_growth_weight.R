# growth weight

# Packages ----
SciViews::R
library(econum)
library(visdat)

# Combine data ----
direction <- "data/cs002/weight"
# create a list with all names
all_data <- dir(direction, full.names = TRUE)
# load data
repos_load(all_data[1])
growth <- EcoNumData_weight
# create a loop to combine all dataset
for(f in all_data[-1]){
  repos_load(f)
  growth1 <- EcoNumData_weight

  growth <- dplyr::bind_rows(growth, growth1)
  remove(growth1)
}
remove(all_data, direction, f, EcoNumData_weight)

# manual correction ----
growth[5,4] <- NA
growth[12,4] <- NA
growth[19,4] <- NA
growth[25,4] <- NA
growth[32,4] <- NA
growth[39,4] <- NA
growth[22,4] <- NA
growth[42,4] <- 2.046
growth[62,4] <- 2.187
growth[82,4] <- 2.234
growth[102,4] <- 2.278
growth[122,4] <- 2.377
growth[142,4] <- 2.469
growth[2,4] <- NA
growth[190, 4] <- 4.171
growth[197, 4] <- NA

# Add buoyant_weight and skeletton weight ----
skeleton_weight <- function(buoyant_weight, S, T, P = 0, rho_aragonite = 2930){
  x <- seacarb::rho(S = S, T = T, P = P)
  y <- buoyant_weight / (1 - (x / rho_aragonite))
  attributes(y) <- NULL
  y
}

growth %>.%
  rename(., buoyant_weight = weight) %>.%
  mutate(., skeleton_weight = skeleton_weight(buoyant_weight = buoyant_weight,
                                              S = salinity,
                                              T = temperature,
                                              P = 0)) -> growth
# combine new variable and recode variable ----
growth$number_day <- round(x = difftime(growth$date,
                                        growth$date[1],
                                        units = "day"), digits = 0)
growth$number_day <- as.numeric(growth$number_day)
growth$localisation <- as.factor(growth$localisation)
growth$species <- as.factor(growth$species)

growth %>.%
  filter(., number_day != 7,
         ! id %in% c(6,9,11,16),
         buoyant_weight != "NA",
         number_day > 17) -> growth

# add condition to datasets ------
test <- data_frame(
  id = c(1, 2, 3, 4, 5, 7, 8, 10, 12, 13, 14, 15, 17, 18, 19, 20),
  cond = c("hypersalin", "control", "hypersalin",
           "control", "control", "hyposalin", "hyposalin", "hypersalin",
           "hypersalin", "hyposalin", "hyposalin", "control", "hyposalin",
           "hyposalin", "hypersalin", "hypersalin"))
growth <- left_join(growth, test, by = "id")
growth$id <- as.factor(growth$id)
growth$cond <- as.factor(growth$cond)
growth %>.%
  mutate(.,phase = case_when(number_day < 29 ~ "acclim",
                  number_day >= 29 & number_day < 36 ~ "expe",
                  number_day >= 36 ~ "recup")) -> growth
growth$phase <- as.factor(growth$phase)

# compute weight_gain, growth rate --------

growth %>.%
  group_by(., id) %>.%
  arrange(., number_day) %>.%
  mutate(.,
         wg_gain = skeleton_weight - skeleton_weight[1],
         wg_rate = (((skeleton_weight - skeleton_weight[1]))/(number_day - number_day[1]))*100,
         g_rate = (((skeleton_weight - skeleton_weight[1]) / skeleton_weight[1])/(number_day - number_day[1]))*100,
         sk_weight_lag = lag(skeleton_weight),
         nb_day_lag = lag(number_day),
         g_rate_day = (((skeleton_weight - sk_weight_lag) / sk_weight_lag)/(number_day - nb_day_lag))*100) -> growth



# chart(growth, formula  = g_rate_day ~ number_day %col=% id |cond ) +
#   geom_point() +
#   geom_line() +
#   geom_vline(xintercept = c(29, 36))

# Add label to variables :

growth <-labelise(growth  ,self = FALSE,
              label = list(localisation = "Aquarium",
                           species = "Espèce",
                           id = "Identifiant",
                           buoyant_weight = "Masse immergée",
                           salinity = "Salinité",
                           temperature = "Température",
                           date = "Date",
                           skeleton_weight = "Masse du squelette",
                           number_day = "Nombre de jour",
                           cond = "Condition",
                           phase = "Phase",
                           wg_gain = "Gain de masse squelettique",
                           g_rate = "Taux de croissance (masse initiale)",
                           g_rate_day = "Taux de croissance"),
              units = list(buoyant_weight = "g",
                           salinity = "PSU",
                           temperature = "C°",
                           skeleton_weight = "g",
                           wg_gain = "g/j",
                           g_rate = "‰/j",
                           g_rate_day = "%/j"),
              as_labelled = TRUE)


write(growth, file = "data/growth.rds", type = "rds", compress = "xz")
