# physico-chemical analysis

# Packages ----
SciViews::R
library(econum)

# Import Data
repos_load("data/cs002/pc/pc_2018-05-29_17.45.00_5B1406AE_pc.RData")

## Correction variable
pc <- EcoNumData_pc
rm(EcoNumData_pc)

pc$S <- as.numeric(pc$S)
pc$T <- as.numeric(pc$T)
pc$pH <- as.numeric(pc$pH)

pc$number_day <-difftime(pc$date, "2018-04-12 00:00:00", units = "day")
pc$number_day <- as.numeric(pc$number_day)

pc[89, 4] <- 31.2
pc[90, 4] <- 36.6
pc[70,5] <- 25.2

pc$cond <- as.factor(pc$cond)
pc$code <- factor(pc$code,levels = c("A0", "A1", "A2"), labels = c("Contrôle", "Hyposalin", "Hypersalin"))

pc %>.%
  rename(., ph = pH) -> pc

## labelise

pc <-labelise(pc,self = FALSE,
              label = list(cond = "Conditions",
                           code = "Conditions",
                           date = "Temps",
                           S = "Salinité",
                           T = "Température",
                           ph = "pH",
                           number_day = "Nombre de jour"
                           ),
              units = list(cond = NA,
                           code = NA,
                           date = NA,
                           S = NA,
                           T = "C°",
                           ph = NA,
                           number_day = NA),
              as_labelled = TRUE)

## new variable
pc %>.%
  mutate(., periode = case_when(number_day >= 30 & number_day < 35.6 ~ "experience",
                                number_day > 35.6 ~ "return",
                                number_day < 30 ~ "change")) -> pc

# Write data
write(pc, file = "data/physico.rds", type = "rds", compress = "xz")
