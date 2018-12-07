SciViews::R
source("R/function.R")
library(econum)
# Import data ----
direct <- "data/cs002/IKS/experiment"
all_data <- dir(direct, full.names = TRUE)

# Boucle pour combiner l'ensemble des fichier
repos_load(all_data[1])

IKS <- EcoNumData_IKS.C
attr(IKS, "metadata")$sampledate -> IKS$sampledate

for(f in all_data[-1]){
  repos_load(f)
  IKS1 <- EcoNumData_IKS.C
  IKS1$sampledate <- attr(IKS1,"metadata")$sampledate
  dplyr::bind_rows(IKS,IKS1) -> IKS
  remove(IKS1)
}

IKS %>.%
  select(., - starts_with("tO2")) -> IKS
rm (EcoNumData_IKS.C, all_data, direct, EcoNumData_pc,f)

# import correct_data_file ----

repos_load("data/cs002/pc/o2_2018-05-11_17.45.00_5B0BD8A8_pc.RData")
o2 <- EcoNumData_pc
rm (EcoNumData_pc)

o2 %>%
  mutate(sonde = case_when(o2$code == "R1" ~ "O2_1",
                           o2$code == "R2" ~ "O2_2",
                           o2$code == "R3" ~ "O2_3",
                           o2$code == "R4" ~ "O2_4",
                           o2$code =="R5" ~ "O2_5",
                           o2$code == "R6" ~ "O2_6",
                           o2$code == "R7" ~ "O2_7",
                           o2$code == "R8" ~ "O2_8")) -> o2
o2$O2 <- as.numeric(o2$O2)
o2$IKS <- as.numeric(o2$IKS)

# recalculate data ----

IKS$O2_1_corr <- correct_monitoring(IKS$Time, IKS$O2_1, o2[o2$sonde == "O2_1", ]$date,
                                    o2[o2$sonde == "O2_1", ]$O2, extrapolate = TRUE)
plot(IKS$O2_1_corr)

IKS$O2_2_corr <- correct_monitoring(IKS$Time, IKS$O2_2, o2[o2$sonde == "O2_2", ]$date,
                                    o2[o2$sonde == "O2_2", ]$O2, extrapolate = TRUE)
plot(IKS$O2_2_corr)

IKS$O2_3_corr <- correct_monitoring(IKS$Time, IKS$O2_3, o2[o2$sonde == "O2_3", ]$date,
                                    o2[o2$sonde == "O2_3", ]$O2, extrapolate = TRUE)
plot(IKS$O2_3_corr)

IKS$O2_4_corr <- correct_monitoring(IKS$Time, IKS$O2_4, o2[o2$sonde == "O2_4", ]$date,
                                    o2[o2$sonde == "O2_4", ]$O2, extrapolate = TRUE)
plot(IKS$O2_4_corr)

IKS$O2_5_corr <- correct_monitoring(IKS$Time, IKS$O2_5, o2[o2$sonde == "O2_5", ]$date,
                                    o2[o2$sonde == "O2_5", ]$O2, extrapolate = TRUE)
plot(IKS$O2_3_corr)

IKS$O2_6_corr <- correct_monitoring(IKS$Time, IKS$O2_6, o2[o2$sonde == "O2_6", ]$date,
                                    o2[o2$sonde == "O2_6", ]$O2, extrapolate = TRUE)
plot(IKS$O2_6_corr)

IKS$O2_7_corr <- correct_monitoring(IKS$Time, IKS$O2_7, o2[o2$sonde == "O2_7", ]$date,
                                    o2[o2$sonde == "O2_7", ]$O2, extrapolate = TRUE)
plot(IKS$O2_7_corr)

IKS$O2_8_corr <- correct_monitoring(IKS$Time, IKS$O2_8, o2[o2$sonde == "O2_8", ]$date,
                                    o2[o2$sonde == "O2_8", ]$O2, extrapolate = TRUE)
plot(IKS$O2_8_corr)

# Rename data ----
IKS %>.%
  rename(., time = Time, iks_time = IKSTime) -> IKS

# Save data ----
write(IKS, file = "data/iks_respiro.rds", type = "rds", compress = "xz")
