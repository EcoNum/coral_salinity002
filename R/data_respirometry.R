SciViews::R
source("R/function.R")
library(econum)
# Import data
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

o2 %>%
  filter(sonde == "O2_1" ) -> o2_1

o2 %>%
  filter(sonde == "O2_2") -> o2_2

o2 %>%
  filter(sonde == "O2_3") -> o2_3

o2 %>%
  filter(sonde == "O2_4")-> o2_4

o2 %>%
  filter(sonde == "O2_5")-> o2_5

o2 %>%
  filter(sonde == "O2_6")-> o2_6

o2 %>%
  filter(sonde == "O2_7")-> o2_7

o2 %>%
  filter(sonde == "O2_8")-> o2_8

o2[o2$sonde == "O2_1", ]$date
o2_1$date

IKS$correct_O2_1 <- correct_monitoring(IKS$Time, IKS$O2_1, o2[o2$sonde == "O2_1", ]$date, o2[o2$sonde == "O2_1", ]$O2, extrapolate = TRUE)

plot(IKS$correct_O2_1)

IKS$correct_O2_2 <- correct_monitoring(IKS$Time, IKS$O2_2, o2[o2$sonde == "O2_2", ]$date, o2[o2$sonde == "O2_2", ]$O2, extrapolate = TRUE)
plot(IKS$correct_O2_2)

IKS$correct_O2_2 <- correct_monitoring(IKS$Time, IKS$O2_2, o2_2$date, o2_2$O2, extrapolate = TRUE)
plot(correct_O2_2)

correct_O2_3 <- correct_monitoring(IKS_exp$Time, IKS_exp$O2_3, o2_3$date, o2_3$O2, extrapolate = TRUE)
plot(correct_O2_3)

correct_O2_4 <- correct_monitoring(IKS_exp$Time,IKS_exp$O2_4, o2_4$date, o2_4$O2, extrapolate = TRUE)
plot(correct_O2_4)

correct_O2_5 <- correct_monitoring(IKS_exp$Time, IKS_exp$O2_5, o2_5$date, o2_5$O2, extrapolate = TRUE)
plot(correct_O2_5)

correct_O2_6 <- correct_monitoring(IKS_exp$Time,IKS_exp$O2_6, o2_6$date, o2_6$O2, extrapolate = TRUE)
plot(correct_O2_6)

correct_O2_7 <- correct_monitoring(IKS_exp$Time, IKS_exp$O2_7, o2_7$date, o2_7$O2, extrapolate = TRUE)
plot(correct_O2_7)

correct_O2_8 <- correct_monitoring(IKS_exp$Time, IKS_exp$O2_8, o2_8$date, o2_8$O2, extrapolate = TRUE)
plot(correct_O2_8)
