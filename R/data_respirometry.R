# analysis respirometry
# Traitements des signaux obtenu avec l'IKS et les sonde O2
# acquisition des données de respiration

# packges and function ----
SciViews::R
source("R/function.R")

# Import data ----
iks <- read("data/iks_respiro.rds")

# Dataset with skeleton_weight
nubbins <- read("data/nubbins.rds")

read("data/growth.rds") %>.%
  filter(., number_day == 32) %>.%
  select(., id, number_day, skeleton_weight) %>.%
  left_join(nubbins,.,  by = "id") -> nubbins


# Calculate respiration rate ----
## add ref_respi for respiration rate
. <- data_frame(respiro = paste("R", 1:8 ,sep = ""),
                        ref_respi = c(-0.024, -0.024, -0.11, -0.091,
                                      -0.079, -0.065, -0.060, -0.045))
nubbins <- left_join(nubbins, ., by = "respiro")


# plot_iks() ----

plot_iks <- function(x,y, data) {
  # add period with interest
  dat <- as.POSIXct("2018-05-14 11:00:00")
  a <- 0:43
  a <- a*60*120
  data_1 <- as.POSIXct("2018-05-14 11:00:00") + a
  data_2 <- as.POSIXct("2018-05-14 12:00:00") + a
  # graph
  plot(x,y, type = "l", xlab = "Temps", ylab = "[O2] mg/L");
  rect(xleft = data_1, xright = data_2, ybottom = 6, ytop = 13,
       density = 40, col ="gainsboro", border = "gainsboro", lwd = 1.5);
  abline (v = as.POSIXct("2018-05-15 11:00:00"), col = "green", lwd = 3);
  abline (v = as.POSIXct("2018-05-15 20:00:00"), col = "purple", lwd = 3);
  abline (v = as.POSIXct("2018-05-16 08:00:00"), col = "blue", lwd = 3);
  abline (v = as.POSIXct("2018-05-16 20:00:00"), col ="purple", lwd = 3);
  abline (v = as.POSIXct("2018-05-17 08:00:00"), col = "blue", lwd = 3);
  abline (v = as.POSIXct("2018-05-17 14:00:00"),  col = "green", lwd = 3);
  grid()
}

# respiro 1 : J1-J

iks %>.%
  mutate(., period = case_when(
    time > as.POSIXct("2018-05-15 11:00:00") & time < as.POSIXct("2018-05-15 20:00:00") ~ "J1_J",
    time > as.POSIXct("2018-05-15 20:00:00") & time < as.POSIXct("2018-05-16 08:00:00") ~ "J1_N",
    time > as.POSIXct("2018-05-16 08:00:00") & time < as.POSIXct("2018-05-16 20:00:00") ~ "J2_J",
    time > as.POSIXct("2018-05-16 20:00:00") & time < as.POSIXct("2018-05-17 08:00:00") ~ "J2_N",
    time > as.POSIXct("2018-05-17 08:00:00") & time < as.POSIXct("2018-05-17 14:00:00") ~ "J3_J"
  )) -> iks

iks %>.%
  rename(., Time = time) -> iks


# Respiro 1 ----

## Graph de base
plot_iks(x =iks$Time, y = iks$O2_1_corr)

## graph en fonction de la période
### J1_J -----
iks %>.%
  filter(., period =="J1_J") %>.%
  plot_iks(x =.$Time, y = .$O2_1_corr)

respi1_pos <- identify(iks$Time, iks$O2_1_corr)
period <- "J1_1"
series <- "O2_1_corr"
mass <- 5.988247 #g
ref.respi <- -0.024
title <- "R1"
n1 <- 1

position <- data_frame(series = series, period = period, pos = respi1_pos)

res <- respirometry(iks, series, respi1_pos, n = n1, mass = mass, ref.respi = - ref.respi, main = title)
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+1, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+2, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+3, mass = mass, ref.respi = ref.respi, main = title))
res$period <- period

# creation d'un data_frame qui garde les position employées
respi <- res

### J1_N ----
iks %>.%
  filter(., period =="J1_N") %>.%
  plot_iks(x =.$Time, y = .$O2_1_corr)

respi1_pos <- identify(iks$Time, iks$O2_1_corr)
period <- "J1_N"
series <- "O2_1_corr"
mass <- 5.988247 #g
ref.respi <- -0.024
title <- "R1"
n1 <- 1

position1 <- data_frame(series = series, period = period, pos = respi1_pos)

res <- respirometry(iks, series, respi1_pos, n = n1, mass = mass, ref.respi = - ref.respi, main = title)
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+1, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+2, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+3, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+4, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+5, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+6, mass = mass, ref.respi = ref.respi, main = title))
res$period <- period

# creation d'un data_frame qui garde les position employées
respi1 <- res
# combinaison des dataframes
respi <- bind_rows(respi, respi1)
position <- bind_rows(position, position1)

### J2_J ----
iks %>.%
  filter(., period =="J2_J") %>.%
  plot_iks(x =.$Time, y = .$O2_1_corr)

respi1_pos <- identify(iks$Time, iks$O2_1_corr)
period <- "J2_J"
series <- "O2_1_corr"
mass <- 5.988247 #g
ref.respi <- -0.024
title <- "R1"
n1 <- 1

position1 <- data_frame(series = series, period = period, pos = respi1_pos)

res <- respirometry(iks, series, respi1_pos, n = n1, mass = mass, ref.respi = - ref.respi, main = title)
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+1, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+2, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+3, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+4, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+5, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+6, mass = mass, ref.respi = ref.respi, main = title))
res$period <- period

# creation d'un data_frame qui garde les position employées
respi1 <- res
# combinaison des dataframes
respi <- bind_rows(respi, respi1)
position <- bind_rows(position, position1)


### J2_N ----
iks %>.%
  filter(., period =="J2_N") %>.%
  plot_iks(x =.$Time, y = .$O2_1_corr)

respi1_pos <- identify(iks$Time, iks$O2_1_corr)
period <- "J2_N"
series <- "O2_1_corr"
mass <- 5.988247 #g
ref.respi <- -0.024
title <- "R1"
n1 <- 1

position1 <- data_frame(series = series, period = period, pos = respi1_pos)

res <- respirometry(iks, series, respi1_pos, n = n1, mass = mass, ref.respi = - ref.respi, main = title)
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+1, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+2, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+3, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+4, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+5, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+6, mass = mass, ref.respi = ref.respi, main = title))
res$period <- period

# creation d'un data_frame qui garde les position employées
respi1 <- res
# combinaison des dataframes
respi <- bind_rows(respi, respi1)
position <- bind_rows(position, position1)

### J3_J ----
iks %>.%
  filter(., period =="J3_J") %>.%
  plot_iks(x =.$Time, y = .$O2_1_corr)

respi1_pos <- identify(iks$Time, iks$O2_1_corr)
period <- "J3_J"
series <- "O2_1_corr"
mass <- 5.988247 #g
ref.respi <- -0.024
title <- "R1"
n1 <- 1

position1 <- data_frame(series = series, period = period, pos = respi1_pos)

res <- respirometry(iks, series, respi1_pos, n = n1, mass = mass, ref.respi = - ref.respi, main = title)
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+1, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+2, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+3, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+4, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+5, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+6, mass = mass, ref.respi = ref.respi, main = title))
res$period <- period

# creation d'un data_frame qui garde les position employées
respi1 <- res
# combinaison des dataframes
respi <- bind_rows(respi, respi1)
position <- bind_rows(position, position1)

# Respiro 2 ----

### J1_J ----
iks %>.%
  filter(., period =="J1_J") %>.%
  plot_iks(x =.$Time, y = .$O2_2_corr)

respi1_pos <- identify(iks$Time, iks$O2_2_corr)
period <- "J1_J"
series <- "O2_2_corr"
mass <- 5.522528 #g
ref.respi <- -0.024
title <- "R2"
n1 <- 1

position1 <- data_frame(series = series, period = period, pos = respi1_pos)

res <- respirometry(iks, series, respi1_pos, n = n1, mass = mass, ref.respi = - ref.respi, main = title)
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+1, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+2, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+3, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+4, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+5, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+6, mass = mass, ref.respi = ref.respi, main = title))
res$period <- period

# creation d'un data_frame qui garde les position employées
respi1 <- res
# combinaison des dataframes
respi <- bind_rows(respi, respi1)
position <- bind_rows(position, position1)


### J1_N ----
iks %>.%
  filter(., period =="J1_N") %>.%
  plot_iks(x =.$Time, y = .$O2_2_corr)

respi1_pos <- identify(iks$Time, iks$O2_2_corr)
period <- "J1_N"
series <- "O2_2_corr"
mass <- 5.522528 #g
ref.respi <- -0.024
title <- "R2"
n1 <- 1

position1 <- data_frame(series = series, period = period, pos = respi1_pos)

res <- respirometry(iks, series, respi1_pos, n = n1, mass = mass, ref.respi = - ref.respi, main = title)
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+1, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+2, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+3, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+4, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+5, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+6, mass = mass, ref.respi = ref.respi, main = title))
res$period <- period

# creation d'un data_frame qui garde les position employées
respi1 <- res
# combinaison des dataframes
respi <- bind_rows(respi, respi1)
position <- bind_rows(position, position1)

### J2_J ----
iks %>.%
  filter(., period =="J2_J") %>.%
  plot_iks(x =.$Time, y = .$O2_2_corr)

respi1_pos <- identify(iks$Time, iks$O2_2_corr)
period <- "J2_J"
series <- "O2_2_corr"
mass <- 5.522528 #g
ref.respi <- -0.024
title <- "R2"
n1 <- 1

position1 <- data_frame(series = series, period = period, pos = respi1_pos)

res <- respirometry(iks, series, respi1_pos, n = n1, mass = mass, ref.respi = - ref.respi, main = title)
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+1, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+2, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+3, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+4, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+5, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+6, mass = mass, ref.respi = ref.respi, main = title))
res$period <- period

# creation d'un data_frame qui garde les position employées
respi1 <- res
# combinaison des dataframes
respi <- bind_rows(respi, respi1)
position <- bind_rows(position, position1)

### J2_N ----
iks %>.%
  filter(., period =="J2_N") %>.%
  plot_iks(x =.$Time, y = .$O2_2_corr)

respi1_pos <- identify(iks$Time, iks$O2_2_corr)
period <- "J2_N"
series <- "O2_2_corr"
mass <- 5.522528 #g
ref.respi <- -0.024
title <- "R2"
n1 <- 1

position1 <- data_frame(series = series, period = period, pos = respi1_pos)

res <- respirometry(iks, series, respi1_pos, n = n1, mass = mass, ref.respi = - ref.respi, main = title)
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+1, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+2, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+3, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+4, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+5, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+6, mass = mass, ref.respi = ref.respi, main = title))
res$period <- period

# creation d'un data_frame qui garde les position employées
respi1 <- res
# combinaison des dataframes
respi <- bind_rows(respi, respi1)
position <- bind_rows(position, position1)

### J3_J ----
iks %>.%
  filter(., period =="J3_J") %>.%
  plot_iks(x =.$Time, y = .$O2_2_corr)

respi1_pos <- identify(iks$Time, iks$O2_2_corr)
period <- "J3_J"
series <- "O2_2_corr"
mass <- 5.522528 #g
ref.respi <- -0.024
title <- "R2"
n1 <- 1

position1 <- data_frame(series = series, period = period, pos = respi1_pos)

res <- respirometry(iks, series, respi1_pos, n = n1, mass = mass, ref.respi = - ref.respi, main = title)
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+1, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+2, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+3, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+4, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+5, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+6, mass = mass, ref.respi = ref.respi, main = title))
res$period <- period

# creation d'un data_frame qui garde les position employées
respi1 <- res
# combinaison des dataframes
respi <- bind_rows(respi, respi1)
position <- bind_rows(position, position1)


# Respiro 3 ----

### J1_J ----

iks %>.%
  filter(., period =="J1_J") %>.%
  plot_iks(x =.$Time, y = .$O2_3_corr)

respi1_pos <- identify(iks$Time, iks$O2_3_corr)
period <- "J1_J"
series <- "O2_3_corr"
mass <- 5.157239 #g
ref.respi <- -0.110
title <- "R3"
n1 <- 1

position1 <- data_frame(series = series, period = period, pos = respi1_pos)

res <- respirometry(iks, series, respi1_pos, n = n1, mass = mass, ref.respi = - ref.respi, main = title)
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+1, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+2, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+3, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+4, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+5, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+6, mass = mass, ref.respi = ref.respi, main = title))
res$period <- period

# creation d'un data_frame qui garde les position employées
respi1 <- res
# combinaison des dataframes
respi <- bind_rows(respi, respi1)
position <- bind_rows(position, position1)


### J1_N ----

iks %>.%
  filter(., period =="J1_N") %>.%
  plot_iks(x =.$Time, y = .$O2_3_corr)

respi1_pos <- identify(iks$Time, iks$O2_3_corr)
period <- "J1_N"
series <- "O2_3_corr"
mass <- 5.157239 #g
ref.respi <- -0.110
title <- "R3"
n1 <- 1

position1 <- data_frame(series = series, period = period, pos = respi1_pos)

res <- respirometry(iks, series, respi1_pos, n = n1, mass = mass, ref.respi = - ref.respi, main = title)
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+1, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+2, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+3, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+4, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+5, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+6, mass = mass, ref.respi = ref.respi, main = title))
res$period <- period

# creation d'un data_frame qui garde les position employées
respi1 <- res
# combinaison des dataframes
respi <- bind_rows(respi, respi1)
position <- bind_rows(position, position1)

### J2_J ----

iks %>.%
  filter(., period =="J2_J") %>.%
  plot_iks(x =.$Time, y = .$O2_3_corr)

respi1_pos <- identify(iks$Time, iks$O2_3_corr)
period <- "J2_J"
series <- "O2_3_corr"
mass <- 5.157239 #g
ref.respi <- -0.110
title <- "R3"
n1 <- 1

position1 <- data_frame(series = series, period = period, pos = respi1_pos)

res <- respirometry(iks, series, respi1_pos, n = n1, mass = mass, ref.respi = - ref.respi, main = title)
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+1, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+2, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+3, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+4, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+5, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+6, mass = mass, ref.respi = ref.respi, main = title))
res$period <- period

# creation d'un data_frame qui garde les position employées
respi1 <- res
# combinaison des dataframes
respi <- bind_rows(respi, respi1)
position <- bind_rows(position, position1)

### J2_N ----

iks %>.%
  filter(., period =="J2_N") %>.%
  plot_iks(x =.$Time, y = .$O2_3_corr)

respi1_pos <- identify(iks$Time, iks$O2_3_corr)
period <- "J2_N"
series <- "O2_3_corr"
mass <- 5.157239 #g
ref.respi <- -0.110
title <- "R3"
n1 <- 1

position1 <- data_frame(series = series, period = period, pos = respi1_pos)

res <- respirometry(iks, series, respi1_pos, n = n1, mass = mass, ref.respi = - ref.respi, main = title)
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+1, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+2, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+3, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+4, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+5, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+6, mass = mass, ref.respi = ref.respi, main = title))
res$period <- period

# creation d'un data_frame qui garde les position employées
respi1 <- res
# combinaison des dataframes
respi <- bind_rows(respi, respi1)
position <- bind_rows(position, position1)

### J3_J ----

iks %>.%
  filter(., period =="J3_J") %>.%
  plot_iks(x =.$Time, y = .$O2_3_corr)

respi1_pos <- identify(iks$Time, iks$O2_3_corr)
period <- "J3_J"
series <- "O2_3_corr"
mass <- 5.157239 #g
ref.respi <- -0.110
title <- "R3"
n1 <- 1

position1 <- data_frame(series = series, period = period, pos = respi1_pos)

res <- respirometry(iks, series, respi1_pos, n = n1, mass = mass, ref.respi = - ref.respi, main = title)
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+1, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+2, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+3, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+4, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+5, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+6, mass = mass, ref.respi = ref.respi, main = title))
res$period <- period

# creation d'un data_frame qui garde les position employées
respi1 <- res
# combinaison des dataframes
respi <- bind_rows(respi, respi1)
position <- bind_rows(position, position1)


# Respiro 4 ----

### J1_J ----

iks %>.%
  filter(., period =="J1_J") %>.%
  plot_iks(x =.$Time, y = .$O2_4_corr)

respi1_pos <- identify(iks$Time, iks$O2_4_corr)
period <- "J1_J"
series <- "O2_4_corr"
mass <- 5.883266 #g
ref.respi <- -0.091
title <- "R4"
n1 <- 1

position1 <- data_frame(series = series, period = period, pos = respi1_pos)

res <- respirometry(iks, series, respi1_pos, n = n1, mass = mass, ref.respi = - ref.respi, main = title)
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+1, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+2, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+3, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+4, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+5, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+6, mass = mass, ref.respi = ref.respi, main = title))
res$period <- period

# creation d'un data_frame qui garde les position employées
respi1 <- res
# combinaison des dataframes
respi <- bind_rows(respi, respi1)
position <- bind_rows(position, position1)


### J1_N ----

iks %>.%
  filter(., period =="J1_N") %>.%
  plot_iks(x =.$Time, y = .$O2_4_corr)

respi1_pos <- identify(iks$Time, iks$O2_4_corr)
period <- "J1_N"
series <- "O2_4_corr"
mass <- 5.883266 #g
ref.respi <- -0.091
title <- "R4"
n1 <- 1

position1 <- data_frame(series = series, period = period, pos = respi1_pos)

res <- respirometry(iks, series, respi1_pos, n = n1, mass = mass, ref.respi = - ref.respi, main = title)
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+1, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+2, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+3, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+4, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+5, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+6, mass = mass, ref.respi = ref.respi, main = title))
res$period <- period

# creation d'un data_frame qui garde les position employées
respi1 <- res
# combinaison des dataframes
respi <- bind_rows(respi, respi1)
position <- bind_rows(position, position1)

### J2_J ----

iks %>.%
  filter(., period =="J2_J") %>.%
  plot_iks(x =.$Time, y = .$O2_4_corr)

respi1_pos <- identify(iks$Time, iks$O2_4_corr)
period <- "J2_J"
series <- "O2_4_corr"
mass <- 5.883266 #g
ref.respi <- -0.091
title <- "R4"
n1 <- 1

position1 <- data_frame(series = series, period = period, pos = respi1_pos)

res <- respirometry(iks, series, respi1_pos, n = n1, mass = mass, ref.respi = - ref.respi, main = title)
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+1, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+2, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+3, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+4, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+5, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+6, mass = mass, ref.respi = ref.respi, main = title))
res$period <- period

# creation d'un data_frame qui garde les position employées
respi1 <- res
# combinaison des dataframes
respi <- bind_rows(respi, respi1)
position <- bind_rows(position, position1)

### J2_N ----

iks %>.%
  filter(., period =="J2_N") %>.%
  plot_iks(x =.$Time, y = .$O2_4_corr)

respi1_pos <- identify(iks$Time, iks$O2_4_corr)
period <- "J2_N"
series <- "O2_4_corr"
mass <- 5.883266 #g
ref.respi <- -0.091
title <- "R4"
n1 <- 1

position1 <- data_frame(series = series, period = period, pos = respi1_pos)

res <- respirometry(iks, series, respi1_pos, n = n1, mass = mass, ref.respi = - ref.respi, main = title)
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+1, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+2, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+3, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+4, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+5, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+6, mass = mass, ref.respi = ref.respi, main = title))
res$period <- period

# creation d'un data_frame qui garde les position employées
respi1 <- res
# combinaison des dataframes
respi <- bind_rows(respi, respi1)
position <- bind_rows(position, position1)


### J3_J ----

iks %>.%
  filter(., period =="J3_J") %>.%
  plot_iks(x =.$Time, y = .$O2_4_corr)

# Respiro 5 ----


### J1_J ----

iks %>.%
  filter(., period =="J1_J") %>.%
  plot_iks(x =.$Time, y = .$O2_5_corr)

respi1_pos <- identify(iks$Time, iks$O2_5_corr)
period <- "J1_J"
series <- "O2_5_corr"
mass <- 5.321132 #g
ref.respi <- -0.079
title <- "R5"
n1 <- 1

position1 <- data_frame(series = series, period = period, pos = respi1_pos)

res <- respirometry(iks, series, respi1_pos, n = n1, mass = mass, ref.respi = - ref.respi, main = title)
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+1, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+2, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+3, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+4, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+5, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+6, mass = mass, ref.respi = ref.respi, main = title))
res$period <- period

# creation d'un data_frame qui garde les position employées
respi1 <- res
# combinaison des dataframes
respi <- bind_rows(respi, respi1)
position <- bind_rows(position, position1)

### J1_N ----

iks %>.%
  filter(., period =="J1_N") %>.%
  plot_iks(x =.$Time, y = .$O2_5_corr)

respi1_pos <- identify(iks$Time, iks$O2_5_corr)
period <- "J1_N"
series <- "O2_5_corr"
mass <- 5.321132 #g
ref.respi <- -0.079
title <- "R5"
n1 <- 1

position1 <- data_frame(series = series, period = period, pos = respi1_pos)

res <- respirometry(iks, series, respi1_pos, n = n1, mass = mass, ref.respi = - ref.respi, main = title)
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+1, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+2, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+3, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+4, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+5, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+6, mass = mass, ref.respi = ref.respi, main = title))
res$period <- period

# creation d'un data_frame qui garde les position employées
respi1 <- res
# combinaison des dataframes
respi <- bind_rows(respi, respi1)
position <- bind_rows(position, position1)



### J2_J ----

iks %>.%
  filter(., period =="J2_J") %>.%
  plot_iks(x =.$Time, y = .$O2_5_corr)

respi1_pos <- identify(iks$Time, iks$O2_5_corr)
period <- "J2_J"
series <- "O2_5_corr"
mass <- 5.321132 #g
ref.respi <- -0.079
title <- "R5"
n1 <- 1

position1 <- data_frame(series = series, period = period, pos = respi1_pos)

res <- respirometry(iks, series, respi1_pos, n = n1, mass = mass, ref.respi = - ref.respi, main = title)
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+1, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+2, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+3, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+4, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+5, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+6, mass = mass, ref.respi = ref.respi, main = title))
res$period <- period

# creation d'un data_frame qui garde les position employées
respi1 <- res
# combinaison des dataframes
respi <- bind_rows(respi, respi1)
position <- bind_rows(position, position1)


### J2_N ----

iks %>.%
  filter(., period =="J2_N") %>.%
  plot_iks(x =.$Time, y = .$O2_5_corr)

respi1_pos <- identify(iks$Time, iks$O2_5_corr)
period <- "J2_N"
series <- "O2_5_corr"
mass <- 5.321132 #g
ref.respi <- -0.079
title <- "R5"
n1 <- 1

position1 <- data_frame(series = series, period = period, pos = respi1_pos)

res <- respirometry(iks, series, respi1_pos, n = n1, mass = mass, ref.respi = - ref.respi, main = title)
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+1, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+2, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+3, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+4, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+5, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+6, mass = mass, ref.respi = ref.respi, main = title))
res$period <- period

# creation d'un data_frame qui garde les position employées
respi1 <- res
# combinaison des dataframes
respi <- bind_rows(respi, respi1)
position <- bind_rows(position, position1)


### J3_J ----

iks %>.%
  filter(., period =="J3_J") %>.%
  plot_iks(x =.$Time, y = .$O2_5_corr)

respi1_pos <- identify(iks$Time, iks$O2_5_corr)
period <- "J3_J"
series <- "O2_5_corr"
mass <- 5.321132 #g
ref.respi <- -0.079
title <- "R5"
n1 <- 1

position1 <- data_frame(series = series, period = period, pos = respi1_pos)

res <- respirometry(iks, series, respi1_pos, n = n1, mass = mass, ref.respi = - ref.respi, main = title)
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+1, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+2, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+3, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+4, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+5, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+6, mass = mass, ref.respi = ref.respi, main = title))
res$period <- period

# creation d'un data_frame qui garde les position employées
respi1 <- res
# combinaison des dataframes
respi <- bind_rows(respi, respi1)
position <- bind_rows(position, position1)

# Respiro 6 ----
#
### J1_J ----

iks %>.%
  filter(., period =="J1_J") %>.%
  plot_iks(x =.$Time, y = .$O2_6_corr)

respi1_pos <- identify(iks$Time, iks$O2_6_corr)
period <- "J1_J"
series <- "O2_6_corr"
mass <- 5.188552 #g
ref.respi <- -0.065
title <- "R6"
n1 <- 1

position1 <- data_frame(series = series, period = period, pos = respi1_pos)

res <- respirometry(iks, series, respi1_pos, n = n1, mass = mass, ref.respi = - ref.respi, main = title)
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+1, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+2, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+3, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+4, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+5, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+6, mass = mass, ref.respi = ref.respi, main = title))
res$period <- period

# creation d'un data_frame qui garde les position employées
respi1 <- res
# combinaison des dataframes
respi <- bind_rows(respi, respi1)
position <- bind_rows(position, position1)

### J1_N ----

iks %>.%
  filter(., period =="J1_N") %>.%
  plot_iks(x =.$Time, y = .$O2_6_corr)

respi1_pos <- identify(iks$Time, iks$O2_6_corr)
period <- "J1_N"
series <- "O2_6_corr"
mass <- 5.188552 #g
ref.respi <- -0.065
title <- "R6"
n1 <- 1

position1 <- data_frame(series = series, period = period, pos = respi1_pos)

res <- respirometry(iks, series, respi1_pos, n = n1, mass = mass, ref.respi = - ref.respi, main = title)
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+1, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+2, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+3, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+4, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+5, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+6, mass = mass, ref.respi = ref.respi, main = title))
res$period <- period

# creation d'un data_frame qui garde les position employées
respi1 <- res
# combinaison des dataframes
respi <- bind_rows(respi, respi1)
position <- bind_rows(position, position1)


### J2_J ----

iks %>.%
  filter(., period =="J2_J") %>.%
  plot_iks(x =.$Time, y = .$O2_6_corr)

respi1_pos <- identify(iks$Time, iks$O2_6_corr)
period <- "J2_J"
series <- "O2_6_corr"
mass <- 5.188552 #g
ref.respi <- -0.065
title <- "R6"
n1 <- 1

position1 <- data_frame(series = series, period = period, pos = respi1_pos)

res <- respirometry(iks, series, respi1_pos, n = n1, mass = mass, ref.respi = - ref.respi, main = title)
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+1, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+2, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+3, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+4, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+5, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+6, mass = mass, ref.respi = ref.respi, main = title))
res$period <- period

# creation d'un data_frame qui garde les position employées
respi1 <- res
# combinaison des dataframes
respi <- bind_rows(respi, respi1)
position <- bind_rows(position, position1)


### J2_N ----

iks %>.%
  filter(., period =="J2_N") %>.%
  plot_iks(x =.$Time, y = .$O2_6_corr)

respi1_pos <- identify(iks$Time, iks$O2_6_corr)
period <- "J2_N"
series <- "O2_6_corr"
mass <- 5.188552 #g
ref.respi <- -0.065
title <- "R6"
n1 <- 1

position1 <- data_frame(series = series, period = period, pos = respi1_pos)

res <- respirometry(iks, series, respi1_pos, n = n1, mass = mass, ref.respi = - ref.respi, main = title)
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+1, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+2, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+3, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+4, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+5, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+6, mass = mass, ref.respi = ref.respi, main = title))
res$period <- period

# creation d'un data_frame qui garde les position employées
respi1 <- res
# combinaison des dataframes
respi <- bind_rows(respi, respi1)
position <- bind_rows(position, position1)

### J3_J ----

iks %>.%
  filter(., period =="J3_J") %>.%
  plot_iks(x =.$Time, y = .$O2_6_corr)

respi1_pos <- identify(iks$Time, iks$O2_6_corr)
period <- "J3_J"
series <- "O2_6_corr"
mass <- 5.188552 #g
ref.respi <- -0.065
title <- "R6"
n1 <- 1

position1 <- data_frame(series = series, period = period, pos = respi1_pos)

res <- respirometry(iks, series, respi1_pos, n = n1, mass = mass, ref.respi = - ref.respi, main = title)
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+1, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+2, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+3, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+4, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+5, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+6, mass = mass, ref.respi = ref.respi, main = title))
res$period <- period

# creation d'un data_frame qui garde les position employées
respi1 <- res
# combinaison des dataframes
respi <- bind_rows(respi, respi1)
position <- bind_rows(position, position1)


# Respiro 7 ----
### J1_J ----

iks %>.%
  filter(., period =="J1_J") %>.%
  plot_iks(x =.$Time, y = .$O2_7_corr)

# inutilisable

### J1_N ----
iks %>.%
  filter(., period =="J1_N") %>.%
  plot_iks(x =.$Time, y = .$O2_7_corr)


respi1_pos <- identify(iks$Time, iks$O2_7_corr)
period <- "J1_N"
series <- "O2_7_corr"
mass <- 4.560010 #g
ref.respi <- -0.060
title <- "R7"
n1 <- 1

position1 <- data_frame(series = series, period = period, pos = respi1_pos)

res <- respirometry(iks, series, respi1_pos, n = n1, mass = mass, ref.respi = - ref.respi, main = title)
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+1, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+2, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+3, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+4, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+5, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+6, mass = mass, ref.respi = ref.respi, main = title))
res$period <- period

# creation d'un data_frame qui garde les position employées
respi1 <- res
# combinaison des dataframes
respi <- bind_rows(respi, respi1)
position <- bind_rows(position, position1)


### J2_J ----
iks %>.%
  filter(., period =="J2_J") %>.%
  plot_iks(x =.$Time, y = .$O2_7_corr)


respi1_pos <- identify(iks$Time, iks$O2_7_corr)
period <- "J2_J"
series <- "O2_7_corr"
mass <- 4.560010 #g
ref.respi <- -0.060
title <- "R7"
n1 <- 1

position1 <- data_frame(series = series, period = period, pos = respi1_pos)

res <- respirometry(iks, series, respi1_pos, n = n1, mass = mass, ref.respi = - ref.respi, main = title)
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+1, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+2, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+3, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+4, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+5, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+6, mass = mass, ref.respi = ref.respi, main = title))
res$period <- period

# creation d'un data_frame qui garde les position employées
respi1 <- res
# combinaison des dataframes
respi <- bind_rows(respi, respi1)
position <- bind_rows(position, position1)


### J2_N ----
iks %>.%
  filter(., period =="J2_N") %>.%
  plot_iks(x =.$Time, y = .$O2_7_corr)


respi1_pos <- identify(iks$Time, iks$O2_7_corr)
period <- "J2_N"
series <- "O2_7_corr"
mass <- 4.560010 #g
ref.respi <- -0.060
title <- "R7"
n1 <- 1

position1 <- data_frame(series = series, period = period, pos = respi1_pos)

res <- respirometry(iks, series, respi1_pos, n = n1, mass = mass, ref.respi = - ref.respi, main = title)
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+1, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+2, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+3, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+4, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+5, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+6, mass = mass, ref.respi = ref.respi, main = title))
res$period <- period

# creation d'un data_frame qui garde les position employées
respi1 <- res
# combinaison des dataframes
respi <- bind_rows(respi, respi1)
position <- bind_rows(position, position1)


### J3_J ----
iks %>.%
  filter(., period =="J3_J") %>.%
  plot_iks(x =.$Time, y = .$O2_7_corr)


respi1_pos <- identify(iks$Time, iks$O2_7_corr)
period <- "J3_J"
series <- "O2_7_corr"
mass <- 4.560010 #g
ref.respi <- -0.060
title <- "R7"
n1 <- 1

position1 <- data_frame(series = series, period = period, pos = respi1_pos)

res <- respirometry(iks, series, respi1_pos, n = n1, mass = mass, ref.respi = - ref.respi, main = title)
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+1, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+2, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+3, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+4, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+5, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+6, mass = mass, ref.respi = ref.respi, main = title))
res$period <- period

# creation d'un data_frame qui garde les position employées
respi1 <- res
# combinaison des dataframes
respi <- bind_rows(respi, respi1)
position <- bind_rows(position, position1)


# Respiro 8 ----
### J1_J ----

iks %>.%
  filter(., period =="J1_J") %>.%
  plot_iks(x =.$Time, y = .$O2_8_corr)


respi1_pos <- identify(iks$Time, iks$O2_8_corr)
period <- "J1_J"
series <- "O2_8_corr"
mass <- 5.841742 #g
ref.respi <- -0.045
title <- "R8"
n1 <- 1

position1 <- data_frame(series = series, period = period, pos = respi1_pos)

res <- respirometry(iks, series, respi1_pos, n = n1, mass = mass, ref.respi = - ref.respi, main = title)
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+1, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+2, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+3, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+4, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+5, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+6, mass = mass, ref.respi = ref.respi, main = title))
res$period <- period

# creation d'un data_frame qui garde les position employées
respi1 <- res
# combinaison des dataframes
respi <- bind_rows(respi, respi1)
position <- bind_rows(position, position1)

### J1_N ----

iks %>.%
  filter(., period =="J1_N") %>.%
  plot_iks(x =.$Time, y = .$O2_8_corr)


respi1_pos <- identify(iks$Time, iks$O2_8_corr)
period <- "J1_N"
series <- "O2_8_corr"
mass <- 5.841742 #g
ref.respi <- -0.045
title <- "R8"
n1 <- 1

position1 <- data_frame(series = series, period = period, pos = respi1_pos)

res <- respirometry(iks, series, respi1_pos, n = n1, mass = mass, ref.respi = - ref.respi, main = title)
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+1, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+2, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+3, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+4, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+5, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+6, mass = mass, ref.respi = ref.respi, main = title))
res$period <- period

# creation d'un data_frame qui garde les position employées
respi1 <- res
# combinaison des dataframes
respi <- bind_rows(respi, respi1)
position <- bind_rows(position, position1)


### J2_J ----

iks %>.%
  filter(., period =="J2_J") %>.%
  plot_iks(x =.$Time, y = .$O2_8_corr)


respi1_pos <- identify(iks$Time, iks$O2_8_corr)
period <- "J2_J"
series <- "O2_8_corr"
mass <- 5.841742 #g
ref.respi <- -0.045
title <- "R8"
n1 <- 1

position1 <- data_frame(series = series, period = period, pos = respi1_pos)

res <- respirometry(iks, series, respi1_pos, n = n1, mass = mass, ref.respi = - ref.respi, main = title)
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+1, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+2, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+3, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+4, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+5, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+6, mass = mass, ref.respi = ref.respi, main = title))
res$period <- period

# creation d'un data_frame qui garde les position employées
respi1 <- res
# combinaison des dataframes
respi <- bind_rows(respi, respi1)
position <- bind_rows(position, position1)


### J2_N ----

iks %>.%
  filter(., period =="J2_N") %>.%
  plot_iks(x =.$Time, y = .$O2_8_corr)


respi1_pos <- identify(iks$Time, iks$O2_8_corr)
period <- "J2_N"
series <- "O2_8_corr"
mass <- 5.841742 #g
ref.respi <- -0.045
title <- "R8"
n1 <- 1

position1 <- data_frame(series = series, period = period, pos = respi1_pos)

res <- respirometry(iks, series, respi1_pos, n = n1, mass = mass, ref.respi = - ref.respi, main = title)
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+1, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+2, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+3, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+4, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+5, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+6, mass = mass, ref.respi = ref.respi, main = title))
res$period <- period

# creation d'un data_frame qui garde les position employées
respi1 <- res
# combinaison des dataframes
respi <- bind_rows(respi, respi1)
position <- bind_rows(position, position1)

### J3_J ----

iks %>.%
  filter(., period =="J3_J") %>.%
  plot_iks(x =.$Time, y = .$O2_8_corr)


respi1_pos <- identify(iks$Time, iks$O2_8_corr)
period <- "J3_J"
series <- "O2_8_corr"
mass <- 5.841742 #g
ref.respi <- -0.045
title <- "R8"
n1 <- 1

position1 <- data_frame(series = series, period = period, pos = respi1_pos)

res <- respirometry(iks, series, respi1_pos, n = n1, mass = mass, ref.respi = - ref.respi, main = title)
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+1, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+2, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+3, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+4, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+5, mass = mass, ref.respi = ref.respi, main = title))
res <- rbind(res, respirometry(iks, series, respi1_pos, n = n1+6, mass = mass, ref.respi = ref.respi, main = title))
res$period <- period

# creation d'un data_frame qui garde les position employées
respi1 <- res
# combinaison des dataframes
respi <- bind_rows(respi, respi1)
position <- bind_rows(position, position1)

# modification de l'encodage ----

respi$period[respi$period == "J1_1"] <- "J1_J"

respi %>.%
  separate(., col = period , into = c("days", "cycle"), remove = FALSE) %>.%
  rename(., respiro = ref) -> respi

# save data -----
write(respi, file = "data/respiration.rds",
      type ="rds", compress ="xz")
write(position, file = "data/position.rds",
      type ="rds", compress ="xz")





