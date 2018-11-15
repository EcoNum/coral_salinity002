# Alcalinité

# Package ----
SciViews::R
library(econum)

# Import & combine data ----
direct <- "data/cs002/AT"
all_data <- dir(direct, full.names = TRUE)
# load data
repos_load(all_data[1])
AT <- EcoNumData_AT
attr(AT, "metadata")$sample -> AT$sample
attr(AT, "metadata")$sampledate -> AT$sampledate
# create a loop to combine all dataset
for(f in all_data[-1]){
  repos_load(f)
  AT1 <- EcoNumData_AT
  AT1$sample <- attr(AT1,"metadata")$sample
  AT1$sampledate <- attr(AT1,"metadata")$sampledate
  dplyr::bind_rows(AT,AT1) -> AT
  remove(AT1)
}

rm (EcoNumData_AT, all_data, direct, f)


AT %>.%
  separate(., sample, into = c("respi", "cycle", "condi"),
           sep = "-", remove = FALSE) -> AT

AT$condi[AT$condi == "0"] <- "O"

# first visualisation ----

chart(AT, mapping = aes( y= AT__ui, x= sampledate, color = respi)) +
  geom_line()

# correction ----

AT[139,5] <- "C7"
AT[139,6] <- "F"
AT[154,4] <- "R8"
AT[154,5] <- "C7"
AT[154,6] <- "O"
AT[34,7] <- "2018-05-15 13:00:00"
AT[70,7] <- "2018-05-15 13:00:00"
AT[93,7] <- "2018-05-15 13:00:00"
AT[117,7] <- "2018-05-15 13:00:00"
AT[154,7] <- "2018-05-15 13:00:00"
AT[105,7] <- "2018-05-15 13:00:00"
AT[83,7] <- "2018-05-16 13:00:00"
AT[142,7] <- "2018-05-16 13:00:00"
AT[69,7] <- "2018-05-16 14:00:00"
AT[81,7] <- "2018-05-16 14:00:00"
AT[104,7] <- "2018-05-16 14:00:00"
AT[116,7] <- "2018-05-16 14:00:00"
AT[128,7] <- "2018-05-16 14:00:00"
AT[140,7] <- "2018-05-16 14:00:00"
AT[153,7] <- "2018-05-16 14:00:00"
AT[101,7] <- "2018-05-16 07:00:00"
AT[137,1] <- 2730.902
AT[135,1] <-2718.144
AT[125,1] <-2724.825
AT[123,1] <-2714.544
AT[155,1] <-2701.531
AT[153,1] <-2688.156
AT[134,1] <-2694.443
AT[132,1] <-2680.608
AT[146,1] <-2691.102
AT[144,1] <-2684.608
AT[150,1] <-2642.067
AT[151,1] <-2647.304
AT[-65,] -> AT
AT[-89,] -> AT
AT[-147,] -> AT
AT[-53,] -> AT

# compute new variable
AT$number_day <- round(x = difftime(AT$sampledate,as.POSIXct("2018-04-12 00:00:00") , units = "day"), digits = 2)
AT$number_day <- as.numeric(AT$number_day)

# rename

AT %>.%
  rename(.,  at = AT__ui, salinity = S) -> AT

AT -> at
rm(AT)

## labelise
at <-labelise(at,self = FALSE,
              label = list(at = "Alcalinité",
                           salinity = "Salinité",
                           sample = "Echantillons",
                           respi = "Respiro",
                           cycle = "Cycle",
                           condi = "Condition",
                           sampledate = "Date",
                           number_day = "Nombre de jour"
              ),
              units = list(at = "mmol*kg^-1",
                           salinity = NA,
                           sample = NA,
                           respi = NA,
                           cycle = NA,
                           condi = NA,
                           sampledate = NA,
                           number_day = NA),
              as_labelled = TRUE)


data.io::write(at, file = "data/alcalinity.rds", type = "rds", compress = "xz")


write(pc, file = "data/physico.rds", type = "rds", compress = "xz")
