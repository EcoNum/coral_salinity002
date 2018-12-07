#data_respi_summary

repos_load("data/cs002/pc/respi_2018-05-29_17.45.00_5B195063_pc.RData")
cs002_respi <- EcoNumData_pc
remove(EcoNumData_pc)

labels <- c("R1" = "Normal", "R2"= "Normal", "R3" = "Hyposalin", "R4" = "Hyposalin", "R5" = "Hyposalin", "R6" = "Hypersalin", "R7" = "Hypersalin", "R8" = "Hypersalin")

cs002_respi$period <-as.factor(cs002_respi$period)
levels(cs002_respi$period)[levels(cs002_respi$period)=="j_c"] <- "Jour"
levels(cs002_respi$period)[levels(cs002_respi$period)=="n_c"] <- "Nuit"
levels(cs002_respi$period)[levels(cs002_respi$period)=="j_1"] <- "Jour_1"
levels(cs002_respi$period)[levels(cs002_respi$period)=="j_2"] <- "Jour_2"
cs002_respi$period <-as.character(cs002_respi$period)

cs002_respi$mean <- as.numeric(cs002_respi$mean)
cs002_respi$sd <- as.numeric(cs002_respi$sd)

cs002_respi %>%
  filter(condi == "R1" | condi == "R2") %>%
  filter(period == "Jour_2" | period == "Nuit") -> aa

aa$period <- as.factor(aa$period)
levels(aa$period)[levels(aa$period)=="Jour_2"] <- "Jour"


cs002_respi %>%
  filter(condi != "R1") %>%
  filter(condi != "R2") %>%
  filter(period == "Jour" | period == "Nuit") -> b

rbind(aa, b) -> ab
ab$mean <- as.numeric(ab$mean)
ab$sd <- as.numeric(ab$sd)


ab %>%
  ggplot(mapping = aes(x = period, y = mean), na.rm = FALSE) +
  geom_point() +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean+ sd)) +
  facet_grid(. ~ condi, labeller = labeller(condi = labels)) +
  geom_hline(yintercept = 0) +
  theme_gray() +
  ggtitle(expression(paste("Taux de respiration des boutures de ", italic ("Seriatopora hystrix"), " durant les conditions expérimentales"))) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(y = (expression(paste("[O"[ 2])~ paste("] moyen (µmol.g"^{ "-1"})~ paste(")"))), x = "Période") +
  theme(plot.title = element_text(size = 20), axis.title = element_text(size = 16), axis.text = element_text(size = 10), strip.text = element_text(size = 12))

ab$condi <- as.factor(ab$condi)
ab %>%
  mutate(loc = case_when(ab$condi == "R1" ~ "A0",
                         ab$condi == "R2" ~ "A0",
                         ab$condi == "R3" ~ "A1",
                         ab$condi == "R4" ~ "A1",
                         ab$condi == "R5" ~ "A1",
                         ab$condi == "R6" ~ "A2",
                         ab$condi == "R7" ~ "A2",
                         ab$condi == "R8" ~ "A2")) -> ab
ab$loc <- as.factor(ab$loc)

ab %>%
  group_by(period, loc) %>%
  mutate(mean_cond = mean(mean, na.rm = TRUE)) %>%
  mutate(sd_cond = sd(sd, na.rm = TRUE)) -> ab

labels <- c("A0" = "Contrôle", "A1" = "Hyposalin", "A2" = "Hypersalin")
ab %>%
  ggplot(mapping = aes(x = period, y = mean_cond), na.rm = FALSE) +
  geom_point() +
  geom_errorbar(aes(ymin = mean_cond - sd_cond, ymax = mean_cond+ sd_cond, width = 0.5 )) +
  facet_grid(. ~ loc, labeller = labeller(loc = labels)) +
  geom_hline(yintercept = 0) +
  theme_gray() +
  ggtitle(expression(paste("Taux de respiration des boutures de ", italic ("Seriatopora hystrix"), " durant les conditions expérimentales"))) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(y = (expression(paste("Flux [O"[ 2])~ paste("]  (µmol.h"^{ "-1"})~ paste(".g"^{ "-1"})~ paste(")"))), x = "Période") +
  theme(plot.title = element_text(size = 32), axis.title = element_text(size = 40), axis.text = element_text(size = 34,colour = "black" ), strip.text = element_text(size = 36), panel.background = element_rect(fill = "white", colour = "black"), panel.grid.major = element_line(colour = "grey"),  panel.grid.minor = element_line(colour = "grey", linetype = "dotted"))

# Test statistique -> Anova
ab %>%
  mutate(loc = case_when(ab$condi == "R1" ~ "A0",
                         ab$condi == "R2" ~ "A0",
                         ab$condi == "R3" ~ "A1",
                         ab$condi == "R4" ~ "A1",
                         ab$condi =="R5" ~ "A1",
                         ab$condi == "R6" ~ "A2",
                         ab$condi == "R7" ~ "A2",
                         ab$condi == "R8" ~ "A2")) -> ab


ab$loc <- as.factor(ab$loc)

# Période jour
ab %>%
  filter(period == "Jour") -> respi_jour

# Période nuit
ab %>%
  filter(period == "Nuit") -> respi_nuit

#1° Normalité des résidus = QQplot
# jour
 library(ggpubr)
a <- ggqqplot (ab, x = "mean")
ggpar(a, title = "qqplot des valeurs moyenne de photosynthèse brute (jour)", xlab = "Théorique", ylab = "Observée")


#2) Homoscédaticité
# jour
respi_jour [-7, ] -> respi_jour

bartlett.test(ab$mean, ab$loc)



#3) ANOVA
# Jour
anova(anova. <- lm(mean ~ loc, data = respi_jour))
summary(anova.)

summary(anovaComp. <- confint(multcomp::glht(anova.,
                                             linfct = multcomp::mcp("loc" = "Tukey")))) # Add a second factor if you want
.oma <- par(oma = c(0, 5.1, 0, 0)); plot(anovaComp.); par(.oma); rm(.oma)

# Nuit
anova(anova. <- lm(mean ~ loc, data = respi_nuit))
summary(anova.)

