# at analysis

# Package and data

SciViews::R
library(econum)
source("R/function.R")

at <- read("data/alcalinity.rds")


# first analyse ----
at %>.%
  filter(., respi == "A0" | respi == "A1" | respi == "A2") %>.%
  pc_plot(data = ., y = "at", x = "number_day", factor = "respi",
          rect_start = 29.83 + 0:6, rect_end = (29.83 + 0:6) + 0.5) +
  geom_rect(ymin = -Inf, ymax = Inf, xmin = 29.88, xmax = 30.38, alpha = 0.002) +
  labs(y = (expression(paste("Alcalinité")~ paste("(mmol.kg"^{ "-1"})~ paste(")"))), color = "Conditions")

# tab recap
at %>.%
  filter(., respi == "A0" | respi == "A1" | respi == "A2") %>.%
  group_by(.,respi) %>.%
  summarise(., mean = mean(at), sd = sd(at), number = n())

# Compute calcification rate

weight <- tibble(respi = paste0("R", 1:8),
                 weight = c(6.224, 5.758, 5.480, 5.272,
                            5.977, 5.209, 5.879, 4.594),
                 loc = c("A0", "A0", rep("A1", times = 3), rep("A2", times = 3)))

at %>.%
  filter(., condi != "NA") %>.%
  arrange(., respi, sampledate) %>.%
  mutate(., at_lag = lead(at),
         period = case_when(cycle == "C4" ~ "nuit",
                            cycle == "C7" ~ "jour",
                            cycle == "C10" ~ "jour")) %>.%
  filter(., condi == "O") -> at_red

left_join(at_red, weight, by = "respi") -> at_red

# compute calcification rate step 2
at_red %>%
  mutate (calc_rate = (((at - at_lag)/2)/weight)) -> at_red

# tab summary
at_red %>.%
  group_by(., loc, period) %>.%
  summarise(., mean = mean(at), sd = sd(at), number = n()) -> at_mean


at_red %>.%
  ungroup(.) %>.%
  group_by(., number_day, loc) %>.%
  arrange(., sampledate, loc) %>.%
  summarise(., mean_calc = mean(calc_rate), sd_calc = sd(calc_rate), number = n()) -> at_red1

at %>.%
  filter(., condi != "NA") %>.%
  arrange(., respi, sampledate) %>.%
  mutate(., at_lag = lead(at),
         period = case_when(cycle == "C4" ~ "nuit",
                            cycle == "C7" ~ "jour",
                            cycle == "C10" ~ "jour")) -> at1

weight <- tibble(respi = paste0("R", 1:8),
                 loc = c("A0", "A0", rep("A1", times = 3), rep("A2", times = 3)))

at1 <- left_join(at1, weight, by = "respi")

at1 %>.%
  filter(., loc == "A0") %>.%
  chart(., at ~ number_day )  +
  stat_summary(geom = "line", fun.y = "mean") +
  stat_summary(mapping = f_aes(at ~ number_day %col=% condi),
               geom = "pointrange", fun.data = "mean_sdl") -> a

at1 %>.%
  filter(., loc == "A1") %>.%
  chart(., at ~ number_day )  +
  stat_summary(geom = "line", fun.y = "mean") +
  stat_summary(mapping = f_aes(at ~ number_day %col=% condi),
               geom = "pointrange", fun.data = "mean_sdl") -> b

at1 %>.%
  filter(., loc == "A2") %>.%
  chart(., at ~ number_day )  +
  stat_summary(geom = "line", fun.y = "mean") +
  stat_summary(mapping = f_aes(at ~ number_day %col=% condi),
               geom = "pointrange", fun.data = "mean_sdl") -> c

alk_plot <- function(data, loca, rect_start, rect_end){

  a <- rect_start
  b <- rect_end
  data_rect <-tibble(x = a, y = b)

  data %>.%
    filter(., loc == loca) %>.%
    chart(., at ~ number_day )  +
    stat_summary(geom = "line", fun.y = "mean") +
    stat_summary(mapping = f_aes(at ~ number_day %col=% condi),
                 geom = "pointrange", fun.data = "mean_sdl") +
    geom_rect(
      data = data_rect[data_rect$x >= min(data$number_day) & data_rect$x <= max(data$number_day), ],
      inherit.aes = FALSE, aes(xmin = x, xmax = y, ymin = -Inf, ymax = Inf, alpha = 0.002),
      show.legend = FALSE)
}

alk_plot(data = at1, loca = "A1",
         rect_start = 29.83 + 0:5, rect_end = (29.83 + 0:5) + 0.5)

combine_charts(chartlist = list(a,b,c), common.legend = TRUE)
rm(a,b,c)

# Calcification rate Anova ----
# test d'hypothèse sur le taux de calcification moyen
# 1) Normalité des résidus

at_red$loc <- as.factor(at_red$loc)
at_red$period <- as.factor(at_red$period)

at_red %>.%
  group_by(., loc, period) %>.%
  summarise(., mean = mean(calc_rate), sd = sd(calc_rate), count = sum(!is.na(calc_rate))) -> a

anova. <- aov(data = at_red, calc_rate ~ loc*period)

#anova. <- aov(data = at_red, calc_rate ~ loc)

#plot(anova., which = 2)
anova. %>.%
  chart(broom::augment(.), aes(sample = .std.resid)) +
  geom_qq() +
  #geom_qq_line(colour = "darkgray") +
  labs(x = "Theoretical quantiles", y = "Standardized residuals") +
  ggtitle("Normal Q-Q")

summary(anovaComp. <- confint(multcomp::glht(anova.,
                                             linfct = multcomp::mcp(loc = "Tukey", period = "Tukey")))) # Add a second factor if you want
.oma <- par(oma = c(0, 5.1, 0, 0)); plot(anovaComp.); par(.oma); rm(.oma)

bartlett.test(data = at_red, calc_rate ~ loc)
bartlett.test(data = at_red, calc_rate ~ period)

broom::tidy(anova.)
