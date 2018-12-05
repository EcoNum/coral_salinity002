

growth <- read("data/growth.rds")

 chart(growth, formula  = g_rate_day ~ number_day %col=% id |cond ) +
   geom_point() +
   geom_line() +
   geom_vline(xintercept = c(29, 36))

growth %>.%
  filter(., number_day == 29) %>.%
  group_by(., phase) %>.%
  summarise(., mean = mean(g_rate_day, na.rm = TRUE), sd = sd(g_rate_day, na.rm = TRUE), count = n())

library(seal.econum)
?skeleton_weight(
)
