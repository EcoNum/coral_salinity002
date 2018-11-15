# p-c analysis

# Package and data

SciViews::R
library(econum)
source("R/function.R")

pc <- read("data/physico.rds")

# first analyse ----

# # caption <- Variation de la salinité durant la phase expérimentale de changement de salinité
#
# pc %>.%
#   filter(., periode == "experience") %>.%
#   group_by(., code) %>.%
#   summarise(., mean = mean(S), sd = sd(S), number = n())

# pc %>.%
#   filter(., T != "NA") %>.%
#   chart(., formula = T ~ number_day) +
#   geom_point(f_aes(T~ number_day%col=% code))+
#   geom_line(f_aes(T~ number_day%col=% code)) +
#   geom_rect(ymin = -1, ymax = 40, xmin = 29.83, xmax = 30.33, alpha = 0.005)+
#   geom_rect(ymin = -1, ymax = 40, xmin = 33.83, xmax = 34.33, alpha = 0.005) +
#   geom_rect(ymin = -1, ymax = 40, xmin = 34.83, xmax = 35.33, alpha = 0.005) +
#   geom_rect(ymin = -1, ymax = 40, xmin = 31.83, xmax = 32.33, alpha = 0.005) +
#   geom_rect(ymin = -1, ymax = 40, xmin = 32.83, xmax = 33.33, alpha = 0.005) +
#   geom_rect(ymin = -1, ymax = 40, xmin = 30.83, xmax = 31.33, alpha = 0.005)
#
#
pc %>.%
  filter(., ph != "NA"  & number_day >= 33) %>.%
  chart(., formula = ph ~ number_day) +
  geom_point(f_aes(ph ~ number_day%col=% code))+
  geom_line(f_aes(ph~ number_day%col=% code)) +
  geom_rect(ymin = -1, ymax = 40, xmin = 33.83, xmax = 34.33, alpha = 0.005) +
  geom_rect(ymin = -1, ymax = 40, xmin = 34.83, xmax = 35.33, alpha = 0.005) +
  geom_rect(ymin = -1, ymax = 40, xmin = 31.83, xmax = 32.33, alpha = 0.005) +
  geom_rect(ymin = -1, ymax = 40, xmin = 32.83, xmax = 33.33, alpha = 0.005) +
  geom_rect(ymin = -1, ymax = 40, xmin = 30.83, xmax = 31.33, alpha = 0.005)


# scatter_plot <- function(data, x, y) {
#   x <- enquo(x)
#   y <- enquo(y)
#
#   ggplot(data) + geom_point(aes(!!x, !!y))
# }
# scatter_plot(mtcars, disp, drat)
# error


# scatter_plot <- function(data, x, y) {
#   ggplot(data) + geom_point(aes_string(x, y))
# }
# scatter_plot(mtcars, "disp", "drat")
# ok

# simplification first step
geom_template <- function(ymin = -Inf, ymax = Inf, alpha = 0.002){
  list(
    geom_rect(ymin = ymin, ymax = ymax, xmin = 29.83, xmax = 30.33, alpha = alpha),
    geom_rect(ymin = ymin, ymax = ymax, xmin = 30.83, xmax = 31.33, alpha = alpha),
    geom_rect(ymin = ymin, ymax = ymax, xmin = 31.83, xmax = 32.33, alpha = alpha),
    geom_rect(ymin = ymin, ymax = ymax, xmin = 32.83, xmax = 33.33, alpha = alpha),
    geom_rect(ymin = ymin, ymax = ymax, xmin = 33.83, xmax = 34.33, alpha = alpha),
    geom_rect(ymin = ymin, ymax = ymax, xmin = 34.83, xmax = 35.33, alpha = alpha)
  )
}

pc %>.%
  filter(., ph != "NA"  & number_day >= 33) %>.%
  chart(., formula = ph ~ number_day) +
  geom_point(f_aes(ph ~ number_day%col=% code))+
  geom_line(f_aes(ph ~ number_day%col=% code)) +
  geom_template()

# simplification second step

data_rect <-tibble(x = 29.83 + 0:5, y = x + 0.5)

pc %>.%
  filter(., ph != "NA"  & number_day >= 33) %>.%
  chart(., formula = ph ~ number_day) +
  geom_point(f_aes(ph ~ number_day%col=% code))+
  geom_line(f_aes(ph ~ number_day%col=% code)) +
  geom_rect(
    data = data_rect[data_rect$x >= min(.$number_day) & data_rect$x <= max(.$number_day), ],
    inherit.aes = FALSE, aes(xmin = x, xmax = y, ymin = -Inf, ymax = Inf, alpha = 0.005), show.legend = FALSE)

# function to plot data pc
#
pc_plot <- function(data, y, x, factor, rect_start, rect_end){

  a <- rect_start
  b <- rect_end
  data_rect <-tibble(x = a, y = b)

  #data[!is.na(data[ , y]), ] -> data

  chart(data, formula = as.formula(paste(y, "~", x))) +
    geom_point(f_aes(as.formula(paste(y, "~", x, "%col=%", factor)))) +
    geom_line(f_aes(as.formula(paste(y, "~", x, "%col=%", factor)))) +
    geom_rect(
      data = data_rect[data_rect$x >= min(data$number_day) & data_rect$x <= max(data$number_day), ],
      inherit.aes = FALSE, aes(xmin = x, xmax = y, ymin = -Inf, ymax = Inf, alpha = 0.005),
      show.legend = FALSE)
}

pc %>.%
  filter(., T != "NA") %>.%
  pc_plot(data = ., y = "T", x = "number_day", factor = "code",
          rect_start = 29.83 + 0:5, rect_end = (29.83 + 0:5) + 0.5)
pc %>.%
  filter(., S != "NA") %>.%
  pc_plot(data = pc, y = "S", x = "number_day", factor = "code",
          rect_start = 29.83 + 0:5, rect_end = (29.83 + 0:5) + 0.5)
pc %>.%
  filter(., ph != "NA"  & number_day >33 ) %>.%
  pc_plot(data = ., y = "ph", x = "number_day", factor = "code",
          rect_start = 29.83 + 0:5, rect_end = (29.83 + 0:5) + 0.5)


