# function

# package ----
SciViews::R

# pc_plot ----
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
      inherit.aes = FALSE, aes(xmin = x, xmax = y, ymin = -Inf, ymax = Inf, alpha = 0.002),
      show.legend = FALSE)
}

# pc %>.%
#   filter(., T != "NA") %>.%
#   pc_plot(data = ., y = "T", x = "number_day", factor = "code",
#           rect_start = 29.83 + 0:5, rect_end = (29.83 + 0:5) + 0.5)



# alk_plot ----
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

# alk_plot(data = at1, loca = "A1",
#          rect_start = 29.83 + 0:5, rect_end = (29.83 + 0:5) + 0.5)
