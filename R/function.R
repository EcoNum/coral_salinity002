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


# correct_monitoring -----

correct_monitoring <- function(dates, values, calib.dates, calib.values, extrapolate = FALSE) {
if (isTRUE(extrapolate)) rule <- 2 else rule <- 1
# Approximate the values in the series to the manual dates
series.values <- approx(dates, values, xout = calib.dates, rule = rule)$y
# Calculate deltas between series measurements and manual values
deltas <- calib.values - series.values
corr <- data.frame(dates = calib.dates, values = calib.values,
                   measures = series.values, deltas = deltas)
# Interpolate linearly these deltas
all_deltas <- approx(calib.dates, deltas, xout = dates, rule = rule)$y
# Apply the correction
res <- values + all_deltas
structure(res, correction = corr, dates = dates, deltas = all_deltas, class = c("corrected", "numeric"))
}

plot.corrected <- function(x, y, ...) {
  if (missing(y)) y <- x - attr(x, "deltas")
  dates <- attr(x, "dates")
  range <- range(x, y, na.rm = TRUE)
  plot(dates, y, type = "l", col = "gray", ylim = range)
  lines(dates, x, type = "l", col = "red")
  points(attr(x, "correction")$dates, attr(x, "correction")$values, col = "red")
}



# Oxygen balance on a chosen time interval from respirometer data
respirometry <- function(data, series, pos, n = 1, mass = 1, vol.respi = 1.3,
                         ref.respi = 0, main = "Variation d'oxygène en respiromètre", ...) {
  if (!is.integer(pos))
    stop("'pos' must be a vector of integers")
  if ((length(pos) %% 2) != 0)
    stop("'pos' must be a vector of even length (pairs of starts and stops)")
  if (length(pos) < n * 2)
    stop("Cannot take 'n' = ", n, " item when only ", length(pos), " elements in 'pos'")
  posn <- pos[c(n * 2 - 1, n * 2)]
  measure <- data[posn[1]:posn[2], c("Time", series)]
  names(measure) <- c("Time", "O2")

  plot(measure$Time, measure$O2, type = "l", xlab = "Temps", ylab = "[O2] (mg/L)",
       main = main, ...)

  reg <- lm(O2 ~ Time, data = measure)
  print(summary(reg))
  res <- coef(reg)["Time"] * 3600 * vol.respi

  abline(coef = coef(reg), col = "red", lwd = 2)

  res_corr <- res - ref.respi

  res_corr_mass <- res_corr / mass
  res <- data.frame(
    time0 = as.POSIXct(measure$Time[1]),
    time1 = as.POSIXct(measure$Time[nrow(measure)]),
    respi = res_corr_mass,
    ref.respi = ref.respi,
    ref = main)
  attr(res, "metadata") <- list(n = n, pos = posn, mass = mass,
                                vol.respi = vol.respi, reg = reg)
  res
}

# model predict by phG

predict_fun <- function(model, ...)
  UseMethod("predict_fun")

predict_fun.default <- predict_fun.lm <- function(model, ...) {
  xname <- attr(terms(pur_lin), "term.labels")
  if (length(xname) != 1)
    stop("You must have one predictor variable (x) only in the model")
  function(x)
    predict(model, newdata = set_names(list(x), xname))
}

predict_fun.nls <- function(model, ...) {
  xname <- names(model$dataClasses)
  if (length(xname) != 1)
    stop("You must have one predictor variable (x) only in the model")
  function(x)
    predict(model, newdata = set_names(list(x), xname))
}
#
# pur <- Puromycin[ Puromycin$state == "treated", ]
# pur_micmen <- nls(data = pur, rate ~ SSmicmen(conc, Vm, K))
# summary(pur_micmen)
# pur_lin <- lm(data = pur, rate ~ conc)
# summary(pur_lin)
# pur_2 <- lm(data = pur, rate ~ conc + I(conc^2))
# summary(pur_2)
# pur_3 <- lm(data = pur, rate ~ conc + I(conc^2) + I(conc^3))
# summary(pur_3)
#
# chart(data = pur, rate ~ conc) +
#   stat_function(fun = predict_fun(pur_micmen), aes(col = "Michaelis-Menten")) +
#   stat_function(fun = predict_fun(pur_lin), aes(col = "Linéaire")) +
#   stat_function(fun = predict_fun(pur_2), aes(col = "Quadrique")) +
#   stat_function(fun = predict_fun(pur_3), aes(col = "Conique")) +
#   geom_point() +
#   labs(col = "Modèle")
