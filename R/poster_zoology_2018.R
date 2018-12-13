# Zoology 2018 antwerpen #########
# poster session
# by Guyliann Engels
# 2018-12-13

# packages ----------------------------------------------------------------
SciViews::R
source("R/function.R")

# Growth rate----------------------------------------------------------------
## importation  & reshape ---------------------------------------------------
read("data/growth.rds") %>.%
  select(., id, skeleton_weight, number_day, cond, phase) %>.%
  group_by(., id) %>.%
  arrange(., number_day) %>.%
  mutate(.,
         skw_log = log(skeleton_weight),
         skm_log_lag = lag(skw_log),
         nd_lag = lag(number_day),
         gr_rate = (exp((skw_log - skm_log_lag)/(number_day-nd_lag)) -1)*100,
         nd1 = number_day - 29) %>.%
  filter(., !is.na(gr_rate))-> gr_visu

gr_visu <- labelise(gr_visu, self = FALSE,
                    label = list(nd1 = "Time",
                                 gr_rate = "Growth rate"),
                    units = list(gr_rate = "%/d",
                                 nd1 = "day"))

gr_visu$cond1 <- factor(gr_visu$cond, levels = c("control", "hypersalin", "hyposalin"), labels = c("Control", "Hypersaline", "Hyposaline"))

chart(gr_visu, formula  = gr_rate ~ nd1 %col=% id) +
  geom_point(show.legend = FALSE) +
  geom_line(show.legend = FALSE) +
  geom_vline(xintercept = c(0,6), linetype = "twodash", color = "red", alpha = .4) +
  facet_wrap(~ cond1, nrow = 3) +
  scale_x_continuous(breaks = c(-5, 0, 5, 10, 15, 20, 25, 30)) +
  annotate("rect", xmin = -Inf, xmax = 0, ymin = 1.6, ymax = 1.9, alpha = .2, fill = "blue") +
  annotate("text", x = -3, y = 1.75 ,label = "No stress") +
  annotate("rect", xmin = 0, xmax = 6, ymin = 1.6, ymax = 1.9, alpha = .2, fill = "red") +
  annotate("text", x = 3, y = 1.75 ,label = "Stress") +
  annotate("rect", xmin = 6, xmax = Inf, ymin = 1.6, ymax = 1.9, alpha = .2, fill = "green") +
  annotate("text", x = 20, y = 1.75 ,label = "After stress")  -> plot1
plot1
ggsave(filename = "analysis/poster/2018_zoology_antwerpen/figure/plot1.png", plot = plot1, dpi = 1000, width = 5.79, height = 4.51)

## modelise --------------
gr_visu %>.%
  filter(., cond == "control") %>.%
  group_by(., number_day) %>.%
  summarise(., control_mean = mean(gr_rate)) -> control_mean
gr_visu <- left_join(gr_visu, control_mean, by ="number_day")
gr_visu %>.%
  mutate(.,
         gr_divctrl = gr_rate / control_mean) -> gr_visu
gr_visu <- labelise(gr_visu, self = FALSE,
                    label = list(gr_divctrl = "Standardised growth rate"),
                    units = list(gr_divctrl = NA))

# modelise for growth rate --------------------------------------------------
anova1 <- lm(data = filter(gr_visu, nd1 == 0), gr_divctrl ~ cond)

mod3 <- nls(data = gr_visu,
            gr_divctrl ~ stress_recovery2(nd1, ymin, ths, thr),
            subset = gr_visu$cond == "hypersalin", trace = TRUE,
            start = list(ymin = 0, ths = 2, thr = 3))
# summary(mod3)
mod4 <- nls(data = gr_visu, gr_divctrl ~ stress_recovery2(nd1, ymin, ths, thr),
            subset = gr_visu$cond == "hyposalin", trace = TRUE,
            start = list(ymin = 0, ths = 2, thr = 3))
# summary(mod4)
nlme::nlsList(data = gr_visu, gr_divctrl ~ stress_recovery2(nd1, ymin, ths, thr) | cond, subset = gr_visu$cond %in% c("hypersalin", "hyposalin"), start = list(ymin = 0, ths = 2, thr = 3)) -> mod2
# summary(mod2)
stats::nls(data = gr_visu, gr_divctrl ~ stress_recovery2(nd1, ymin, ths, thr), subset = gr_visu$cond %in% c("hypersalin", "hyposalin"), start = list(ymin = 0, ths = 2, thr = 3)) -> nlsreduc
# summary(nlsreduc)
nlme::nlsList(data = gr_visu, gr_divctrl ~ stress_recovery2(nd1, ymin, ths, thr) | cond, subset = gr_visu$cond %in% c("hypersalin", "hyposalin"), start = list(ymin = 0, ths = 2, thr = 3)) -> nlsfull
# summary(nlsfull)
(nlshelper::anova_nlslist(nlsfull, nlsreduc) -> anova.)


chart(data = gr_visu, formula  = gr_divctrl ~ nd1 %col=% cond) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~ cond1 ,nrow =3) +
  geom_vline(xintercept = c(0,6), linetype = "twodash", color = "red", alpha = .4) +
  scale_color_manual(values=c("#969696", "#f6a924", "#00abcc")) +
  stat_function(fun = predict_fun(mod4), color ="#00abcc" ) +
  stat_function(fun = predict_fun(mod3),  color = "#f6a924") +
  geom_hline(yintercept = 1, color = "#969696") +
  theme(legend.position="bottom") +
  scale_x_continuous(breaks = c(-5, 0, 5, 10, 15, 20, 25, 30)) +
  annotate("rect", xmin = -Inf, xmax = 0, ymin = 1.7, ymax = 2,
           alpha = .2, fill = "blue") +
  annotate("text", x = -3, y = 1.85 ,label = "No stress") +
  annotate("rect", xmin = 0, xmax = 6, ymin = 1.7, ymax = 2,
           alpha = .2, fill = "red") +
  annotate("text", x = 3, y = 1.85 ,label = "Stress") +
  annotate("rect", xmin = 6, xmax = Inf, ymin = 1.7, ymax = 2,
           alpha = .2, fill = "green") +
  annotate("text", x = 20, y = 1.85 ,label = "After stress") +
  labs(y = "Growth rate ratio")-> plot2
plot2
ggsave(filename = "analysis/poster/2018_zoology_antwerpen/figure/plot2.png", plot = plot2, dpi = 1000, width = 5.79, height = 4.51)

tab_model <- data_frame(Condition = c("Hypersaline", "Hyposaline"),
                   ymin =  c(0.080, 0.424),
                   ths = c(2.809, 1.763),
                   thr = c(7.352, 12.765))
ggpubr::ggtexttable(tab_model, rows = NULL) -> tab1
ggsave(filename = "analysis/poster/2018_zoology_antwerpen/figure/tab1.png", plot = tab1, dpi = 1000, width = 5.79, height = 4.51)

# Respiration rate ------------------------------------------------------------
respi <- read("data/respiration.rds")
nubbins <- read("data/nubbins.rds")
respi <- left_join(respi, nubbins, by ="respiro")

respi %>.%
  group_by(., respiro, cycle, condition) %>.%
  summarise(., mean = mean(respi)) -> respi_mean

respi_mean <- labelise(respi_mean, self = FALSE,
                       label = list(mean = "Respiration rate",
                                    cycle = "Period"))

respi_mean$cond1 <- factor(respi_mean$condition, levels = c("control", "hypersalin", "hyposalin"), labels = c("Control", "Hypersaline", "Hyposaline"))
respi_mean$cycle1 <- factor(respi_mean$cycle, levels =c("J", "N"), labels = c("Day", "Night"))

chart(respi_mean, mean ~ cycle1 %col=% respiro | cond1) +
    geom_point(show.legend = FALSE) +
    geom_hline(linetype = "dashed", yintercept = 0) +
    scale_y_continuous(limits = c(-0.35,0.35)) +
    labs(x = "Period")  +
    labs(y = "Respiration rate [µmol/h.g]") -> plot3
plot3
ggsave(filename = "analysis/poster/2018_zoology_antwerpen/figure/plot3.png", plot = plot3, dpi = 1000, width = 5.79, height = 4.51)

respi_mean %>.%
  filter(., cycle =="J") -> respi_d

respi_mean %>.%
  filter(., cycle =="N") -> respi_n

bartlett.test(data = respi_d, mean ~ cond1)
anova(anova. <- lm(data = respi_d, mean ~ cond1))
summary(anovaComp. <- confint(multcomp::glht(anova.,
                                             linfct = multcomp::mcp(cond1 = "Tukey")))) # Add a second factor if you want
.oma <- par(oma = c(0, 5.1, 0, 0)); plot(anovaComp.); par(.oma); rm(.oma)

tab_test <- data_frame(Condition = c("Hypersalin - Control == 0",
                                     "Hyposalin - Control == 0",
                                     "Hyposalin - Hypersalin == 0"),
                       "Estimate value" = c(-0.168, -0.122, 0.046 ),
                       "P-value" =  c(0.002, 0.010, 0.187))

ggpubr::ggtexttable(tab_test, rows = NULL) -> tab2
ggsave(filename = "analysis/poster/2018_zoology_antwerpen/figure/tab2.png", plot = tab2, dpi = 1000, width = 5.79, height = 4.51)

anova(anova. <- lm(data = respi_n, mean ~ cond1))
