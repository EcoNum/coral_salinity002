# poster MDC 2019 #########
# poster session
# by Guyliann Engels
# 2019-02-19
####################################
####################################

source("R/poster_zoology_2018.R")

gr_visu$cond1 <- factor(gr_visu$cond, levels = c("control", "hypersalin", "hyposalin"), labels = c("Contrôle", "Hypersaline", "Hyposaline"))

chart(gr_visu, formula  = gr_rate ~ nd1 %col=% id) +
  geom_point(show.legend = FALSE) +
  geom_line(show.legend = FALSE) +
  geom_vline(xintercept = c(0,6), linetype = "twodash", color = "red", alpha = .4) +
  facet_wrap(~ cond1, nrow = 3) +
  scale_x_continuous(breaks = c(-5, 0, 5, 10, 15, 20, 25, 30)) +
  annotate("rect", xmin = -Inf, xmax = 0, ymin = 1.6, ymax = 1.9, alpha = .2, fill = "blue") +
  annotate("text", x = -3, y = 1.75 ,label = "Av.stress") +
  annotate("rect", xmin = 0, xmax = 6, ymin = 1.6, ymax = 1.9, alpha = .2, fill = "red") +
  annotate("text", x = 3, y = 1.75 ,label = "Stress") +
  annotate("rect", xmin = 6, xmax = Inf, ymin = 1.6, ymax = 1.9, alpha = .2, fill = "green") +
  annotate("text", x = 20, y = 1.75 ,label = "Récupération") +
  labs( x = "Temps [j]", y = "Taux croissance [%/j]")-> plot1
plot1
#ggsave(filename = "analysis/poster/2019_mdc_mons/figure/plot1.png", plot = plot1, dpi = 1000, width = 5.79, height = 4.51)
#
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
  annotate("text", x = -3, y = 1.85 ,label = "Av.stress") +
  annotate("rect", xmin = 0, xmax = 6, ymin = 1.7, ymax = 2,
           alpha = .2, fill = "red") +
  annotate("text", x = 3, y = 1.85 ,label = "Stress") +
  annotate("rect", xmin = 6, xmax = Inf, ymin = 1.7, ymax = 2,
           alpha = .2, fill = "green") +
  annotate("text", x = 20, y = 1.85 ,label = "Récupération") +
  labs(y = "Rapport des taux de croissance", x ="Temps [j]" )-> plot2
plot2
#ggsave(filename = "analysis/poster/2019_mdc_mons/figure/plot2.png", plot = plot2, dpi = 1000, width = 5.79, height = 4.51)
#
tab_model <- data_frame(Condition = c("Hypersaline", "Hyposaline"),
                        ymin =  c(0.080, 0.424),
                        ths = c(2.809, 1.763),
                        thr = c(7.352, 12.765))
ggpubr::ggtexttable(tab_model, rows = NULL) -> tab1

#ggsave(filename = "analysis/poster/2019_mdc_mons/figure/tab1.png", plot = tab1, dpi = 1000, width = 5.79, height = 4.51)
#
respi_mean$cond1 <- factor(respi_mean$condition, levels = c("control", "hypersalin", "hyposalin"), labels = c("Contrôle", "Hypersaline", "Hyposaline"))
respi_mean$cycle1 <- factor(respi_mean$cycle, levels =c("J", "N"), labels = c("Jour", "Nuit"))

chart(respi_mean, mean ~ cycle1 %col=% respiro | cond1) +
  geom_point(show.legend = FALSE) +
  geom_hline(linetype = "dashed", yintercept = 0) +
  scale_y_continuous(limits = c(-0.35,0.35)) +
  labs(x = "Période")  +
  labs(y = "Taux de respiration [µmol/h.g]") -> plot3
plot3
#ggsave(filename = "analysis/poster/2019_mdc_mons/figure/plot3.png", plot = plot3, dpi = 1000, width = 5.79, height = 4.51)
#
tab_test <- data_frame(Condition = c("Hypersalin - Contrôle == 0",
                                     "Hyposalin - Contrôle == 0",
                                     "Hyposalin - Hypersalin == 0"),
                       "Valeur" = c(-0.168, -0.122, 0.046 ),
                       "Valeur de P" =  c(0.002, 0.010, 0.187))

ggpubr::ggtexttable(tab_test, rows = NULL) -> tab2
#ggsave(filename = "analysis/poster/2019_mdc_mons/figure/tab2.png", plot = tab2, dpi = 1000, width = 5.79, height = 4.51)
