#=========================================================================================
# P-value curves to reproduce plots in the paper
# Author: Denis Infanger
# Date (dd.mm.yyyy): 21.11.2018
#=========================================================================================
#-----------------------------------------------------------------------------------------
# Load packages
#-----------------------------------------------------------------------------------------

library(ggpubr)
library(metafor)

#-----------------------------------------------------------------------------------------
# Set paths
#-----------------------------------------------------------------------------------------

# setwd("")

gpath <- "./output/graphics"
tpath <- "./output/text"
# scripts_path <- ""

#-----------------------------------------------------------------------------------------
# Load function
#-----------------------------------------------------------------------------------------

source(paste(scripts_path, "confidence_distributions.R", sep = "/"))

#=========================================================================================
# Graphics
#=========================================================================================
#-----------------------------------------------------------------------------------------
# Stamatakis et al. (2017): br J Sports Med. Dichotomous illustration (Poole 1987) (Figure 1)
#-----------------------------------------------------------------------------------------

plot_dat <- data.frame(
  HR = seq(0.7, 1.9, length.out = 100)
)

plot_dat$pvalue <- NA
plot_dat$pvalue[plot_dat$HR < 0.92 | plot_dat$HR > 1.55] <- 0
plot_dat$pvalue[plot_dat$HR >= 0.92 & plot_dat$HR <= 1.55] <- 1


theme_set(theme_bw())
p_stama_dichotom <- ggplot(data = plot_dat, aes(x = HR, y = pvalue)) +
  # geom_line(size = 1.5) +
  geom_segment(aes(x = 0.7, xend = 0.92, y = 0, yend = 0), size = 1.5) +
  geom_segment(aes(x = 1.55, xend = 1.9, y = 0, yend = 0), size = 1.5) +
  geom_segment(aes(x = 0.92, xend = 1.55, y = 1, yend = 1), size = 1.5) +
  geom_segment(aes(x = 0.92, xend = 0.92, y = 0, yend = 1), size = 1.5) +
  geom_segment(aes(x = 1.55, xend = 1.55, y = 0, yend = 1), size = 1.5) +
  # geom_segment(aes(x = 1.19, xend = 1.19, y = 0, yend = 1), size = 1.1, linetype = 2) +
  geom_point(aes(x = 1.19, y = 1), size = 4.2, pch = 21, colour = "black", fill = "black", stroke = 1.7) +
  geom_segment(aes(x = 1, xend = 1, y = 0, yend = 1), size = 1, linetype = 2) +
  xlab("HR") +
  ylab("Relation\nto\n95% confidence\ninterval") +
  scale_y_continuous(breaks = c(0, 1), labels = c("Incompatible/\nsignificant", "Compatible/\nnot significant")) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10), trans = "log") +
  theme(
    axis.title.y=element_text(colour = "black", size = 17, hjust = 0.5, vjust = 0.5, margin=margin(t = 0, r = -60, b = 0, l = 0), angle = 0),
    axis.title.x=element_text(colour = "black", size = 17),
    # axis.title.y=element_text(size=15,hjust=0.5, vjust=1),
    axis.text.x=element_text(colour = "black", size=15),
    axis.text.y=element_text(colour = "black", size=15),
    # plot.margin=unit(c(2,2,2,2,2),"line"),
    legend.position=c(0.9, 0.9),
    legend.title = element_blank(),
    legend.text=element_text(size=14),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    # panel.grid.major = element_line(colour=grey(0.8), size=0.5),
    legend.key=element_blank(),
    plot.title = element_text(size = 15, face = "bold"),
    strip.text.x=element_text(size=15)
  )

p_stama_dichotom

#-----------------------------------------------------------------------------------------
# Stamatakis et al. (2017): Br J Sports Med (Figure 2A)
#-----------------------------------------------------------------------------------------

stama <- conf_dist(
  estimate = c(
    0.1730998 # Stamatakis total sitting time
    # , log(1.10) # Petersen total sitting time (both sexes)
  )
  , stderr = c(
    0.1336387
  )
  , type = "coxreg"
  , plot_type = "p_val"
  , n_values = 1e4L
  , conf_level = c(0.95)
  , null_values = log(c(1))
  , trans = "exp"
  , alternative = "two_sided"
  , log_yaxis = FALSE
  , cut_logyaxis = 0.05
  , xlab = "HR"
  , xlim = log(c(0.7, 1.9))
  , together = FALSE
  , plot_p_limit = 1-0.999
  , plot_counternull = TRUE
)

stama$plot <- stama$plot +
  annotate(geom = "text", label = "A", x = 0.7, y = 0.99, size = 10, fontface = "bold")

stama$plot

stama$conf_frame
stama$counternull_frame

#-----------------------------------------------------------------------------------------
# Stamatakis et al. (2017) + Petersen et al. (2016) + Meta-Analysis (Figure 2B)
#-----------------------------------------------------------------------------------------

stama_petersen <- conf_dist(
  estimate = c(
    0.1730998 # Stamatakis total sitting time
    , log(1.10) # Petersen total sitting time (both sexes)
    , 0.1148205 # Meta-analytic estimate
  )
  , stderr = c(
    0.1336387 # Stamatakis total sitting time
    , 0.0773228 # Petersen total sitting time (both sexes)
    , 0.06692738 # Meta-analytic standard error
  )
  , est_names = c(
    "Stamatakis et al. (2017)"
    , "Petersen et al. (2016)"
    , "Meta-analysis"
  )
  , type = "coxreg"
  , plot_type = "p_val"
  , n_values = 1e4L
  , conf_level = c(0.95)
  , null_values = log(c(1))
  , trans = "exp"
  , alternative = "two_sided"
  , log_yaxis = FALSE
  , cut_logyaxis = 0.05
  , xlab = "HR"
  , xlim = log(c(0.7, 1.9))
  , together = TRUE
  , plot_p_limit = 0.001
  , plot_counternull = FALSE
)

stama_petersen$plot$layers[[1]] <- NULL

stama_petersen$plot <- stama_petersen$plot +
  geom_line(aes(x = values, y = p_two, linetype = variable), size = 1.25, inherit.aes = FALSE) +
  # scale_colour_manual(values = c("black", "black"), name = "") +
  scale_linetype_manual(values = c(1, 2, 3), name = "") +
  theme(
    legend.title = element_blank()
    , legend.spacing.x = unit(0.5, "cm")
    , legend.key.size = unit(1.6, "line")
    , legend.position= c(0.815, 0.87)
    
  ) +
  guides(
    linetype = guide_legend(nrow = 3, keywidth = 2.5)
  ) +
  annotate(geom = "text", label = "B", x = 0.7, y = 1, size = 10, fontface = "bold")

stama_petersen$plot

stama_petersen$conf_frame
stama_petersen$counternull_frame

#-----------------------------------------------------------------------------------------
# Critical-value plot (Figure 2C)
#-----------------------------------------------------------------------------------------

stama_petersen$res_frame$quant_vals <- qnorm(stama_petersen$res_frame$p_one)

theme_set(theme_bw())
p <- ggplot(data = stama_petersen$res_frame, aes(x = values, y = quant_vals)) +
  geom_line(aes(linetype = variable), size = 1.5) +
  geom_hline(yintercept = qnorm(0.05/2), linetype = 2) +
  xlab("HR") +
  ylab("Critical value") +
  scale_x_continuous(trans = "log", limits = c(0.7, 1.9), breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = seq(-4, 0, 0.5)) +
  scale_linetype_manual(name = "", values = c(1, 2, 3)) +
  theme(
    legend.title = element_blank()
    , legend.spacing.x = unit(0.5, "cm")
    , legend.key.size = unit(1.6, "line")
    , legend.position= c(0.82, 0.87)
    , axis.title.y.left=element_text(colour = "black", size = 17, hjust = 0.5, margin = margin(0, 10, 0, 0))
    , axis.title.y.right=element_text(colour = "black", size = 17, hjust = 0.5, margin = margin(0, 0, 0, 10))
    , axis.text.x=element_text(colour = "black", size=15)
    , axis.text.y=element_text(colour = "black", size=15)
    , axis.title.x=element_text(colour = "black", size = 17)
    , panel.grid.minor.y = element_blank()
    , plot.title = element_text(face = "bold")
    , legend.text=element_text(size=15)
  ) +
  guides(
    linetype = guide_legend(nrow = 3, keywidth = 2.5)
  ) +
  annotate(geom = "text", label = "C", x = 0.7, y = 0, size = 10, fontface = "bold") +
  geom_label(
    data = data.frame(theor_values = 0, p_value = qnorm(0.05/2), label = "0.05")
    , mapping = aes(x = theor_values, y = p_value, label = label)
    , inherit.aes = FALSE
    , label.size = NA
    , parse = TRUE
    , size = 5.5
    , hjust = "inward"
  )


# Combine the three plots into one

stama_peter_comb <- ggarrange(
  stama$plot
  , stama_petersen$plot
  , p
  , ncol = 1
  , nrow = 3
  # , heights = c(1, 1.1)
  , align = c("hv")
)

# ggsave(paste(gpath, "Fig1.tiff", sep = "/"), stama_peter_comb, width = 17.5*0.6, height = 27*0.6, dpi = 400, compression = "lzw")

#-----------------------------------------------------------------------------------------
# Canning et al. (2014) (Figure 3)
#-----------------------------------------------------------------------------------------

canning <- conf_dist(
  estimate = c(
    log(0.73)
  )
  , stderr = c(
    0.23849
  )
  , type = "general_z"
  , plot_type = "p_val"
  , n_values = 1e4L
  , conf_level = c(0.95)
  , null_values = c(0)
  , trans = "exp"
  , alternative = "two_sided"
  , log_yaxis = FALSE
  , cut_logyaxis = 0.05
  , xlab = "IRR"
  , xlim = log(c(0.35, 1.71))
  , together = FALSE
  , plot_p_limit = 1-0.999
  , plot_counternull = TRUE
)

canning$plot <- canning$plot +
  geom_vline(xintercept = 0.6, linetype = 2, size = 0.5) +
  scale_x_continuous(trans = "log", breaks = c(seq(0.3, 1, 0.1), seq(1, 1.8, 0.2)))

canning$conf_frame
canning$counternull_frame

# Assume clinically relevant reduction of risk of 40%, IRR = 0.6
# Proportional distance

log(0.73) - log(0.60)
log(1) - log(0.73)

# Proportional distance to confidence intervals

log(1.17) - log(1) # Distance upper limits to 1
log(0.60) - log(0.45) # Distance lower limit to 0.6

#-----------------------------------------------------------------------------
# Höchsmann et al. (2017) (Figure 4)
#-----------------------------------------------------------------------------

hochsmann <- conf_dist(
  estimate = c(1.36)
  # , n = c(30)
  , df = c(29)
  , stderr = abs(c(1.36/qt(0.327/2, df = 29)))
  # , tstat = c(1.5751)
  , type = "linreg"
  , plot_type = "p_val"
  , n_values = 1e4L
  , est_names = c("VO2peak")
  , conf_level = c(0.95)
  , null_values = c(-3.5)
  , log_yaxis = FALSE
  , cut_logyaxis = 0.05
  , trans = "identity"
  , alternative = "one_sided"
  , xlab = expression("Adjusted difference in VO"[2*"peak"]*" [ml/(kg" %*% " min)]")
  , xlim = c(-4, 6.5)
  , together = FALSE
  , plot_p_limit = 1 - 0.9999
)

hochsmann$plot$layers[[1]] <- NULL

hochsmann$plot <- hochsmann$plot +
  geom_line(aes(x = values, y = p_two, linetype = hypothesis), size = 1.25, inherit.aes = FALSE) +
  scale_linetype_manual(values = c(1, 2), name = "", labels = c(
    expression("H"[1]*": "*beta*" < "*theta)
    , expression("H"[1]*": "*beta*" > "*theta))
  ) +
  scale_x_continuous(limits = c(-4, 6.5), breaks = seq(-100, 100, 1)) +
  theme(
    legend.title = element_blank()
    , legend.text.align = 0
    , legend.text=element_text(size=17)
    , legend.spacing.x = unit(0.5, "cm")
    , legend.key.size = unit(2.1, "line")
    , legend.position= c(0.9, 0.9)
    , axis.title.x=element_text(colour = "black", size = 17, margin=margin(11, 0, 0, 0))
  )

hochsmann$conf_frame
hochsmann$counternull_frame
