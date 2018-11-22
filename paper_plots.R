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
scripts_path <- "./scripts"

#-----------------------------------------------------------------------------------------
# Load function
#-----------------------------------------------------------------------------------------

source(paste(scripts_path, "confidence_distributions.R", sep = "/"))

#=========================================================================================
# Graphics
#=========================================================================================
#-----------------------------------------------------------------------------------------
# Stamatakis et al. (2017): br J Sports Med
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
  , plot_type = "s_val"
  , n_values = 1e4L
  , conf_level = c(0.95)
  , null_values = c(0)
  , trans = "exp"
  , alternative = "two_sided"
  , log_yaxis = TRUE
  , cut_logyaxis = 0.05
  , xlab = "HR"
  , xlim = log(c(0.7, 1.9))
  , together = FALSE
  , plot_p_limit = 1-0.999
)

stama$plot <- stama$plot +
  annotate(geom = "text", label = "A", x = 0.7, y = 0.99, size = 10, fontface = "bold") +
  # geom_segment(aes(x = 1.413685, y = -0.001, xend = 1.413685, yend = 0.195), linetype = 3) +
  geom_point(aes(x = 1.413685, y = 0.195), size = 4, pch = 21, colour = "black", fill = "white", stroke = 1.7)

stama$plot

stama$conf_frame
stama$counternull_frame

#-----------------------------------------------------------------------------------------
# Stamatakis et al. (2017) + Petersen et al. (2016)
#-----------------------------------------------------------------------------------------

stama_petersen <- conf_dist(
  estimate = c(
    0.1730998   # Stamatakis total sitting time
    , log(1.10) # Petersen total sitting time (both sexes)
  )
  , stderr = c(
    0.1336387   # Stamatakis total sitting time
    , 0.0773228 # Petersen total sitting time (both sexes)
  )
  , est_names = c(
    "Stamatakis et al. (2017)"
    , "Petersen et al. (2016)"
  )
  , type = "coxreg"
  , plot_type = "p_val"
  , n_values = 1e4L
  , conf_level = c(0.95)
  , null_values = c(0)
  , trans = "exp"
  , alternative = "two_sided"
  , log_yaxis = TRUE
  , cut_logyaxis = 0.05
  , xlab = "HR"
  , xlim = log(c(0.7, 1.9))
  , together = TRUE
  , plot_p_limit = 0.001
)

stama_petersen$plot$layers[[1]] <- NULL

stama_petersen$plot <- stama_petersen$plot +
  geom_line(aes(x = values, y = p_two, linetype = variable), size = 1.25, inherit.aes = FALSE) +
  scale_linetype_manual(values = c(1, 2), name = "") +
  theme(
    legend.title = element_blank()
    , legend.spacing.x = unit(0.5, "cm")
    , legend.key.size = unit(2.1, "line")
    , legend.position= c(0.815, 0.9)
  ) +
  guides(
    linetype = guide_legend(nrow = 2, keywidth = 2.5)
  ) +
  annotate(geom = "text", label = "B", x = 0.7, y = 1, size = 10, fontface = "bold")

stama_petersen$plot

stama_petersen$conf_frame
stama_petersen$counternull_frame

# Combine those

stama_peter_comb <- ggarrange(
  stama$plot
  , stama_petersen$plot
  , ncol = 1
  , nrow = 2
  , heights = c(1, 1.1)
)

stama_peter_comb

#-----------------------------------------------------------------------------------------
# Canning et al. (2014)
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
  , log_yaxis = TRUE
  , cut_logyaxis = 0.05
  , xlab = "IRR"
  , xlim = log(c(0.25, 1.61))
  , together = FALSE
  , plot_p_limit = 1-0.999
)

canning$plot <- canning$plot +
  geom_vline(xintercept = 0.6, linetype = 2, size = 0.5) +
  geom_point(aes(x = 0.5329, y = 0.1851), size = 4, pch = 21, colour = "black", fill = "white", stroke = 1.7) +
  scale_x_continuous(breaks = seq(0.2, 1.6, 0.1), limits = c(0.25, 1.61))

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
# Höchsmann et al. (2017)
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
  , log_yaxis = TRUE
  , cut_logyaxis = 0.05
  , trans = "identity"
  , alternative = "one_sided"
  , xlab = expression("Adjusted difference in VO"[2*"peak"]*" [ml/(kg" %*% " min)]")
  , xlim = c(-4, 6.5)
  , together = FALSE
  , plot_p_limit = 1-0.9999
)

hochsmann$plot$layers[[1]] <- NULL

hochsmann$plot <- hochsmann$plot +
  geom_line(aes(x = values, y = p_two, linetype = hypothesis), size = 1.25, inherit.aes = FALSE) +
  # scale_colour_manual(values = c("black", "black"), name = "") +
  scale_linetype_manual(values = c(1, 2), name = "", labels = c(
    expression("H"[1]*": "*beta*" < "*theta)
    , expression("H"[1]*": "*beta*" > "*theta))
  ) +
  # scale_linetype_manual(values = c(1, 2), name = "", labels = c(
  #   expression("H"[1]*": "*beta*" < "*delta)
  #   , expression("H"[1]*": "*beta*" > -"*delta))
  # ) +
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
