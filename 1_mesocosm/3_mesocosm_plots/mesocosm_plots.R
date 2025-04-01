setwd("~/zostera_multi-stressor_sfbay/1_mesocosm")

#### I. Packages ####
library(tidyverse)
library(ggplot2)
library(viridis)
library(ggpubr)
library(scales)

#### II. Functions ####
## confidence interval function ##
ci <- function(x) {
  ci <- (sd(na.omit(x))*(1.96))/sqrt(length(na.omit(x)))
  return(ci)
}

## boxplot function ##
box_plot <- function(
    y_data,
    y_jitter,
    hline,
    y_lab,
    y_limits,
    y_breaks,
    y_labels,
    legend
) { 
  ggplot() +
    geom_boxplot(
      data = plant_dat,
      aes(
        x = avalida,
        y = y_data,
        fill = CO2_light_combo
      ),
      size = 0.5,
      color = "black",
      alpha = 0.25,
      outlier.shape = NA
    ) +
    geom_point(
      data = plant_dat,
      aes(
        x = avalida,
        y = y_data,
        # shape = CO2_light_combo,
        color = CO2_light_combo,
        fill = CO2_light_combo
      ),
      position = position_jitterdodge(
        dodge.width = 0.75,
        jitter.width = y_jitter
      ),
      size = 1.5,
      alpha = 0.75
    ) +
    geom_hline(
      yintercept = hline,
      lwd = 0.25,
      color = "black"
    ) +
    labs(
      x = NULL,
      y = y_lab
    ) +
    theme_bw(
      # base_size = 11
    ) +
    scale_color_manual(
      name = expression("CO"[2]*"-Light Treatment"),
      labels = c(
        expression(atop(NA, 
                        atop(textstyle("Ambient CO"[2]),
                             textstyle("450 µE m"^-2*" s"^-1)))),
        expression(atop(NA, 
                        atop(textstyle("Ambient CO"[2]),
                             textstyle("150 µE m"^-2*" s"^-1)))),
        expression(atop(NA, 
                        atop(textstyle("Elevated CO"[2]),
                             textstyle("450 µE m"^-2*" s"^-1)))),
        expression(atop(NA, 
                        atop(textstyle("Elevated CO"[2]),
                             textstyle("150 µE m"^-2*" s"^-1))))
      ),
      values = viridis(
        4, 
        begin = 0, 
        end = 1,
        direction = -1
        )
      ) +
    scale_fill_manual(
      name = expression("CO"[2]*"-Light Treatment"),
      labels = c(
        expression(atop(NA, 
                        atop(textstyle("Ambient CO"[2]),
                             textstyle("450 µE m"^-2*" s"^-1)))),
        expression(atop(NA, 
                        atop(textstyle("Ambient CO"[2]),
                             textstyle("150 µE m"^-2*" s"^-1)))),
        expression(atop(NA, 
                        atop(textstyle("Elevated CO"[2]),
                             textstyle("450 µE m"^-2*" s"^-1)))),
        expression(atop(NA, 
                        atop(textstyle("Elevated CO"[2]),
                             textstyle("150 µE m"^-2*" s"^-1))))
      ),
      values = viridis(
        4, 
        begin = 0, 
        end = 1,
        direction = -1
        )
    )+
    scale_x_discrete(
      breaks = c(
        "absent", 
        "present"
      ), 
      labels = c(
        expression(paste(italic("A. valida"), " Absent")),
        expression(paste(italic("A. valida"), " Present"))
      )
    ) +
    scale_y_continuous(
      limits = y_limits,
      breaks = y_breaks,
      labels = y_labels
    ) +
    guides(
      fill = guide_legend(byrow = TRUE),
      color = guide_legend(byrow = TRUE)
      ) +
    theme(
      legend.position = legend,
      legend.key.size = unit(1, "cm"),
      legend.spacing.x = unit(0.25, "cm"),
      legend.spacing.y = unit(0.25, "cm"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(
        linewidth = 0.5, 
        color = "black"
      ),
      axis.title = element_text(
        size = 10, 
        color = "black"
      ),
      axis.text = element_text(
        size = 10,
        color = "black"
      ),
      axis.ticks = element_line(
        linewidth = 0.5, 
        color = "black"
      ),
      legend.title = element_text(
        size = 8, 
        color = "black"
      ),
      legend.text = element_text(
        size = 8, 
        color = "black"
      )
    )
}


#### III. Data Set Up ####
plant_dat <- read_csv("mesocosm_plant data.csv")

#### ...set CO2, light, and avalida treatments as factors ####
plant_dat <- plant_dat %>% 
  mutate(treatment = as.factor(treatment),
         CO2 = as.factor(CO2),
         light = as.factor(light),
         avalida = as.factor(avalida),
         CO2_light_combo = factor(CO2_light_combo,
                                 levels = c("ambient_ambient",
                                            "ambient_low",
                                            "high_ambient",
                                            "high_low")))
str(plant_dat)


#### 1. Box Plots ####
#### ...Change in Canopy Height ####
canopy_ht_boxplot <- box_plot(
  y_data = plant_dat$shoot_length_change_cm,
  y_jitter = 0.1,
  hline = 0,
  y_lab = "Change in Canopy Height (cm)",
  y_limits = c(-10, 100),
  y_breaks = seq(0, 100, by = 25),
  y_labels = seq(0, 100, by = 25),
  legend = "right"
)
print(canopy_ht_boxplot)


#### ...Rhizome Growth ####
rhizome_growth_boxplot <- box_plot(
  y_data = plant_dat$rhizome_length_change_cm,
  y_jitter = 0.1,
  hline = NULL,
  y_lab = "Rhizome Growth (cm)",
  y_limits = c(0, 8),
  y_breaks = seq(0, 8, by = 2),
  y_labels = seq(0, 8, by = 2),
  legend = "right"
)
print(rhizome_growth_boxplot)


#### ...Sheath Width ####
sheath_width_boxplot <- box_plot(
  y_data = plant_dat$sheath_width_cm,
  y_jitter = 0.1,
  hline = NULL,
  y_lab = "Sheath Width (cm)",
  y_limits = c(0, 1.05),
  y_breaks = seq(0, 1, by = .25),
  y_labels = seq(0, 1, by = .25),
  legend = "right"
)
print(sheath_width_boxplot)


#### ...Leaf 3 Growth ####
leaf3_growth_boxplot <- box_plot(
  y_data = plant_dat$leaf3_growth_cm,
  y_jitter = 0.1,
  hline = NULL,
  y_lab = "Leaf 3 Growth (cm)",
  y_limits = c(0, 125),
  y_breaks = seq(0, 125, by = 30),
  y_labels = seq(0, 125, by = 30),
  legend = "right"
)
print(leaf3_growth_boxplot)


#### ...Aboveground Biomass ####
abv_biomass_boxplot <- box_plot(
  y_data = plant_dat$dry_wt_abv_g,
  y_jitter = 0.1,
  hline = NULL,
  y_lab = "Aboveground Biomass (g)",
  y_limits = c(0, 2.4),
  y_breaks = seq(0, 2.4, by = .6),
  y_labels = seq(0, 2.4, by = .6),
  legend = "right"
)
print(abv_biomass_boxplot)


#### ...Belowground Biomass ####
blw_biomass_boxplot <- box_plot(
  y_data = plant_dat$dry_wt_blw_g,
  y_jitter = 0.1,
  hline = NULL,
  y_lab = "Belowgound Biomass (g)",
  y_limits = c(0, 1),
  y_breaks = seq(0, 1, by = .25),
  y_labels = seq(0, 1, by = .25),
  legend = "right"
)
print(blw_biomass_boxplot)


#### ...Root-Shoot Ratio ####
root_shoot_boxplot <- box_plot(
  y_data = plant_dat$root_shoot_ratio,
  y_jitter = 0.1,
  hline = NULL,
  y_lab = "Root-Shoot Ratio",
  y_limits = c(0, 0.8),
  y_breaks = seq(0, 0.8, by = .2),
  y_labels = seq(0, 0.8, by = .2),
  legend = "right"
)
print(root_shoot_boxplot)


#### ...Epiphyte Load ####
epi_load_boxplot <- box_plot(
  y_data = plant_dat$epi_load,
  y_jitter = 0.1,
  hline = NULL,
  y_lab = "Epiphyte Load (g)",
  y_limits = c(0, 4),
  y_breaks = seq(0, 4, by = 1),
  y_labels = seq(0, 4, by = 1),
  legend = "right"
)
print(epi_load_boxplot)


#### ...Carbon ####
carbon_boxplot <- box_plot(
  y_data = plant_dat$carbon,
  y_jitter = 0.1,
  hline = NULL,
  y_lab = "Carbon (%)",
  y_limits = c(0, 40),
  y_breaks = seq(0, 40, by = 10),
  y_labels = seq(0, 40, by = 10),
  legend = "right"
)
print(carbon_boxplot)


#### ...Nitrogen ####
nitrogen_boxplot <- box_plot(
  y_data = plant_dat$nitrogen,
  y_jitter = 0.1,
  hline = NULL,
  y_lab = "Nitrogen (%)",
  y_limits = c(0, 2.5),
  y_breaks = seq(0, 2.5, by = 0.6),
  y_labels = seq(0, 2.5, by = 0.6),
  legend = "right"
)
print(nitrogen_boxplot)


#### ...C-N Ratio ####
cn_ratio_boxplot <- box_plot(
  y_data = plant_dat$c_to_n_ratio,
  y_jitter = 0.1,
  hline = NULL,
  y_lab = "Carbon-Nitrogen Ratio",
  y_limits = c(0, 24),
  y_breaks = seq(0, 24, by = 6),
  y_labels = seq(0, 24, by = 6),
  legend = "right"
)
print(cn_ratio_boxplot)


#### 2. Combined Plots for Manuscript ####
#### ...Figure 1 ####
manuscript_plot1 <- ggarrange(canopy_ht_boxplot,
                              leaf3_growth_boxplot,
                              sheath_width_boxplot,
                              rhizome_growth_boxplot,
                              nrow = 2,
                              ncol = 2,
                              labels = c("(a)", "(b)", "(c)", "(d)"),
                              label.x = 0.88,
                              label.y = 0.96,
                              font.label = list(
                                size = 10,
                                color = "black",
                                face = "plain"
                                ),
                              legend = "bottom",
                              common.legend = TRUE)
print(manuscript_plot1)


#### ...Figure 2 ####
manuscript_plot2 <- ggarrange(abv_biomass_boxplot,
                              blw_biomass_boxplot,
                              root_shoot_boxplot,
                              nrow = 2,
                              ncol = 2,
                              labels = c("(a)", "(b)", "(c)", "(d)"),
                              label.x = 0.88,
                              label.y = 0.96,
                              font.label = list(
                                size = 10,
                                color = "black",
                                face = "plain"
                              ),
                              legend = "bottom",
                              common.legend = TRUE)
print(manuscript_plot2)


#### ...Figure 3 ####
manuscript_plot3 <- ggarrange(carbon_boxplot,
                              nitrogen_boxplot,
                              cn_ratio_boxplot,
                              epi_load_boxplot,
                              nrow = 2,
                              ncol = 2,
                              labels = c("(a)", "(b)", "(c)", "(d)"),
                              label.x = 0.88,
                              label.y = 0.96,
                              font.label = list(
                                size = 10,
                                color = "black",
                                face = "plain"
                              ),
                              legend = "bottom",
                              common.legend = TRUE)
print(manuscript_plot3)
