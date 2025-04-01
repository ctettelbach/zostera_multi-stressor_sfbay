setwd("~/zostera_multi-stressor_sfbay/2_feeding assay")


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

## assay boxplot function ##
assay_box_plot <- function(
    y_data,
    x_fill,
    y_jitter,
    hline,
    posthoc_labels,
    y_lab,
    y_limits,
    y_breaks,
    y_labels,
    legend
) { 
  ggplot() +
    geom_boxplot(
      data = assay_dat,
      aes(
        x = dummy,
        y = y_data,
        fill = x_fill
      ),
      size = 0.5,
      color = "black",
      alpha = 0.25,
      outlier.shape = NA
    ) +
    geom_point(
      data = assay_dat,
      aes(
        x = dummy,
        y = y_data,
        color = x_fill,
        fill = x_fill
      ),
      position = position_jitterdodge(
        dodge.width = 0.75,
        jitter.width = y_jitter
      ),
      size = 1,
      alpha = 0.75
    ) +
    stat_summary(
      data = assay_dat,
      aes(x = dummy,
          y = y_data,
          group = x_fill),
      fun = max,
      geom = "text",
      label = posthoc_labels,
      position = position_dodge(width = 0.75),
      size = 2.5,
      vjust = -2
    ) +
    labs(
      title = expression(atop(paste("\n"), 
                              paste(italic("A. valida "), "Treatment"))),
      x = NULL,
      y = y_lab
    ) +
    scale_color_manual(
      name = "Eelgrass Treatment",
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
      name = "Eelgrass Treatment",
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
    scale_x_discrete(
      breaks = c(
        "ambient_ambient",
        "ambient_reduced",
        "reduced_ambient",
        "reduced_reduced"
      ),
      labels = NULL
    ) +
    scale_y_continuous(limits = y_limits,
                       breaks = y_breaks,
                       labels = y_labels) +
    facet_grid(
      avalida_light~avalida_CO2,
      scales = "free",
      space = "free_x",
      labeller = label_parsed
      ) +
    theme_bw() +
    guides(
      fill = guide_legend(byrow = TRUE),
      color = guide_legend(byrow = TRUE)
    ) +
    theme(plot.title = element_text(size = 10,
                                    color = "black",
                                    hjust = 0.5),
          legend.position = legend,
          legend.key.size = unit(1, "cm"),
          legend.spacing.x = unit(0.25, "cm"),
          legend.spacing.y = unit(0.25, "cm"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(linewidth = 0.5, 
                                      color = "black"),
          axis.title = element_text(size = 10, 
                                    color = "black"),
          axis.text = element_text(color = "black"),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_line(linewidth = 0.5, 
                                      color = "black"),
          legend.title = element_text(size = 8, 
                                      color = "black"),
          legend.text = element_text(size = 8, 
                                     color = "black"),
          strip.text.x = element_text(size = 8,
                                      color = "black"),
          strip.text.y = element_text(size = 8,
                                      color = "black"),
          strip.background = element_blank()
    )
}


#### III. Data Set Up ####
assay_dat <- read_csv("feeding assay_data.csv")

assay_dat <- assay_dat %>% 
  mutate(eelgrass_CO2 = as.factor(eelgrass_CO2),
         eelgrass_light = as.factor(eelgrass_light),
         avalida_CO2 = as.factor(avalida_CO2),
         avalida_light = as.factor(avalida_light),
         eelgrass_combo = factor(eelgrass_combo,
                                 levels = c("ambient_ambient",
                                            "ambient_low",
                                            "high_ambient",
                                            "high_low")))


#### 1. Box Plots ####
#### ...Herbivory Per Individual Per Day ####
levels(assay_dat$avalida_CO2) <- c(expression("Ambient CO"[2]), 
                                   expression("Elevated CO"[2]))
levels(assay_dat$avalida_light) <- c(expression("450 µE m"^-2*" s"^-1), 
                                      expression("150 µE m"^-2*" s"^-1))

herbivory_per_avalida_boxplot <- assay_box_plot(
  y_data = assay_dat$herbivory_per_day_per_avalida,
  y_jitter = 0.25,
  x_fill = assay_dat$eelgrass_combo,
  hline = NULL,
  posthoc_labels = c("abcd", "bcde", "cde", "cde",
                     "abc", "a", "cde", "ab",
                     "de", "de", "e", "cde",
                     "bcde", "de", "cde", "de"),
  y_lab = bquote("Herbivory ("~cm^2~Individual^-1~Day^-1*")"),
  y_limits = c(0, 1.1),
  y_breaks = seq(0, 1, by = 0.25),
  y_labels = seq(0, 1, by = 0.25),
  legend = "bottom"
)
print(herbivory_per_avalida_boxplot)
