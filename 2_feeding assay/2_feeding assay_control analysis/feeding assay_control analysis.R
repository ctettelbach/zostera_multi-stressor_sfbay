setwd("~/zostera_multi-stressor_sfbay/2_feeding assay")


#### I. Packages ####
library(tidyverse)
library(lme4)
library(ggpubr)
library(lmerTest)
library(fitdistrplus)
library(car)


#### II. Data Set Up ####
assay_control_dat <- read_csv("feeding assay_control data.csv")

assay_control_dat <- assay_control_dat %>% 
  mutate(CO2 = as.factor(CO2),
         light = as.factor(light))


#### 1. Stats ####
#### ...Effect of CO2*Light on Controls ####
## full model ##
control_surface_area_change_lmer <- lmer(surface_area_change_cm2 ~ CO2 * light + (1 | tank), 
                                         data = assay_control_dat,
                                         REML = T)

## check residuals ##
plot(control_surface_area_change_lmer)
hist(resid(control_surface_area_change_lmer))
ggqqplot(resid(control_surface_area_change_lmer))

## check random effect residuals ##
hist(ranef(control_surface_area_change_lmer)$tank[, "(Intercept)"])
ggqqplot(ranef(control_surface_area_change_lmer)$tank[, "(Intercept)"])


## stats results ##
control_surface_area_change_lmer_summary <- summary(control_surface_area_change_lmer, 
                                                    ddf = "Kenward-Roger")
control_surface_area_change_lmer_summary

control_surface_area_change_lmer_anova <- anova(control_surface_area_change_lmer,
                                                type = 3,
                                                ddf = "Kenward-Roger")
control_surface_area_change_lmer_anova


## code for generating tables ##
# control_surface_area_change_lmer_reffects_table <- data.frame(control_surface_area_change_lmer_summary[["varcor"]]) %>% 
#   dplyr::select(grp, vcov, sdcor) %>% 
#   rename("Group" = grp,
#          "Variance" = vcov,
#          "Std. Dev." = sdcor)
# 
# control_surface_area_change_lmer_anova_table <- data.frame(control_surface_area_change_lmer_anova) %>% 
#   rename("Sum Sq." = Sum.Sq,
#          "Mean Sq." = Mean.Sq,
#          "Num. DF" = NumDF,
#          "Den. DF" = DenDF,
#          "F" = F.value,
#          "p-value" = Pr..F.)
# 
# write.csv(control_surface_area_change_lmer_reffects_table,
#           "~/feeding assay_control surface area change_random effects table.csv")
# 
# write.csv(control_surface_area_change_lmer_anova_table,
#           "~/feeding assay_control surface area change_anova table.csv")
