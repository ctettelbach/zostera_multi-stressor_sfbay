setwd("~/zostera_multi-stressor_sfbay/1_mesocosm")

#### I. Packages ####
library(tidyverse)
library(lme4)
library(ggpubr)
library(lmerTest)
library(fitdistrplus)
library(car)


#### II. Data Set Up ####
plant_dat <- read_csv("mesocosm_plant data.csv")

#### ...set CO2, light, and A. valida treatments as factors ####
plant_dat <- plant_dat %>% 
  mutate(CO2 = as.factor(CO2),
         light = as.factor(light),
         amp = as.factor(avalida))


#### 1. Stats ####
#### ...Change in Canopy Height ####
## full model ##
canopy_ht_lmer <- lmer(shoot_length_change_cm ~ CO2 * light * avalida + (1 | tank), 
                       data = plant_dat,
                       REML = T)

## check residuals ##
plot(canopy_ht_lmer)
hist(resid(canopy_ht_lmer))
ggqqplot(resid(canopy_ht_lmer))

## check random effect residuals ##
hist(ranef(canopy_ht_lmer)$tank[, "(Intercept)"])
ggqqplot(ranef(canopy_ht_lmer)$tank[, "(Intercept)"])


## stats results ##
canopy_ht_lmer_summary <- summary(canopy_ht_lmer, 
                                  ddf = "Kenward-Roger")
canopy_ht_lmer_summary

canopy_ht_lmer_anova <- anova(canopy_ht_lmer, 
                              type = 3, 
                              ddf = "Kenward-Roger")
canopy_ht_lmer_anova


## code for generating tables ##
# canopy_ht_reffects_table <- data.frame(canopy_ht_lmer_summary[["varcor"]]) %>%
#   dplyr::select(grp, vcov, sdcor) %>%
#   rename("Group" = grp,
#          "Variance" = vcov,
#          "Std. Dev." = sdcor)
# 
# canopy_ht_anova_table <- data.frame(canopy_ht_lmer_anova) %>%
#   rename("Sum Sq." = Sum.Sq,
#          "Mean Sq." = Mean.Sq,
#          "Num. DF" = NumDF,
#          "Den. DF" = DenDF,
#          "F" = F.value,
#          "p-value" = Pr..F.)
# 
# write.csv(canopy_ht_reffects_table,
#           "~/mesocosm_canopy height random effects table.csv")
#
# write.csv(canopy_ht_anova_table,
#           "~/mesocosm_canopy height anova table.csv")
##________________________________________________________________________________________##

#### ...Rhizome Growth ####
## full model ##
rhizome_growth_lmer <- lmer(rhizome_length_change_cm ~ CO2 * light * avalida + (1 | tank),
                            data = plant_dat,
                            REML = T)

## check residuals ##
plot(rhizome_growth_lmer)
hist(resid(rhizome_growth_lmer))
ggqqplot(resid(rhizome_growth_lmer))

## check random effect residuals ##
hist(ranef(rhizome_growth_lmer)$tank[, "(Intercept)"])
ggqqplot(ranef(rhizome_growth_lmer)$tank[, "(Intercept)"])


## stats results ##
rhizome_growth_lmer_summary <- summary(rhizome_growth_lmer, 
                                       ddf = "Kenward-Roger")
rhizome_growth_lmer_summary

rhizome_growth_lmer_anova <- anova(rhizome_growth_lmer,
                                   type = 3,
                                   ddf = "Kenward-Roger")
rhizome_growth_lmer_anova


## code for generating tables ##
# rhizome_growth_reffects_table <- data.frame(rhizome_growth_lmer_summary[["varcor"]]) %>%
#   dplyr::select(grp, vcov, sdcor) %>%
#   rename("Group" = grp,
#          "Variance" = vcov,
#          "Std. Dev." = sdcor)
# 
# rhizome_growth_anova_table <- data.frame(rhizome_growth_lmer_anova) %>%
#   rename("Sum Sq." = Sum.Sq,
#          "Mean Sq." = Mean.Sq,
#          "Num. DF" = NumDF,
#          "Den. DF" = DenDF,
#          "F" = F.value,
#          "p-value" = Pr..F.)
# 
# write.csv(rhizome_growth_reffects_table,
#           "~/mesocosm_rhizome growth random effects table.csv")
# 
# write.csv(rhizome_growth_anova_table,
#           "~/mesocosm_rhizome growth anova table.csv")
##________________________________________________________________________________________##

#### ...Sheath Width ####
## full model ##
sheath_width_lmer <- lmer(sheath_width_cm ~ CO2 * light * avalida + (1 | tank),
                          data = plant_dat,
                          REML = T)
## ERROR: is singular ##

## check random effect estimates ##
summary(sheath_width_lmer)$varcor
## Std. Dev. for tank-effect is 0 ##


## linear model without random effect ##
sheath_width_lm <- lm(sheath_width_cm ~ CO2 * light * avalida,
                      data = plant_dat)

## check residuals ##
plot(sheath_width_lm$residuals)
hist(resid(sheath_width_lm))
ggqqplot(resid(sheath_width_lm))


## stats results ##
sheath_width_lm_summary <- summary(sheath_width_lm)
sheath_width_lm_summary

sheath_width_lm_anova <- anova(sheath_width_lm)
sheath_width_lm_anova


## code for generating tables ##
# sheath_width_anova_table <- data.frame(sheath_width_lm_anova) %>% 
#   rename("Sum Sq." = Sum.Sq,
#          "Mean Sq." = Mean.Sq,
#          "DF" = Df,
#          "F" = F.value,
#          "p-value" = Pr..F.)
# 
# write.csv(sheath_width_anova_table,
#           "~/mesocosm_sheath width anova table.csv")
##________________________________________________________________________________________##

#### ...Leaf 3 Growth ####
## full model ##
leaf3_growth_lmer <- lmer(leaf3_growth_cm ~ CO2 * light * avalida + (1 | tank),
                          data = plant_dat,
                          REML = T)

## check residuals ##
plot(leaf3_growth_lmer)
hist(resid(leaf3_growth_lmer))
ggqqplot(resid(leaf3_growth_lmer))

## check random effect residuals ##
hist(ranef(leaf3_growth_lmer)$tank[, "(Intercept)"])
ggqqplot(ranef(leaf3_growth_lmer)$tank[, "(Intercept)"])


## stats results ##
leaf3_growth_lmer_summary <- summary(leaf3_growth_lmer,
                                     ddf = "Kenward-Roger")
leaf3_growth_lmer_summary

leaf3_growth_lmer_anova <- anova(leaf3_growth_lmer,
                                 type = 3,
                                 ddf = "Kenward-Roger")
leaf3_growth_lmer_anova


## code for generating tables ##
# leaf3_growth_reffects_table <- data.frame(leaf3_growth_lmer_summary[["varcor"]]) %>%
#   dplyr::select(grp, vcov, sdcor) %>%
#   rename("Group" = grp,
#          "Variance" = vcov,
#          "Std. Dev." = sdcor)
# 
# leaf3_growth_anova_table <- data.frame(leaf3_growth_lmer_anova) %>%
#   rename("Sum Sq." = Sum.Sq,
#          "Mean Sq." = Mean.Sq,
#          "Num. DF" = NumDF,
#          "Den. DF" = DenDF,
#          "F" = F.value,
#          "p-value" = Pr..F.)
# 
# write.csv(leaf3_growth_reffects_table,
#           "~/mesocosm_new leaf growth random effects table.csv")
# 
# write.csv(leaf3_growth_anova_table,
#           "~/mesocosm_new leaf growth anova table.csv")
##________________________________________________________________________________________##

#### ...Aboveground Biomass ####
## full model ##
aboveground_biomass_lmer <- lmer(dry_wt_abv_g ~ CO2 * light * avalida + (1 | tank),
                                 data = plant_dat,
                                 REML = T)

## check residuals ##
plot(aboveground_biomass_lmer)
hist(resid(aboveground_biomass_lmer))
ggqqplot(resid(aboveground_biomass_lmer))

## check random effect residuals ##
hist(ranef(aboveground_biomass_lmer)$tank[, "(Intercept)"])
ggqqplot(ranef(aboveground_biomass_lmer)$tank[, "(Intercept)"])


## stats results ##
aboveground_biomass_lmer_summary <- summary(aboveground_biomass_lmer,
                                            ddf = "Kenward-Roger")
aboveground_biomass_lmer_summary

aboveground_biomass_lmer_anova <- anova(aboveground_biomass_lmer,
                                        type = 3,
                                        ddf = "Kenward-Roger")
aboveground_biomass_lmer_anova


## code for generating tables ##
# aboveground_biomass_reffects_table <- data.frame(aboveground_biomass_lmer_summary[["varcor"]]) %>%
#   dplyr::select(grp, vcov, sdcor) %>%
#   rename("Group" = grp,
#          "Variance" = vcov,
#          "Std. Dev." = sdcor)
# 
# aboveground_biomass_anova_table <- data.frame(aboveground_biomass_lmer_anova) %>%
#   rename("Sum Sq." = Sum.Sq,
#          "Mean Sq." = Mean.Sq,
#          "Num. DF" = NumDF,
#          "Den. DF" = DenDF,
#          "F" = F.value,
#          "p-value" = Pr..F.)
# 
# write.csv(aboveground_biomass_reffects_table,
#           "~/mesocosm_aboveground biomass random effects table.csv")
# 
# write.csv(aboveground_biomass_anova_table,
#           "~/mesocosm_aboveground biomass anova table.csv")
##________________________________________________________________________________________##

#### ...Belowground Biomass ####
## full model ##
belowground_biomass_lmer <- lmer(dry_wt_blw_g ~ CO2 * light * avalida + (1 | tank),
                                 data = plant_dat,
                                 REML = T)

## check residuals ##
plot(belowground_biomass_lmer)
hist(resid(belowground_biomass_lmer))
ggqqplot(resid(belowground_biomass_lmer))

## check random effect residuals ##
hist(ranef(belowground_biomass_lmer)$tank[, "(Intercept)"])
ggqqplot(ranef(belowground_biomass_lmer)$tank[, "(Intercept)"])


## stats results ##
belowground_biomass_lmer_summary <- summary(belowground_biomass_lmer,
                                            ddf = "Kenward-Roger")
belowground_biomass_lmer_summary

belowground_biomass_lmer_anova <- anova(belowground_biomass_lmer,
                                        type = 3,
                                        ddf = "Kenward-Roger")
belowground_biomass_lmer_anova


## code for generating tables ##
# belowground_biomass_reffects_table <- data.frame(belowground_biomass_lmer_summary[["varcor"]]) %>%
#   dplyr::select(grp, vcov, sdcor) %>%
#   rename("Group" = grp,
#          "Variance" = vcov,
#          "Std. Dev." = sdcor)
# 
# belowground_biomass_anova_table <- data.frame(belowground_biomass_lmer_anova) %>%
#   rename("Sum Sq." = Sum.Sq,
#          "Mean Sq." = Mean.Sq,
#          "Num. DF" = NumDF,
#          "Den. DF" = DenDF,
#          "F" = F.value,
#          "p-value" = Pr..F.)
# 
# write.csv(belowground_biomass_reffects_table,
#           "~/mesocosm_belowground biomass random effects table.csv")
# 
# write.csv(belowground_biomass_anova_table,
#           "~/mesocosm_belowground biomass anova table.csv")
##________________________________________________________________________________________##

#### ...Root Shoot Ratio (Log-Transformed) ####
## full model ##
rootshoot_ratio_lmer <- lmer(root_shoot_ratio ~ CO2 * light * avalida + (1 | tank),
                             data = plant_dat,
                             REML = T)

## check residuals ##
plot(rootshoot_ratio_lmer)
hist(resid(rootshoot_ratio_lmer))
ggqqplot(resid(rootshoot_ratio_lmer))
## non-normal residuals ##


## check for log-transformation or glmm ##
descdist(as.numeric(na.omit(plant_dat$root_shoot_ratio)), boot = 1000, discrete = F)

## normal ##
rootshoot_ratio_norm = fitdist(as.numeric(na.omit(plant_dat$root_shoot_ratio)), "norm")
plot(rootshoot_ratio_norm)
summary(rootshoot_ratio_norm) ## AIC: -349.6525

## log-normal ##
rootshoot_ratio_lnorm = fitdist(as.numeric(na.omit(plant_dat$root_shoot_ratio)), "lnorm")
plot(rootshoot_ratio_lnorm)
summary(rootshoot_ratio_lnorm) ## AIC: -375.2678 (lowest AIC)

## gamma##
rootshoot_ratio_gamma = fitdist(as.numeric(na.omit(plant_dat$root_shoot_ratio)), "gamma")
plot(rootshoot_ratio_gamma)
summary(rootshoot_ratio_gamma) ## AIC: -372.266


## full model with log-transformed data ##
rootshoot_ratio_log_lmer <- lmer(log(root_shoot_ratio) ~ CO2 * light * avalida + (1 | tank),
                                 data = plant_dat,
                                 REML = T)

## check residuals ##
plot(rootshoot_ratio_log_lmer)
hist(resid(rootshoot_ratio_log_lmer))
ggqqplot(resid(rootshoot_ratio_log_lmer))

## check random effect residuals ##
hist(ranef(rootshoot_ratio_log_lmer)$tank[, "(Intercept)"])
ggqqplot(ranef(rootshoot_ratio_log_lmer)$tank[, "(Intercept)"])


## stats results ##
rootshoot_ratio_log_lmer_summary <- summary(rootshoot_ratio_log_lmer,
                                            ddf = "Kenward-Roger")
rootshoot_ratio_log_lmer_summary

rootshoot_ratio_log_lmer_anova <- anova(rootshoot_ratio_log_lmer,
                                        type = 3,
                                        ddf = "Kenward-Roger")
rootshoot_ratio_log_lmer_anova


## code for generating tables ##
# rootshoot_ratio_reffects_table <- data.frame(rootshoot_ratio_log_lmer_summary[["varcor"]]) %>%
#   dplyr::select(grp, vcov, sdcor) %>%
#   rename("Group" = grp,
#          "Variance" = vcov,
#          "Std. Dev." = sdcor)
# 
# rootshoot_ratio_anova_table <- data.frame(rootshoot_ratio_log_lmer_anova) %>%
#   rename("Sum Sq." = Sum.Sq,
#          "Mean Sq." = Mean.Sq,
#          "Num. DF" = NumDF,
#          "Den. DF" = DenDF,
#          "F" = F.value,
#          "p-value" = Pr..F.)
# 
# write.csv(rootshoot_ratio_reffects_table,
#           "~/mesocosm_root shoot ratio random effects table.csv")
# 
# write.csv(rootshoot_ratio_anova_table,
#           "~/mesocosm_root shoot ratio anova table.csv")
##________________________________________________________________________________________##

#### ...Epiphyte Load ####
## full model ##
epi_load_lmer <- lmer(epi_load ~ CO2 * light * avalida + (1 | tank),
                      data = plant_dat,
                      REML = T)

## check residuals ##
plot(epi_load_lmer)
hist(resid(epi_load_lmer))
ggqqplot(resid(epi_load_lmer))

## check random effect residuals ##
hist(ranef(epi_load_lmer)$tank[, "(Intercept)"])
ggqqplot(ranef(epi_load_lmer)$tank[, "(Intercept)"])


## stats results ##
epi_load_lmer_summary <- summary(epi_load_lmer)
epi_load_lmer_summary

epi_load_lmer_anova <- anova(epi_load_lmer,
                             type = 3,
                             ddf = "Kenward-Roger")
epi_load_lmer_anova


## code for generating tables ##
# epi_load_reffects_table <- data.frame(epi_load_lmer_summary[["varcor"]]) %>%
#   dplyr::select(grp, vcov, sdcor) %>%
#   rename("Group" = grp,
#          "Variance" = vcov,
#          "Std. Dev." = sdcor)
# 
# epi_load_anova_table <- data.frame(epi_load_lmer_anova) %>%
#   rename("Sum Sq." = Sum.Sq,
#          "Mean Sq." = Mean.Sq,
#          "Num. DF" = NumDF,
#          "Den. DF" = DenDF,
#          "F" = F.value,
#          "p-value" = Pr..F.)
# 
# write.csv(epi_load_reffects_table,
#           "~/mesocosm_epiCO2yte load random effects table.csv")
# 
# write.csv(epi_load_anova_table,
#           "~/mesocosm_epiCO2yte load anova table.csv")
##________________________________________________________________________________________##

#### ...Carbon Concentration ####
## full model ##
carbon_lmer <- lmer(carbon ~ CO2 * light * avalida + (1 | tank),
                    data = plant_dat,
                    REML = T)

## check residuals ##
plot(carbon_lmer)
hist(resid(carbon_lmer))
ggqqplot(resid(carbon_lmer))

## check for outliers ##
influencePlot(carbon_lmer)
## rows 8 and 141 ##


## re-run model without outliers in rows 8 and 141 ##
carbon_lmer2 <- lmer(carbon ~ CO2 * light * avalida + (1 | tank),
                     data = plant_dat[c(1:7, 9:140, 142:238),],
                     REML = T)

## check residuals ##
plot(carbon_lmer2)
hist(resid(carbon_lmer2))
ggqqplot(resid(carbon_lmer2))

## check random effect residuals ##
hist(ranef(carbon_lmer2)$tank[, "(Intercept)"])
ggqqplot(ranef(carbon_lmer2)$tank[, "(Intercept)"])


## stats results ##
carbon_lmer_summary <- summary(carbon_lmer2,
                               ddf = "Kenward-Roger")
carbon_lmer_summary

carbon_lmer_anova <- anova(carbon_lmer2,
                           type = 3,
                           ddf = "Kenward-Roger")
carbon_lmer_anova


## code for generating tables ##
# carbon_reffects_table <- data.frame(carbon_lmer_summary[["varcor"]]) %>%
#   dplyr::select(grp, vcov, sdcor) %>%
#   rename("Group" = grp,
#          "Variance" = vcov,
#          "Std. Dev." = sdcor)
# 
# carbon_anova_table <- data.frame(carbon_lmer_anova) %>%
#   rename("Sum Sq." = Sum.Sq,
#          "Mean Sq." = Mean.Sq,
#          "Num. DF" = NumDF,
#          "Den. DF" = DenDF,
#          "F" = F.value,
#          "p-value" = Pr..F.)
# 
# write.csv(carbon_reffects_table,
#           "~/mesocosm_carbon random effects table.csv")
# 
# write.csv(carbon_anova_table,
#           "~/mesocosm_carbon anova table.csv")
##________________________________________________________________________________________##

#### ...Nitrogen Concentration ####
## full model ##
nitrogen_lmer <- lmer(nitrogen ~ CO2 * light * avalida + (1 | tank),
                      data = plant_dat,
                      REML = T)

## check residuals ##
plot(nitrogen_lmer)
hist(resid(nitrogen_lmer))
ggqqplot(resid(nitrogen_lmer))

## check for outliers ##
influencePlot(nitrogen_lmer)
## row 8 ##


## re-run model without outlier in row 8 ##
nitrogen_lmer2 <- lmer(nitrogen ~ CO2 * light * avalida + (1 | tank),
                       data = plant_dat[c(1:7, 9:238),],
                       REML = T)

## check residuals ##
plot(nitrogen_lmer2)
hist(resid(nitrogen_lmer2))
ggqqplot(resid(nitrogen_lmer2))

## check random effect residuals ##
hist(ranef(nitrogen_lmer2)$tank[, "(Intercept)"])
ggqqplot(ranef(nitrogen_lmer2)$tank[, "(Intercept)"])


## stats results ##
nitrogen_lmer_summary <- summary(nitrogen_lmer2,
                                 ddf = "Kenward-Roger")
nitrogen_lmer_summary

nitrogen_lmer_anova <- anova(nitrogen_lmer2,
                             type = 3,
                             ddf = "Kenward-Roger")
nitrogen_lmer_anova


## code for generating tables ##
# nitrogen_reffects_table <- data.frame(nitrogen_lmer_summary[["varcor"]]) %>%
#   dplyr::select(grp, vcov, sdcor) %>%
#   rename("Group" = grp,
#          "Variance" = vcov,
#          "Std. Dev." = sdcor)
# 
# nitrogen_anova_table <- data.frame(nitrogen_lmer_anova) %>%
#   rename("Sum Sq." = Sum.Sq,
#          "Mean Sq." = Mean.Sq,
#          "Num. DF" = NumDF,
#          "Den. DF" = DenDF,
#          "F" = F.value,
#          "p-value" = Pr..F.)
# 
# write.csv(nitrogen_reffects_table,
#           "~/mesocosm_nitrogen random effects table.csv")
# 
# write.csv(nitrogen_anova_table,
#           "~/mesocosm_nitrogen anova table.csv")
##________________________________________________________________________________________##

#### ...CN Ratio (Log-Transformed) ####
## full model without carbon and nitrogen outliers ##
cn_ratio_lmer <- lmer(c_to_n_ratio ~ CO2 * light * avalida + (1 | tank),
                      data = plant_dat[c(1:7, 9:140, 142:238),],
                      REML = T)

## check residuals ##
plot(cn_ratio_lmer)
hist(resid(cn_ratio_lmer))
ggqqplot(resid(cn_ratio_lmer))
## non-normal residuals ##


## full model with log-transformed data ##
cn_ratio_log_lmer <- lmer(log(c_to_n_ratio) ~ CO2 * light * avalida + (1 | tank),
                          data = plant_dat[c(1:7, 9:140, 142:238),],
                          REML = T)

## check residuals ##
plot(cn_ratio_log_lmer)
hist(resid(cn_ratio_log_lmer))
ggqqplot(resid(cn_ratio_log_lmer))

## visual check for normality of random effect residuals ##
hist(ranef(cn_ratio_log_lmer)$tank[, "(Intercept)"])
ggqqplot(ranef(cn_ratio_log_lmer)$tank[, "(Intercept)"])


## stats results ##
cn_ratio_log_lmer_summary <- summary(cn_ratio_log_lmer,
                                     ddf = "Kenward-Roger")
cn_ratio_log_lmer_summary

cn_ratio_log_lmer_anova <- anova(cn_ratio_log_lmer,
                                 type = 3,
                                 ddf = "Kenward-Roger")
cn_ratio_log_lmer_anova


## code for generating tables ##
# cn_ratio_reffects_table <- data.frame(cn_ratio_log_lmer_summary[["varcor"]]) %>%
#   dplyr::select(grp, vcov, sdcor) %>%
#   rename("Group" = grp,
#          "Variance" = vcov,
#          "Std. Dev." = sdcor)
# 
# cn_ratio_anova_table <- data.frame(cn_ratio_log_lmer_anova) %>%
#   rename("Sum Sq." = Sum.Sq,
#          "Mean Sq." = Mean.Sq,
#          "Num. DF" = NumDF,
#          "Den. DF" = DenDF,
#          "F" = F.value,
#          "p-value" = Pr..F.)
# 
# write.csv(cn_ratio_reffects_table,
#           "~/mesocosm_carbon-nitrogen ratio random effects table.csv")
# 
# write.csv(cn_ratio_anova_table,
#           "~/mesocosm_carbon-nitrogen ratio anova table.csv")
##________________________________________________________________________________________##
