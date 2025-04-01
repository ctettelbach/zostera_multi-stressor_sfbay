setwd("~/zostera_multi-stressor_sfbay/2_feeding assay")


#### I. Packages ####
library(tidyverse)
library(lme4)
library(ggpubr)
library(fitdistrplus)
library(car)
library(emmeans)
library(multcomp)


#### II. Data Set Up ####
assay_dat <- read_csv("feeding assay_data.csv")

#### ...set CO2, light, and Ampithoe treatments as factors ####
assay_dat <- assay_dat %>% 
  mutate(eelgrass_CO2 = as.factor(eelgrass_CO2),
         eelgrass_light = as.factor(eelgrass_light),
         avalida_CO2 = as.factor(avalida_CO2),
         avalida_light = as.factor(avalida_light))
str(assay_dat)


#### 1. Stats ####
#### ...Linear Mixed-Effects Model ####
## full model ##
herbivory_per_avalida_lmer <- lmer(herbivory_per_day_per_avalida ~ eelgrass_CO2 * eelgrass_light * avalida_CO2 * avalida_light + (1 | tank), 
                                   data = assay_dat,
                                   REML = T)
## ERROR: is singular ##

## check random effect variance ##
summary(herbivory_per_avalida_lmer)$varcor
## Std. Dev. for tank is 0 ##


#### ...Linear Model Without Random-Effect ####
herbivory_per_avalida_lm <- lm(herbivory_per_day_per_avalida ~ eelgrass_CO2 * eelgrass_light * avalida_CO2 * avalida_light,
                               data = assay_dat)

## check residuals ##
plot(herbivory_per_avalida_lm$residuals)
hist(resid(herbivory_per_avalida_lm))
ggqqplot(resid(herbivory_per_avalida_lm))
## non-normal residuals ##


## check for log-transformation or glmm ##
descdist(as.numeric(na.omit(assay_dat$herbivory_per_day_per_avalida)), boot = 1000, discrete = F)

## normal ##
herbivory_norm <- fitdist(as.numeric(na.omit(assay_dat$herbivory_per_day_per_avalida)), "norm")
plot(herbivory_norm)
summary(herbivory_norm) ## AIC: -233.0957

## log-normal ##
herbivory_lnorm <- fitdist(as.numeric(na.omit(assay_dat$herbivory_per_day_per_avalida)), "lnorm")
plot(herbivory_lnorm)
summary(herbivory_lnorm) ## AIC: -305.5984

## gamma##
herbivory_gamma <- fitdist(as.numeric(na.omit(assay_dat$herbivory_per_day_per_avalida)), "gamma")
plot(herbivory_gamma)
summary(herbivory_gamma) ## AIC: -383.5216 (lowest AIC)


#### ...Generalized Linear Mixed-Effects Model ####
herbivory_per_avalida_glmm <- glmer(herbivory_per_day_per_avalida ~ eelgrass_CO2 * eelgrass_light * avalida_CO2 * avalida_light + (1|tank),
                                    data = assay_dat,
                                    family = Gamma (link = "log"))
## ERROR: is singular ##

## check random effect variance ##
summary(herbivory_per_avalida_glmm)$varcor
## Std. Dev. for tank is 0 ##


#### ...Generalized Linear Model Without Random Effect ####
herbivory_per_avalida_glm <- glm(herbivory_per_day_per_avalida ~ eelgrass_CO2 * eelgrass_light * avalida_CO2 * avalida_light,
                                 data = assay_dat,
                                 family = Gamma (link = "log"))

## check residuals ##
plot(resid(herbivory_per_avalida_glm))
hist(resid(herbivory_per_avalida_glm))
ggqqplot(resid(herbivory_per_avalida_glm))

## stats results ##
herbivory_per_avalida_glm_summary <- summary(herbivory_per_avalida_glm)
herbivory_per_avalida_glm_summary

herbivory_per_avalida_glm_anova <- Anova(herbivory_per_avalida_glm,
                                         type = 3,
                                         ddf = "Kenward-Roger")
herbivory_per_avalida_glm_anova


## code for generating tables ##
# herbivory_per_avalida_anova_table <- data.frame(herbivory_per_avalida_glm_anova) %>% 
#   rename("Chi Sq." = LR.Chisq,
#          "DF" = Df,
#          "p-value" = Pr..Chisq.)
# 
# write.csv(herbivory_per_avalida_anova_table,
#           "~/feeding assay_herbivory per avalida per day_anova table.csv")


#### 2. Post-Hoc Analysis ####
#### ...Letters for Figure 4 ####
emm <- emmeans(herbivory_per_avalida_glm, ~ eelgrass_CO2 * eelgrass_light * avalida_CO2 * avalida_light, type = "response")

emm_letters <- as.data.frame(cld(emm, Letters = letters)) %>% 
  rename("Eelgrass CO2" = eelgrass_CO2,
         "Eelgrass Light" = eelgrass_light,
         "A. valida CO2" = avalida_CO2,
         "A. valida Light" = avalida_light,
         "Mean" = response,
         "Lower CL" = lower.CL,
         "Upper CL" = upper.CL,
         "Group" = .group)

# write.csv(EMM_letters,
#           "~/feeding assay_posthoc analysis_treatment summary table.csv")


#### ...Simple Contrasts for Three-Way Interaction ####
emm2 <- emmeans(herbivory_per_avalida_glm, ~ eelgrass_light * avalida_CO2 * avalida_light, type = "response")

emm2_pairs <- pairs(emm2, simple = "each", adjust = "tukey")

simp_test_3way_1 <- test(emm2_pairs[[1]], by = NULL, adjust = "none") %>% 
  mutate(eelgrass_light = NA)
simp_test_3way_2 <- test(emm2_pairs[[2]], by = NULL, adjust = "none") %>% 
  mutate(avalida_CO2 = NA)
simp_test_3way_3 <- test(emm2_pairs[[3]], by = NULL, adjust = "none") %>% 
  mutate(avalida_light = NA)

emm2_simple <- rbind(simp_test_3way_1,
                     simp_test_3way_2,
                     simp_test_3way_3) %>% 
  relocate(eelgrass_light, .before = avalida_CO2)

# write.csv(emm2_simple,
#           "~/feeding assay_simple contrasts_three-way interaction.csv")
