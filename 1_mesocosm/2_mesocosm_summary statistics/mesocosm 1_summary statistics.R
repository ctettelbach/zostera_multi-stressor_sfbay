setwd("~/zostera_multi-stressor_sfbay/1_mesocosm")


#### I. Packages ####
library(tidyverse)
library(lubridate)
library(scales)


#### II. Functions ####
## confidence interval function ##
ci <- function(x) {
  ci <- (sd(na.omit(x))*(1.96))/sqrt(length(na.omit(x)))
  return(ci)
}


#### III. Data Set Up ####
plant_dat <- read_csv("mesocosm_plant data.csv")
plant_dat <- plant_dat %>% 
  mutate(treatment = factor(treatment, levels = c("1", "2", "3", "5", "4", "6", "7", "8")))


#### 1. Summary Statistics (Response Variables) ####
#### ...wide data frame with summary statistics ####
summary_statistics1 <- plant_dat %>% 
  dplyr::select(treatment, CO2, light, avalida, shoot_length_change_cm, rhizome_length_change_cm, sheath_width_cm, leaf3_growth_cm, dry_wt_abv_g, dry_wt_blw_g, root_shoot_ratio, epi_load, carbon, nitrogen, c_to_n_ratio) %>% 
  rename("Canopy Height Change (cm)" = shoot_length_change_cm,
         "Rhizome Growth (cm)" = rhizome_length_change_cm,
         "Sheath Width (cm)" = sheath_width_cm,
         "Leaf 3 Growth (cm)" = leaf3_growth_cm,
         "Aboveground Biomass (g)" = dry_wt_abv_g,
         "Belowground Biomass (g)" = dry_wt_blw_g,
         "Root-Shoot Ratio" = root_shoot_ratio,
         "Epiphyte Load" = epi_load,
         "Carbon Concentration" = carbon,
         "Nitrogen Concentration" = nitrogen,
         "C-N Ratio" = c_to_n_ratio) %>% 
  group_by(treatment, CO2, light, avalida) %>%
  summarise(across(everything(),
                   list(mean = ~mean(., na.rm = T),
                        sd = ~sd(., na.rm = T),
                        min = ~min(., na.rm = T),
                        max = ~max(., na.rm = T),
                        n = ~length(na.omit(.)),
                        ci = ~ci(.)))) %>% 
  ungroup()

#### ...cleaned data frame with summary statistics ####
summary_statistics2 <- summary_statistics1 %>% 
  pivot_longer(c(-treatment, -CO2, -light, -avalida)) %>% 
  separate(name, into = c("parameter", "stat"), sep = "_") %>%
  pivot_wider(names_from = stat, values_from = value) %>% 
  mutate("Lower CL" = mean - ci,
         "Upper CL" = mean + ci,
         CO2 = if_else(CO2 == "ambient", "Ambient", "Low"),
         light = if_else(light == "ambient", "Ambient", "Low"),
         avalida = if_else(avalida == "absent", "Absent", "Present"),
         parameter = factor(parameter, levels = c("Canopy Height Change (cm)",
                                                  "Leaf 3 Growth (cm)",
                                                  "Sheath Width (cm)",
                                                  "Aboveground Biomass (g)",
                                                  "Rhizome Growth (cm)",
                                                  "Belowground Biomass (g)",
                                                  "Root-Shoot Ratio",
                                                  "Epiphyte Load",
                                                  "Carbon Concentration",
                                                  "Nitrogen Concentration",
                                                  "C-N Ratio"))) %>%
  rename(Treatment = treatment,
         Light = light,
         Ampithoe = avalida,
         Parameter = parameter,
         Mean = mean,
         "Std. Dev." = sd,
         Min = min,
         Max = max,
         N = n,
         CI = ci) %>% 
  mutate_if(is.numeric, round, digits = 2) %>% 
  arrange(Parameter)

# write.csv(summary_statistics2,
#           "~/mesocosm_summary statistics.csv",
#           row.names = FALSE)
