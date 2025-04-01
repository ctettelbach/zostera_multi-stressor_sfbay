setwd("~/zostera_multi-stressor_sfbay/3_water chemistry")


#### I. Packages ####
library(tidyverse)
library(seacarb)


#### II. Data Prep ####
pH_dat <- read_csv("mesocosm_water sample pH data.csv") %>% 
  dplyr::select(-processing_date, -mean, -std_dev, 
         -std_dev_x2, -cv, -percent, 
         -min, -max, -range)
alk_dat <- read_csv("mesocosm_water sample alk data.csv") %>% 
  dplyr::select(-processing_date, -mean, -std_dev, 
         -std_dev_x2, -cv, -percent, 
         -min, -max, -range)

pHalk_dat <- left_join(alk_dat, 
                       pH_dat[,c("sampling_date_time",
                                 "spec_pH_25degC",
                                 "spec_pH_insitu_temp")],
                       by = "sampling_date_time") %>% 
  filter(!is.na(spec_pH_insitu_temp)
         & !row_number() %in% c(1, 2, 3)
         ) %>% 
  mutate(alk_insitu_temp_mol = alk_insitu_temp * 1e-6)


#### 1. Carbonate Chemistry Estimates ####
carb_chem <- carb(flag = 8, 
                  na.omit(pHalk_dat$spec_pH_insitu_temp), 
                  na.omit(pHalk_dat$alk_insitu_temp_mol), 
                  S = na.omit(pHalk_dat$salinity), 
                  T = na.omit(pHalk_dat$temperature), 
                  Patm=1, P=0, Pt=0, Sit=0,
                  k1k2="x", kf="x", ks="d", pHscale="T", 
                  b="u74", gas="potential", warn="y", eos="eos80", 
                  long=1.e20, lat=1.e20) %>% 
  mutate(ALK = ALK * 1e6,
         DIC = DIC * 1e6)


#### III. Output Data Prep
final_dat <- cbind(pHalk_dat, carb_chem) %>% 
  dplyr::select("Treatment" = CO2_treatment,
         "Temperature" = temperature,
         "Salinity" = salinity,
         "pH",
         "Alkalinity" = ALK,
         "pCO2" = pCO2,
         "CO2" = CO2,
         "HCO3" = HCO3,
         "CO3" = CO3,
         "DIC" = DIC,
         "Ar" = OmegaAragonite) %>%
  pivot_longer(cols = "Temperature":"Ar",
               names_to = "Parameter",
               values_to = "value") %>%
  group_by(Treatment, Parameter) %>%
  summarise(across(everything(),
                   list(mean = ~mean(., na.rm = T),
                        sd = ~sd(., na.rm = T),
                        min = ~min(., na.rm = T),
                        max = ~max(., na.rm = T),
                        n = ~length(na.omit(.))))) %>% 
  mutate(Parameter = factor(Parameter, levels = c("pH",
                                                  "Alkalinity",
                                                  "pCO2",
                                                  "DIC",
                                                  "Ar",
                                                  "CO2",
                                                  "HCO3",
                                                  "CO3",
                                                  "Temperature",
                                                  "Salinity"))) %>% 
  arrange(Treatment, Parameter) %>% 
  rename("Mean" = value_mean,
         "SD" = value_sd,
         "Min" = value_min,
         "Max" = value_max,
         "N" = value_n) %>% 
  mutate_if(is.numeric, round, digits = 10)
  

## create csv file ## 
# write.csv(final_dat, 
#           "~/mesocosm_water chemistry estimates.csv")
