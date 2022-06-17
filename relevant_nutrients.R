
library(tidyverse)

load(file='data/afcd.rda')

afcd_orig = afcd 

afcd_raw = afcd_orig %>% 
  filter(food_prep == "raw")

ear_orig <- readRDS("data/GENUS_EAR_population_data_merged.Rds") 

ear_us = ear_orig %>%
  filter(iso3=="USA") %>%
  mutate(ear_units=gsub("/d", "", units)) 

# 1. do for one group first
ear_us_g1 = ear_us %>%
  filter(sex=="Children", age=="5-9") 

# for each nutrient get the corresponding ear 
nutrients = afcd_raw %>%
  merge(ear_us_g1, by="nutrient") %>% 
  select(1:33, ear, units, sex, age, ear_units) %>%
  # need to test this
  mutate(value=case_when(nutrient_units=="g" & ear_units=="mg" ~ value*1000, 
                         TRUE ~ value), # when species units is g and ear_units are mg
         nutrient_units=case_when(nutrient_units=="g" & ear_units=="mg" ~ "mg",
                                  TRUE ~ nutrient_units)) %>% # change units
  mutate(ear_proportion = value/ear, 
         ear_perc = 100*value/ear) 

# find incongruous units and change above 
nutrients %>% mutate(ear_units=gsub("/d", "", units)) %>%
  mutate(units=paste(nutrient_units, ear_units)) %>%
  select(units) %>%
  unique() 

# get median for all species
median_nutrients = nutrients %>% group_by(sciname, nutrient, sex, age) %>%
  summarize(median_ear=median(ear_proportion), median_ear_perc=median(ear_perc))

# get upper and lower bounds for all ear groups in the us
ear_ul = ear_us %>% 
  group_by(nutrient) %>%
  mutate(
    max_ear = max(ear),
    min_ear = min(ear)
  ) %>%
  select(c(iso3, country, nutrient_type, nutrient, units, max_ear, min_ear, ear_units)) %>%
  arrange(nutrient) %>%
  unique()

# get upper and lower bound 
nutrients_ul = afcd_raw %>%
  merge(ear_ul, by="nutrient") %>% 
  select(1:33, units, min_ear, max_ear, ear_units) %>%
  # need to test this
  mutate(value=case_when(nutrient_units=="g" & ear_units=="mg" ~ value*1000, # TODO: move this to clean afcd_raw in the beginning
                         TRUE ~ value), # when species units is g and ear_units are mg
         nutrient_units=case_when(nutrient_units=="g" & ear_units=="mg" ~ "mg",
                                  TRUE ~ nutrient_units)) %>% # change units
  mutate(ear_proportion_lb = value/min_ear, 
         ear_perc_lb = 100*value/min_ear,
         ear_proportion_ub = value/max_ear, 
         ear_perc_ub = 100*value/max_ear) 


# export data 
write.csv(median_nutrients,"output/median_nutrients.csv", row.names = FALSE)
write.csv(nutrients_ul,"output/ub_nutrients.csv", row.names = FALSE)
write.csv(nutrients,"output/nutrients.csv", row.names = FALSE)


# divide value by EAR, RDA (global? , age groups?)

# 2. do a weighted average 
  # for each species
  #   for each nutrient
  #     divide value by EAR, RDA (global? , age groups?)



# questions, where do I get RDA data? 
# only has 14 nutrients in total, is this ok? 


