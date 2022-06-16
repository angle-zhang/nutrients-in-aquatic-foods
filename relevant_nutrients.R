
library(tidyverse)

load(file='data/afcd.rda')

afcd_orig = afcd 

afcd_raw = afcd_orig %>% 
  filter(food_prep == "raw")

ear_orig <- readRDS("data/GENUS_EAR_population_data_merged.Rds") 

ear_us = ear_orig %>%
  filter(iso3=="USA")

# 1. do for one group first
ear_us_g1 = ear_us %>%
  filter(sex=="Children", age=="5-9") 

# for each nutrient get the corresponding ear 
nutrients = afcd_raw %>%
  merge(ear_us_g1, by="nutrient") %>% 
  select(1:33, ear, units, sex, age) %>%
  mutate(ear_units=gsub("/d", "", units)) %>%
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

# divide value by EAR, RDA (global? , age groups?)

# 2. do a weighted average 
  # for each species
  #   for each nutrient
  #     divide value by EAR, RDA (global? , age groups?)



# questions, where do I get RDA data? 
# only has 14 nutrients in total, is this ok? 


