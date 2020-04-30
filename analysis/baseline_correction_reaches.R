## Load packages
rm(list = ls())

library(data.table)
library(tidyverse)
source("analysis/analysisFunctions.R")

##----
## make a baseline file
make_blsummaries <- function(){ 
  
  baseline_summarries <- omnibus_data %>%
  filter(block_num < 18) %>%
  group_by(target_distance, ppid, type, targetAngle) %>%
  summarise(mean_theta = mean(theta, na.rm = TRUE), 
            sd = sd(theta, na.rm = TRUE), 
            ci = vector_confint(theta), 
            n = n(), 
            median_theta = median(theta, na.rm = TRUE)) 
  
  baseline_summarries$type <- baseline_summarries$type %>%
    recode(aligned = "reach")
  
  return(baseline_summarries)
}

# apply bl correction
apply_blcorrection <- function(rot_data_row, bl_df){
  
  # make sure input is in format: target_distance, ppt, type, target, theta
  
  bl <- filter(bl_df, 
               target_distance == as.numeric(rot_data_row[1]),
               ppid == as.numeric(rot_data_row[2]),
               type == rot_data_row[3],
               targetAngle == as.numeric(rot_data_row[4]))$mean_theta %>% 
    mean(na.rm = TRUE)
  
  corrected_dev <- as.numeric(rot_data_row[5]) - bl
  
  return(corrected_dev)
}

applY_uqID <- function(row){
  paste(row[1], row[2], sep = "_")
}

apply_dummy <- function(rot_data_row){
  ## Input: ppid, trial_num, type, instruction
  if(rot_data_row[3] == "rotated" | rot_data_row[3] == "aligned" ){
    if(as.numeric(rot_data_row[2]) >= 149 & as.numeric(rot_data_row[2]) <= 152)
      return(as.factor(1))
    else if(as.numeric(rot_data_row[2]) >= 153 & as.numeric(rot_data_row[2]) <= 156)
      return(as.factor(2))
    else if(as.numeric(rot_data_row[2]) >= 245 & as.numeric(rot_data_row[2]) <= 248)
      return(as.factor(3))
    else 
      return(as.factor("N"))
  }
  else {
    if(rot_data_row[4] == "E")
      return(as.factor("E"))
    else if(rot_data_row[4] == "I")
      return(as.factor("I"))
    else
      return(as.factor("X"))
  }
}

# do
omnibus_path <- "data/omnibus/all_reaches.csv"

omnibus_data <- loadData(omnibus_path)

# make functions and move up if possible
# this should have summary stats (per exp, ppt, type, target)
baseline_summarries <- make_blsummaries()

rot_data <- omnibus_data %>%
  filter(block_num >= 18)

rot_data$type_new <- rot_data$type %>%
  recode(aligned = "reach", rotated = "reach")

rot_data$temp <- select(rot_data, target_distance, ppid, type_new, targetAngle, theta) %>%
  apply(1, FUN = apply_blcorrection, bl_df = baseline_summarries)

rot_data$type_new <- NULL

# rename some columns
rot_data <- 
  rot_data %>% 
  rename(raw_theta = theta) %>%
  rename(theta = temp)

# SAVE HERE
# fwrite(rot_data, file = paste("data/wide_format", "rot_data_bl_corrected.csv", sep = '/'))


## Make JASPable
# Add dummies for JASP
rot_data$dummy <- rot_data %>%
  select(ppid, trial_num, type, instruction) %>%
  apply(1, FUN = apply_dummy)

rot_data_JASP <- rot_data %>%
  group_by(target_distance, ppid, dummy) %>%
  summarise(mean_dev = mean(theta, na.rm = TRUE), 
            sd = sd(theta, na.rm = TRUE), 
            ci = vector_confint(theta), 
            n = n(), 
            median_dev = median(theta, na.rm = TRUE)) 

rot_data_JASP <- rot_data_JASP %>%
  select(target_distance, ppid, dummy, mean_dev) %>%
  pivot_wider(names_from = dummy, values_from = mean_dev) %>%
  select(-N)

# fwrite(rot_data_JASP, file = paste("data/wide_format", "blocked_JASP.csv", sep = '/'))


## Make Excel-able
# make column data (pivot)

rot_data$uqID <- select(rot_data, ppid, target_distance) %>%
  apply(1, FUN = applY_uqID)

rot_data_wide_reaches <- rot_data %>%
  select(uqID, trial_num, block_num, type, instruction, theta) %>%
  filter(type == "rotated") %>%
  pivot_wider(names_from = uqID, values_from = theta)

# SAVE HERE
# fwrite(rot_data_wide_reaches, file = paste("data/wide_format", "reaches.csv", sep = '/'))

# do the same for instructed, and non-instructed data
rot_data_wide_nocur <- rot_data %>%
  select(uqID, block_num, trial_num, type, instruction, theta) %>%
  filter(type == "nocursor") %>%
  pivot_wider(names_from = uqID, values_from = theta)

# fwrite(rot_data_wide_nocur, file = paste("data/wide_format", "no_cursors.csv", sep = '/'))
