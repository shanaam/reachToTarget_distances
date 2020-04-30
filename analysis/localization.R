## Load packages
rm(list = ls())

library(data.table)
library(tidyverse)
library(ggalt)
source("analysis/analysisFunctions.R")

## installing dply 1.0.0, mainly to try out row-wise operations
# remotes::install_github("tidyverse/dplyr")

# Anything < block 18 is in the 'aligned' session
set_session <- function(block){
  if(block < 18)
    return("aligned")
  else
    return("rotated")
}



## do
omnibus_path <- "data/omnibus/all_locs.csv"

omnibus_data <- loadData(omnibus_path)

omnibus_data$target_distance <- as.factor(omnibus_data$target_distance)

omnibus_data <- omnibus_data %>%
  rowwise() %>%
  mutate(session = set_session(block_num))



## visualize
# aligned only
p <- omnibus_data %>%
  filter(session == "aligned") %>%
  ggplot() +
  annotate("path",
           x= 0 + .15*cos(seq(0, 1*pi,length.out=100)),
           y= 0 + .15*sin(seq(0, 1*pi,length.out=100)), 
           size = 1.5, alpha = 0.15) +
  annotate("path",
           x= 0 + .20*cos(seq(0, 1*pi,length.out=100)),
           y= 0 + .20*sin(seq(0, 1*pi,length.out=100)), 
           size = 1.5, alpha = 0.15) +
  geom_point(aes(x = loc_x - home_x, y = loc_z - home_z), 
             colour = "#FF5A5A", alpha = 0.6, size = 2) +
  geom_point(aes(x = target_x - home_x, y = target_z - home_z), 
             colour = "#91BFF2", alpha = 0.6, size = 2) +
  geom_segment(aes(x = target_x - home_x, y = target_z - home_z,
                   xend = loc_x - home_x, yend = loc_z - home_z,
                   colour = target_distance), 
               alpha = 0.2, size = 1) +
  theme_minimal() +
  coord_fixed() +
  theme(text = element_text(size=40), 
        axis.text = element_text(size=40), 
        legend.text = element_text(size=48)) +
  NULL

p

ggsave(p, height = 20, width = 20, device = "svg", filename = "plots/aligned_loc_plot.svg")


# rotated only
p <- omnibus_data %>%
  filter(session == "rotated") %>%
  ggplot() +
  annotate("path",
           x= 0 + .15*cos(seq(0, 1*pi,length.out=100)),
           y= 0 + .15*sin(seq(0, 1*pi,length.out=100)), 
           size = 1.5, alpha = 0.15) +
  annotate("path",
           x= 0 + .20*cos(seq(0, 1*pi,length.out=100)),
           y= 0 + .20*sin(seq(0, 1*pi,length.out=100)), 
           size = 1.5, alpha = 0.15) +
  geom_point(aes(x = loc_x - home_x, y = loc_z - home_z), 
             colour = "#FF5A5A", alpha = 0.6, size = 2) +
  geom_point(aes(x = target_x - home_x, y = target_z - home_z), 
             colour = "#91BFF2", alpha = 0.6, size = 2) +
  geom_segment(aes(x = target_x - home_x, y = target_z - home_z,
                   xend = loc_x - home_x, yend = loc_z - home_z,
                   colour = target_distance), 
               alpha = 0.2, size = 1) +
  theme_minimal() +
  coord_fixed() +
  theme(text = element_text(size=40), 
        axis.text = element_text(size=40), 
        legend.text = element_text(size=48)) +
  NULL

p

ggsave(p, height = 20, width = 20, device = "svg", filename = "plots/rotated_loc_plot.svg")

