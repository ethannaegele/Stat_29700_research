library(knockoff)
library(pracma)
library(tidyverse)
library(TreeTools)

# IMPORTANT NOTE: HIGHER MOMENTS FOR SETTINGS 2 AND 3 ARE NOT VIABLE DUE TO MOMENT CALCULATION FOR
# F DISTRIBUTION DEPEDNENT ON THE GAMMA FUNCTION 

set.seed(12)
ep_moment_method_setting3_m2 <- run_ep_moment_method_setting3_simulation(moment = 2)



