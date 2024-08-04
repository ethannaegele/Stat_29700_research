library(knockoff)
library(pracma)
library(tidyverse)
library(TreeTools)

set.seed(12)
ep_moment_method_setting2_m2_v2 <- run_ep_moment_method_setting2_simulation(moment = 2)

set.seed(12)
ep_moment_method_setting2_m2_dmp_v2 <- run_ep_moment_method_setting2_simulation(moment = 2, dampen = TRUE)