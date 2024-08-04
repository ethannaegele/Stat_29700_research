library(knockoff)
library(pracma)
library(tidyverse)
library(TreeTools)


set.seed(12)
ep_moment_method_setting1_m2_dmp <- run_ep_moment_method_setting1_simulation(moment = 2, dampen = TRUE)

set.seed(12)
ep_moment_method_setting1_m4_dmp <- run_ep_moment_method_setting1_simulation(moment = 4, dampen = TRUE)

set.seed(12)
ep_moment_method_setting1_m6_dmp <- run_ep_moment_method_setting1_simulation(moment = 6, dampen = TRUE)

set.seed(12)
ep_moment_method_setting1_m8_dmp <- run_ep_moment_method_setting1_simulation(moment = 8, dampen = TRUE)

set.seed(12)
ep_moment_method_setting1_m10_dmp <- run_ep_moment_method_setting1_simulation(moment = 10, dampen = TRUE)

set.seed(12)
ep_moment_method_setting2_m2_dmp <- run_ep_moment_method_setting2_simulation(moment = 2, dampen = TRUE)

set.seed(12)
ep_moment_method_setting2_m4_dmp <- run_ep_moment_method_setting2_simulation(moment = 4, dampen = TRUE)

set.seed(12)
ep_moment_method_setting2_m6_dmp <- run_ep_moment_method_setting2_simulation(moment = 6, dampen = TRUE)

set.seed(12)
ep_moment_method_setting2_m8_dmp <- run_ep_moment_method_setting2_simulation(moment = 8, dampen = TRUE)

set.seed(12)
ep_moment_method_setting2_m10_dmp <- run_ep_moment_method_setting2_simulation(moment = 10, dampen = TRUE)
