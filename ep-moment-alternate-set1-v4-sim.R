library(knockoff)
library(pracma)
library(tidyverse)
library(TreeTools)

set.seed(12)
ep_moment_method_alternate_setting1_m2_v4 <- run_ep_moment_method_alternate_setting1_simulation(moment = 2)

set.seed(12)
ep_moment_method_alternate_setting1_m4_v4 <- run_ep_moment_method_alternate_setting1_simulation(moment = 4)

set.seed(12)
ep_moment_method_alternate_setting1_m6_v4 <- run_ep_moment_method_alternate_setting1_simulation(moment = 6)

set.seed(12)
ep_moment_method_alternate_setting1_m8_v4 <- run_ep_moment_method_alternate_setting1_simulation(moment = 8)

set.seed(12)
ep_moment_method_alternate_setting1_m10_v4 <- run_ep_moment_method_alternate_setting1_simulation(moment = 10)

set.seed(12)
ep_moment_method_alternate_setting1_m2_dmp_v4 <- run_ep_moment_method_alternate_setting1_simulation(moment = 2, dampen = TRUE)

set.seed(12)
ep_moment_method_alternate_setting1_m4_dmp_v4 <- run_ep_moment_method_alternate_setting1_simulation(moment = 4, dampen = TRUE)

set.seed(12)
ep_moment_method_alternate_setting1_m6_dmp_v4 <- run_ep_moment_method_alternate_setting1_simulation(moment = 6, dampen = TRUE)

set.seed(12)
ep_moment_method_alternate_setting1_m8_dmp_v4 <- run_ep_moment_method_alternate_setting1_simulation(moment = 8, dampen = TRUE)

set.seed(12)
ep_moment_method_alternate_setting1_m10_dmp_v4 <- run_ep_moment_method_alternate_setting1_simulation(moment = 10, dampen = TRUE)
