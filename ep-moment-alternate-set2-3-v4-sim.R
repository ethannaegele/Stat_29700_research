library(knockoff)
library(pracma)
library(tidyverse)
library(TreeTools)


set.seed(12)
ep_moment_method_alternate_setting2_m2_v4 <- run_ep_moment_method_alternate_setting2_simulation(moment = 2)

set.seed(12)
ep_moment_method_alternate_setting2_m2_dmp_v4 <- run_ep_moment_method_alternate_setting2_simulation(moment = 2, dampen = TRUE)

set.seed(12)
ep_moment_method_alternate_setting3_m2_v4 <- run_ep_moment_method_alternate_setting3_simulation(moment = 2)

set.seed(12)
ep_moment_method_alternate_setting3_m2_dmp_v4 <- run_ep_moment_method_alternate_setting3_simulation(moment = 2, dampen = TRUE)