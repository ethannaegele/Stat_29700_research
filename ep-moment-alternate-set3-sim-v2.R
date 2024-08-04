library(knockoff)
library(pracma)
library(tidyverse)
library(TreeTools)


set.seed(12)
ep_moment_method_alternate_setting3_m2_v2 <- run_ep_moment_method_alternate_setting3_simulation(moment = 2)

set.seed(12)
ep_moment_method_alternate_setting3_m4_v2 <- run_ep_moment_method_alternate_setting3_simulation(moment = 4)

set.seed(12)
ep_moment_method_alternate_setting3_m6_v2 <- run_ep_moment_method_alternate_setting3_simulation(moment = 6)
