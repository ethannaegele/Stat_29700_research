library(knockoff)
library(pracma)
library(tidyverse)
library(TreeTools)

set.seed(12)
ep_moment_method_setting3_m2 <- run_ep_moment_method_setting3_simulation(moment = 2, dampen = TRUE)

set.seed(12)
ep_moment_method_setting3_m4 <- run_ep_moment_method_setting3_simulation(moment = 4, dampen = TRUE)

set.seed(12)
ep_moment_method_setting3_m6 <- run_ep_moment_method_setting3_simulation(moment = 6, dampen = TRUE)

set.seed(12)
ep_moment_method_setting3_m8 <- run_ep_moment_method_setting3_simulation(moment = 8, dampen = TRUE)

set.seed(12)
ep_moment_method_setting3_m10 <- run_ep_moment_method_setting3_simulation(moment = 10, dampen = TRUE)