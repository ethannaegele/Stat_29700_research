library(knockoff)
library(pracma)
library(TreeTools)

set.seed(12)
ep_moment_method_sigma_setting1_m2_v4 <- run_ep_moment_method_setting1_simulation(sigma = 1)

set.seed(12)
ep_moment_method_sigma_setting2_m2_v4 <- run_ep_moment_method_setting2_simulation(sigma = 1)

set.seed(12)
ep_moment_method_sigma_setting3_m2_v4 <- run_ep_moment_method_setting3_simulation(sigma = 1)