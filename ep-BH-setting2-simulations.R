library(knockoff)
library(pracma)
library(tidyverse)

set.seed(12)
ep_method1_0_setting2 <- run_ep_method1_setting2_simulation(exponent = 0)

set.seed(12)
ep_method1_1_16_setting2 <- run_ep_method1_setting2_simulation(exponent = 1/16)

set.seed(12)
ep_method1_1_8_setting2 <- run_ep_method1_setting2_simulation(exponent = 1/8)

set.seed(12)
ep_method1_1_4_setting2 <- run_ep_method1_setting2_simulation(exponent = 1/4)

set.seed(12)
ep_method1_3_4_setting2 <- run_ep_method1_setting2_simulation(exponent = 3/4)

set.seed(12)
ep_method1_9_10_setting2 <- run_ep_method1_setting2_simulation(exponent = 9/10)

set.seed(12)
ep_method1_1_setting2 <- run_ep_method1_setting2_simulation(exponent = 1)

