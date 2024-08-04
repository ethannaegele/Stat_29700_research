library(knockoff)
library(pracma)
library(tidyverse)
library(TreeTools)

set.seed(12)
method1_lfl_setting_mcc_v2 <- run_sarkar_tang_method1_mcc_lfl_setting_simulation(a_vec = c(3.63), 
                                                                              alpha = c(.01, .05, .10, .20), 
                                                                              num_iter = 400)


