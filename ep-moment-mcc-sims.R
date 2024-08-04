library(knockoff)
library(pracma)
library(tidyverse)
library(TreeTools)

set.seed(12)
ep_moment_method_lfl_setting_mcc <- run_ep_moment_method_mcc_lfl_setting_simulation(type = 'MCC', 
                                                                                    a_vec = c(3.63), 
                                                                                    alpha = c(.01, .05, .10, .20), 
                                                                                    num_iter = 400)
