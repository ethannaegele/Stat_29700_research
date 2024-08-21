library(knockoff)
library(pracma)
library(tidyverse)
library(TreeTools)

set.seed(12)
knockoffs_equi_lfl_setting_iid <- run_knockoffs_mcc_lfl_setting_simulation(type = "IID_Normal", 
                                                                           a_vec = c(.073, .083), 
                                                                           alpha = c(.01, .05, .10, .20), 
                                                                           num_iter = 400)


set.seed(12)
knockoffs_sdp_lfl_setting_iid <- run_knockoffs_mcc_lfl_setting_simulation(type = "IID_Normal", 
                                                                          a_vec = c(.073, .083), 
                                                                          alpha = c(.01, .05, .10, .20), 
                                                                          num_iter = 400,
                                                                          method = 'sdp')

