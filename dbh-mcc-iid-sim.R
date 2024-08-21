library(knockoff)
library(pracma)
library(tidyverse)
library(dbh)

set.seed(12)
dbh_lfl_setting_mcc <- run_dbh_mcc_lfl_setting_simulation(type = 'MCC', a_vec = c(3.63, 3.66), 
                                                          alpha = c(.01, .05, .10, .20),
                                                          num_iter = 400)

set.seed(12)
dbh_lfl_setting_iid <- run_dbh_mcc_lfl_setting_simulation(type = 'IID_Normal', a_vec = c(.073, .083), 
                                                          alpha = c(.01, .05, .10, .20),
                                                          num_iter = 400)
