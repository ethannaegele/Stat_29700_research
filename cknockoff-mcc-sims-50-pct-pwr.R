library(knockoff)
library(pracma)
library(tidyverse)
library(cknockoff)

# using the beta* value for 50% power only
set.seed(12)
CKnockoff_lfl_setting_mcc_.5_pwr <- run_CKnockoffs_mcc_lfl_setting_simulation(a_vec = c(3.66),
                                                                       alpha = c(.01, .05, .10, .20), 
                                                                       num_iter = 400)