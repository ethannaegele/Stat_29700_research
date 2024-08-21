library(knockoff)
library(pracma)
library(tidyverse)
library(TreeTools)
library(cknockoff)

set.seed(12)
CKnockoff_lfl_setting_iid <- run_CKnockoffs_mcc_lfl_setting_simulation(type = "IID_Normal",
                                                                       a_vec = c(.073, .083),
                                                                       alpha = c(.01, .05, .10, .20), 
                                                                       num_iter = 400)