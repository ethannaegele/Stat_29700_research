library(knockoff)
library(pracma)
library(tidyverse)
library(TreeTools)
library(cknockoff)

CKnockoff_lfl_setting_mcc <- run_CKnockoffs_mcc_lfl_setting_simulation(a_vec = c(3.63),
                                                                       alpha = c(.01, .05, .10, .20), 
                                                                       num_iter = 400)