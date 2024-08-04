library(knockoff)
library(pracma)
library(tidyverse)
library(TreeTools)
library(here)
library(foreach)
library(doParallel)

set.seed(12)
ep_moment_method_alternate_lfl_setting_mcc <- run_ep_moment_method_alternate_mcc_lfl_setting_simulation()

set.seed(12)
ep_moment_method_alternate_lfl_setting_mcc_block <- run_ep_moment_method_alternate_mcc_lfl_setting_simulation(type = 'MCC_Block')
