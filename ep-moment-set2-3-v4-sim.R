library(knockoff)
library(pracma)
library(tidyverse)
library(TreeTools)

set.seed(12)
ep_moment_method_setting2_m2 <- method_setting_simulation(method = ep_moment_method, 
                                                             type = 'X_AR', 
                                                             n = 500, 
                                                             d = 50,
                                                             num_true_sigs = 10,
                                                             random_true_signal_indices = FALSE,
                                                             moment = 2)

set.seed(12)
ep_moment_method_setting2_m2_dmp <- method_setting_simulation(method = ep_moment_method, 
                                                                 type = 'X_AR', 
                                                                 n = 500, 
                                                                 d = 50,
                                                                 num_true_sigs = 10,
                                                                 random_true_signal_indices = FALSE,
                                                                 moment = 2,
                                                                 dampen = TRUE)
set.seed(12)
ep_moment_method_setting3_m2 <- method_setting_simulation(method = ep_moment_method, 
                                                             type = 'X_AR', 
                                                             n = 1000, 
                                                             d = 100,
                                                             num_true_sigs = 20,
                                                             random_true_signal_indices = FALSE,
                                                             moment = 2)
  
set.seed(12)
ep_moment_method_setting3_m2_dmp <- method_setting_simulation(method = ep_moment_method, 
                                                                 type = 'X_AR', 
                                                                 n = 1000, 
                                                                 d = 100,
                                                                 num_true_sigs = 20,
                                                                 random_true_signal_indices = FALSE,
                                                                 moment = 2,
                                                                 dampen = TRUE)
