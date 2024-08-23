library(knockoff)
library(pracma)
library(tidyverse)

set.seed(16)
ep_moment_method_setting1_mult_1.98 <- method_setting_simulation(method = ep_moment_method, 
                                                                                       type = 'X_AR', 
                                                                                       n = 200, 
                                                                                       d = 40,
                                                                                       num_true_sigs = 8,
                                                                                       random_true_signal_indices = FALSE,
                                                                 multiplier = 1.98)

ep_moment_method_setting1_mult_1.96 <- method_setting_simulation(method = ep_moment_method, 
                                                                 type = 'X_AR', 
                                                                 n = 200, 
                                                                 d = 40,
                                                                 num_true_sigs = 8,
                                                                 random_true_signal_indices = FALSE,
                                                                 multiplier = 1.96)

ep_moment_method_setting1_mult_1.94 <- method_setting_simulation(method = ep_moment_method, 
                                                                 type = 'X_AR', 
                                                                 n = 200, 
                                                                 d = 40,
                                                                 num_true_sigs = 8,
                                                                 random_true_signal_indices = FALSE,
                                                                 multiplier = 1.94)

ep_moment_method_setting1_mult_1.92 <- method_setting_simulation(method = ep_moment_method, 
                                                                 type = 'X_AR', 
                                                                 n = 200, 
                                                                 d = 40,
                                                                 num_true_sigs = 8,
                                                                 random_true_signal_indices = FALSE,
                                                                 multiplier = 1.92)

ep_moment_method_setting1_mult_1.90 <- method_setting_simulation(method = ep_moment_method, 
                                                                 type = 'X_AR', 
                                                                 n = 200, 
                                                                 d = 40,
                                                                 num_true_sigs = 8,
                                                                 random_true_signal_indices = FALSE,
                                                                 multiplier = 1.98)
