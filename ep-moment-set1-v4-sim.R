library(knockoff)
library(pracma)
library(tidyverse)
library(TreeTools)


set.seed(12)
ep_moment_method_setting1_m2 <- method_setting_simulation(method = ep_moment_method, 
                                                type = 'X_AR', 
                                                n = 200, 
                                                d = 40,
                                                num_true_sigs = 8,
                                                random_true_signal_indices = FALSE,
                                                moment = 2)

set.seed(12)
ep_moment_method_setting1_m4 <- method_setting_simulation(method = ep_moment_method, 
                                                             type = 'X_AR', 
                                                             n = 200, 
                                                             d = 40,
                                                             num_true_sigs = 8,
                                                             random_true_signal_indices = FALSE,
                                                             moment = 4)

set.seed(12)
ep_moment_method_setting1_m6 <- method_setting_simulation(method = ep_moment_method, 
                                                             type = 'X_AR', 
                                                             n = 200, 
                                                             d = 40,
                                                             num_true_sigs = 8,
                                                             random_true_signal_indices = FALSE,
                                                             moment = 6)

set.seed(12)
ep_moment_method_setting1_m8 <- method_setting_simulation(method = ep_moment_method, 
                                                             type = 'X_AR', 
                                                             n = 200, 
                                                             d = 40,
                                                             num_true_sigs = 8,
                                                             random_true_signal_indices = FALSE,
                                                             moment = 8)

set.seed(12)
ep_moment_method_setting1_m10 <- method_setting_simulation(method = ep_moment_method, 
                                                              type = 'X_AR', 
                                                              n = 200, 
                                                              d = 40,
                                                              num_true_sigs = 8,
                                                              random_true_signal_indices = FALSE,
                                                              moment = 10)

set.seed(12)
ep_moment_method_setting1_m2_dmp <- method_setting_simulation(method = ep_moment_method, 
                                                                 type = 'X_AR', 
                                                                 n = 200, 
                                                                 d = 40,
                                                                 num_true_sigs = 8,
                                                                 random_true_signal_indices = FALSE,
                                                                 moment = 2,
                                                                 dampen = TRUE)

set.seed(12)
ep_moment_method_setting1_m4_dmp <- method_setting_simulation(method = ep_moment_method, 
                                                                 type = 'X_AR', 
                                                                 n = 200, 
                                                                 d = 40,
                                                                 num_true_sigs = 8,
                                                                 random_true_signal_indices = FALSE,
                                                                 moment = 4,
                                                                 dampen = TRUE)

set.seed(12)
ep_moment_method_setting1_m6_dmp <- method_setting_simulation(method = ep_moment_method, 
                                                                 type = 'X_AR', 
                                                                 n = 200, 
                                                                 d = 40,
                                                                 num_true_sigs = 8,
                                                                 random_true_signal_indices = FALSE,
                                                                 moment = 6,
                                                                 dampen = TRUE)

set.seed(12)
ep_moment_method_setting1_m8_dmp <- method_setting_simulation(method = ep_moment_method, 
                                                                 type = 'X_AR', 
                                                                 n = 200, 
                                                                 d = 40,
                                                                 num_true_sigs = 8,
                                                                 random_true_signal_indices = FALSE,
                                                                 moment = 8,
                                                                 dampen = TRUE)
set.seed(12)
ep_moment_method_setting1_m10_dmp <- method_setting_simulation(method = ep_moment_method, 
                                                                  type = 'X_AR', 
                                                                  n = 200, 
                                                                  d = 40,
                                                                  num_true_sigs = 8,
                                                                  random_true_signal_indices = FALSE,
                                                                  moment = 10,
                                                                  dampen = TRUE)
