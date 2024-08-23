library(knockoff)
library(pracma)
library(TreeTools)

set.seed(18)
ep_moment_method_sigma_setting1_m2 <- method_setting_simulation(method = ep_moment_method, 
                                                                   type = 'X_AR', 
                                                                   n = 200, 
                                                                   d = 40,
                                                                   num_true_sigs = 8,
                                                                   random_true_signal_indices = FALSE,
                                                                   sigma = 1)


ep_moment_method_sigma_setting2_m2 <- method_setting_simulation(method = ep_moment_method, 
                                                                   type = 'X_AR', 
                                                                   n = 500, 
                                                                   d = 50,
                                                                   num_true_sigs = 10,
                                                                   random_true_signal_indices = FALSE,
                                                                   sigma = 1)

ep_moment_method_sigma_setting3_m2 <- method_setting_simulation(method = ep_moment_method, 
                                                                   type = 'X_AR', 
                                                                   n = 1000, 
                                                                   d = 100,
                                                                   num_true_sigs = 20,
                                                                   random_true_signal_indices = FALSE,
                                                                   sigma = 1)