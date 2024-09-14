library(knockoff)
library(pracma)
library(tidyverse)

set.seed(31)
ep_method1_cutoff_.1_setting2 <- method_setting_simulation(method = ep_sarkar_tang_method1_cutoff, 
                                                              type = 'X_AR', 
                                                              n = 500, 
                                                              d = 50,
                                                              num_true_sigs = 10,
                                                              random_true_signal_indices = FALSE,
                                                              cutoff = .1)

ep_method1_cutoff_.2_setting2 <- method_setting_simulation(method = ep_sarkar_tang_method1_cutoff, 
                                                              type = 'X_AR', 
                                                              n = 500, 
                                                              d = 50,
                                                              num_true_sigs = 10,
                                                              random_true_signal_indices = FALSE,
                                                              cutoff = .2)


ep_method1_cutoff_.3_setting2 <- method_setting_simulation(method = ep_sarkar_tang_method1_cutoff, 
                                                              type = 'X_AR', 
                                                              n = 500, 
                                                              d = 50,
                                                              num_true_sigs = 10,
                                                              random_true_signal_indices = FALSE,
                                                              cutoff = .3)


ep_method1_cutoff_.4_setting2 <- method_setting_simulation(method = ep_sarkar_tang_method1_cutoff, 
                                                              type = 'X_AR', 
                                                              n = 500, 
                                                              d = 50,
                                                              num_true_sigs = 10,
                                                              random_true_signal_indices = FALSE,
                                                              cutoff = .4)


ep_method1_cutoff_.5_setting2 <- method_setting_simulation(method = ep_sarkar_tang_method1_cutoff, 
                                                              type = 'X_AR', 
                                                              n = 500, 
                                                              d = 50,
                                                              num_true_sigs = 10,
                                                              random_true_signal_indices = FALSE,
                                                              cutoff = .5)


ep_method1_cutoff_.6_setting2 <- method_setting_simulation(method = ep_sarkar_tang_method1_cutoff, 
                                                              type = 'X_AR', 
                                                              n = 500, 
                                                              d = 50,
                                                              num_true_sigs = 10,
                                                              random_true_signal_indices = FALSE,
                                                              cutoff = .6)


ep_method1_cutoff_.7_setting2 <- method_setting_simulation(method = ep_sarkar_tang_method1_cutoff, 
                                                              type = 'X_AR', 
                                                              n = 500, 
                                                              d = 50,
                                                              num_true_sigs = 10,
                                                              random_true_signal_indices = FALSE,
                                                              cutoff = .7)


ep_method1_cutoff_.8_setting2 <- method_setting_simulation(method = ep_sarkar_tang_method1_cutoff, 
                                                              type = 'X_AR', 
                                                              n = 500, 
                                                              d = 50,
                                                              num_true_sigs = 10,
                                                              random_true_signal_indices = FALSE,
                                                              cutoff = .8)


ep_method1_cutoff_.9_setting2 <- method_setting_simulation(method = ep_sarkar_tang_method1_cutoff, 
                                                              type = 'X_AR', 
                                                              n = 500, 
                                                              d = 50,
                                                              num_true_sigs = 10,
                                                              random_true_signal_indices = FALSE,
                                                              cutoff = .9)


ep_method1_cutoff_1_setting2 <- method_setting_simulation(method = ep_sarkar_tang_method1_cutoff, 
                                                             type = 'X_AR', 
                                                             n = 500, 
                                                             d = 50,
                                                             num_true_sigs = 10,
                                                             random_true_signal_indices = FALSE,
                                                             cutoff = 1)
