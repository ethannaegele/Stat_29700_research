library(knockoff)
library(pracma)
library(tidyverse)

set.seed(12)
ep_method1_cutoff_.1_setting3 <- method_setting_simulation(method = ep_sarkar_tang_method1_cutoff, 
                                                              type = 'X_AR', 
                                                              n = 1000, 
                                                              d = 100,
                                                              num_true_sigs = 20,
                                                              random_true_signal_indices = FALSE,
                                                              cutoff = .1)

set.seed(12)
ep_method1_cutoff_.2_setting3 <- method_setting_simulation(method = ep_sarkar_tang_method1_cutoff, 
                                                              type = 'X_AR', 
                                                              n = 1000, 
                                                              d = 100,
                                                              num_true_sigs = 20,
                                                              random_true_signal_indices = FALSE,
                                                              cutoff = .2)

set.seed(12)
ep_method1_cutoff_.3_setting3 <- method_setting_simulation(method = ep_sarkar_tang_method1_cutoff, 
                                                              type = 'X_AR', 
                                                              n = 1000, 
                                                              d = 100,
                                                              num_true_sigs = 20,
                                                              random_true_signal_indices = FALSE,
                                                              cutoff = .3)

set.seed(12)
ep_method1_cutoff_.4_setting3 <- method_setting_simulation(method = ep_sarkar_tang_method1_cutoff, 
                                                              type = 'X_AR', 
                                                              n = 1000, 
                                                              d = 100,
                                                              num_true_sigs = 20,
                                                              random_true_signal_indices = FALSE,
                                                              cutoff = .4)

set.seed(12)
ep_method1_cutoff_.5_setting3 <- method_setting_simulation(method = ep_sarkar_tang_method1_cutoff, 
                                                              type = 'X_AR', 
                                                              n = 1000, 
                                                              d = 100,
                                                              num_true_sigs = 20,
                                                              random_true_signal_indices = FALSE,
                                                              cutoff = .5)

set.seed(12)
ep_method1_cutoff_.6_setting3 <- method_setting_simulation(method = ep_sarkar_tang_method1_cutoff, 
                                                              type = 'X_AR', 
                                                              n = 1000, 
                                                              d = 100,
                                                              num_true_sigs = 20,
                                                              random_true_signal_indices = FALSE,
                                                              cutoff = .6)

set.seed(12)
ep_method1_cutoff_.7_setting3 <- method_setting_simulation(method = ep_sarkar_tang_method1_cutoff, 
                                                              type = 'X_AR', 
                                                              n = 1000, 
                                                              d = 100,
                                                              num_true_sigs = 20,
                                                              random_true_signal_indices = FALSE,
                                                              cutoff = .7)

set.seed(12)
ep_method1_cutoff_.8_setting3 <- method_setting_simulation(method = ep_sarkar_tang_method1_cutoff, 
                                                              type = 'X_AR', 
                                                              n = 1000, 
                                                              d = 100,
                                                              num_true_sigs = 20,
                                                              random_true_signal_indices = FALSE,
                                                              cutoff = .8)

set.seed(12)
ep_method1_cutoff_.9_setting3 <- method_setting_simulation(method = ep_sarkar_tang_method1_cutoff, 
                                                              type = 'X_AR', 
                                                              n = 1000, 
                                                              d = 100,
                                                              num_true_sigs = 20,
                                                              random_true_signal_indices = FALSE,
                                                              cutoff = .9)

set.seed(12)
ep_method1_cutoff_1_setting3 <- method_setting_simulation(method = ep_sarkar_tang_method1_cutoff, 
                                                             type = 'X_AR', 
                                                             n = 1000, 
                                                             d = 100,
                                                             num_true_sigs = 20,
                                                             random_true_signal_indices = FALSE,
                                                             cutoff = 1)
