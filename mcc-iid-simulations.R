library(knockoff)
library(pracma)
library(cknockoff)
library(dbh)
library(TreeTools)

# Excluding cKnockoff simulations, to be run separately

set.seed(13)

method1_mcc <- method_setting_simulation(method = sarkar_tang_method1, 
                                              type = 'MCC', 
                                              n = 1000,
                                         a_vec = c(3.63, 3.66),
                                         alpha = c(.01, .05, .10, .20),
                                              d = 100,
                                              num_true_sigs = 10,
                                         num_iter = 400,
                                              random_true_signal_indices = FALSE)

method1_iid <- method_setting_simulation(method = sarkar_tang_method1, 
                                         type = 'IID_Normal', 
                                         n = 1000, 
                                         d = 100,
                                         a_vec = c(.073, .083),
                                         alpha = c(.01, .05, .10, .20),
                                         num_true_sigs = 10,
                                         num_iter = 400,
                                         random_true_signal_indices = FALSE)

method2_mcc <- method_setting_simulation(method = sarkar_tang_method2, 
                                         type = 'MCC', 
                                         n = 1000,
                                         a_vec = c(3.63, 3.66),
                                         alpha = c(.01, .05, .10, .20),
                                         d = 100,
                                         num_true_sigs = 10,
                                         num_iter = 400,
                                         random_true_signal_indices = FALSE,
                                         eta = .5)

method2_iid <- method_setting_simulation(method = sarkar_tang_method2, 
                                         type = 'IID_Normal', 
                                         n = 1000, 
                                         d = 100,
                                         a_vec = c(.073, .083),
                                         alpha = c(.01, .05, .10, .20),
                                         num_true_sigs = 10,
                                         random_true_signal_indices = FALSE,
                                         num_iter = 400,
                                         eta = .5)

knockoffs_mcc <- method_setting_simulation(method = knockoff_procedure, 
                                         type = 'MCC', 
                                         n = 1000, 
                                         d = 100,
                                         a_vec = c(3.63, 3.66),
                                         alpha = c(.01, .05, .10, .20),
                                         num_true_sigs = 10,
                                         random_true_signal_indices = FALSE,
                                         num_iter = 400,
                                         ko_method = 'sdp')

knockoffs_iid <- method_setting_simulation(method = knockoff_procedure, 
                                         type = 'IID_Normal', 
                                         n = 1000, 
                                         d = 100,
                                         a_vec = c(.073, .083),
                                         alpha = c(.01, .05, .10, .20),
                                         num_true_sigs = 10,
                                         random_true_signal_indices = FALSE,
                                         num_iter = 400,
                                         ko_method = 'sdp')

ep_moment_method_mcc <- method_setting_simulation(method = ep_moment_method, 
                                           type = 'MCC', 
                                           n = 1000, 
                                           d = 100,
                                           a_vec = c(3.63, 3.66),
                                           alpha = c(.01, .05, .10, .20),
                                           num_true_sigs = 10,
                                           num_iter = 400,
                                           random_true_signal_indices = FALSE)

ep_moment_method_iid <- method_setting_simulation(method = ep_moment_method, 
                                           type = 'IID_Normal', 
                                           n = 1000, 
                                           d = 100,
                                           a_vec = c(.073, .083),
                                           alpha = c(.01, .05, .10, .20),
                                           num_true_sigs = 10,
                                           num_iter = 400,
                                           random_true_signal_indices = FALSE)

ep_moment_method_alternate_mcc <- method_setting_simulation(method = ep_moment_method_alternate, 
                                                  type = 'MCC', 
                                                  n = 1000, 
                                                  d = 100,
                                                  a_vec = c(3.63, 3.66),
                                                  alpha = c(.01, .05, .10, .20),
                                                  num_true_sigs = 10,
                                                  num_iter = 400,
                                                  random_true_signal_indices = FALSE)

ep_moment_method_alternate_iid <- method_setting_simulation(method = ep_moment_method_alternate, 
                                                  type = 'IID_Normal', 
                                                  n = 1000, 
                                                  d = 100,
                                                  a_vec = c(.073, .083),
                                                  alpha = c(.01, .05, .10, .20),
                                                  num_true_sigs = 10,
                                                  num_iter = 400,
                                                  random_true_signal_indices = FALSE)

bh_mcc <- method_setting_simulation(method = bh_regression, 
                                                            type = 'MCC', 
                                                            n = 1000, 
                                                            d = 100,
                                    a_vec = c(3.63, 3.66),
                                    alpha = c(.01, .05, .10, .20),
                                                            num_true_sigs = 10,
                                    num_iter = 400,
                                                            random_true_signal_indices = FALSE)

bh_iid <- method_setting_simulation(method = bh_regression, 
                                                            type = 'IID_Normal', 
                                                            n = 1000, 
                                                            d = 100,
                                    a_vec = c(.073, .083),
                                    alpha = c(.01, .05, .10, .20),
                                                            num_true_sigs = 10,
                                    num_iter = 400,
                                                            random_true_signal_indices = FALSE)

dbh_mcc <- method_setting_simulation(method = dbh_procedure, 
                                    type = 'MCC', 
                                    n = 1000, 
                                    d = 100,
                                    a_vec = c(3.63, 3.66),
                                    alpha = c(.01, .05, .10, .20),
                                    num_true_sigs = 10,
                                    num_iter = 400,
                                    random_true_signal_indices = FALSE)

dbh_iid <- method_setting_simulation(method = dbh_procedure, 
                                    type = 'IID_Normal', 
                                    n = 1000, 
                                    d = 100,
                                    a_vec = c(.073, .083),
                                    alpha = c(.01, .05, .10, .20),
                                    num_true_sigs = 10,
                                    num_iter = 400,
                                    random_true_signal_indices = FALSE)


