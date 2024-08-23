library(knockoff)
library(pracma)
library(cknockoff)
library(dbh)
library(TreeTools)


set.seed(14)
CKnockoff_setting1 <- method_setting_simulation(method = cknockoff_procedure, 
                                                   type = 'X_AR', 
                                                   n = 200, 
                                                   d = 40,
                                                   num_true_sigs = 8,
                                                   random_true_signal_indices = FALSE)

CKnockoff_setting2 <- method_setting_simulation(method = cknockoff_procedure, 
                                              type = 'X_AR', 
                                              n = 500, 
                                              d = 50,
                                              num_true_sigs = 10,
                                              random_true_signal_indices = FALSE)

CKnockoff_setting3 <- method_setting_simulation(method = cknockoff_procedure, 
                                              type = 'X_AR', 
                                              n = 1000, 
                                              d = 100,
                                              num_true_sigs = 20,
                                              random_true_signal_indices = FALSE)

CKnockoff_mcc <- method_setting_simulation(method = cknockoff_procedure, 
                                           type = 'MCC', 
                                           n = 1000, 
                                           d = 100,
                                           a_vec = c(3.63, 3.66),
                                           alpha = c(.01, .05, .10, .20),
                                           num_true_sigs = 10,
                                           num_iter = 400,
                                           random_true_signal_indices = FALSE)

CKnockoff_iid <- method_setting_simulation(method = cknockoff_procedure, 
                                           type = 'IID_Normal', 
                                           n = 1000, 
                                           d = 100,
                                           a_vec = c(.073, .083),
                                           alpha = c(.01, .05, .10, .20),
                                           num_true_sigs = 10,
                                           num_iter = 400,
                                           random_true_signal_indices = FALSE)
