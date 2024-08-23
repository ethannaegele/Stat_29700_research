library(knockoff)
library(pracma)
library(cknockoff)
library(dbh)
library(TreeTools)

# Excluding cKnockoff simulations, to be run separately
set.seed(12)

method1_setting1 <- method_setting_simulation(method = sarkar_tang_method1, 
                                                 type = 'X_AR', 
                                                 n = 200, 
                                                 d = 40,
                                                 num_true_sigs = 8,
                                                 random_true_signal_indices = FALSE,
                                                 multiplier = 1.9)


method1_setting2 <- method_setting_simulation(method = sarkar_tang_method1, 
                                                 type = 'X_AR', 
                                                 n = 500, 
                                                 d = 50,
                                                 num_true_sigs = 10,
                                                 random_true_signal_indices = FALSE,
                                                 multiplier = 1.9)

method1_setting3 <- method_setting_simulation(method = sarkar_tang_method1, 
                                              type = 'X_AR', 
                                              n = 1000, 
                                              d = 100,
                                              num_true_sigs = 20,
                                              random_true_signal_indices = FALSE,
                                              multiplier = 1.9)

method2_setting1 <- method_setting_simulation(method = sarkar_tang_method2, 
                                              type = 'X_AR', 
                                              n = 200, 
                                              d = 40,
                                              num_true_sigs = 8,
                                              random_true_signal_indices = FALSE,
                                              eta = .5,
                                              multiplier = 1.9)


method2_setting2 <- method_setting_simulation(method = sarkar_tang_method2, 
                                              type = 'X_AR', 
                                              n = 500, 
                                              d = 50,
                                              num_true_sigs = 10,
                                              random_true_signal_indices = FALSE,
                                              eta = .5,
                                              multiplier = 1.9)

method2_setting3 <- method_setting_simulation(method = sarkar_tang_method2, 
                                              type = 'X_AR', 
                                              n = 1000, 
                                              d = 100,
                                              num_true_sigs = 20,
                                              random_true_signal_indices = FALSE,
                                              eta = .5,
                                              multiplier = 1.9)



independent_BH_setting1 <- method_setting_simulation(method = independent_BH, 
                                              type = 'X_AR', 
                                              n = 200, 
                                              d = 40,
                                              num_true_sigs = 8,
                                              random_true_signal_indices = FALSE)


independent_BH_setting2 <- method_setting_simulation(method = independent_BH, 
                                              type = 'X_AR', 
                                              n = 500, 
                                              d = 50,
                                              num_true_sigs = 10,
                                              random_true_signal_indices = FALSE)

independent_BH_setting3 <- method_setting_simulation(method = independent_BH, 
                                              type = 'X_AR', 
                                              n = 1000, 
                                              d = 100,
                                              num_true_sigs = 20,
                                              random_true_signal_indices = FALSE)

knockoffs_setting1 <- method_setting_simulation(method = knockoff_procedure, 
                                                     type = 'X_AR', 
                                                     n = 200, 
                                                     d = 40,
                                                     num_true_sigs = 8,
                                                     random_true_signal_indices = FALSE)


knockoffs_setting2 <- method_setting_simulation(method = knockoff_procedure, 
                                                     type = 'X_AR', 
                                                     n = 500, 
                                                     d = 50,
                                                     num_true_sigs = 10,
                                                     random_true_signal_indices = FALSE)

knockoffs_setting3 <- method_setting_simulation(method = knockoff_procedure, 
                                                     type = 'X_AR', 
                                                     n = 1000, 
                                                     d = 100,
                                                     num_true_sigs = 20,
                                                     random_true_signal_indices = FALSE)

ep_moment_method_setting1_m2 <- method_setting_simulation(method = ep_moment_method, 
                                                type = 'X_AR', 
                                                n = 200, 
                                                d = 40,
                                                num_true_sigs = 8,
                                                random_true_signal_indices = FALSE)


ep_moment_method_setting2_m2 <- method_setting_simulation(method = ep_moment_method, 
                                                type = 'X_AR', 
                                                n = 500, 
                                                d = 50,
                                                num_true_sigs = 10,
                                                random_true_signal_indices = FALSE)

ep_moment_method_setting3_m2 <- method_setting_simulation(method = ep_moment_method, 
                                                type = 'X_AR', 
                                                n = 1000, 
                                                d = 100,
                                                num_true_sigs = 20,
                                                random_true_signal_indices = FALSE)

ep_moment_method_alternate_setting1_m2 <- method_setting_simulation(method = ep_moment_method_alternate, 
                                                       type = 'X_AR', 
                                                       n = 200, 
                                                       d = 40,
                                                       num_true_sigs = 8,
                                                       random_true_signal_indices = FALSE)


ep_moment_method_alternate_setting2_m2 <- method_setting_simulation(method = ep_moment_method_alternate, 
                                                       type = 'X_AR', 
                                                       n = 500, 
                                                       d = 50,
                                                       num_true_sigs = 10,
                                                       random_true_signal_indices = FALSE)

ep_moment_method_alternate_setting3_m2 <- method_setting_simulation(method = ep_moment_method_alternate, 
                                                       type = 'X_AR', 
                                                       n = 1000, 
                                                       d = 100,
                                                       num_true_sigs = 20,
                                                       random_true_signal_indices = FALSE)

bh_setting1 <- method_setting_simulation(method = bh_regression, 
                                                       type = 'X_AR', 
                                                       n = 200, 
                                                       d = 40,
                                                       num_true_sigs = 8,
                                                       random_true_signal_indices = FALSE)


bh_setting2 <- method_setting_simulation(method = bh_regression, 
                                                       type = 'X_AR', 
                                                       n = 500, 
                                                       d = 50,
                                                       num_true_sigs = 10,
                                                       random_true_signal_indices = FALSE)

bh_setting3 <- method_setting_simulation(method = bh_regression, 
                                                       type = 'X_AR', 
                                                       n = 1000, 
                                                       d = 100,
                                                       num_true_sigs = 20,
                                                       random_true_signal_indices = FALSE)

dbh_setting1 <- method_setting_simulation(method = dbh_procedure, 
                                                       type = 'X_AR', 
                                                       n = 200, 
                                                       d = 40,
                                                       num_true_sigs = 8,
                                                       random_true_signal_indices = FALSE)


dbh_setting2 <- method_setting_simulation(method = dbh_procedure, 
                                                       type = 'X_AR', 
                                                       n = 500, 
                                                       d = 50,
                                                       num_true_sigs = 10,
                                                       random_true_signal_indices = FALSE)

dbh_setting3 <- method_setting_simulation(method = dbh_procedure, 
                                                       type = 'X_AR', 
                                                       n = 1000, 
                                                       d = 100,
                                                       num_true_sigs = 20,
                                                       random_true_signal_indices = FALSE)


