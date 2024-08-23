library(knockoff)
library(pracma)

set.seed(17)
method1_setting1_sigma <- method_setting_simulation(method = sarkar_tang_method1_sigma, 
                                                   type = 'X_AR', 
                                                   n = 200, 
                                                   d = 40,
                                                   num_true_sigs = 8,
                                                   random_true_signal_indices = FALSE)

method1_setting2_sigma <- method_setting_simulation(method = sarkar_tang_method1_sigma, 
                                                    type = 'X_AR', 
                                                    n = 500, 
                                                    d = 50,
                                                    num_true_sigs = 10,
                                                    random_true_signal_indices = FALSE)

method1_setting3_sigma <- method_setting_simulation(method = sarkar_tang_method1_sigma, 
                                                    type = 'X_AR', 
                                                    n = 1000, 
                                                    d = 100,
                                                    num_true_sigs = 20,
                                                    random_true_signal_indices = FALSE)
