library(knockoff)
library(pracma)
library(cknockoff)
library(dbh)
library(TreeTools)

# General function for running a simulation on a method with fixed signal amplitude, number of true signals, and design size and type.
method_simulation <- function(method, 
                                  n, d, a, 
                                  num_true_sigs, 
                                  alpha = .05, 
                                  type = 'MCC',
                                  random_true_signal_indices = TRUE,
                                  num_iter = 500, 
                                  ...){
  # method (function) - the multiple testing method to be simulated
  # n (integer) - number of rows in design matrix
  # d (integer) - number of columns in design matrix
  # a (numeric) - signal amplitude
  # num_true_sigs - number of true (nonzero) signals in the setting
  # alpha (numeric) - nominal FDR level
  # type (char) - the type of design matrix, e.g. MCC, IID_Normal, X_AR
  # random_true_signal_indices (logical) - whether the signal indices are placed at random or are just the first k indices
  # num_iter (integer) - number of iterations to perform the simulation
  # ... - additional arguments to be supplied to the method, e.g. multiplier, moment, dampen. See the methods file for additional function arguments
  # Sets up the regression problem with specified dimensions and number of true signals, all of which have
  # the same amplitude. 
  fdp_vec <- numeric(num_iter)
  pwr_vec <- c(num_iter)
  for (m in 1:num_iter){
    feature_indices <- 1:d
    regression <- get_regression_model(type = type,
                                       n = n, 
                                       d = d, 
                                       a = a, 
                                       num_true_signals = num_true_sigs,
                                       random_true_signal_indices = random_true_signal_indices)
    true_nonzero_indices <- regression$nonzero_indices
    rejected_indices <- method(regression$X, regression$Y, alpha = alpha, ...)
    num_rejections <- length(rejected_indices)
    num_correct_rejections <- length(intersect(rejected_indices, true_nonzero_indices))
    num_false_rejections <- length(intersect(feature_indices[-true_nonzero_indices], rejected_indices))
    fdp_vec[m] <- num_false_rejections / max(num_rejections, 1)
    pwr_vec[m] <- num_correct_rejections / max(length(true_nonzero_indices), 1)
  }
  fdr <- mean(fdp_vec)
  pwr <- mean(pwr_vec)
  return(list(fdr = fdr, power = pwr))
}

# Function for running a simulation on a method in a certain setting over a set of signal amplitudes and/or alpha levels.
method_setting_simulation <- function(method, 
                                      type = 'MCC',
                                      n,
                                      d,
                                      num_true_sigs,
                                      random_true_signal_indices = TRUE,
                                      a_vec = c(2, 4, 6, 8, 10), 
                                      alpha = .05,
                                      num_iter = 500,
                                      ...){
  # method (function) - the multiple testing method to be simulated
  # n (integer) - number of rows in design matrix
  # d (integer) - number of columns in design matrix
  # a (vector[numeric]) - signal amplitudes
  # num_true_sigs - number of true (nonzero) signals in the setting
  # alpha (vector[numeric]) - nominal FDR levels
  # type (char) - the type of design matrix, e.g. MCC, IID_Normal, X_AR
  # random_true_signal_indices (logical) - whether the signal indices are placed at random or are just the first k indices
  # num_iter (integer) - number of iterations to perform the simulation
  # ... - additional arguments to be supplied to the method, e.g. multiplier, moment, dampen. See the methods file for additional function arguments
  # Sets up the regression problem with specified dimensions and number of true signals, all of which have
  # the same amplitude. 

  # Initialize result lists
  result_list <- list()
  
  # Iterate over combinations of a_vec and alpha
  for (amp in c(a_vec)){
    for (alp in c(alpha)){
      result <- method_simulation(method = method,
                                  type = type,
                                  n = n, 
                                  d = d,
                                  a = amp, 
                                  num_true_sigs = num_true_sigs,
                                  random_true_signal_indices = random_true_signal_indices,
                                  alpha = alp,
                                  num_iter = num_iter,
                                  ...
                                  )
      # Store the results in a list
      result_list[[paste("a", amp, "alpha", alp, sep = "_")]] <- data.frame(a = amp, alpha = alp, power = result$power, fdr = result$fdr)
    }
  }
  
  # Combine all results into a single data frame
  result_df <- do.call(rbind, result_list)
  return(result_df)
}




