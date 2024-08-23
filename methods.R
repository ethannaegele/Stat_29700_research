library(knockoff)
library(pracma)
library(cknockoff)
library(dbh)
library(TreeTools)

sarkar_tang_method1 <- function(X, Y, alpha = .05, multiplier = 1.9){
  d <- ncol(X)
  nu <- nrow(X) - 2 * d # n - 2d assuming n > 2d
  T1 <- get_T_1(X, Y, multiplier = multiplier)
  T2 <- get_T_2(X, Y) # some simulations showed that power is highest when you only use multiplier for T1
  P_t1 <- pf(T1^2, 1, nu, lower.tail = FALSE) # to calculate probabilities with the t^2_n distribution, have to use the F(1, n) distribution
  P_t2 <- pf(T2^2, 1, nu, lower.tail = FALSE)
  P_tilde <- numeric(d)
  for (j in 1:d){
    if (P_t1[j] > sqrt(alpha)){
      P_tilde[j] <- 1
    }
    else{
      P_tilde[j] <- P_t2[j]
    }
  }
  P_tilde_BH <- p.adjust(P_tilde, method = "BH")
  rejected_indices <- which(P_tilde_BH <= sqrt(alpha))
  return(rejected_indices) # the indices of the features that the test concludes are nonzero
  
}

sarkar_tang_method2 <- function(X, Y, alpha = .05, eta = .7, multiplier = 1.9){
  d <- ncol(X)
  nu <- nrow(X) - 2 * d # n - 2d assuming n > 2d
  T1 <- get_T_1(X, Y, multiplier = multiplier)
  T2 <- get_T_2(X, Y)
  P_t1 <- pf(T1^2, 1, nu, lower.tail = FALSE) # to calculate probabilities with the t^2_n distribution, have to use the F(1, n) distribution
  P_t2 <- pf(T2^2, 1, nu, lower.tail = FALSE)
  pi_0_hat <- (d - sum(as.numeric(P_t2 <= eta)) + 1) / (d * (1 - eta)) # Storey-type null proportion estimate in Step 2
  P_star <- numeric(d)
  for (j in 1:d){
    if (P_t1[j] > sqrt(alpha)){
      P_star[j] <- 1
    }
    else{
      P_star[j] <- P_t2[j] * pi_0_hat
    }
  }
  P_star_BH <- p.adjust(P_star, method = "BH")
  rejected_indices <- which(P_star_BH <= sqrt(alpha))
  return(rejected_indices) # the indices of the features that the test concludes are nonzero
  
}

sarkar_tang_method1_sigma <- function(X, Y, alpha = .05, sigma = 1, multiplier = 1.9){
  # In this setting, sigma is known
  
  d <- ncol(X)
  sigma_mat <- t(X) %*% X
  equi <- create_equicorrelated_mult(X, multiplier = multiplier) # USE THE MODIFIED FUNCTION FOR BETTER MATRIX CONDITION
  D1 <- equi$D
  X_ko1 <- equi$Xk
  two_sigma_minus_D_inv <- solve(2 * sigma_mat - D1)
  beta_hat_1 <- two_sigma_minus_D_inv %*% t(X + X_ko1) %*% Y
  
  D2 <- diag(create.solve_equi(t(X) %*% X))
  X_ko2 <- create.fixed(X, method = 'equi')$Xk
  beta_hat_2 <- solve(D2) %*% t(X - X_ko2) %*% Y # as defined in section 2 of sarkar and tang
  
  beta_hat_1_sds <- sqrt(diag(2 * sigma^2 * two_sigma_minus_D_inv)) # standard deviation of the beta_1 coefficient estimates
  beta_hat_2_sds <- sqrt(diag(2 * sigma^2 * solve(D2))) # standard deviation of the beta_2 coefficient estimates
  z_scores_1 <- beta_hat_1 / beta_hat_1_sds
  z_scores_2 <- beta_hat_2 / beta_hat_2_sds
  P_t1 <- 2 * (1 - pnorm(abs(z_scores_1)))
  P_t2 <- 2 * (1 - pnorm(abs(z_scores_2)))
  
  P_tilde <- numeric(d)
  for (j in 1:d){
    if (P_t1[j] > sqrt(alpha)){
      P_tilde[j] <- 1
    }
    else{
      P_tilde[j] <- P_t2[j]
    }
  }
  P_tilde_BH <- p.adjust(P_tilde, method = "BH")
  rejected_indices <- which(P_tilde_BH <= sqrt(alpha))
  return(rejected_indices) # the indices of the features that the test concludes are nonzero
  
}

knockoff_procedure <- function(X, Y, alpha = .05, ko_method = 'equi'){
  return(knockoff.filter(X, 
                         as.vector(Y), 
                         knockoffs = function(Z) create.fixed(Z, method = ko_method), 
                         statistic = stat.lasso_lambdasmax,
                         fdr = alpha,
                         offset = 1)$selected) # offset 1 to run knockoff+
  
}

independent_BH <- function(X, Y, alpha = .05){
  nu <- nrow(X) - 2 * ncol(X)
  T2 <- get_T_2(X, Y)
  P <- pf(T2^2, 1, nu, lower.tail = FALSE)
  P_BH <- p.adjust(P, method = "BH")
  rejected_indices <- which(P_BH <= alpha)
  return(rejected_indices) # the indices of the features that the test concludes are nonzero
}


ep_moment_method_sigma <- function(X, Y, alpha = .05, sigma = 1, moment = 2, dampen = FALSE, multiplier = 1.9){
  # In this setting, sigma is known
  # check that moment is an even integer
  if (!is.numeric(moment) | moment %% 2 != 0 | moment <= 0) {
    stop("Moment must be a positive even integer.")
  }
  
  standard_gaussian_moment <- DoubleFactorial(moment - 1) # formula for the even moments of standard Gaussian
  
  sigma_mat <- t(X) %*% X
  equi <- create_equicorrelated_mult(X, multiplier = multiplier) # USE THE MODIFIED FUNCTION FOR BETTER MATRIX CONDITION
  D1 <- equi$D
  X_ko1 <- equi$Xk
  two_sigma_minus_D_inv <- solve(2 * sigma_mat - D1)
  beta_hat_1 <- two_sigma_minus_D_inv %*% t(X + X_ko1) %*% Y
  
  D2 <- diag(create.solve_equi(t(X) %*% X))
  X_ko2 <- create.fixed(X, method = 'equi')$Xk
  beta_hat_2 <- solve(D2) %*% t(X - X_ko2) %*% Y # as defined in section 2 of sarkar and tang
  
  beta_hat_1_sds <- sqrt(diag(2 * sigma^2 * two_sigma_minus_D_inv)) # standard deviation of the beta_1 coefficient estimates
  beta_hat_2_sds <- sqrt(diag(2 * sigma^2 * solve(D2))) # standard deviation of the beta_2 coefficient estimates
  z_scores <- beta_hat_2 / beta_hat_2_sds
  p_values <- 2 * (1 - pnorm(abs(z_scores)))
  
  e_values <- (beta_hat_1 / beta_hat_1_sds)^moment / standard_gaussian_moment
  if (dampen){
    e_values <- (1/2) + (1/2) * e_values # trick for dampening the volatility of the e values
  }
  rejected_indices <- ep_BH(p_values = p_values, e_values = e_values, alpha = alpha)
  
  return(rejected_indices)
  
}

# This function is the primary approach to the moment method, which uses the F distribution, and uses 
# valid p values and e values. The p values are not independent, but since the p values are approximately equal to 
# p values one would get from the known sigma case, they are "approximately independent" 
# and empirically, the method controls the FDR in the cases I have examined.
ep_moment_method <- function(X, Y, alpha = .05, moment = 2, sigma = NULL, dampen = FALSE, multiplier = 1.9){
  # check that moment is an even integer
  if (!is.numeric(moment) | moment %% 2 != 0 | moment <= 0) {
    stop("Moment must be a positive even integer.")
  }
  
  if (is.null(sigma)){
    # the unknown variance version of the method
    nu <- nrow(X) - 2 * ncol(X)
    
    if (moment == 2){
      distribution_moment <- nu / (nu - 2) # Mean of the F(1, nu) distribution. Avoids computing the gamma function if n is large, which would otherwise break the function
    }
    else{
      distribution_moment <- f_distribution_kth_moment(1, nu, k = moment / 2) # not viable if n is large, since this computes the gamma function
    }
    
    
    sigma_mat <- t(X) %*% X
    equi <- create_equicorrelated_mult(X, multiplier = multiplier) # USE THE MODIFIED FUNCTION FOR BETTER MATRIX CONDITION
    D <- equi$D
    X_ko <- equi$Xk
    two_sigma_minus_D_inv <- solve(2 * sigma_mat - D)
    
    beta_hat_1 <- two_sigma_minus_D_inv %*% t(X + X_ko) %*% Y # as defined in section 2 of sarkar and tang
    
    sigma_hat <- get_variance_parameter_estimate(cbind(X, X_ko), Y)
    
    T2 <- get_T_2(X, Y)
    P_t2 <- pf(T2^2, 1, nu, lower.tail = FALSE) # to calculate probabilities with the t^2_n distribution, have to use the F(1, n) distribution
    
    beta_hat_1_sds <- sqrt(diag(2 * sigma_hat^2 * two_sigma_minus_D_inv))
    e_values <- (beta_hat_1 / beta_hat_1_sds)^moment / distribution_moment
    #e_values <- ((nu - 2) / nu) * (beta_hat_1^2 / beta_hat_1_variances)
    if (dampen){
      e_values <- (1/2) + (1/2) * e_values # trick for dampening the volatility of the e values
    }
    
    rejected_indices <- ep_BH(p_values = P_t2, e_values = e_values, alpha = alpha)
  }
  
  else if (sigma <= 0 | !is.numeric(sigma)){
    stop("Variance parameter sigma must be positive.")
  }
  else{
    rejected_indices <- ep_moment_method_sigma(X, Y, 
                                                  alpha = alpha, 
                                                  sigma = sigma, 
                                                  moment = moment, 
                                                  dampen = dampen, 
                                                  multiplier = multiplier)
  }
  return(rejected_indices)
  
}


ep_moment_method_alternate <- function(X, Y, alpha = .05, moment = 2, dampen = FALSE, multiplier = 1.9){
  # this function is an attempt at an alternate approach to the moment method, where
  # we instead estimate sigma with sigma_hat and use the method for when sigma is known, assuming that
  # sigma_hat is equal to sigma.
  X_ko <-  create_equicorrelated_mult(X, multiplier = multiplier)$Xk
  sigma_hat <- get_variance_parameter_estimate(cbind(X, X_ko), Y)
  rejected_indices <- ep_moment_method_sigma(X, Y, 
                                                alpha = alpha, 
                                                sigma = sigma_hat, 
                                                moment = moment, 
                                                dampen = dampen,
                                                multiplier = multiplier)
  return(rejected_indices)
}

ep_BH <- function(p_values, e_values, alpha = .05){
  # p_values is a vector of p values
  # e_values is a vector of e values
  # p_values and e_values must be of the same length.
  # Weights each p value by its corresponding e value and then runs BH on the weighted p values at level alpha.
  Q <- pmin(p_values / e_values, 1)
  Q_BH <- p.adjust(Q, method = 'BH')
  rejected_indices <- which(Q_BH <= alpha)
  return(rejected_indices)
}

# making an ep-BH function with arbitrary cutoff, to test the effect of cutoffs better
ep_sarkar_tang_method1_cutoff <- function(X, Y, alpha = .05, cutoff = .2, multiplier = 1.9){
  d <- ncol(X)
  nu <- nrow(X) - 2 * d # n - 2d assuming n > 2d
  T1 <- get_T_1(X, Y, multiplier = multiplier)
  T2 <- get_T_2(X, Y)
  P_t1 <- pf(T1^2, 1, nu, lower.tail = FALSE) # to calculate probabilities with the t^2_n distribution, have to use the F(1, n) distribution
  P_t2 <- pf(T2^2, 1, nu, lower.tail = FALSE)
  E <- as.numeric(P_t1 <= cutoff) / cutoff # vector of e values
  rejected_indices <- ep_BH(P_t2, E, alpha = alpha) # run ep BH at level alpha
  return(rejected_indices)
}

bh <- function(p_values, alpha = .05){
  Q_BH <- p.adjust(p_values, method = 'BH')
  rejected_indices <- which(Q_BH <= alpha)
  return(rejected_indices)
}

bh_regression <- function(X, Y, alpha = .05){
  sigma_hat <- get_variance_parameter_estimate(X, Y)
  model <- lm(Y ~ 0 + ., data=as.data.frame(X))
  beta_hat <- coef(model)
  sigma_mat_inv <- solve(t(X) %*% X)
  beta_hat_sds <- sqrt(diag(sigma_hat^2 * sigma_mat_inv))
  t_statistics <- beta_hat / beta_hat_sds
  p_values <- 2 * (1 - pt(abs(t_statistics), df = nrow(X) - ncol(X)))
  rejected_indices <- bh(p_values = p_values, alpha = alpha)
  return(rejected_indices)
}

cknockoff_procedure <- function(X, Y, alpha = .05){
  cknockoff(X, 
            Y,
            intercept = FALSE,
            alpha = alpha)$selected
}


dbh_procedure <- function(X, Y, alpha = .05){
  dBH_lm(Y, 
         X, 
         intercept = FALSE, 
         side = 'two', 
         alpha = alpha, 
         avals_type = 'BH')$initrejs
}
