library(knockoff)
library(pracma)

# Fast versions of diag(d) %*% X and X %*% diag(d).
`%diag*%` <- function(d, X) d * X
`%*diag%` <- function(X, d) t(t(X) * d)

is_posdef = function(A, tol=1e-9) {
  p = nrow(matrix(A))
  
  if (p<500) {
    lambda_min = min(eigen(A)$values)
  }
  else {
    oldw <- getOption("warn")
    options(warn = -1)
    lambda_min = RSpectra::eigs(A, 1, which="SM", opts=list(retvec = FALSE, maxitr=100, tol))$values
    options(warn = oldw)
    if( length(lambda_min)==0 ) {
      # RSpectra::eigs did not converge. Using eigen instead."
      lambda_min = min(eigen(A)$values)
    }
  }
  return (lambda_min>tol*10)
}

# Scale the columns of a matrix to have unit norm.
normc = function(X,center=T) {
  X.centered = scale(X, center=center, scale=F)
  X.scaled = scale(X.centered, center=F, scale=sqrt(colSums(X.centered^2)))
  X.scaled[,] # No attributes
}


# Reduced SVD with canonical sign choice.
# Our convention is that the sign of each vector in U is chosen such that the
# coefficient with the largest absolute value is positive.
canonical_svd = function(X) {
  X.svd = tryCatch({
    svd(X)
  }, warning = function(w){}, error = function(e) {
    stop("SVD failed in the creation of fixed-design knockoffs. Try upgrading R to version >= 3.3.0")
  }, finally = {})
  
  for (j in 1:min(dim(X))) {
    i = which.max(abs(X.svd$u[,j]))
    if (X.svd$u[i,j] < 0) {
      X.svd$u[,j] = -X.svd$u[,j]
      X.svd$v[,j] = -X.svd$v[,j]
    }
  }
  return(X.svd)
}

#' Compute the SVD of X and construct an orthogonal matrix U_perp such that U_perp * U = 0.
#'  
decompose <- function(X, randomize=FALSE) {
  n = nrow(X); p = ncol(X)
  stopifnot(n >= 2*p)
  
  result = canonical_svd(X)
  Q = qr.Q(qr(cbind(result$u, matrix(0,n,p))))
  u_perp = Q[,(p+1):(2*p)]
  if (randomize) {
    Q = qr.Q(qr(rnorm_matrix(p,p)))
    u_perp = u_perp %*% Q
  }
  result$u_perp = u_perp
  result
}

# Modified function - Equicorrelated with multiplier to try and have a better conditioned matrix
create_equicorrelated_mult <- function(X, multiplier=2) {
  # Compute SVD and U_perp.
  X.svd = decompose(X)
  if (any(X.svd$d <= 1e-5 * max(X.svd$d)))
    stop(paste('Data matrix is rank deficient.',
               'Equicorrelated knockoffs will have no power.'))
  lambda_min = min(X.svd$d)^2
  s = min(multiplier*lambda_min, 1)
  D <- diag(rep(s, times = ncol(X))) # I am constructing the diagonal matrix from the obtained s, as required in the formulas
  
  # Construct the knockoff according to Equation 1.4.
  s_diff = pmax(0, 2*s - (s/X.svd$d)^2) # can be negative due to numerical error
  X_ko = (X.svd$u %*diag% (X.svd$d - s / X.svd$d) +
            X.svd$u_perp %*diag% sqrt(s_diff)) %*% t(X.svd$v)
  list(D = D, Xk = X_ko)
}

# faster function for calculating norm of vector
norm_vec <- function(x) sqrt(sum(x^2))

# obtain the unbiased estimator of the variance parameter tau in the regression setting Y = N(X * beta, tau^2 I_n)
get_variance_parameter_estimate <- function(X, Y){
  n <- nrow(X)
  d <- ncol(X)
  # Ensure Y is a vector and has appropriate length
  #if (!is.vector(Y) || length(Y) != nrow(X)) {
  # stop("Y must be a vector and match the number of rows in X")
  # }
  
  # Convert X to a data frame
  df_X <- as.data.frame(X)
  
  # Add Y to the dataframe
  df_X$Y <- Y
  
  # Fit the model using the formula
  model <- lm(Y ~ 0 + ., data=df_X)
  residuals_sum_sq <- deviance(model)
  return(sqrt(residuals_sum_sq / (n - d)))
}

# get the T_1  estimate provided at the beginning of section 2 of Sarkar and Tang's paper
# SQRTM USES THE PRACMA VERSION HERE CURRENTLY
get_T_1 <- function(X, Y, multiplier = 1.9){
  sigma <- t(X) %*% X
  equi <- create_equicorrelated_mult(X, multiplier = multiplier) # TRYING TO USE THE MODIFIED FUNCTION FOR BETTER MATRIX CONDITION
  D <- equi$D # need to turn the output into a diagonal matrix
  X_ko <- equi$Xk
  
  beta_hat_1 <- solve(2 * sigma - D) %*% t(X + X_ko) %*% Y
  # tau hat estimated with the columnwise concatenation of X and knockoff X
  T_1 <- (1 / (get_variance_parameter_estimate(cbind(X, X_ko), Y) * sqrt(2))) *
    sqrtm(diag(diag(solve(2 * sigma - D))))$Binv %*% beta_hat_1
  return(T_1)
}

# get the T_2 estimate from Sarkar and Tang's paper
get_T_2 <- function(X, Y){
  D <- diag(create.solve_equi(t(X) %*% X)) # note: D calculated in slightly different ways for T1 and T2; simulations show power is slightly higher through this approach
  X_ko <- create.fixed(X, method = 'equi')$Xk
  beta_hat_2 <- solve(D) %*% t(X - X_ko) %*% Y
  T_2 <- (1 / (get_variance_parameter_estimate(cbind(X, X_ko), Y) * sqrt(2))) * sqrtm(D)$B %*% beta_hat_2
  return(T_2)
}

# generate a regression model with autoregressive design matrix X as described in Sarkar and Tang
get_X_AR_regression_model <- function(n, d, a, num_true_signals, random_true_signal_indices = FALSE, sigma = 1){
  # random_true_signal_indices selects which indices will be true signals randomly
  # n = sample size, ie number of rows in the matrix
  # d = number of features
  # a = signal strength
  # num_true_signals = number of nonzero beta coefficients
  # Returns an X matrix, response Y, and the positions of the true signals, as according to the simulation setup described in the beginning of Section 3 of Sarkar and Tang
  rho <- 0.5
  cor_matrix <- matrix(nrow = d, ncol = d)
  for (i in 1:d) {
    for (j in 1:d) {
      cor_matrix[i, j] <- rho^abs(i - j)
    }
  }
  
  # Perform Cholesky decomposition of the correlation matrix
  cholesky_factor <- chol(cor_matrix)
  
  # Generate independent standard normal variables
  random_normals <- matrix(rnorm(n * d), nrow = n, ncol = d)
  
  # Transform the independent normals using the Cholesky factor to get correlated normals
  X <- random_normals %*% cholesky_factor
  
  X <- normc(X, center = F) # X must be normalized to get sensible values from the knockoff functions. Here using the normalizing function used internally in the package
  
  if (random_true_signal_indices){
    nonzero_indices <- sample(d, num_true_signals) # randomly sample some indices to be the true signal indices
    beta_true <- a * (1:d %in% nonzero_indices) # all the other positions in the vector will be 0
  }
  else{
    nonzero_indices <- 1:num_true_signals
    beta_true <- c(rep(a, times = num_true_signals), rep(0, times = d - num_true_signals))
  }
  
  Y <- X %*% beta_true + sigma * rnorm(n) # linear regression model
  
  return(list('X' = X, 'Y' = Y, 'nonzero_indices' = nonzero_indices))
}

# generate design matrix randomly
gene_X <- function(X_type = "IID_Normal", n, p, X_seed = NULL){
  if(!is.null(X_seed)){
    set.seed(X_seed)
  }
  
  #X_type <- str_split(X_type, pattern = "_D_")[[1]][1]
  
  model_X <- F # set False if experiment with fixed-X
  if(model_X){
    basis <- matrix(rnorm(n*p), n)
  } else{
    basis <- qr.Q(qr(matrix(rnorm(n*p), n)))
  }
  
  cor_radius <- 5
  if(X_type == "IID_Normal"){
    cov_mat <- diag(p)
    X <- matrix(rnorm(n*p), n)
  } else if(X_type == "Coef_AR"){
    rho <- 0.5
    
    cov_mat <- solve(rho^(abs(outer(1:p, 1:p, "-"))))
    normalizer <- diag(1 / sqrt(diag(cov_mat)))
    cov_mat <- normalizer %*% cov_mat %*% normalizer
    
    R <- chol(cov_mat)
    X <- basis %*% R
  } else if(X_type == "X_AR"){
    rho <- 0.5
    
    cov_mat <- rho^(abs(outer(1:p, 1:p, "-")))
    normalizer <- diag(1 / sqrt(diag(cov_mat)))
    cov_mat <- normalizer %*% cov_mat %*% normalizer
    
    R <- chol(cov_mat)
    X <- basis %*% R
  } else if(X_type == "Homo_Block"){
    rho <- 0.5
    block_size <- 10
    
    blockSigma <- matrix(rho, block_size, block_size)
    diag(blockSigma) <- 1
    
    cov_mat <- as.matrix(diag(p / block_size) %x% blockSigma)
    normalizer <- diag(1 / sqrt(diag(cov_mat)))
    cov_mat <- normalizer %*% cov_mat %*% normalizer
    
    R <- chol(cov_mat)
    X <- basis %*% R
  } else if(X_type == "MCC"){
    if(n %% (p+1) == 0){
      X <- lapply(1:(n/(p+1)), function(i){
        rbind(diag(rep(1, p)), rep(0, p))
      })
      X <- do.call(rbind, X)
      X <- scale(X, center = T, scale = F)
      X <- scale(X, center = F, scale = sqrt(colSums(X^2)))
    } else{
      cov_mat <- matrix(-1/p, p, p)
      diag(cov_mat) <- 1
      
      R <- chol(cov_mat)
      X <- basis %*% R
    }
  } else if(X_type == "MCC_Block"){
    block_size <- 5
    
    blockSigma <- matrix(-1/block_size, block_size, block_size)
    diag(blockSigma) <- 1
    
    cov_mat <- as.matrix(diag(p / block_size) %x% blockSigma)
    
    R <- chol(cov_mat)
    X <- basis %*% R
  } else if(X_type == "Sparse"){
    sparsity <- 0.01
    X <- diag(1, nrow = n, ncol = p)
    lower_tri <- lower.tri(X)
    X[lower_tri] <- replicate(sum(lower_tri), rbinom(1, 1, sparsity))
  }
  # X <- scale(X, center = FALSE, scale = sqrt(colSums(X^2)))
  if(!exists("cov_mat")) cov_mat <- NA
  
  return(list(X = X, Xcov.true = cov_mat))
}

get_regression_model <- function(type = 'MCC', n, d, a, num_true_signals, random_true_signal_indices = TRUE, sigma = 1){
  if (type %in% c('IID_Normal', 'Coef_AR' ,'X_AR', 'Homo_Block', 'MCC', 'MCC_Block', 'Sparse')){
    if (type == 'X_AR'){
      return(get_X_AR_regression_model(n = n, d = d, a = a, 
                                       num_true_signals = num_true_signals, 
                                       random_true_signal_indices = random_true_signal_indices,
                                       sigma = sigma))
    }
    X <- gene_X(X_type = type, n = n, p = d)$X
    if (random_true_signal_indices){
      nonzero_indices <- sort(sample(d, num_true_signals)) # randomly sample some indices to be the true signal indices
      beta_true <- a * (1:d %in% nonzero_indices) # all the other positions in the vector will be 0
    }
    else{
      nonzero_indices <- 1:num_true_signals
      beta_true <- c(rep(a, times = num_true_signals), rep(0, times = d - num_true_signals))
    }
    
    Y <- X %*% beta_true + sigma * rnorm(n) # linear regression model
    
    return(list('X' = X, 'Y' = Y, 'nonzero_indices' = nonzero_indices))
  }
  else {
    stop("Type should be 'IID_Normal', 'Coef_AR' ,'X_AR', 'Homo_Block', 'MCC', 'MCC_Block', or 'Sparse'.")
  }
}

# formula for calculating the kth moment of the F(d1, d2) distribution
f_distribution_kth_moment <- function(d1, d2, k) {
  # Calculate the terms of the formula
  term1 <- (d2 / d1) ^ k
  term2 <- gamma((d1 / 2) + k)
  term3 <- gamma((d2 / 2) - k)
  term4 <- gamma(d1 / 2)
  term5 <- gamma(d2 / 2)
  
  # Calculate the final value
  result <- term1 * term2 * term3 / (term4 * term5)
  
  return(result)
}

