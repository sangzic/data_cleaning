Whiten <- function(input.data, transform.data) {
  # Whitens data based on transformation from input data
  # Args:
  #   input.data: data used to make whitening transformation
  #   transform.data: data to be transformed.
  # Returns:
  #   transformed data with reduced correlation
  # Get eigenvalues and eigenvectors
  cov.matrix <- cov(input.data)
  eig <- eigen(cov_CV1)
  #Get the data transformation
  sigma_tsp <- t(eig_CV1$vectors)
  lambda_root <- solve(diag(eig$values) ^(0.5))
  # transform the data
  trans.data <- t(lambda_root %*% sigma_tsp %*% t(transform.data))
  trans.data
}

