mstep <- function(x, posterior) {
  
  mc <- colSums(posterior)
  k=3
  mean = matrix(,3,4) 
  
  m= matrix(,3,3)
  cov = list(m,m,m)
  for ( i in 1:k) {
    
    mean[k, ]= colSums(iris[, 1:4] * posterior[, k]) * 1/mc[k]
    
    cov[[k]] <- t(posterior[, k] * t(apply(iris[, 1:4], 1, function(x) x - mean[k, ]))) %*% 
      (posterior[, k] * t(apply(iris[, 1:4], 1, function(x) x - mean[k, ]))) * 1/mc[k]
    
  }
  weight <- mc / length(x)
  
  list("mean" = mean,
       "cov" = cov,
       "clusterweigths" = weight)
}