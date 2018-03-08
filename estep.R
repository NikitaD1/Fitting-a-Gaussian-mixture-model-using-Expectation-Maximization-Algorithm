estep <- function(x,mean, std, weight) {
  i=3
  posterior = matrix(,150,3)
  prior = matrix(,150,3)
  for (k in 1:i) 
    prior[,k] = apply(x, 1, function(x) exp(-(1/2) * (t(x) - mean[k,]) %*% MASS::ginv(std[[k]]) %*% 
                                              t(t(x) - mean[k,]))/sqrt(det(2 * pi * std[[k]])))
  
  for ( k in 1:i)
  posterior[,k] = (weight[k]*prior[,k])/rowSums(t((t(prior)*weight)))
  logpost <- log(posterior)
  loglikelihood <- sum(logpost,2)
  
  list("loglikelihood" = loglikelihood,
       "posterior" = posterior)
}
