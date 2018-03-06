estep <- function(x,mean, std, weight) {
  k=3
  posterior = matrix(,150,3)
  prior = matrix(,150,3)
  for (i in 1:k) {
    prior[,k] = apply(x, 1, function(x) exp(-(1/2) * (t(x) - mean[k,]) %*% MASS::ginv(std[[k]]) %*% 
                                              t(t(x) - mean[k,]))/sqrt(det(2 * pi * std[[k]])))
    posterior[,k] = (weight[1]*prior[,1])/rowSums(t((t(prior) * weight)))
    
  }
  logpost <- log(posterior)
  loglikelihood <- sum(logpost,2)
  
  list("loglikelihood" = loglikelihood,
       "posterior" = posterior)
}