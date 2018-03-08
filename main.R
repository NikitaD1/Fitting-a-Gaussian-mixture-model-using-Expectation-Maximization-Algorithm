for (i in 1:50) {
  
  iris.kmeans = kmeans(iris[1:4],3)
  cluster = iris.kmeans$cluster
  
  Irisdataclusters <- split(iris[, 1:4], cluster)
  clustermean <- t(sapply(Irisdataclusters, colMeans))
  clusterweights = c(1/3, 1/3, 1/3)
  clustervariance = list(diag(4), diag(4), diag(4))
  if (i == 1) {
    # Initialization
    e.step <- estep (iris[,1:4], clustermean, clustervariance,
                     clusterweights)
    m.step <- mstep(iris[,1:4], e.step[["posterior"]])
    cur.loglik <- e.step[["loglikelihood"]]
    
  } 
  
  else {
    # Repeat E and M steps till convergence
    e.step <- estep(iris[,1:4], m.step[["mean"]], m.step[["cov"]], m.step[["clusterweigths"]])
    m.step <- mstep(iris[,1:4], e.step[["posterior"]])
    
    loglik.diff <- abs((cur.loglik - e.step[["loglikelihood"]]))
    
    if(loglik.diff < 1e-6) 
      break
    cur.loglik <- e.step[["loglikelihood"]]
    }
  }

