theta <- 0.65
Sigma <- matrix(c(2,1,1,2),2)
beta <- c(3, 5)
ns <- rep(10,10)
X <- glmmVpc::rgen01(ns)
num <- 100
family <- "negative_binomial"
link <- "log"
formula <- Feature ~ X+(X|cluster)
phi <- 5
data <- glmmVpc::batchGLMMData(beta=beta,ns=ns,Sigma=Sigma,num=num,X=X,
                               family=family,link=link, theta=theta)
fits <- glmmVpc::batchGLMMFit(formula=formula, data=data, family=family)
vpcs <- glmmVpc::vpc(fits,x=1)
true_vpc <- calculate_vpc_for_family(family, list(phi=theta,Sigma=Sigma,beta=beta), x=1)
ci <- confint(vpcs,type="bootstrap")

confint_analysis <- function(vpcs, true_vpc, verbose=FALSE) {
  ci <- confint(vpcs, verbose=verbose)
  result <- sapply(1:nrow(ci), function(i) {
    lower <- as.numeric(ci[i,"Lower"])
    upper <- as.numeric(ci[i,"Upper"])
    contains_trueVpc <- (true_vpc >= lower) & (true_vpc <= upper)
    in_range <- is.logical((lower >= 0 & lower <= 1) &
                             (upper >= 0 & upper <= 1))
    cov_width <- upper - lower
    return(c(contains_trueVpc, in_range, cov_width))
  })
  bounds_true <- as.logical(result[1,])
  in_range <- as.logical(result[2,])
  cov_width <- result[3,]
  return(list(trueVpc=true_vpc,
              result=cbind(ci,
                           bounds_true=bounds_true,
                           in_range=in_range,
                           cov_width=cov_width)
  )
  )
}
