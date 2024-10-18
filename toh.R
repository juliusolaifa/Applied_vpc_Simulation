beta <- c(3,5)
ns <- rep(6,10)
X <- glmmVpc::rgen01(ns)
Sigma <- matrix(c(2,1,1,2),2)
covariate <- 0
family <- "negative_binomial"
link <- "log"
seed <- 123
theta <- 0.5
num <- 1000
num_cores <- 4
alpha <- 0.05
type="all"
null <- Feature ~ X
alt <- Feature ~ X + (X | cluster)


#######    Power Analysis ########


result <- testVpc( null=null, alt=alt, beta=beta, ns=ns, Sigma = Sigma,
                   alpha = alpha, num = num, X = X, covariate = covariate,
                   family = family,link = link, num_cores=num_cores,
                   type=type,seed = seed,theta=theta)

result <- result[, !names(result) %in% "LR_stat"] < alpha
print(colMeans(result, na.rm = T))



#######   Type I error Analysis  ######

result <- testVpc( null=null, alt=alt, beta=beta, ns=ns,
                   alpha = alpha, num = num, X = X, covariate = covariate,
                   family = family,link = link, num_cores=num_cores,
                   type=type,seed = seed,theta=theta)

result <- result[, !names(result) %in% "LR_stat"] < alpha
print(colMeans(result, na.rm = T))
