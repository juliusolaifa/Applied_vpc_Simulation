b0 <- c(3, 5)
b1 <- c(5, 7)
sig11 <- 2
sig12 <- c(-1,0,1)
sig22 <- 2
theta <- c(0.02, 0.09, 0.18,
           0.39, 0.64, 0.95,
           1.33, 1.81, 2.44,
           3.30, 4.61, 5.59,
           6.72, 40)

paramNBmat <- expand.grid(b0 = b0, b1 = b1, theta = theta,
                          sig11 = sig11, sig12 = sig12,
                          sig22 = sig22)


###############################  COMPARE MODELS  ###############################

main_cm <- function(gs) {

  ############################# Negative Binomial  ##########################

  execution_time_nb <- system.time({
    resultnb <- generatecompareEstimation(paramNBmat, ns=rep(gs,10),
                                          X=glmmVpc::rgen01(rep(gs,10)),
                                          gen_family = "negative_binomial",
                                          another_family = "tweedie",iter=1,
                                          gen_link = "log", fit_transform = T,
                                          fit_formula = Feature ~ X + (X|cluster),
                                          vpc_input_values=c(0,1), num_cores=4,
                                          seed = 123)
  })

  plot(resultnb)

  png(paste0(gs,"nb.png"), width = 800, height = 600)
  plot(resultnb)
  dev.off()

  ################################# Tweedie  #################################

  paramTWmat <- read.csv("tweedie.csv")
    resulttw <- generatecompareEstimation(paramTWmat, ns=rep(gs,10),
                                          X=glmmVpc::rgen01(rep(gs,10)),
                                          gen_family = "tweedie",iter=1,
                                          another_family = "negative_binomial",
                                          gen_link = "log", fit_transform = T,
                                          fit_formula = Feature ~ X + (X|cluster),
                                          vpc_input_values=c(0,1), num_cores=4,
                                          seed = 123)

  plot(resulttw)

  png(paste0(gs,"tw.png"), width = 800, height = 600)
  plot(resulttw)
  dev.off()

  return(list("nb" = resultnb, "tw" = resulttw))
}


###############################  COMPARE GROUPS  ###############################

main_cg <- function(gs) {
  result <- generatecompareGroups(paramNBmat, ns= rep(gs,10),
                      family="negative_binomial", link= "log",
                      formula= Feature ~ X + (X|cluster),
                      X = glmmVpc::rgen01(rep(gs,10)),
                      vpc_input_values = c(0,1),num_cores=4,
                      seed = 123)
  plot(result)

  png("cg_nb.png", width = 800, height = 600)
  plot(result)
  dev.off()
  return(result)
}


##############################  COMPARE POINTS  ###############################

main_pt <- function(gs) {
  b0 <- 2
  b1 <- 3
  sig11 <- 2
  sig12 <- 1
  sig22 <- 2
  theta <- c(0.11, 0.25, 0.43,
             0.67, 1.00, 1.50,
             2.34, 4.02, 9.05)

  params <- expand.grid(b0 = b0, b1 = b1, theta = theta,
                       sig11 = sig11, sig12 = sig12,
                       sig22 = sig22)

  result <- comparePoints(params=params, ns=rep(gs,10),
                          X=glmmVpc::rgen01(rep(gs,10)),
                          family = "negative_binomial",
                          link="log", formula=Feature~X+(X|cluster),
                          vpc_input_values=c(0,1), iter=100,
                          seed = 123, num_cores=4)
  boxplot(result)
  customboxplot(result)

  png("pt_nb.png", width = 800, height = 800)
  boxplot(result)
  dev.off()

  return(result)
}

