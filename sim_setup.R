## gs = group sample size
main <- function(gs) {
  b0 <- c(3, 5)
  b1 <- c(5, 7)
  sig11 <- 2
  sig12 <- 1
  sig22 <- 2
  theta <- c(0.02, 0.09, 0.18,
             0.39, 0.64, 0.95,
             1.33, 1.81, 2.44,
             3.30, 4.61, 5.59,
             6.72)

  paramNBmat <- expand.grid(b0 = b0, b1 = b1, theta = theta,
                            sig11 = sig11, sig12 = sig12,
                            sig22 = sig22)


  ############################## Negative Binomial  #############################

  execution_time_nb <- system.time({
    resultnb <- generatecompareEstimation(paramNBmat, ns=rep(gs,10),
                                          X=glmmVpc::rgen01(rep(gs,10)),
                                          gen_family = "negative_binomial",
                                          another_family = "tweedie",
                                          gen_link = "log", fit_transform = T,
                                          fit_formula = Feature ~ X + (X|cluster),
                                          vpc_input_values=c(0,1), num_cores=8,
                                          seed = 123)
  })

  plot(resultnb)

  png(paste0(gs,"nb.png"), width = 800, height = 600)
  plot(resultnb)
  dev.off()

  ################################# Tweedie  #####################################

  paramTWmat <- read.csv("tweedie.csv")
  execution_time_tw <- system.time({
    resulttw <- generatecompareEstimation(paramTWmat, ns=rep(gs,10),
                                          X=glmmVpc::rgen01(rep(gs,10)),
                                          gen_family = "tweedie",
                                          another_family = "negative_binomial",
                                          gen_link = "log", fit_transform = T,
                                          fit_formula = Feature ~ X + (X|cluster),
                                          vpc_input_values=c(0,1), num_cores=8,
                                          seed = 123)
  })

  plot(resulttw)

  png(paste0(gs,"tw.png"), width = 800, height = 600)
  plot(resulttw)
  dev.off()

  return(list("nbtime" = execution_time_nb, "twtime" = execution_time_tw))
}

