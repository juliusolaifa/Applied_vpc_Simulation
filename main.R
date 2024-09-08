if (!requireNamespace("SimulationStudyVPC", quietly = TRUE)) {
  if (!requireNamespace("remotes", quietly = TRUE)) {
    install.packages("remotes")
  }
  remotes::install_github("juliusolaifa/SimulationStudyVPC", upgrade = "never",
                          force=TRUE)
}

library(SimulationStudyVPC)


est_sim <- function(b0, b1, sigmas,
                    phis, power, ns,
                    covariates, formula,
                    true_fam, false_fam,
                    iter, vpc_func, seed=NULL,
                    output_dir=NULL,
                    plot_width = 800,
                    plot_height = 800,...) {

    X <- vpc::rgen01(ns)
    args <- list(...)

    params <- SimulationStudyVPC::paramgridWithVPC(b0, b1, sigmas, phis,
                                                   power=power,
                                                   vpc_func=vpc_func)

    result <- SimulationStudyVPC::compareGLMMFit(params=params,
                                                  covariates=covariates,
                                                  X=X, ns=ns,
                                                   true_family=true_fam,
                                                   false_family=false_fam,
                                                   formula=formula, iter=iter,
                                                   seed = seed)

    plot <- SimulationStudyVPC::percomPlot(params,result$coefs_true, result$coefs_false,
                                   result$coefs_vst)
    if (!is.null(output_dir)) {
      if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
      }
      write.csv(params, paste0(output_dir,"/true.csv"))
      for (name in names(result)) {
        file_name <- paste0(output_dir, "/", name, ".csv")
        write.csv(result[[name]], file_name)
      }
      png(paste0(output_dir,"/plot.png"), width = plot_width,
          height = plot_height)
      SimulationStudyVPC::percomPlot(params, result$coefs_true,
                                     result$coefs_false, result$coefs_vst)
      dev.off()
    }

    return(result)
}

out <- est_sim(b0=c(3,5),
        b1=c(5,7),
        sigmas=list(
                  sig1 = c(2,1,2)#,
                  # sig2 = c(2,-1,2),
                  # sig3 = c(2,0,2),
                  # sig4 = c(2,1,4),
                  # sig5 = c(2,-1,4),
                  # sig6 = c(2,0,4)
        ),
        phis=c(seq(0.01,0.4,0.02)),
               # seq(0.41,1,0.03),
               # seq(1.1,2,0.05),
               # seq(2.1,3,0.1),
               # seq(3,5,0.2),
               # seq(5.1,13,0.5),
               # seq(13.5,40,5)),
        power=NULL,
        ns=rep(10,10),
        covariates=c(0,1),
        formula=y ~ x + (1 + x | group),
        true_fam="nb",
        false_fam="tw",
        iter=1, #seed=123,
        output_dir = "./Applied_vpc_Simulation/result",
        vpc_func=vpc::vpc.nb)
