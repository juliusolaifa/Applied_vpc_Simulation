if (!requireNamespace("SimulationStudyVPC", quietly = TRUE)) {
  if (!requireNamespace("remotes", quietly = TRUE)) {
    install.packages("remotes")
  }
  remotes::install_github("juliusolaifa/SimulationStudyVPC", upgrade = "never",
                          force=TRUE)
}

library(SimulationStudyVPC)


source(file.path(getwd(), "sim_setup.R"))


main(10)
