if (!requireNamespace("SimulationStudyVPC", quietly = TRUE)) {
  if (!requireNamespace("remotes", quietly = TRUE)) {
    install.packages("remotes")
  }
  remotes::install_github("juliusolaifa/SimulationStudyVPC", upgrade = "never",
                          force=TRUE)
}

library(SimulationStudyVPC)


source(file.path(getwd(), "sim_setup.R"))


#There are 10 groups(clusters, starains)
#gs is group sample size or strain sample size
main(gs=10)
