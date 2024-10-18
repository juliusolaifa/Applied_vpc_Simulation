if (!requireNamespace("SimulationStudyVPC", quietly = TRUE)) {
  if (!requireNamespace("remotes", quietly = TRUE)) {
    install.packages("remotes")
  }
  remotes::install_github("juliusolaifa/SimulationStudyVPC", upgrade = "never",
                          force=TRUE)
}

library(SimulationStudyVPC)
source(file.path(getwd(), "sim_setup.R"))


#There are 10 groups(clusters, strains)
#gs is group sample size or strain sample size

# Caompare models
main_cm(gs=10)

# Compare groups
#main_cg(gs=10)

# Compare points
main_pt(gs=10)
