library(DependenciesGraphs)
library(simutils)
library(depgraph)
library(devtools)

dep_read_simData <- funDependencies("package:simutils", "read_simData")
plot(dep_read_simData)

dep_updateSimInput <- funDependencies("package:simutils", "updateSimInput")
plot(dep_updateSimInput)


dep_compute_total <- funDependencies("package:simutils", "compute_total")
plot(dep_compute_total)


dep_all <- envirDependencies("package:simutils")
plot(dep_all)


dep_readWKT_as_sf <- funDependencies("package:simutils", 'readWKT_as_sf')
plot(dep_all)

dep_packages <- Pck.load.to.vis("simutils")
plot(dep_packages)



packDep1 <- available.packages()
our_packgs <-  c('rJava', 'xml2', 'purrr', 'configr', 'fs',
'sf', 'rgeos', 'stars', 'readr', 'data.table', 'tibble', 'dplyr')
packDep1 <- packDep1[rownames(packDep1) %in% our_packgs,]
packDep1[, 'Depends'] <- NA
packDep1[, 'Imports'] <- NA
packDep1[, 'LinkingTo'] <- NA

plot_dependency_graph(
  pkg = 'C:/Users/Administrador/Documents/INE.Packages.v2/simutils', 
  option = "cividis",
  suggests = FALSE, enhances = FALSE, availPkgs = packDep1)



