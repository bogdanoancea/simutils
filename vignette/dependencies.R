library(DependenciesGraphs)
library(simutils)


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
