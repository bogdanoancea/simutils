#' Create SDM (Signal Dominance Measure) from the SimData object.
#'
#' This function computes the SDM (Signal Dominance Measure)
#' from the RSS (Radio Signal Strength) of each antenna, which is contained
#' in the simData$grid object, whereas the parameters related with SDM
#' are available in the simData$network object. It returns the SDM as
#' a new attribute of simData$grid.
#'
#' @param simData list of different elements from the simulation.
#'
#' @rdname compute_sdm
#'
#' @name compute_sdm
#'
#' @import sf data.table stars
#'
#' @include read_simData.R
#'
#' @examples
#' filename_map <- c(
#'   xml = system.file("extdata/input_files", "map.xml", package = "simutils"),
#'   xsd = ""
#' )
#'
#' filename_network <- c(
#'   csv = system.file("extdata/output_files/antennas.csv", package = "simutils"),
#'   xml = system.file("extdata/metadata/output_files/antennas_dict.xml", package = "simutils")
#' )
#'
#' filename_signal <- c(
#'   csv = system.file("extdata/output_files/SignalMeasure_MNO1.csv", package = "simutils"),
#'   xml = system.file("extdata/metadata/output_files/SignalMeasure_dict.xml", package = "simutils")
#' )
#'
#' filename_coverage <- c(
#'   csv = system.file("extdata/output_files", "AntennaCells_MNO1.csv", package = "simutils"),
#'   xml = system.file("extdata/metadata/output_files/AntennaCells_dict.xml", package = "simutils")
#' )
#'
#' filename_events <- c(
#'   csv = system.file("extdata/output_files/AntennaInfo_MNO_MNO1.csv", package = "simutils"),
#'   xml = system.file("extdata/metadata/output_files/events_dict.xml", package = "simutils")
#' )
#'
#' filename_grid <- c(
#'   csv = system.file("extdata/output_files/grid.csv", package = "simutils"),
#'   xml = system.file("extdata/metadata/output_files/grid_dict.xml", package = "simutils")
#' )
#'
#' filename_individ <- c(
#'   csv = system.file("extdata/output_files/persons.csv", package = "simutils"),
#'   xml = system.file("extdata/metadata/output_files/persons_dict.xml", package = "simutils")
#' )
#'
#' filenames <- list(
#'   map                = filename_map,
#'   network_parameters = filename_network,
#'   signal             = filename_signal,
#'   events             = filename_events,
#'   coverage_cells     = filename_coverage,
#'   grid               = filename_grid,
#'   individuals        = filename_individ
#' )
#'
#' simData <- simutils::read_simData(filenames, crs = 2062)
#' str(simData)
#'
#' compute_sdm(simData)
#'
#' @export
#'
compute_sdm <- function(simData) {
  `Antenna ID` <- NULL

  simDataNetwork.dt <- sf::st_drop_geometry(simData$network)
  simData$grid$SDM <- copy(simData$grid$RSS)
  lx <- dim(simData$grid$RSS)[[1]]
  ly <- dim(simData$grid$RSS)[[2]]
  simDataGridSDM <- lapply(seq_len(dim(simData$grid$RSS)[3]), function(k) {
    antenna_name <- dimnames(simData$grid$RSS)[[3]][k]
    SStep_k <- simDataNetwork.dt[`Antenna ID` == antenna_name]$SSteep
    RSS_k <- simData$grid$RSS[1:lx, 1:ly, k]
    Smid_k <- simDataNetwork.dt[`Antenna ID` == antenna_name]$Smid

    simData$grid$SDM[1:lx, 1:ly, k] <- 1 / (1 + exp(-SStep_k * (RSS_k - Smid_k)))
  })

  simData$grid$SDM <- array(as.numeric(unlist(simDataGridSDM)),
    dim = c(
      nrow(simDataGridSDM[[1]]),
      ncol(simDataGridSDM[[1]]), length(simDataGridSDM)
    )
  )
  dimnames(simData$grid$SDM) <- dimnames(simData$grid$RSS)

  return(simData)
}
