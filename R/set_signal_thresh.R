#' Create RSS_thresh and SDM_thresh from the SimData object.
#'
#' This function computes the RSS_thresh and the SDM_thresh of each antenna
#' according to the minimum values (Smin and Qmin), contained in
#' the simData$network object, related with RSS and SDM attributes from
#' the simData$grid object. It returns the RSS_thresh and SDM_thresh as
#' new attributes of simData$grid.
#'
#' @param simData list of different elements from the simulation.
#'
#' @rdname set_signal_thresh
#'
#' @name set_signal_thresh
#'
#' @import sf data.table stars
#'
#' @include read_simData.R compute_sdm.R
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
#' simData <- simutils::compute_sdm(simData)
#' set_signal_thresh(simData)
#'
#' @export
set_signal_thresh <- function(simData) {
  `Antenna ID` <- NULL
  simDataNetwork.dt <- sf::st_drop_geometry(simData$network)
  simData$grid$RSS_thresh <- copy(simData$grid$RSS)
  simData$grid$SDM_thresh <- copy(simData$grid$SDM)
  lx <- dim(simData$grid$RSS)[[1]]
  ly <- dim(simData$grid$RSS)[[2]]

  # Como SDM es una copia de RSS, las dimensiones (en concreto la k, que es la que nos interesa)
  # deben ser las mismas y coincidir en el mismo orden exacto. ¿Es posible que esto no fuera así en algún momento?

  simDataGrid <- lapply(seq_len(dim(simData$grid$RSS)[3]), function(k) {
    antenna_name <- dimnames(simData$grid$RSS)[[3]][k]
    Smin_k <- simDataNetwork.dt[`Antenna ID` == antenna_name]$Smin
    RSS_k <- simData$grid$RSS[1:lx, 1:ly, k]
    Qmin_k <- simDataNetwork.dt[`Antenna ID` == antenna_name]$Qmin
    SDM_k <- simData$grid$SDM[1:lx, 1:ly, k]

    simData$grid$RSS_thresh[1:lx, 1:ly, k] <- ifelse(RSS_k < Smin_k, NA, RSS_k)
    simData$grid$SDM_thresh[1:lx, 1:ly, k] <- ifelse(SDM_k < Qmin_k, NA, SDM_k)
    output <- list(
      simData$grid$RSS_thresh[1:lx, 1:ly, k],
      simData$grid$SDM_thresh[1:lx, 1:ly, k]
    )
  })

  simDataGridRSS <- lapply(simDataGrid, "[[", 1)
  simDataGridSDM <- lapply(simDataGrid, "[[", 2)

  simData$grid$RSS_thresh <- array(as.numeric(unlist(simDataGridRSS)),
    dim = c(
      nrow(simDataGridRSS[[1]]),
      ncol(simDataGridRSS[[1]]),
      length(simDataGridRSS)
    )
  )

  dimnames(simData$grid$RSS_thresh) <- dimnames(simData$grid$RSS)

  simData$grid$SDM_thresh <- array(as.numeric(unlist(simDataGridSDM)),
    dim = c(
      nrow(simDataGridSDM[[1]]),
      ncol(simDataGridSDM[[1]]),
      length(simDataGridSDM)
    )
  )

  dimnames(simData$grid$SDM_thresh) <- dimnames(simData$grid$SDM)
  return(simData)
}
