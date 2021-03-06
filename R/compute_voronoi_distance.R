#' Compute for each device and each time instant the distance d(Voronoi_conn, Voronoi_true).
#'
#' @param voronoi_positions \code{sf} object with information from the simulation input.It is filled
#' by events characteristics matched with PersonId
#'
#' @param voronoi_ring_dist \code{matrix} with the ring-distance between polygons.
#'
#' @return An \code{sf} object with the ring-distance between true and estimated (connection)
#' voronoi locations.
#'
#' @rdname compute_voronoi_distance
#'
#' @name compute_voronoi_distance
#'
#' @import sf
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
#' mnd_info <- simutils::get_mnd(simData, groundTruth = TRUE)
#' voronoi.sf <- simutils::compute_voronoi_sf(simData)
#' voronoi_positions <- simutils::compute_voronoi_positions(mnd_info, voronoi.sf)
#' voronoi_connections <- simutils::compute_voronoi_connections(
#'   simData, voronoi.sf$polygons,
#'   voronoi_positions
#' )
#' voronoi_ring_dist.mat <- simutils::compute_voronoi_rings(voronoi.sf)
#' compute_voronoi_distance(voronoi_connections, voronoi_ring_dist.mat)
#'
#' @export
compute_voronoi_distance <- function(voronoi_positions, voronoi_ring_dist) {
  voronoi_positions$voronoi_ring_dist <- unname(mapply(
    function(x, y) {
      if (is.na(x) | is.na(y)) {
        return(NA_real_)
      }
      voronoi_ring_dist[x, y]
    },
    voronoi_positions$trueVoronoi,
    voronoi_positions$connVoronoi
  ))
  return(voronoi_positions)
}
