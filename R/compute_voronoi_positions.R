#' Determine the Voronoi polygon where each device is at each time instant (Voronoi_true).
#' and the Voronoi polygon of the antenna to which each device is connected at each time instant (Voronoi_conn).
#'
#' @param mnd_info \code{list} with two elements related with the simulation input. The first element is related
#' with the observed mnd (\code{data.table}) whereas the second component contains the groundTruth (\code{sf} object)
#'
#' @param voronoi \code{list} of \code{sf} objects: triangles.sf and polygons.sf with:
#' geometry - coordinates of triangles and polygons info of antennas which compose the objects
#'
#' @return a \code{list} with two \code{sf} objects: groundTruth, which adds the Voronoi_true and Voronoi_conn positions;
#' and polygons, which adds the antenna id to each polygon.
#'
#' @rdname compute_voronoi_positions
#'
#' @name compute_voronoi_positions
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
#' voronoi <- simutils::compute_voronoi_sf(simData)
#' compute_voronoi_positions(mnd_info, voronoi)
#'
#' @export
compute_voronoi_positions <- function(mnd_info, voronoi) {
  specs_mnd <- attributes(mnd_info$mnd)$specs
  name_Antennas_grTr <- names(specs_mnd)[which(specs_mnd == "specs_cells")]
  groundTruth.sf <- mnd_info$grTruth
  # groundTruth_network.sf <- groundTruth.sf[!is.na(groundTruth.sf[[name_Antennas_grTr]]),]

  polygons.sf <- voronoi$polygons
  name_Antennas_polyg <- setdiff(names(polygons.sf), c("geometry", "xAntenna", "yAntenna"))

  inPol.mat <- st_intersects(groundTruth.sf, polygons.sf, sparse = FALSE)
  inPol.mat <- unlist(apply(inPol.mat, 1, function(x) {
    antID <- polygons.sf[[name_Antennas_polyg]][x]
    if (length(antID) > 1) antID <- sample(antID, 1)
    return(antID)
  }))

  groundTruth.sf$trueVoronoi <- inPol.mat
  return(groundTruth.sf)
}
