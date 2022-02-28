#' Determine the Voronoi polygon where each device is at each time instant (Voronoi_true).
#' and the Voronoi polygon of the antenna to which each device is connected at each time instant (Voronoi_conn).
#'
#' @param simData \code{list} with components map, network, coverage, grid, 
#' individuals as output by function \link{read_simData}.
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
#' filename_map      <- c(
#'  xml= system.file("extdata/input_files", "map.xml", package = "simutils"),
#'  xsd= '')
#'   
#' filename_network  <- c(
#'  csv= system.file("extdata/output_files/antennas.csv", package = "simutils"),
#'  xml= system.file("extdata/metadata/output_files/antennas_dict.xml", package = "simutils"))
#'                    
#' filename_signal <- c(
#'  csv= system.file("extdata/output_files/SignalMeasure_MNO1.csv", package = "simutils"),
#'  xml= system.file("extdata/metadata/output_files/SignalMeasure_dict.xml", package = "simutils"))
#'                  
#' filename_coverage <- c(
#'  csv= system.file("extdata/output_files", "AntennaCells_MNO1.csv", package = "simutils"),
#'  xml= system.file("extdata/metadata/output_files/AntennaCells_dict.xml", package = "simutils"))
#'
#' filename_events <- c(
#'  csv= system.file("extdata/output_files/AntennaInfo_MNO_MNO1.csv", package = "simutils"),
#'  xml= system.file("extdata/metadata/output_files/events_dict.xml", package = "simutils"))
#'                        
#' filename_grid <- c(
#'   csv= system.file("extdata/output_files/grid.csv", package = "simutils"),
#'   xml= system.file("extdata/metadata/output_files/grid_dict.xml", package = "simutils")) 
#' 
#' filename_individ <- c(
#'   csv= system.file("extdata/output_files/persons.csv", package = "simutils"),
#'   xml= system.file("extdata/metadata/output_files/persons_dict.xml", package = "simutils"))   
#'                        
#' filenames <- list(
#'   map                = filename_map,
#'   network_parameters = filename_network,
#'   signal             = filename_signal,
#'   events             = filename_events,
#'   coverage_cells     = filename_coverage,
#'   grid               = filename_grid,
#'   individuals        = filename_individ)
#'   
#' simData  <- simutils::read_simData(filenames, crs = 2062)
#' mnd_info <- simutils::get_mnd(simData, groundTruth = TRUE)
#' voronoi  <- simutils::compute_voronoi_sf(simData)
#' voronoi_positions <- simutils::compute_voronoi_positions(mnd_info, voronoi)
#' compute_voronoi_connections(simData, polygons.sf, voronoi_positions)
#' 
#' @export
compute_voronoi_connections <- function(simData, polygons.sf, voronoi_positions){
  
  network.sf <- simData$network
  specs_network <- attributes(network.sf)$specs
  name_Antennas <- names(specs_network)[which(specs_network == 'specs_cells')]
  tempCorresp.sf <- st_join(polygons.sf, simData$network[, c(name_Antennas, 'geometry')], left = TRUE)
  names(tempCorresp.sf) <- c('xAntenna', 'yAntenna', 'voronoi_id', name_Antennas, 'geometry')
  corresp_ant_voronoi <- tempCorresp.sf$voronoi_id
  names(corresp_ant_voronoi) <- tempCorresp.sf[[name_Antennas]]
  voronoi_positions$connVoronoi <- corresp_ant_voronoi[voronoi_positions$AntennaId]
  return(voronoi_positions)
  

}



