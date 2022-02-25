#' Determine the Voronoi polygon where each device is at each time instant (Voronoi_true).
#' and the Voronoi polygon of the antenna to which each device is connected at each time instant (Voronoi_conn).
#'
#' @param mnd_info \code{list} with two elements related with the simulation input. The first element is related
#' with the observed mnd (\code{data.table}) whereas the second component contains the groundTruth (\code{sf} object)
#' 
#' @param voronoi \code{list} of \code{sf} objects:triangles.sf and polygons.sf with:
#' geometry - coordinates of triangles and polygons
#' info of antennas which compose the objects
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
#' compute_voronoi_positions(mnd_info, voronoi)
#' 
#' @export
compute_voronoi_positions <- function(mnd_info, voronoi){
  
  specs_mnd <- attributes(mnd_info$mnd)$specs
  name_Antennas_grTr <- names(specs_mnd)[which(specs_mnd == 'specs_cells')]
  groundTruth.sf <- mnd_info$grTruth
  groundTruth_network.sf <- groundTruth.sf[!is.na(groundTruth.sf[[name_Antennas_grTr]]),]

  polygons.sf <- voronoi$polygons
  name_Antennas_pol <- setdiff(names(polygons.sf), c('geometry', 'xAntenna', 'yAntenna'))
  Voronoi_true_insct <- st_intersects(groundTruth_network.sf$geometry, polygons.sf$geometry)
  Voronoi_true_insct <- lapply(Voronoi_true_insct, function(x){if( length(x) > 0 ){ sample(x, 1) } else{ x }})
  Voronoi_true_idx <- unlist(Voronoi_true_insct)
  Voronoi_true <- groundTruth_network.sf[[name_Antennas_grTr]][Voronoi_true_idx]
  groundTruth_network.sf$trueVoronoi <- Voronoi_true
  return(groundTruth_network.sf)
  
  #es susceptible de ser paralelizable, tarda unos 3 minutos

  groundTruth.sf$Voronoi_true <- lapply(seq(1:nrow(groundTruth.sf)), function(i){
   
    return(polygons.sf[groundTruth.sf$Voronoi_true_aux[[i]],]$id)
  
  })
  # Voronoi_conn
  
  #asociar la antenna id, el polígono se ve con el id de antenna con la fila del polygons
  
  groundTruth.sf$Voronoi_conn <- groundTruth.sf$AntennaId
  groundTruth.sf$Voronoi_conn[which(groundTruth.sf$Voronoi_conn %in% c('8','9'))] <- '10'
  
  voronoi <- list(
    groundTruth = groundTruth.sf,
    polygons    = polygons.sf
  )
  
  return(voronoi)
  
}



#ggplot() + geom_sf(data = polygons$geometry[which(polygons$id == '19')], color = 'blue') 
#+ geom_sf(data = polygons$geometry[which(polygons$id == '1')])
#+ geom_sf(data = groundTruth$geometry[1], size = 2) 


