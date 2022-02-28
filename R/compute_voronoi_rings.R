#'  Establish an integer-valued distance between Voronoi polygons. 
#'  This distance basically determines the Voronoi ring for each antenna.
#'
#' @param voronoi \code{list} of \code{sf} objects:triangles.sf and polygons.sf with:
#' geometry - coordinates of triangles and polygons
#' info of antennas which compose the objects 
#'
#' @return a list with the distance in rings between polygons. 
#'
#' @rdname compute_voronoi_rings
#'
#' @name compute_voronoi_rings
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
#' simData <- simutils::read_simData(filenames, crs = 2062)
#' voronoi <- simutils::compute_voronoi_sf(simData)
#' compute_voronoi_rings(voronoi) -> x
#' 
#' @export
compute_voronoi_rings <- function(voronoi){
  
  polygons <- voronoi$polygons 
  name_Antennas_polyg <- setdiff(names(polygons), c('geometry', 'xAntenna', 'yAntenna'))
  
  #Compute the euclidean distance matrix between all the polygons
  distanceMatrix <- sf::st_distance(polygons, polygons)
  antIDs <- polygons[[name_Antennas_polyg]]
  rownames(distanceMatrix) <- antIDs
  colnames(distanceMatrix) <- antIDs


  #Build the first ring for each polygon 
  polygons.dist <- lapply(rownames(distanceMatrix), function(row_name){
    
    row_idx <- which(abs(as.numeric(distanceMatrix[row_name,])) < .Machine$double.eps)
    voronoi_ring <- antIDs[row_idx]
    return(list(voronoi_ring))
    
  })
  names(polygons.dist) <- antIDs

  #Build the second ring for each polygon (due to some differences in the access to the list)
  for(i in seq(along = antIDs)){

    for (antID in antIDs){

      ring_dist <- unlist(polygons.dist[[antID]])
      if (length(ring_dist) >= (ncol(distanceMatrix)) ) next 
      auxVector <- c()
      for (runn_antID in ring_dist){

        idx <- which(abs(as.numeric(distanceMatrix[runn_antID,])) < .Machine$double.eps)
        auxVector <- c(auxVector, rownames(distanceMatrix)[idx])
      
      }
      auxVector <- unique(auxVector)
      auxVector <- auxVector[-which(auxVector %in% ring_dist)]
      #auxVector <- auxVector[auxVector != antID]
      polygons.dist[[antID]] <- c(polygons.dist[[antID]], list(auxVector))
    }
    if (all(sapply(polygons.dist, function(x){length(unlist(x))})) >= ncol(distanceMatrix)) break
  }

  polygons_ringDist <- Reduce(rbind, lapply(antIDs, function(antID){
    
    rings.lst <- polygons.dist[[antID]]
    output.lst <- sapply(seq(along = rings.lst), function(i){
      
      dist <- rep(i, length(rings.lst[[i]]))
      names(dist) <- rings.lst[[i]]
      return(dist)
    })
    output.vec <- Reduce(c, output.lst)
    output.vec <- output.vec[antIDs]
    return(output.vec)
    
  }))
  
  rownames(polygons_ringDist) <- antIDs
  colnames(polygons_ringDist) <- antIDs
  
  for (antID in antIDs){ polygons_ringDist[antID, antID] <- 0 }
  return(polygons_ringDist)
  
}
  


