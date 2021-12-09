#' @title Compute Voronoi neighbours of antenna sites.
#'
#' @description Find neighbours of antennas and number of neighbours for each 
#' antenna in addition to the distance matrix. If latlong is TRUE then it 
#' returns the distance matrix in km.
#'
#' @param simData \code{list} with components map, network, coverage, grid, 
#' individuals as output by function \link{read_simData}.
#' 
#' @param PLOT logical (default FALSE) to indicate whether to plot the 
#' tesselation or not.
#' 
#' @param latlong logical (default FALSE) to indicate whether \code{x} and 
#' \code{y} refer to longitude and latitude.
#' 
#' @param R numeric vector of lenght 1 denoting the Earth's radius in km.
#'
#' @return list with:
#' Adj - Adjacency matrix
#' NumN - number of neighbours per antenna
#' D - Distance matrix
#'
#'
#' @rdname compute_voronoi_neighbours
#'
#' @name compute_voronoi_neighbours
#' 
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
#' compute_voronoi_neighbours(simData, PLOT = TRUE)
#' 
#' 
#' @export
compute_voronoi_neighbours <- function(simData, PLOT = FALSE, latlong = FALSE, R = 6371){
  
  data <- data.frame(st_coordinates(simData$network))
  P <- nrow(data)
  
  map_bbox <- st_bbox(simData$map)
  xmin <- map_bbox$xmin
  ymin <- map_bbox$ymin
  xmax <- map_bbox$xmax
  ymax <- map_bbox$ymax

  #find adjacent VPs
  tesselation <- deldir::deldir(data$X, data$Y, plot= PLOT, rw = c(xmin, xmax, ymin, ymax))

  #adjacent matrix
  A <-  matrix(FALSE,P,P)
  A[as.matrix(tesselation$delsgs[, c("ind1", "ind2")])] <- 1.0
  A[as.matrix(tesselation$delsgs[, c("ind2", "ind1")])] <- 1.0#because symmetric
  
  #distance matrix
  D <- matrix(NA,P,P)
  if (latlong == TRUE){
    
    haversine.dist <- function(lat1, lat2, long1, long2, R= R){
      #convert to radians
      lat1  <- lat1*pi/180
      lat2  <- lat2*pi/180
      long1 <- long1*pi/180
      long2 <- long2*pi/180
      dlat  <- lat2-lat1
      dlong <- long2-long1
      
      #haversine
      a <- sin(dlat / 2)**2 + cos(lat1) * cos(lat2) * sin(dlong / 2)**2
      c <- 2 * asin(sqrt(a))
      return(c * R)
    }
    
    
    for(i in 1:(P-1)){
      
      for(j in (i+1):P){
        
        D[i,j]<- D[j,i] <- haversine.dist(data$x[i], data$x[j], data$y[i], data$y[j])
      }
    }
  } else {
    
    D[as.matrix(tesselation$delsgs[, c("ind1", "ind2")])] <- sqrt(
      (tesselation$delsgs[, "x1"] - tesselation$delsgs[, "x2"])^2 +
      (tesselation$delsgs[, "y1"] - tesselation$delsgs[, "y2"])^2
    )
    D[as.matrix(tesselation$delsgs[, c("ind2", "ind1")])] <- sqrt(
      (tesselation$delsgs[, "x1"] - tesselation$delsgs[, "x2"])^2+
      (tesselation$delsgs[, "y1"] - tesselation$delsgs[, "y2"])^2)
  }
  
  #number of neighbours for each antenna
  N <- matrix(colSums(A), P, 1)
  diag(A) <- NA #since self adjacency is not true
  
  return(list(Adj = A, NumN = N, D = D))
}




