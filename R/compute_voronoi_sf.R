#'  Compute Delaunay triangles and Voronoi tesselation of antennas.
#'
#' @param simData \code{list} with components map, network, coverage, grid, 
#' individuals as output by function \link{read_simData}.
#' 
#' @return a list of sf objects: triangles.sf and polygons.sf with:
#' geometry - coordinates of triangles and polygons
#' info of antennas which compose the objects
#'
#'
#' @rdname compute_voronoi_sf
#'
#' @name compute_voronoi_sf
#' 
#' @import sf sfheaders 
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
#' compute_voronoi_sf(simData)
#' 
#' @export
compute_voronoi_sf <- function(simData){
  
  data <- data.frame(st_coordinates(simData$network))
  P <- nrow(data)
  
  map_bbox <- st_bbox(simData$map)
  xmin <- map_bbox$xmin
  ymin <- map_bbox$ymin
  xmax <- map_bbox$xmax
  ymax <- map_bbox$ymax
  crs <- st_crs(simData$network)
  
  #find adjacent VPs
  tesselation <- deldir::deldir(data$X, data$Y, plot= FALSE, rw = c(xmin, xmax, ymin, ymax))
  triangles   <- deldir::triang.list(tesselation)
  polygons    <- deldir::tile.list(tesselation)

  # TRIANGLES 
  triangles.df <- do.call(rbind, triangles)
  n            <- nrow(triangles.df)/3
  id_vector    <- rep(1:n, each = 3)
  triangles.df$id <- id_vector
  triangles.sf <- sf_multipolygon(triangles.df, multipolygon_id = 'id', x = 'x', y = 'y')
  numAntennas.list <- list()[1:(nrow(triangles.df)/3)] 
  
  for(i in seq(1, nrow(triangles.df), by = 3)){
    numAntennas.list[[(i+2)/3]] <- eval(parse(text = paste0('c(',triangles.df$ptNum[i], ',', triangles.df$ptNum[i+1], ',', triangles.df$ptNum[i+2], ')')))
  } 
  triangles.sf$numAntennas <- numAntennas.list
  triangles.sf             <- triangles.sf[, !(names(triangles.sf) %in% c('id'))]
  triangles.sf             <- st_set_crs(triangles.sf, crs)
  
  # POLYGONS 
  polygons.matrix  <- do.call(rbind, polygons)
  polygons.df  <- as.data.frame(polygons.matrix)
  rownames(polygons.df) <- NULL
  
  x <- as.matrix(NA)
  y <- as.matrix(NA)
  id <- list()
  xAntenna <- list()
  yAntenna <- list()
  for (i in 1:nrow(polygons.df)){
    xi <- t(do.call(rbind,polygons.df$x[i]))
    x  <- rbind(x,xi)
    yi <- t(do.call(rbind,polygons.df$y[i]))
    y  <- rbind(y,yi)
    id <- append(id,rep(i,length(xi)))
    xAntenna <- append(xAntenna,polygons.df$pt[[i]][[1]])
    yAntenna <- append(yAntenna,polygons.df$pt[[i]][[2]])
      
  }
  x <- x[-1]
  y <- y[-1]
  id <- unlist(id)
  xAntenna <- unlist(xAntenna)
  yAntenna <- unlist(yAntenna)
  polygons.df <- data.frame(x = x, y = y, id = id)
  polygons.sf <- sf_multipolygon(polygons.df, multipolygon_id = 'id', x = 'x', y = 'y')
  polygons.sf$xAntenna <- xAntenna
  polygons.sf$yAntenna <- yAntenna
  polygons.sf          <- polygons.sf[, !(names(polygons.sf) %in% c('id'))]
  polygons.sf <- st_set_crs(polygons.sf, crs)
  attrib_network <- attributes(simData$network)$specs
  name_Antennas <- names(attrib_network)[which(attrib_network == 'specs_cells')]
  
  polygons.sf <- st_join(polygons.sf, simData$network[, c(name_Antennas, 'geometry')], left = TRUE)
  polygons.sf <- st_difference(polygons.sf)

  output <- list(
    triangles = triangles.sf,
    polygons = polygons.sf)
  
  return(output)
  
}


####### PLOT TRIANGLES AND POLYGONS ####
#ggplot() +
  #geom_sf(data = voronoi$triangles, size = 1.1, colour = 'black', fill = NA) +
  #geom_sf(data = voronoi$polygons, fill = NA)


