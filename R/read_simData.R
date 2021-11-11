#' Create list of parameters of the simulation.
#'
#' Create a named list of different elements for the simulation. It basically
#' contains the map (regions included), the network configuration (position of 
#' BTS, power, attenuation factor, etc.), the grid, the radio cells associated 
#' to each antenna, and the telecommunication signal measures (RSS, SDM, ...). 
#'
#' @param filenames Named list of filenames with each element of the simulation.
#' 
#' @param crs integer or character; coordinate reference system for the geometry
#'  as in function \code{sf::st_as_sfc}
#'
#' @rdname read_simData
#'
#' @name read_simData
#' 
#' @import sf data.table stars
#' 
#' @include read_csv.R xml_attrs2dt.R RcppExports.R xml_getters.R
#' 
#' @examples
#' filename_map      <- c(
#'  xml= system.file("extdata/input_files", "map.xml", package = "simutils"),
#'  xsd= '')
#'   
#' filename_network  <- c(
#'  csv= system.file("extdata/output_files/antennas.csv", package = "simutils"),
#'  xml= system.file("extdata/metadata/output_files/antennas_dict.xml", 
#'                    package = "simutils"))
#'                    
#' filename_signal <- c(
#'  csv= system.file("extdata/output_files/SignalMeasure_MNO1.csv", package = "simutils"),
#'  xml= system.file("extdata/metadata/output_files/SignalMeasure_dict.xml", 
#'                    package = "simutils"))
#'                  
#' filename_coverage <- c(
#'  csv= system.file("extdata/output_files", "AntennaCells_MNO1.csv", 
#'                   package = "simutils"),
#'  xml= system.file("extdata/metadata/output_files/AntennaCells_dict.xml", 
#'                   package = "simutils"))
#'                        
#' filename_grid <- c(
#'   csv= system.file("extdata/output_files/grid.csv", package = "simutils"),
#'   xml= system.file("extdata/metadata/output_files/grid_dict.xml", 
#'                    package = "simutils")) 
#' 
#' filename_individ <- c(
#'   csv= system.file("extdata/output_files/persons_dash.csv", package = "simutils"),
#'   xml= system.file("extdata/metadata/output_files/persons_dash_dict.xml", 
#'                    package = "simutils"))   
#'                        
#' filenames <- list(
#'   map                = filename_map,
#'   network_parameters = filename_network,
#'   signal             = filename_signal,
#'   coverage_cells     = filename_coverage,
#'   grid               = filename_grid,
#'   individuals        = filename_individ)
#'   
#' simData <- read_simData(filenames, crs = 2062)
#' str(simData)
#'
#' @export
read_simData <- function(filenames, crs = NA_integer_){
  
  
  name <- V1 <- nDev <- `Device 1` <- `Device 2` <- NULL
  
  if (is.null(names(filenames))) {
    stop('[simutils::read_simData] filenames must be a named list.\n')
  }
  
  if (!all(names(filenames) %in% c('map', 'network_parameters', 'signal', 'coverage_cells', 'grid', 'individuals'))){
    stop("[simutils::read_simData] The names of list filenames must be contained in c('map', 'network_parameters', 'signal', 'coverage_cells', 'grid', 'individuals').\n")
  }
  
  
  # Read map file
  cat('[simutils::read_simData] Reading and parsing xml file for the map...')
  map.sf <- read_xml_map(filenames$map['xml'])
  map.sf <-  st_set_crs(map.sf, st_crs(crs))
  cat(' ok.\n')
  
  # Region names
  label_spUnit <- getSpatialUnitName(filenames$map[['xml']], 'map')
  names_spUnit <- map.sf[[label_spUnit]]
  label_nestSpUnits <- getNestingSpatialUnitName(filenames$map[['xml']], 'map')
  
  # Read network parameters
  cat('[simutils::read_simData] Reading and parsing network parameters file...\n')
  network.dt <- read_csv(filenames$network_parameters['xml'], filenames$network_parameters['csv'])
  coords_name <- getCoordsNames(filenames$network_parameters['xml'], 'antennas')
  coords_name_idx <- which(names(network.dt) %in% coords_name)
  network_attr <- attr(network.dt, 'specs')[-coords_name_idx]
  network.sf <- st_as_sf(network.dt, coords = coords_name, crs = crs)
  attr(network.sf, 'specs') <- unname(network_attr)

  network_in_geom_unit_idx <- sapply(st_intersects(network.sf, map.sf), function(x) sample(x, 1))
  network.sf[[label_spUnit]] <- names_spUnit[network_in_geom_unit_idx]
  
  map_nogeom <- st_drop_geometry(map.sf[, c(label_spUnit, label_nestSpUnits)])
  map_cols_idx <- which(names(map.sf) %in% c(label_spUnit, label_nestSpUnits))
  map_cols_attr <- attr(map.sf, 'specs')[map_cols_idx]

  network.sf <- dplyr::left_join(
    network.sf, 
    st_drop_geometry(map.sf[, c(label_spUnit, label_nestSpUnits)]))
  attr(network.sf, 'specs') <- c(unname(network_attr), map_cols_attr, 'geometry')
  cat(' ok.\n')

  # Read coverage file 
  cat('[simutils::read_simData] Reading and parsing coverage cells file...')
  cellCoord_name <- getCellCoordName(filenames$coverage_cells['xml'], 'cells')
  options_wkt <- paste0('GEOM_POSSIBLE_NAMES=', cellCoord_name)
  cellID_name <- getCellIDName(filenames$coverage_cells['xml'], 'cells')
  aggr_spec <- 'identity'
  names(aggr_spec) <- cellID_name
  coverage.sf <- read_sf(filenames$coverage_cells['csv'], options = options_wkt,
                         agr = aggr_spec)
  cellCoord_colname <- gsub(' ', '.', cellCoord_name, fixed = TRUE)
  coverage.sf <- st_set_crs(coverage.sf, st_crs(crs))
  coverage.sf[[cellCoord_colname]] <- NULL
  coverage.sf <- st_intersection(coverage.sf, st_union(map.sf))
  attr(coverage.sf, 'specs') <- c('specs_cells', 'geometry')
  cat(' ok.\n')  

  # Read signal per tile
  cat('[simutils::read_simData] Reading and parsing signal file...\n')
  signal.dt <- read_csv(filenames$signal['xml'], filenames$signal['csv'])
  signal_type <- getSignalType(filenames$signal['xml'], 'signal')
  cat(' ok.\n')
  
  # Read grid
  cat('[simutils::read_simData] Reading grid file and creating stars object...')
  grid.dt <- read_csv(filenames$grid['xml'], filenames$grid['csv'])
  nx <- as.integer(grid.dt[[getGridNoTilesX(filenames$grid['xml'], 'grid')]])
  ny <- as.integer(grid.dt[[getGridNoTilesY(filenames$grid['xml'], 'grid')]])
  deltax <- grid.dt[[getXTileDimColName(filenames$grid['xml'], 'grid')]]
  deltay <- grid.dt[[getYTileDimColName(filenames$grid['xml'], 'grid')]]
  
  idVar <- names(signal.dt)[!grepl('[Tt]ile', names(signal.dt))]
  tileCols <- setdiff(names(signal.dt), idVar)
  signal.array <- array(t(as.matrix(signal.dt[, .SD, .SDcols = tileCols])), 
                        dim = c(nx, ny, nrow(signal.dt)),
                        dimnames = list(0:(nx-1), 0:(ny-1), signal.dt[[idVar]])
  )
  
  grid.stars <- st_as_stars(signal.array)
  names(grid.stars) <- signal_type
  names(attr(grid.stars, 'dimensions')) <- c('x', 'y', idVar)
  
  map_bbox <- st_bbox(map.sf)
  xmin <- map_bbox$xmin
  ymax <- map_bbox$ymax
  
  attr(grid.stars, 'dimensions')[['x']][['offset']] <- unname(xmin)
  attr(grid.stars, 'dimensions')[['x']][['delta']]  <- deltax
  attr(grid.stars, 'dimensions')[['x']][['refsys']] <- st_crs(crs)
  attr(grid.stars, 'dimensions')[['x']][['point']]  <- FALSE
  attr(grid.stars, 'dimensions')[['x']][['values']]  <- NULL
  
  attr(grid.stars, 'dimensions')[['y']][['offset']] <- unname(ymax)
  attr(grid.stars, 'dimensions')[['y']][['delta']]  <- -deltay
  attr(grid.stars, 'dimensions')[['y']][['refsys']] <- st_crs(crs)
  attr(grid.stars, 'dimensions')[['y']][['point']]  <- FALSE
  attr(grid.stars, 'dimensions')[['y']][['values']]  <- NULL
  
  attr(grid.stars, 'dimensions')[[idVar]][['values']]  <- NULL
  
  attr(attr(grid.stars, 'dimensions'), 'raster')[['dimensions']] <- c('x', 'y')
  grid.stars <- st_crop(grid.stars, st_union(map.sf))
  cat(' ok.\n')
  
  # Read individuals
  cat('[simutils::read_simData] Reading and parsing persons file ...\n')
  individuals.dt <- read_csv(filenames$individuals['xml'], filenames$individuals['csv'])
  attr_indiv <- attr(individuals.dt, 'specs')
  deviceColNames <- names(individuals.dt)[grep('Device', names(individuals.dt))]
  for (devCol in deviceColNames){
    
    individuals.dt[, paste0('n_', devCol) := ifelse(is.na(get(devCol)), 0L, 1L)]
  }
  individuals.dt[
    , nDev := rowSums(.SD, na.rm = TRUE), .SDcols = paste0('n_', deviceColNames)][
    , (paste0('n_', deviceColNames)) := NULL]
  individuals.sf <- st_as_sf(individuals.dt, coords = c('x', 'y'), crs = crs)
  attr_indiv <- attr_indiv[-which(attr_indiv == 'specs_person_coords')]
  
  individual_in_geom_unit_idx <- sapply(st_intersects(individuals.sf, map.sf), function(x) sample(x, 1))
  individuals.sf[[label_spUnit]] <- names_spUnit[individual_in_geom_unit_idx]
  individuals.sf <- dplyr::left_join(
    individuals.sf, 
    st_drop_geometry(map.sf[, c(label_spUnit, label_nestSpUnits)]))
  attr(individuals.sf, 'specs') <- c(
    attr_indiv, 'specs_ndev', paste0('specs_', label_spUnit),
    paste0('specs_', label_nestSpUnits), 'geometry')
  cat(' ok.\n')
  
  
  simElements <- list(
    map = map.sf,
    network = network.sf,
    coverage = coverage.sf,
    grid = grid.stars,
    individuals = individuals.sf
  )
  return(simElements)
  
  
}