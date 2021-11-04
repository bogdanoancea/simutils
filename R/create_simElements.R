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
#' @rdname create_simElements
#'
#' @name create_simElements
#' 
#' @import sf data.table stars
#' 
#' @importFrom readr read_delim
#' 
#' @importFrom tibble as_tibble 
#' 
#' @include readWKT_as_sf.R readWKT_as_sfc.R read_csv.R xml_attrs2dt.R RcppExports.R xml_getters.R
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
#'   csv= system.file("extdata/output_files/persons.csv", package = "simutils"),
#'   xml= system.file("extdata/metadata/output_files/persons_dict.xml", 
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
#' simElements <- create_simElements(filenames, crs = 2062)
#' str(simElements)
#'
#' @export
create_simElements <- function(filenames, crs = NA_integer_){
  
  
  name <- V1 <- nDev <- `Device 1` <- `Device 2` <- NULL
  
  if (is.null(names(filenames))) {
    stop('[simutils::create_simElements] filenames must be a named list.\n')
  }
  
  if (!all(names(filenames) %in% c('map', 'network_parameters', 'signal', 'coverage_cells', 'grid', 'individuals'))){
    stop("[simutils::create_simElements] The names of list filenames must be contained in c('map', 'network_parameters', 'signal', 'coverage_cells', 'grid', 'individuals').\n")
  }
  
  
  # Read map file
  cat('[simutils::create_simElements] Reading and parsing xml file for the map...')
  map.sf <- read_xml_map(filenames$map['xml'])
  map.sf <-  st_set_crs(map.sf, st_crs(crs))
  map.sf$area <- st_area(map.sf)
  cat(' ok.\n')
  
  # Read network parameters
  cat('[simutils::create_simElements] Reading and parsing network parameters file...\n')
  network.dt <- read_csv(filenames$network_parameters['xml'], filenames$network_parameters['csv'])
  coords_name <- getCoordsNames(filenames$network_parameters['xml'], 'antennas')
  network.sf <- st_as_sf(network.dt, coords = coords_name, crs = crs)
  cat(' ok.\n')
  
  # Read coverage file 
  cat('[simutils::create_simElements] Reading and parsing coverage cells file...')
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
  coverage.sf$area <- st_area(coverage.sf)
  cat(' ok.\n')  
  
  # Read signal per tile
  cat('[simutils::create_simElements] Reading and parsing signal file...\n')
  signal.dt <- read_csv(filenames$signal['xml'], filenames$signal['csv'])
  signal_type <- getSignalType(filenames$signal['xml'], 'signal')
  cat(' ok.\n')
  
  # Read grid
  cat('[simutils::create_simElements] Reading grid file and creating stars object...')
  grid.dt <- read_csv(filenames$grid['xml'], filenames$grid['csv'])
  nx <- as.integer(grid.dt[[getGridNoTilesX(filenames$grid['xml'], 'grid')]])
  ny <- as.integer(grid.dt[[getGridNoTilesY(filenames$grid['xml'], 'grid')]])
  deltax <- grid.dt[[getXTileDimColName(filenames$grid['xml'], 'grid')]]
  deltay <- grid.dt[[getYTileDimColName(filenames$grid['xml'], 'grid')]]
  
  idVar <- names(signal.dt)[!grepl('[Tt]ile', names(signal.dt))]
  tileCols <- setdiff(names(signal.dt), idVar)
  signal.array <- array(t(as.matrix(signal.dt[, ..tileCols])), 
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
  
  ##### REVISE INDIVIDUALS  
  
  # Read individuals
  cat('[simutils::create_simElements] Reading and parsing persons file ...')
  individuals.dt <- fread(filenames$individuals[['csv']], sep = '\n', stringsAsFactors = FALSE)
  names_individuals.dt <- strsplit(names(individuals.dt), split=",")[[1]]
  
  xmlParam.dt <- xml_attrs2dt(filenames$individuals['xml'], 'individuals')
  colnames_csv <- xmlParam.dt$name
  xmlParam.dt <- xmlParam.dt[name %in% names_individuals.dt]
  classes_csv  <- xmlParam.dt$class
  names(classes_csv) <- xmlParam.dt$name
  classes_csv_num  <- classes_csv[which(classes_csv == 'numeric')]
  classes_csv_int  <- classes_csv[which(classes_csv == 'integer')]
  classes_csv_char <- classes_csv[which(classes_csv == 'character')]
  
  setnames(individuals.dt, 'V1')
  individuals_parsed.dt <- individuals.dt[, tstrsplit(V1, split = ',')]
  setnames(individuals_parsed.dt, colnames_csv)
  
  
  if ( length(classes_csv_num) > 0 ) {
    individuals_parsed.dt[, names(classes_csv_num) := lapply(.SD, as.numeric), .SDcols = names(classes_csv_num)]
  }
  if ( length(classes_csv_int) > 0 ) {
    individuals_parsed.dt[, names(classes_csv_int) := lapply(.SD, as.integer), .SDcols = names(classes_csv_int)]
  }
  if ( length(classes_csv_char) > 0 ) {
    individuals_parsed.dt[, names(classes_csv_char) := lapply(.SD, as.character), .SDcols = names(classes_csv_char)]
  }
  
  individuals_parsed.dt[, nDev := fcase(
    is.na(`Device 1`) & is.na(`Device 2`), 0L,
    !is.na(`Device 1`) & is.na(`Device 2`), 1L,
    is.na(`Device 1`) & !is.na(`Device 2`), 1L,
    !is.na(`Device 1`) & !is.na(`Device 2`), 2L)]
  
  individuals.sf <- st_as_sf(individuals_parsed.dt, coords = c('x', 'y'), crs = crs)
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