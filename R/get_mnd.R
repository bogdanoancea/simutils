#' Get mobile network data.
#'
#' Create a named list of two elements related with the simulation input. The first element is 
#' related to the observed mobile network data (mnd) whereas the second component contains the 
#' ground truth. It basically contains the events detected in the network and the ground truth that 
#' is fill by events characteristics matched with PersonId
#'
#' @param simData list of different information elements from the simulation (see 
#' \code{\link{read_simData}}).
#' 
#' @param devices character vector with the codes of the devices for which the data are to be 
#' obtained; if nothing is specified, all are taken. 
#' 
#' @param t_range numeric vector with the range of time instants whose events are to be obtained; 
#' it has length 2, where first component contains the initial instant and the second the final one. 
#' By default, the whole range of times is chosen.
#' 
#' @param groundTruth logical vector indicating if the ground truth is included in the output or not
#' 
#' @rdname get_mnd
#'
#' @name get_mnd
#' 
#' @import sf data.table
#' 
#' @include read_simData.R
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
#'
#' get_mnd(simData)
#'
#' get_mnd(simData, devices = c('209', '894'), t_range = c(56,65), groundTruth = TRUE)
#'
#' @export
get_mnd <- function(simData, devices, t_range, groundTruth = FALSE){
  
  mnd_vars <- variable <- observed_mnd_specs <- NULL
  
  events.dt <- sf::st_drop_geometry(simData$events)

  observed_mnd_specs <- attributes(events.dt)$specs

  name_devices   <- names(observed_mnd_specs)[observed_mnd_specs == 'specs_devices']
  name_time      <- names(observed_mnd_specs)[observed_mnd_specs == 'specs_time']
  
  name_tile       <- names(observed_mnd_specs)[observed_mnd_specs == 'specs_tile']
  name_spUnit     <- names(observed_mnd_specs)[observed_mnd_specs == 'specs_spUnit_name']
  name_spNestUnit <- names(observed_mnd_specs)[observed_mnd_specs == 'specs_spNestUnit_name']
  
  if (!missing(devices)) {
    
    events.dt <- events.dt[get(name_devices) %in% devices]
    devIDs <- unique(events.dt[[name_devices]])
    devNotFound <- setdiff(devices, devIDs)
    if (length(devNotFound) > 0) {
      
      stop(paste0('[simutils::get_mnd] The following devices have not been found in the dataset:', paste0(devNotFound, collapse = ', '), '\n'))
      
    }
  }
  
  
  if (!missing(t_range)) {
    
    events.dt <- events.dt[get(name_time) %between% t_range]
    if (nrow(events.dt) == 0) {
      
      stop('[simutils::get_mnd] No event data in the specified time range.\n')
      
    }
  }
  
  mnd_vars <- setdiff(names(events.dt), c(name_tile, name_spUnit, name_spNestUnit))
  observed_mnd.dt <- events.dt[, ..mnd_vars][
    , (name_devices) := as.character(get(name_devices))]
                                   
  if (groundTruth) {
    
    individuals_specs <- attributes(simData$individuals)$specs
    
    name_time2      <- names(individuals_specs)[individuals_specs == 'specs_time']
    name_person     <- names(individuals_specs)[individuals_specs == 'specs_personID']
    name_tile       <- names(individuals_specs)[individuals_specs == 'specs_tile']
    name_dev1       <- names(individuals_specs)[individuals_specs == 'specs_device_1']
    name_dev2       <- names(individuals_specs)[individuals_specs == 'specs_device_2']
    
    individuals.sf <- simData$individuals
    if (!missing(devices)) {
      
    individuals.sf <- individuals.sf[
      simData$individuals[[name_dev1]] %in% devices | simData$individuals[[name_dev2]] %in% devices,]
    
    }
    
    if (!missing(t_range)) {
      
    individuals.sf <- individuals.sf[individuals.sf[[name_time2]] %between% t_range, ]  
      
    }
        
    individuals.dt <- sf::st_drop_geometry(individuals.sf)
    indiv_coord.dt <- sf::st_coordinates(individuals.sf)
    
    individuals.dt <- cbind(individuals.dt, indiv_coord.dt)
    
    indiv_long.dt <- melt(individuals.dt, 
                          id.vars = setdiff(names(individuals.dt), c(name_dev1, name_dev2)),
                          value.name = name_devices, measure.vars = c(name_dev1, name_dev2))[
        , variable := NULL][
        is.na(get(name_devices)), (name_devices) := '-'][
        , (name_devices) := as.character(get(name_devices))]
    setnames(indiv_long.dt, name_time2, name_time)

    if (!missing(devices)){
      
      indiv_long.dt <- indiv_long.dt[get(name_devices) %in% devices]
      
    }
    
    grTruth.dt <- merge(observed_mnd.dt, indiv_long.dt, 
                        by = c(name_time, name_devices), all = TRUE)
    grTruth_specs <- c(observed_mnd_specs, individuals_specs)
    grTruth.sf <- sf::st_as_sf(grTruth.dt, coords = c('X', 'Y'))
    grTruth_specs <- grTruth_specs[names(grTruth.sf)]
    st_crs(grTruth.sf) <- st_crs(simData$individuals)
    attr(grTruth.sf, 'specs') <- grTruth_specs
    
  } else {
    
    grTruth.sf <- st_sf(st_sfc())
    
  }
  
  output <- list(mnd = observed_mnd.dt, grTruth = grTruth.sf)
  return(output)
}