#' Get mobile network data.
#'
#' Create a named list of two elements related with the simulation input. The first element is 
#' related to the observed mobile network data (mnd) whereas the second component contains the 
#' ground truth. It basically contains the events detected in the network and the ground truth that 
#' is fill by events characteristics matched with PersonId
#'
#' @param simData list of different information elements from the simulation (see 
#' \ref{\code{read_simData}}).
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
#' @import sf data.table stars dplyr
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
#' 
get_mnd <- function(simData, devices, t_range, groundTruth = FALSE){

  observed_mnd.dt <- sf::st_drop_geometry(simData$events)
  observed_mnd_specs <- attributes(observed_mnd.dt)$specs
return(observed_mnd_specs)
  name_tile       <- names(observed_mnd_specs)[observed_mnd_specs == 'specs_tile']
  name_spUnit     <- names(observed_mnd_specs)[observed_mnd_specs == 'specs_sp_name_long']
  name_spNestUnit <- names(observed_mnd_specs)[observed_mnd_specs == 'specs_nesting_name_long']
  mnd_vars <- setdiff(names(observed_mnd.dt), c(name_tile, name_spUnit, name_spNestUnit))
return(mnd_vars)  
  observed_mnd.dt <- observed_mnd.dt[, ..mnd_vars]
  return(observed_mnd.dt)
      
  events_coord.dt <- sf::st_coordinates(simData$events)
  
  individuals.dt <- sf::st_drop_geometry(simData$individuals)
  indiv_coord.dt <- sf::st_coordinates(simData$individuals)

return(list(observed_mnd.dt, individuals.dt ))  
  
  
  individuals.specs  <- attributes(simData$individuals)$specs
  nameDev            <- names(observed_mnd.specs)[observed_mnd.specs == 'specs_devices']
  nameTA             <- names(observed_mnd.specs)[observed_mnd.specs == 'specs_TA']
  
  namePerson         <- names(individuals.specs)[individuals.specs == 'specs_personID']
  
  if (!missing(devices)) {
    
    observed_mnd.dt <- observed_mnd.dt[get(nameDev) %in% devices]
    devIDs <- unique(observed_mnd.dt[[nameDev]])
    devNotFound <- setdiff(devices, devIDs)
    if (length(devNotFound) > 0) {
      
      stop(paste0('[simutils::get_mnd] The following devices have not been found in the dataset:', paste0(devNotFound, collapse = ', '), '\n'))
      
    }
  }
  
  
  if (!missing(t_range)) {
    
    namet <- names(observed_mnd.specs)[observed_mnd.specs == 'specs_time']
    observed_mnd.dt <- observed_mnd.dt[get(namet) %between% t_range]
    if (nrow(observed_mnd.dt) == 0) {
      
      stop('[simutils::get_mnd] No event data in the specified time range.\n')
      
    }
  }

  if (groundTruth == TRUE) {
    
    namet       <- names(individuals.specs)[individuals.specs == 'specs_time']
    nameDevice1 <- names(individuals.specs)[individuals.specs == 'specs_device_1']
    nameDevice2 <- names(individuals.specs)[individuals.specs == 'specs_device_2']
    individuals.dt <- melt(individuals.dt[
      , c(namet, namePerson, nameDevice1, nameDevice2), with = FALSE], 
      id.vars = c(namet, namePerson), measure.vars = c(nameDevice1, nameDevice2),
      value.name = nameDev)[
      !is.na(get(nameDev))][
      , variable := NULL] 
    groundTruth.sf <- dplyr::left_join(observed_mnd.sf, individuals.dt, 
                                       by = c(namet, nameDev))
    groundTruth.sf <- groundTruth.sf[, !(names(groundTruth.sf) %in% nameTA)]
return(groundT)    
   } 
  
  if (groundTruth == FALSE) {
    
     groundTruth.dt <- data.table(
       value = c(unname(observed_mnd.specs[observed_mnd.specs != 'geometry']),
                 'specs_personID', 'specs_x', 'specs_y'), 
       names = c(names(c(observed_mnd.specs[observed_mnd.specs != 'geometry'],
                         individuals.specs[individuals.specs == 'specs_personID'])),
                  'x', 'y'))
     groundTruth.dt <- groundTruth.dt[!duplicated(groundTruth.dt, by = 'value')]
     
     typeObsmnd     <- unname(unlist(lapply(sf::st_drop_geometry(observed_mnd.sf), class)))
     typeInd        <- typeof(eval(parse(text = paste0('simData$individuals$`', namePerson, '`'))))
     groundTruth.dt <- groundTruth.dt[, type := c(typeObsmnd, typeInd, 'double', 'double')]
     
     groundTruth           <- as.data.table(matrix(ncol = nrow(groundTruth.dt), nrow = 0))
     colnames(groundTruth) <- groundTruth.dt$names
     lapply(1:nrow(groundTruth.dt), function(i){
       groundTruth <- groundTruth[, eval(parse(text = paste0('`', groundTruth.dt$names[i], '` := as.', groundTruth.dt$type[i], '()')))]
     })
     
     groundTruth.sf <- sf::st_as_sf(groundTruth, coords = c('x', 'y'))
  }
  
  observed_mnd.sf <- observed_mnd.sf[, !(names(observed_mnd.sf) %in% c(nameTile, nameSpLong, nameNestingLong))]
  observed_mnd.dt <- sf::st_drop_geometry(observed_mnd.sf)
  output <- list()
  output$observed_mnd <- observed_mnd.dt
  output$groundTruth  <- groundTruth.sf
  
  return(output)
  
}