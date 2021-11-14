#' Compute od matrix for different entities (individuals, devices...).
#'
#' @param individuals.sf sf object with data about the individuals.
#' 
#' @param what character vector denoting what entity to compute. Possible values
#' are 'individuals', 'individuals_dev0', 'individuals_dev1', 'individuals_dev2'
#' , 'devices'.
#' 
#' @param by character vector denoting the variable names to group the 
#' computation.
#' 
#' @details Return a data.table with the origin-destination matrices by group.
#' 
#' @rdname compute_odmatrix
#'
#' @name compute_odmatrix
#' 
#' @import sf data.table
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
#' simData <- simutils::read_simData(filenames, crs = 2062)
#' # Counting individuals by subregion and time
#' compute_odmatrix(simData$individuals, 'individuals', 
#'               by = c('t', 'Subregion_long'))
#'
#' # Counting individuals by subregion and time with 0 devices
#' compute_odmatrix(simData$individuals, 'individuals_dev0', 
#'               by = c('t', 'Subregion_long'))
#' 
#' # Counting individuals by subregion and time with 1 device
#' compute_odmatrix(simData$individuals, 'individuals_dev1', 
#'               by = c('t', 'Subregion_long'))
#'
#' # Counting individuals by subregion and time with 2 devices
#' compute_total(simData$individuals, 'individuals_dev2', 
#'compute_odmatrixby = c('t', 'Subregion_long'))
#' 
#' # Counting devices by subregion and time
#' compute_odmatrix(simData$individuals, 'devices', 
#'               by = c('t', 'Subregion_long'))
#' 
#' # Counting multiple totals by subregion and time
#' totals <- c('individuals', 'individuals_dev0', 'devices')
#' compute_odmatrix(simData$individuals, totals, by = c('t', 'Subregion_long'))
#' 
#' @export
compute_odmatrix <- function(individuals.sf, what, by){
  
  
  what_allwdVal <- c(
    'individuals', 'individuals_dev0', 'individuals_dev1', 
    'individuals_dev2', 'devices'
  )
  
  what_wrong <- setdiff(what, what_allwdVal)
  
  if (length(what_wrong) > 0) {
    
    stop(paste0('[simutils::compute_odmatrix] The following variables in what are not allowed:', 
                paste0(what_wrong, collapse = ', ')))
    
  }
  
  by_missing <- setdiff(by, names(individuals.sf))
  
  if (length(by_missing) > 0) {
    
    stop(paste0('[simutils::compute_odmatrix] The following variables in by are missing in the data set:', 
                paste0(by_missing, collapse = ', ')))
    
  }
  
  individuals.dt <- sf::st_drop_geometry(individuals.sf)
  var_t <- names(individuals.dt)[which(attr(individuals.dt, 'specs') == 'specs_time')]
  if (!var_t %in% by) {
    
    stop('[simutils::compute_odmatrix] A variable denoting time is missing in the data set.')
    
  }

  times <- sort(as.numeric(unique(individuals.dt[[var_t]])))
  time_initial <- times[1]
  time_final   <- times[length(times)]
  time_increm  <- diff(times)
  time_increm.dt <- data.table(t = times[-length(times)], t_increm = time_increm)
  setnames(time_increm.dt, 't', var_t)
  
  
  by_values.list <- vector('list', length(by))
  names(by_values.list) <- by
  for (by_var in by) {
    
    by_values.list[[by_var]] <- unique(individuals.dt[[by_var]])
    
  }
  master.dt <- data.table(Reduce(expand.grid, by_values.list))
  setnames(master.dt, by)

  od_to.dt <- merge(individuals.dt, time_increm.dt, by = var_t, all.x = TRUE)[
    , time_pk := get(var_t) + t_increm][
    , t_increm := NULL]
  setnames(od_to.dt, by, paste0('to_', by))
  setnames(od_to.dt, c('time_pk', paste0('to_', var_t)), c(paste0('to_', var_t), 'time_pk'))

  od_from.dt <- copy(individuals.dt)[
    , time_pk := get(var_t)]
  setnames(od_from.dt, by, paste0('from_', by))
  
  od.dt <- merge(od_from.dt, od_to.dt, by = intersect(names(od_from.dt), names(od_to.dt)))[
    !is.na(get(paste0('to_', var_t)))]
  
  names_from   <- paste0('from_', by)
  names_to     <- paste0('to_', by)
  names_tofrom <- c(names_from, names_to)
  
  
  od.list <- lapply(what, function(wt){
  
    if (wt == 'individuals') {
      
      od.dt <- od.dt[
        , .SD, .SDcols = names_tofrom][
        , .N, by = names_tofrom][order(get(paste0('from_', var_t)))]
      setnames(od.dt, 'N', wt)
      
      od.dt <- merge(od.dt, master.dt, by.x = names_from, by.y = by, all = TRUE)[
        !is.na(get(paste0('to_', var_t)))][
        is.na(get(wt)), (wt) := 0]
      
      return(od.dt)
    }
    
    if (wt == 'individuals_dev0') {
      
      var_ndev <- names(individuals.dt)[which(attr(individuals.dt, 'specs') == 'specs_ndev')]
      od.dt <- od.dt[
        get(var_ndev) == 0][
          , .N, by = names_tofrom][order(get(paste0('from_', var_t)))]
      setnames(od.dt, 'N', wt)
      
      
      od.dt <- merge(od.dt, master.dt, by.x = names_from, by.y = by, all = TRUE)[
        !is.na(get(paste0('to_', var_t)))][
          is.na(get(wt)), (wt) := 0]
      
      return(od.dt)    
      
    }
    
    if (wt == 'individuals_dev1') {
      
      var_ndev <- names(individuals.dt)[which(attr(individuals.dt, 'specs') == 'specs_ndev')]
      od.dt <- od.dt[
        get(var_ndev) == 1][
          , .N, by = names_tofrom][order(get(paste0('from_', var_t)))]
      setnames(od.dt, 'N', wt)
      
      od.dt <- merge(od.dt, master.dt, by.x = names_from, by.y = by, all = TRUE)[
        !is.na(get(paste0('to_', var_t)))][
          is.na(get(wt)), (wt) := 0]
      
      return(od.dt)    
      
    }
    
    if (wt == 'individuals_dev2') {
      
      var_ndev <- names(individuals.dt)[which(attr(individuals.dt, 'specs') == 'specs_ndev')]
      od.dt <- od.dt[
        get(var_ndev) == 2][
          , .N, by = names_tofrom][order(get(paste0('from_', var_t)))]
      setnames(od.dt, 'N', wt)
      
      od.dt <- merge(od.dt, master.dt, by.x = names_from, by.y = by, all = TRUE)[
        !is.na(get(paste0('to_', var_t)))][
          is.na(get(wt)), (wt) := 0]
      
      return(od.dt)    
      
    }
    
    if (wt == 'devices') {
      
      var_ndev <- names(individuals.dt)[which(attr(individuals.dt, 'specs') == 'specs_ndev')]
      od.dt <- od.dt[
        , list(devices = sum(get(var_ndev))), by = names_tofrom][order(get(paste0('from_', var_t)))]
  
      od.dt <- merge(od.dt, master.dt, by.x = names_from, by.y = by, all = TRUE)[
        !is.na(get(paste0('to_', var_t)))][
          is.na(get(wt)), (wt) := 0]
      
      return(od.dt)    
      
    }
  
  })
    
  N_combined.dt <- Reduce(function(x, y){merge(x, y, by = intersect(names(x), names(y)))}, od.list)
  return(N_combined.dt)
  
}