#' Compute totals for the number of entities (individuals, devices...).
#'
#' @param individuals.sf sf object with data about the individuals.
#'
#' @param what character vector denoting what total to compute. Possible values
#' are 'individuals', 'individuals_dev0', 'individuals_dev1', 'individuals_dev2'
#' , 'devices'.
#'
#' @param by character vector denoting the variable names to group the
#' computation.
#'
#' @param by_values named list (names given by variable \code{by}) with the
#' values of each by variable. The default value is \code{NULL}, in which case
#' values are taken from \code{individuals.sf}.
#'
#' @details Return a data.table with the totals by group. Notice than when
#' not specifying the values of the \code{by} argument, missing values are not
#' included in the final \code{data.table}, i.e. amounting to implicit value 0.
#' Otherwise, all values of the \code{by} variable will be included with the
#' corresponding increase of computational overhead to explicitly include zero
#' values.
#'
#' @rdname compute_total
#'
#' @name compute_total
#'
#' @import data.table
#'
#' @examples
#' filename_map <- c(
#'   xml = system.file("extdata/input_files", "map.xml", package = "simutils"),
#'   xsd = ""
#' )
#'
#' filename_network <- c(
#'   csv = system.file("extdata/output_files/antennas.csv", package = "simutils"),
#'   xml = system.file("extdata/metadata/output_files/antennas_dict.xml", package = "simutils")
#' )
#'
#' filename_signal <- c(
#'   csv = system.file("extdata/output_files/SignalMeasure_MNO1.csv", package = "simutils"),
#'   xml = system.file("extdata/metadata/output_files/SignalMeasure_dict.xml", package = "simutils")
#' )
#'
#' filename_coverage <- c(
#'   csv = system.file("extdata/output_files", "AntennaCells_MNO1.csv", package = "simutils"),
#'   xml = system.file("extdata/metadata/output_files/AntennaCells_dict.xml", package = "simutils")
#' )
#'
#' filename_events <- c(
#'   csv = system.file("extdata/output_files/AntennaInfo_MNO_MNO1.csv", package = "simutils"),
#'   xml = system.file("extdata/metadata/output_files/events_dict.xml", package = "simutils")
#' )
#'
#' filename_grid <- c(
#'   csv = system.file("extdata/output_files/grid.csv", package = "simutils"),
#'   xml = system.file("extdata/metadata/output_files/grid_dict.xml", package = "simutils")
#' )
#'
#' filename_individ <- c(
#'   csv = system.file("extdata/output_files/persons.csv", package = "simutils"),
#'   xml = system.file("extdata/metadata/output_files/persons_dict.xml", package = "simutils")
#' )
#'
#' filenames <- list(
#'   map                = filename_map,
#'   network_parameters = filename_network,
#'   signal             = filename_signal,
#'   events             = filename_events,
#'   coverage_cells     = filename_coverage,
#'   grid               = filename_grid,
#'   individuals        = filename_individ
#' )
#'
#' simData <- simutils::read_simData(filenames, crs = 2062)
#'
#' # Counting individuals by subregion and time
#' compute_total(simData$individuals, "individuals", by = c("t", "Subregion_long"))
#'
#' # Counting individuals by subregion and time with 0 devices
#' compute_total(simData$individuals, "individuals_dev0", by = c("t", "Subregion_long"))[]
#'
#' # Counting individuals by subregion and time with 1 device
#' compute_total(simData$individuals, "individuals_dev1", by = c("t", "Subregion_long"))[]
#'
#' # Counting individuals by subregion and time with 2 devices
#' compute_total(simData$individuals, "individuals_dev2", by = c("t", "Subregion_long"))[]
#'
#' # Counting devices by subregion and time
#' compute_total(simData$individuals, "devices", by = c("t", "Subregion_long"))[]
#'
#' # Counting multiple totals by subregion and time
#' totals <- c("individuals", "individuals_dev0", "devices")
#' compute_total(simData$individuals, totals, by = c("t", "Subregion_long"))[]
#'
#'
#' # Counting devices by tile and time
#' compute_total(simData$individuals, "devices",
#'   by = c("t", "Tile ID"),
#'   by_values = list(`Tile ID` = 0:1599)
#' )[]
#'
#' @export
compute_total <- function(individuals.sf, what, by, by_values = NULL) {
  what_allwdVal <- c(
    "individuals", "individuals_dev0", "individuals_dev1",
    "individuals_dev2", "devices"
  )

  what_wrong <- setdiff(what, what_allwdVal)

  if (length(what_wrong) > 0) {
    stop(paste0(
      "[simutils::compute_total] The following variables in what are not allowed:",
      paste0(what_wrong, collapse = ", ")
    ))
  }

  by_missing <- setdiff(by, names(individuals.sf))

  if (length(by_missing) > 0) {
    stop(paste0(
      "[simutils::compute_total] The following variables in by are missing in the data set:",
      paste0(by_missing, collapse = ", ")
    ))
  }

  individuals.dt <- sf::st_drop_geometry(individuals.sf)


  if (is.null(by_values)) {
    by_values.list <- vector("list", length(by))
    names(by_values.list) <- by
    for (by_var in by) {
      by_values.list[[by_var]] <- sort(unique(individuals.dt[[by_var]]))
    }
  } else {
    if (any(!names(by_values) %in% by)) {
      stop("[simutils::compute_total] The names of the list by_values must be the variables in by.")
    }

    values_nonSpec <- setdiff(by, names(by_values))
    by_values.list <- vector("list", length(by))
    names(by_values.list) <- by
    for (by_var in by) {
      if (by_var %in% values_nonSpec) {
        by_values.list[[by_var]] <- sort(unique(individuals.dt[[by_var]]))
      } else {
        by_values.list[[by_var]] <- sort(by_values[[by_var]])
      }
    }
  }

  master.dt <- data.table(Reduce(function(x, y) {
    expand.grid(x, y, stringsAsFactors = FALSE)
  }, by_values.list))
  setnames(master.dt, by)

  totals.list <- lapply(what, function(wt) {

    # what == individuals
    if (wt == "individuals") {
      N.dt <- individuals.dt[
        , .N,
        by = by
      ]
      setnames(N.dt, "N", wt)

      N.dt <- merge(N.dt, master.dt, by = by, all = TRUE)[
        is.na(get(wt)), (wt) := 0
      ]
      return(N.dt)
    }

    # what == individuals with 0 devices
    if (wt == "individuals_dev0") {
      var_ndev <- names(individuals.dt)[which(attr(individuals.dt, "specs") == "specs_ndev")]
      N.dt <- individuals.dt[
        get(var_ndev) == 0
      ][
        , .N,
        by = by
      ]
      setnames(N.dt, "N", wt)
      N.dt <- merge(N.dt, master.dt, by = by, all = TRUE)[
        is.na(get(wt)), (wt) := 0
      ]
      return(N.dt)
    }

    # what == individuals with 0 devices
    if (wt == "individuals_dev1") {
      var_ndev <- names(individuals.dt)[which(attr(individuals.dt, "specs") == "specs_ndev")]
      N.dt <- individuals.dt[
        get(var_ndev) == 1
      ][
        , .N,
        by = by
      ]
      setnames(N.dt, "N", wt)
      N.dt <- merge(N.dt, master.dt, by = by, all = TRUE)[
        is.na(get(wt)), (wt) := 0
      ]
      return(N.dt)
    }

    # what == individuals with 2 devices
    if (wt == "individuals_dev2") {
      var_ndev <- names(individuals.dt)[which(attr(individuals.dt, "specs") == "specs_ndev")]
      N.dt <- individuals.dt[
        get(var_ndev) == 2
      ][
        , .N,
        by = by
      ]
      setnames(N.dt, "N", wt)
      N.dt <- merge(N.dt, master.dt, by = by, all = TRUE)[
        is.na(get(wt)), (wt) := 0
      ]
      return(N.dt)
    }

    # what == devices
    if (wt == "devices") {
      var_ndev <- names(individuals.dt)[which(attr(individuals.dt, "specs") == "specs_ndev")]
      N.dt <- individuals.dt[
        , list(devices = as.integer(sum(get(var_ndev)))),
        by = by
      ]
      N.dt <- merge(N.dt, master.dt, by = by, all = TRUE)[
        is.na(get(wt)), (wt) := 0
      ]
      return(N.dt)
    }
  }) # totals.list
  N_combined.dt <- Reduce(function(x, y) {
    merge(x, y, by = intersect(names(x), names(y)))
  }, totals.list)
  return(N_combined.dt)
}
