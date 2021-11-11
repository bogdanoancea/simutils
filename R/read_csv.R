#' @title Read a csv file according to a schema specified in an xml file.
#' 
#' @description \code{read_csv} reads a csv file assigning column names and 
#' column types according to the specifications in an xml file.
#' 
#' 
#' @param xmlFileName File name of the xml file containing the schema for the 
#' csv file.
#' 
#' @param csvFileName File name of the csv file to validate.
#' 
#' 
#' @details Return a \code{data.table} with the contents of the csv file.
#' 
#' @examples 
#' rootPath <- file.path(system.file(package = "simutils"), 'extdata')  
#' 
#' # simulation file
#' csv_fn <- file.path(rootPath, "output_files", "antennas.csv")
#' xml_fn <- file.path(rootPath, "metadata/output_files", "antennas_dict.xml")
#' dt <- read_csv(xml_fn, csv_fn)
#' str(dt)
#' 
#' # coverage file
#' csv_fn <- file.path(rootPath, "output_files", "AntennaCells_MNO1.csv")
#' xml_fn <- file.path(rootPath, "metadata/output_files", "antennaCells_dict.xml")
#' dt <- read_csv(xml_fn, csv_fn)
#' str(dt)

#' 
#' @include validate_csv.R xml_attrs2dt.R
#' 
#' @import data.table
#'  
#' @export
read_csv <- function(xmlFileName, csvFileName) {
  
  spec <- variable <- variable2 <- var_coincidence1 <- var_coincidence2 <-
    var_coincidence <- NULL
  
  mc <- match.call()
  mc[[1L]] <- validate_csv
  valid_csv <- eval(mc, parent.frame())
  if (!valid_csv) {
    
    stop('[simviz::read_csv] The csv file is not valid according to the xml file.\n')
    
  }
  
  xml <- read_xml(xmlFileName)
  xml.list <- as_list(xml)[[1]]
  colnames_xml <- unlist(purrr::map(xml.list, function(x){x[grep('ColName', names(x))]}))
  specnames_xml <- names(colnames_xml)
  names(colnames_xml) <- specnames_xml

  if (grepl('[Ss]ignal', basename(csvFileName))) {
    
    noTiles <- as.integer(xml.list$specs_signal$noTiles[[1]])
    tilenames <- paste0('Tile', 0:(noTiles-1))
    names(tilenames) <- rep('specs_signal', noTiles)
    colnames_xml <- colnames_xml[-which(colnames_xml == 'Tile')]
    colnames_xml <- c(colnames_xml, tilenames)
    
  }
  
  specs.dt <- data.table(
    spec = names(colnames_xml), colnames_xml = colnames_xml
  )[
    , c('spec', 'variable') := tstrsplit(spec, split = '.', fixed = TRUE)][
    , variable := gsub('ColName', '', variable)  
    ]
  
  types_xml <- unlist(purrr::map(xml.list, function(x){x[grep('value_type', names(x), fixed = TRUE)]}))
  specnames_xml <- names(types_xml)
  names(types_xml) <- specnames_xml
  types_xml <- unlist(purrr::map(types_xml, xmlTypes2RTypes))

  types.dt <- data.table(
    spec = names(types_xml), types_xml = types_xml
  )[
    , spec := gsub('value_type', '', spec, fixed = TRUE)][
    , c('spec', 'variable2') := tstrsplit(spec, split = '.', fixed = TRUE)][
    , variable2 := gsub('_', '', variable2)]
  

  specs.dt <- merge(specs.dt, types.dt, all = TRUE)[, row := .I]
  specs.dt[
    , var_coincidence1 := (grep(variable, variable2) > 0) * 1L, by = 'row'][
    , var_coincidence2 := (grep(variable2, variable) > 0) * 1L, by = 'row'][
    , var_coincidence  := rowSums(.SD, na.rm= TRUE), .SDcols = c('var_coincidence1', 'var_coincidence2')]
    specs.dt <- specs.dt[is.na(variable2) | var_coincidence > 0]
  colnames_csv <- names(fread(csvFileName, nrows = 0))
  types_csv.dt <- specs.dt[colnames_xml %in% colnames_csv]
  types_csv <- types_csv.dt[['types_xml']]
  names(types_csv) <- specs.dt[colnames_xml %in% colnames_csv][['colnames_xml']]
  
  csv.dt <- fread(csvFileName, colClasses = types_csv, header = TRUE, stringsAsFactors = FALSE)
  
  specs_csv <- types_csv.dt[colnames_xml %in% colnames_csv][['spec']]
  names(specs_csv) <- types_csv.dt$colnames_xml
  specs_csv <- specs_csv[names(csv.dt)]
  setattr(csv.dt, 'specs', specs_csv)
  
  if (grepl('[Pp]ersons', basename(csvFileName))) {

   colName <- getDeviceIDColName(xmlFileName, 'individuals')  
   devIDs <- csv.dt[, tstrsplit(get(colName), split = '-')]
   setnames(devIDs, paste0('Device', 1:dim(devIDs)[2]))
   csv.dt <- data.table(csv.dt, devIDs)[
     , (colName) := NULL]
   specs_csv <- specs_csv[-which(names(specs_csv) == colName)]
   setattr(csv.dt, 'specs', c(unname(specs_csv), paste0('specs_device_', 1:dim(devIDs)[2])))
  }
  return(csv.dt)

}