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
  
  mc <- match.call()
  mc[[1L]] <- validate_csv
  valid_csv <- eval(mc, parent.frame())
  if (!valid_csv) {
    
    stop('[simviz::read_csv] The csv file is not valid according to the xml file.\n')
    
  }
  
  xml <- read_xml(xmlFileName)
  xml.list <- as_list(xml)[[1]]
  
  colnames_xml <- unlist(purrr::map(xml.list, function(x){x[grep('ColName', names(x))]}))
  specnames_xml <- vapply(strsplit(names(colnames_xml), '.', fixed = TRUE), `[`, 1, FUN.VALUE = character(1))
  names(colnames_xml) <- specnames_xml
  if (grepl('[Ss]ignal', basename(csvFileName))) {
    
    noTiles <- as.integer(xml.list$specs_signal$noTiles[[1]])
    tilenames <- paste0('Tile', 0:(noTiles-1))
    names(tilenames) <- rep('specs_signal', noTiles)
    colnames_xml <- colnames_xml[-which(colnames_xml == 'Tile')]
    colnames_xml <- c(colnames_xml, tilenames)
    
  }
  
  colnames_csv <- names(fread(csvFileName, nrows = 0))
  
  types_xml <- unlist(purrr::map(xml.list, function(x){x[grep('type', names(x))]}))
  specnames_xml <- vapply(strsplit(names(types_xml), '.', fixed = TRUE), `[`, 1, FUN.VALUE = character(1))
  names(types_xml) <- specnames_xml
  types_xml <- unlist(purrr::map(types_xml, xmlTypes2RTypes))
  specs.dt <- data.table(
    spec = names(colnames_xml), colnames_xml = colnames_xml
  )
  
  types.dt <- data.table(
    spec = names(types_xml), types_xml = types_xml
  )[types_xml != '']  ## to supress the row on types (omnidirectional, directional) of antennas
  specs.dt <- specs.dt[types.dt, on = 'spec']
  specs.dt <- specs.dt[!duplicated(specs.dt, by = c('spec', 'colnames_xml'))]
  
  types_csv <- specs.dt[colnames_xml %in% colnames_csv][['types_xml']]
  names(types_csv) <- specs.dt[colnames_xml %in% colnames_csv][['colnames_xml']]
  
  csv.dt <- fread(csvFileName, colClasses = types_csv, header = TRUE, stringsAsFactors = FALSE)
  
  return(csv.dt)  
}