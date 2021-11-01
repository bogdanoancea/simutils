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
#' # simulation file (by filename)
#' csv_fn <- file.path(rootPath, "output_files", "antennas.csv")
#' xml_fn <- file.path(rootPath, "metadata/output_files", "antennas_dict.xml")
#' dt <- read_csv(xml_fn, csv_fn)
#' str(dt)
#' 
#' @include validate_csv.R
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
  
  colnames_csv <- names(fread(csvFileName, nrows = 0))
  invalid_colnames <- colnames_csv[!colnames_csv %in% colnames_xml] 
  
  
  types_xml <- unlist(purrr::map(xml.list, function(x){x[grep('type', names(x))]}))
  specnames_xml <- vapply(strsplit(names(types_xml), '.', fixed = TRUE), `[`, 1, FUN.VALUE = character(1))
  names(types_xml) <- specnames_xml
  types_xml <- unlist(purrr::map(types_xml, xmlTypes2RTypes))
  
  specs.dt <- data.table(
    spec = names(colnames_xml), colnames_xml = colnames_xml
  )
  types.dt <- data.table(
    spec = names(types_xml), types_xml = types_xml
  )[types_xml != '']  ## to supress the row on types of antennas
  specs.dt <- specs.dt[types.dt, on = 'spec']
  
  types_csv <- specs.dt[colnames_xml %in% colnames_csv][['types_xml']]
  names(types_csv) <- specs.dt[colnames_xml %in% colnames_csv][['colnames_xml']]
  
  csv.dt <- fread(csvFileName, colClasses = 'character', header = TRUE)
  cols_num_csv <- names(types_csv)[types_csv %in% c('numeric', 'integer')]
  for (colnm in cols_num_csv){
    
    cat(paste0('Fixing numeric type of column ', colnm, '...'))
    if (any(regexpr('[:digit:]*', csv.dt[[colnm]]) != 1)) {
      
      stop(paste0('[validate_csv::validate_csv] Column ', colnm, ' has non-numeric values.'))
    }
    cat(' ok.\n')
    if (types_csv[colnm] == 'numeric') {
      
      csv.dt[, (colnm) := as.numeric(get(colnm))]
      
    }
    if (types_csv[colnm] == 'integer') {
      
      csv.dt[, (colnm) := as.integer(get(colnm))]
      
    }
    
  }
  
  
  return(csv.dt)  
}
