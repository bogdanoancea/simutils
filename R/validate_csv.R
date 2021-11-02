#' @title Validate a csv file using an xml file as schema.
#' 
#' @description \code{validate_csv} validates a csv file using an xml file as 
#' schema, i.e. specifying its column names and value ranges (not yet 
#' implemented).
#' 
#' 
#' @param xmlFileName File name of the xml file containing the schema for the 
#' csv file.
#' 
#' @param csvFileName File name of the csv file to validate.
#' 
#' 
#' @details Return \code{TRUE} if the csv file is validated, otherwise, it 
#' returns \code{FALSE}. 
#' 
#' @examples 
#' rootPath <- file.path(system.file(package = "simutils"), 'extdata')  
#' 
#' # antennas file
#' csv_fn <- file.path(rootPath, "output_files", "antennas.csv")
#' xml_fn <- file.path(rootPath, "metadata/output_files", "antennas_dict.xml")
#' validate_csv(xml_fn, csv_fn)
#'  
#' @export
validate_csv <- function(xmlFileName, csvFileName) {
  
  xml <- read_xml(xmlFileName)
  xml.list <- as_list(xml)[[1]]

  csv_fn <- xml.list$specs_file$fileName[[1]]

  if (basename(csvFileName) != csv_fn) {
    
    stop('[simviz::validate_csv] The name of the csv file is not valid.')
  }

  colnames_xml <- unlist(purrr::map(xml.list, function(x){x[grep('ColName', names(x))]}))
  specnames_xml <- vapply(strsplit(names(colnames_xml), '.', fixed = TRUE), `[`, 1, FUN.VALUE = character(1))
  names(colnames_xml) <- specnames_xml

  if (grepl('[Ss]ignal', csv_fn)) {
    
    noTiles <- as.integer(xml.list$specs_signal$noTiles[[1]])
    tilenames <- paste0('Tile', 0:(noTiles-1))
    names(tilenames) <- rep('specs_signal', noTiles)
    colnames_xml <- colnames_xml[-which(colnames_xml == 'Tile')]
    colnames_xml <- c(colnames_xml, tilenames)
    
  }
  colnames_csv <- names(fread(csvFileName, nrows = 0))
  invalid_colnames <- colnames_csv[!colnames_csv %in% colnames_xml] 

  if (length(invalid_colnames) > 0) {
    
    stop(paste0('[simviz::validate_csv] Column names ', 
                paste0(invalid_colnames, collapse = ', '),
                 ' are not valid column names.'))
    
  }
  
  cat('[simviz::validate_csv] The column names are valid.\n')
  return(TRUE)
  
  # We should consider the possibility of specifying a range element for each 
  # variable in the xml file and check these ranges for each variable
  
}
