#' @title Validate an xml file using an xsd schema file.
#' 
#' @description \code{checkXML} validates an xml file using an xsd schema 
#' file.
#' 
#' This function is a wrapper for a validator written in Java. Thus, it needs a 
#' jar file, which is included in this package. If the xml file does not conform to
#'  the schema definition, it provides information where the error is.
#'  
#' The xsdFileName and xmlFileName arguments must be provided with absolute paths. 
#' 
#' 
#' @param xsdFileName Path of the xsd file containing the schema.
#' 
#' @param xmlFileName Path of the xml file.
#' 
#' 
#' @details Return 0 if xml file is validated and a non-zero value otherwise.
#' 
#' @import rJava
#' 
#' @examples 
#' rootPath <- system.file(package = "simutils")  
#' 
#' # xml file 1
#' xsdName  <- file.path(rootPath, 
#'     "extdata/metadata/input_files/schema_definition", "antennas_dict.xsd")
#' xmlFile  <- file.path(rootPath, "extdata/input_files", "antennas.xml")
#' checkXML(xsdName, xmlFile)
#' 
#' # xml file 2
#' xsdName  <- file.path(rootPath, 
#'     "extdata/metadata/input_files/schema_definition", "simulation_dict.xsd")
#' xmlFile  <- file.path(rootPath, "extdata/input_files", "simulation.xml")
#' checkXML(xsdName, xmlFile)
#' 
#' @export
checkXML <- function(xsdFileName, xmlFileName) {
  hjw <- .jnew("xsd/Parser")
  out <- .jcall(hjw,"I", "testXML", xsdFileName, xmlFileName)
  return(out)
}
