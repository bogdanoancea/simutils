#' @title Update simulation xml input files.
#'
#' @description This function takes an xml input file for the simulator and
#' updates it according to the set of new parameters specified in the input
#' parameter list \code{newParam}. The schema definition of the xml file is also
#' required as an input parameter to validate the structure of the output file.
#'
#' @param xmlSimInput Object of class \code{xml_document} from package
#' \code{xml2} or of class \code{character} specifying the path of the
#' simulation input xml file to update.
#'
#' @param newParam \code{list} with the new values of the parameters to be
#' updated
#'
#' @param xsdName \code{character} of length 1 with the name of xsd file
#' containing the schema definition of the xml input file.
#'
#' @param newFileName \code{character} of length 1 with the name of the updated
#' xml input file.
#'
#' @details Return an \code{xml-document} object of package \code{xml2} with
#' the parameters updated according to the specified input parameters. If
#' \code{newFileName} is specified, this object is written in the corresponding
#'  xml file.
#'
#' @include validate_xml.R flatten_deepest.R list_deepest.R
#'
#' @import xml2
#' @import fs
#'
#' @examples
#' library(fs)
#' rootPath <- system.file(package = "simutils")
#' xmlSimInput <- file.path(rootPath, "extdata/input_files", "simulation.xml")
#' newParam.lst <- list(
#'   end_time = 11,
#'   movement_pattern = structure(list(
#'     manhattan_grid = structure(
#'       list(x_step = 40, y_step = 40, x_origin = 0, y_origin = 0),
#'       type = "home_work_manhattan"
#'     )
#'   ))
#' )
#' xsdName <- file.path(
#'   rootPath,
#'   "extdata/metadata/input_files/schema_definition", "simulation_dict.xsd"
#' )
#' newSimInputFile <- file.path(path_home(), "newSimulation.xml")
#' update_SimInput(
#'   xmlSimInput = xmlSimInput,
#'   newParam = newParam.lst,
#'   xsdName = xsdName,
#'   newFileName = newSimInputFile
#' )
#'
#' @export
update_SimInput <- function(xmlSimInput, newParam, xsdName, newFileName = NULL) {
  xmlSimInputName <- deparse(substitute(xmlSimInput))
  cat(paste0("[simutils::update_SimInput] Validating ", xmlSimInputName, "...   "))

  xmlValid <- validate_xml(xsdName, xmlSimInput)
  if (xmlValid != TRUE) {
    stop(paste0("[simutils::update_SimInput] The xml object ", xmlSimInputName, " is not valid.\n"))
  }
  if (inherits(xmlSimInput, "xml_document")) {
    xmlInput.lst <- flatten_deepest(as_list(xmlSimInput))
  }

  if (inherits(xmlSimInput, "character")) {
    xmlInput.lst <- flatten_deepest(as_list(read_xml(xmlSimInput)))
  }

  newxmlInput.lst <- list_deepest(purrr::list_modify(xmlInput.lst, newParam))
  newxmlInput.xml <- as_xml_document(newxmlInput.lst)
  cat("[simutils::update_SimInput] Validating updated xml object... \n")
  newxmlValid <- validate_xml(xsdName, newxmlInput.xml)
  if (newxmlValid != TRUE) {
    stop(paste0("[simutils::update_SimInput] The new xml object ", xmlSimInputName, " is not valid.\n"))
  }
  if (!is.null(newFileName)) {
    write_xml(newxmlInput.xml, newFileName, encoding = "UTF-8")
    cat(paste0("[simutils::update_SimInput] New xml file written in ", newFileName, ".\n"))
  }

  cat("\n")

  return(newxmlInput.xml)
}
