#' @title Validate an xml file using an xsd schema file.
#'
#' @description \code{validate_xml} validates an xml file using an xsd schema
#' file.
#'
#' This function is a wrapper for a validator written in Java. Thus, it needs a
#' jar file, which is included in this package. If the xml file does not conform
#'  to the schema definition, it provides information where the error is.
#'
#' The xsdFileName and xmlFileName arguments must be provided with absolute
#' paths.
#'
#'
#' @param xsdFileName Path of the xsd file containing the schema.
#'
#' @param xmlObject Object of class \code{xml-document} from package \code{xml2}
#' or \code{character} of length 1 with the path of the xml file.
#'
#'
#' @details Return 0 if xml file is validated and a non-zero value otherwise.
#'
#' @import rJava
#'
#' @examples
#' rootPath <- file.path(system.file(package = "simutils"), "extdata")
#'
#' # simulation file (by filename)
#' xml_fn <- file.path(rootPath, "input_files", "simulation.xml")
#' xsd_fn <- file.path(rootPath, "metadata/input_files/schema_definition", "simulation_dict.xsd")
#' validate_xml(xsd_fn, xml_fn)
#'
#' # antennas file (by filename)
#' xml_fn <- file.path(rootPath, "input_files", "antennas.xml")
#' xsd_fn <- file.path(rootPath, "metadata/input_files/schema_definition", "antennas_dict.xsd")
#' validate_xml(xsd_fn, xml_fn)
#'
#' # persons file (by filename)
#' xml_fn <- file.path(rootPath, "input_files", "persons.xml")
#' xsd_fn <- file.path(rootPath, "metadata/input_files/schema_definition", "persons_dict.xsd")
#' validate_xml(xsd_fn, xml_fn)
#'
#' # probabilities file (by filename)
#' xml_fn <- file.path(rootPath, "input_files", "probabilities.xml")
#' xsd_fn <- file.path(rootPath, "metadata/input_files/schema_definition", "probabilities_dict.xsd")
#' validate_xml(xsd_fn, xml_fn)
#'
#' # antennaCells_dict file (by filename)
#' xml_fn <- file.path(rootPath, "metadata/output_files", "antennaCells_dict.xml")
#' xsd_fn <- file.path(rootPath, "metadata/output_files/schema_definition", "antennaCells_dict.xsd")
#' validate_xml(xsd_fn, xml_fn)
#'
#' # antennas_dict file (by filename)
#' xml_fn <- file.path(rootPath, "metadata/output_files", "antennas_dict.xml")
#' xsd_fn <- file.path(rootPath, "metadata/output_files/schema_definition", "antennas_dict.xsd")
#' validate_xml(xsd_fn, xml_fn)
#'
#' # events_dict file (by filename)
#' xml_fn <- file.path(rootPath, "metadata/output_files", "events_dict.xml")
#' xsd_fn <- file.path(rootPath, "metadata/output_files/schema_definition", "events_dict.xsd")
#' validate_xml(xsd_fn, xml_fn)
#'
#' # grid_dict file (by filename)
#' xml_fn <- file.path(rootPath, "metadata/output_files", "grid_dict.xml")
#' xsd_fn <- file.path(rootPath, "metadata/output_files/schema_definition", "grid_dict.xsd")
#' validate_xml(xsd_fn, xml_fn)
#'
#' # persons_dict file (by filename)
#' xml_fn <- file.path(rootPath, "metadata/output_files", "persons_dict.xml")
#' xsd_fn <- file.path(rootPath, "metadata/output_files/schema_definition", "persons_dict.xsd")
#' validate_xml(xsd_fn, xml_fn)
#'
#' # SignalMeasure_dict file (by filename)
#' xml_fn <- file.path(rootPath, "metadata/output_files", "SignalMeasure_dict.xml")
#' xsd_fn <- file.path(rootPath, "metadata/output_files/schema_definition", "SignalMeasure_dict.xsd")
#' validate_xml(xsd_fn, xml_fn)
#'
#' @export
validate_xml <- function(xsdFileName, xmlObject) {
  if (inherits(xmlObject, "xml_document")) {
    xmlFileName <- deparse(substitute(xmlObject))
    write_xml(xmlObject, xmlFileName)
  }

  if (inherits(xmlObject, "character")) {
    xmlFileName <- xmlObject
  }

  hjw <- .jnew("xsd/Parser")
  out <- .jcall(hjw, "I", "testXML", xsdFileName, xmlFileName)

  if (inherits(xmlObject, "xml_document")) {
    file.remove(xmlFileName)
  }

  out <- ifelse(out == 0, TRUE, FALSE)

  return(out)
}
