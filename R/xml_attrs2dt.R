#' Create a data.table from a file metadata xml.
#'
#' @param xmlType character vector with the name of the type of an xml element
#'
#' @param xmlname character vector of length 1 with the name of the xml file
#'
#' @param dataset character vector of length 1 with the dataset name
#'
#' @import sf xml2 data.table
#'
#' @examples
#' xmlname <- system.file(
#'   "extdata/metadata/output_files", "antennas_dict.xml",
#'   package = "simutils"
#' )
#' xml_attrs2dt(xmlname, "antennas")
#'
#' xmlname <- system.file(
#'   "extdata/metadata/output_files", "persons_dict.xml",
#'   package = "simutils"
#' )
#' xml_attrs2dt(xmlname, "individuals")
#'
#' @rdname xml_attrs2dt
#'
#' @export
xmlTypes2RTypes <- function(xmlType) {
  rType <- ""
  if (xmlType == "integer") {
    rType <- "integer"
  } else if (xmlType == "string") {
    rType <- "character"
  } else if (xmlType == "decimal") {
    rType <- "numeric"
  } else if (xmlType == "unsignedInt") {
    rType <- "integer"
  }
  return(rType)
}

#' @rdname xml_attrs2dt
#'
#' @export
xml_attrs2dt <- function(xmlname, dataset) {
  if (dataset == "antennas" || dataset == "antenna_cells" || dataset == "grid" || dataset == "individuals" || dataset == "signal") {
    xml <- read_xml(xmlname)
    ant <- as_list(xml)[[1]]
    # get column names and column types
    antennas_colNames <- c()
    antennas_typeNames <- c()
    for (i in 1:length(ant)) {
      k <- 0
      l <- 0
      for (j in 1:length(ant[[i]])) {
        nodeName <- names(ant[[i]])[j]
        if (endsWith(nodeName, "ColName")) {
          colName <- (ant[[i]])[[j]][[1]]
          antennas_colNames <- c(antennas_colNames, colName)
          k <- k + 1
        }
        if (endsWith(nodeName, "value_type")) {
          typeName <- (ant[[i]])[[j]][[1]]
          antennas_typeNames <- c(antennas_typeNames, typeName)
          l <- l + 1
        }
      }
      if (k > l) {
        num <- k - l
        lastType <- antennas_typeNames[length(antennas_typeNames)]
        for (e in 1:num) {
          antennas_typeNames <- c(antennas_typeNames, typeName)
        }
      }
    }
    antennas_typeNames <- sapply(antennas_typeNames, xmlTypes2RTypes, USE.NAMES = FALSE)

    dt <- as.data.table(cbind(name = antennas_colNames, class = antennas_typeNames))
    return(dt[])
  }

  stop("[xml_attrs2dt] dataset not yet implemented.\n")
}
