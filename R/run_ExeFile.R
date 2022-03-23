#' @title Run an MND simulation using the simulator.exe file.
#'
#' @description \code{run_ExeFile} reads the simulator path to the executable file of the simulator
#' of mobile network event data and the input file parameters to execute the simulator.exe from this
#'  path.
#'
#' If the executable file doesn't exist, the function will donwload it automatically from the
#' Assests  section of this URL in github.com (for version 1.2.0):
#'
#' https://github.com/bogdanoancea/simulator/releases/tag/1.2.0
#'
#' The name of the executable file is \code{simulator.exe}.
#'
#' For Windows, the user can download an installer from the URL (for version 1.2.0):
#'
#' https://github.com/bogdanoancea/simulator/releases/tag/1.2.0-kit
#'
#' @param path_to_exe character vector with the absolute path to the executable file of the
#' simulator of mobile network event data.
#'
#' @param simulator_version character denoting the version of the simulator (e.g. "1.2.0")
#'
#' @param input_folder absolute path where the simulation input files are.
#'
#' @param simulationCFGFile name of the configuration simulation file (input file).
#'
#' @param mapFile name of the map file (input file).
#'
#' @param personsCFGFile name of the persons file (input file).
#'
#' @param antennasCFGFile name of the antennas file (input file).
#'
#'
#' @details Return invisible \code{NULL} after placing output files of the
#' simulation in the output folder.
#'
#' @examples
#' \dontrun{
#' # simulator.exe will be downloaded from our github repo
#' run_ExeFile(
#'   path_to_exe       = "",
#'   simulator_version = "1.2.0",
#'   input_folder      = file.path(rootPath, "extdata/input_files"),
#'   simulationCFGFile = "simulation.xml",
#'   mapFile           = "map.wkt",
#'   personsCFGFile    = "persons.xml",
#'   antennasCFGFile   = "antennas.xml"
#' )
#' }
#'
#' @export
run_ExeFile <- function(path_to_exe,
                        simulator_version,
                        input_folder,
                        simulationCFGFile,
                        mapFile,
                        personsCFGFile,
                        antennasCFGFile) {
  sysinfo <- Sys.info()
  if (sysinfo["sysname"] == "Windows") {
    exe_file_name <- "simulator.exe"
  } else {
    exe_file_name <- "simulator"
  }

  if (path_to_exe == "") {
    cat("Path to simulator executable file not set.\n")
    if (simulator_version == "") {
      stop("[simutils::run_ExeFile] Please provide the version of the simulation software you want to use. For example simulator_version=1.2.0\n")
    } else {
      url_to_download <- paste0("https://github.com/bogdanoancea/simulator/releases/download/", simulator_version, "/", exe_file_name)
      sim_path <- file.path(paste0(Sys.getenv("R_USER"), "/simulator"))
      if (!file.exists(sim_path)) {
        result <- dir.create(sim_path, recursive = TRUE)

        if (result[[1]] != TRUE) {
          stop("[simutils::run_ExeFile] download folder for simulation software cannot be created")
        }
      }
      if (!file.exists(file.path(paste0(sim_path, "/", exe_file_name)))) {
        cat("Downloading the simulator executable file now.\n")
        utils::download.file(url_to_download, file.path(paste0(sim_path, "/", exe_file_name)), mode = "wb")
      } else {
        cat("We found an already downloaded file and we will use it!\n")
      }
      path_to_exe <- sim_path
    }
  } else {
    if (!file.exists(file.path(path_to_exe, exe_file_name))) {
      url_to_download <- paste0("https://github.com/bogdanoancea/simulator/releases/download/", simulator_version, "/", exe_file_name)
      sim_path <- file.path(paste0(Sys.getenv("R_USER"), "/simulator"))
      if (!file.exists(sim_path)) {
        result <- dir.create(sim_path, recursive = TRUE)
        if (result[[1]] != TRUE) {
          stop("[simutils::run_ExeFile] download folder for simulation software cannot be created")
        }
      }
      if (!file.exists(file.path(paste0(sim_path, "/", exe_file_name)))) {
        cat("Downloading the simulator executable file now.\n")
        utils::download.file(url_to_download, file.path(paste0(sim_path, "/", exe_file_name)), mode = "wb")
      } else {
        cat("We found an already downloaded file and we will use it!\n")
      }
      path_to_exe <- sim_path
    }
  }


  exe_file <- file.path(path_to_exe, exe_file_name)


  if (!file.exists(exe_file)) {
    stop(paste0("[simutils::run_ExeFile] simulator executable file cannot be found at ", path_to_exe, "\n"))
  }


  # Execute the simulator
  initial_wd <- getwd()
  setwd(path_to_exe)
  on.exit(setwd(initial_wd))
  if (sysinfo["sysname"] == "Windows") {
    system2(
      exe_file,
      args = paste0(
        " -s ",
        file.path(input_folder, simulationCFGFile),
        " -m ",
        file.path(input_folder, mapFile),
        " -p ",
        file.path(input_folder, personsCFGFile),
        " -a ",
        file.path(input_folder, antennasCFGFile)
      )
    )
  } else {
    system(
      paste0(
        exe_file,
        " -s ",
        file.path(input_folder, simulationCFGFile),
        " -m ",
        file.path(input_folder, mapFile),
        " -p ",
        file.path(input_folder, personsCFGFile),
        " -a ",
        file.path(input_folder, antennasCFGFile)
      )
    )
  }

  simulation.xml <- xml2::read_xml(file.path(input_folder, simulationCFGFile))
  outputFolder <- xml2::xml_contents(xml2::xml_child(simulation.xml, search = "output_dir"))

  cat(paste0("[simutils::run_ExeFile] output written in ", paste0(path_to_exe, "/", outputFolder)))
  return(invisible(NULL))
}
