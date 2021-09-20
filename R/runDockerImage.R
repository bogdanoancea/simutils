#' @title Run the simulation software using the docker image.
#' 
#' @description \code{runDockerImage} runs the simulation software using
#' the docker image. 
#' 
#' @param dockerImage Absolute path where the file to run the docker image is.
#' 
#' @param input_folder Absolute path of the folder which contains the input data.
#' 
#' @param simulationCFGFile Name of the simulation configuration file.
#' 
#' @param mapFile Name of the map file.
#' 
#' @param personsCFGFile Name of the persons' configuration file.
#' 
#' @param antennasCFGFile Name of the antennas' configuration file.
#' 
#' @param output_folder Name of the output folder indicated in the 
#' simulation's configuration file. 
#' 
#' @details Return invisible NULL after executing the docker image.
#' 
#' @export 
runDockerImage <- function(
  dockerImage,
	input_folder,
	simulationCFGFile,
	mapFile,
	personsCFGFile,
	antennasCFGFile,
	output_folder) {

	if (!file.exists(dockerImage)){
	  
		stop("[runDockerImage] Docker image does not exists.\n")
	  
	}
  
	sysinfo <- Sys.info()
	if (sysinfo['sysname'] == "Windows"){
	  
		system2("docker", args = paste0("load -i ", dockerImage))
	  
	} else {
	  
	  username <- sysinfo[['user']]
		system(paste0("sudo -u ", username, " docker load -i ", dockerImage))
		
	}
	setwd(dirname(dockerImage))

	# Prepare input files
	outputFolderName <- file.path(dirname(dockerImage), output_folder)
	if (!file.exists(outputFolderName)) {
	  
		result <- dir.create(outputFolderName)
		if(result[[1]] != TRUE){
		  
			stop("[runDockerImage] output folder for docker cannot be created")
		  
		}
	} else {
	  
		existingFiles <- list.files(outputFolderName)
		if (length(existingFiles) >= 1) {
			for (i in 1:length(existingFiles)){
			  
				file.remove(file.path(outputFolderName, existingFiles[[i]]))
			  
			}
		}
	}
	# Copy the input files files
	file.copy(
	  from      = file.path(input_folder, simulationCFGFile), 
	  to        = outputFolderName, 
	  overwrite = TRUE
	)
	file.copy( 
	  from      = file.path(input_folder, mapFile), 
	  to        = outputFolderName, 
	  overwrite = TRUE 
	)
	file.copy( 
	  from      = file.path(input_folder, personsCFGFile), 
	  to        = outputFolderName, 
	  overwrite = TRUE
	)
	file.copy( 
	  from      = file.path(input_folder, antennasCFGFile),
	  to        = outputFolderName, 
	  overwrite = TRUE
	)

	# Command line arguments for docker
	simargs <- paste0(
		" -s ",
		file.path(output_folder, simulationCFGFile),
		" -m ",
		file.path(output_folder, mapFile),
		" -p ",
		file.path(output_folder, personsCFGFile),
		" -a ",
		file.path(output_folder, antennasCFGFile)
	)

	# Run!
	cmd_args = paste0(
		"run --rm -v ",
		file.path(dirname(dockerImage), output_folder),
		":/repo/",
		output_folder,
		" simulator ",
		simargs
	)
	if (sysinfo['sysname'] == "Windows"){
	  
		system2( "docker", args = cmd_args )
	  
	} else {
	  
	  username = sysinfo[['user']]
	  system(paste0("sudo -u ", username, " docker ", cmd_args) )
	}
  
	return(invisible(NULL))
	
}
