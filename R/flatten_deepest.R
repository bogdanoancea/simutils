#' @title Transform a list of lists whose final nodes are \code{list} into
#' a list of lists with this final nodes as different elements (e.g: \code{character}).
#' 
#' @description \code{flatten_deepest} receives a list of lists with final nodes as \code{list}
#' and transforms it into a list of lists with final nodes as different elements, 
#' for example, as \code{character} class; keeping the names and attributes' structure.
#' 
#' @param list A \code{list} of lists which must be named. 
#' 
#' @details Return a list of lists with final nodes as different classes 
#' (\code{character}, \code{integer}, ...) keeping the original names' structure.
#' 
#' @examples 
#' 
#' @import purrr
#' 
#' @export

flatten_deepest <- function(list){
  
  if (!is.list(list)) return(list)
  comp_names <- names(list)
  if (length(list) > 0 && length(comp_names) == 0) stop('[flatten_deepest] list must be named.\n')
  attr_original <- attributes(list)
  output <- lapply(comp_names, function(comp){
    
    if (purrr::vec_depth(list[[comp]]) == 2) {
      
      attr_original <- attributes(list[[comp]])
      list[[comp]] <- unlist(list[[comp]])
      attributes(list[comp]) <- attr_original
      return(list[[comp]])
    }

    return(flatten_deepest(list[[comp]]))
    
  })
  attributes(output) <- attr_original
  return(output)
  
}