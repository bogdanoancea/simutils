.onUnload <- function (libpath) {
  library.dynam.unload("simutils", libpath)
}