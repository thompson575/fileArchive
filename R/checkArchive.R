#' @title checkArchive
#'
#' @description
#' \code{checkArchive} checks that index and archive contents agree.
#'
#' @details
#' \code{checkArchive} checks that index and archive contents agree.
#'
#' see help(createArchive) for details of other archiving functions and of the contents of the index.
#'
#' @param path path to the archive
#'
#' @examples
#' checkArchive("C:/myStore")
#'
#' @export
#'
checkArchive <- function(path) {

  # --- check that archive exists ---------------------------------
  if( !file.exists(path) ) {
    stop(paste("archive:", path, "does not exist"))
  }
  if( !file.exists( file.path(path, "index.rds") ) ) {
    stop(paste(path, "is not an archive"))
  }

  # --- files in archive ------------------------------------------
  files <- list.files(path)
  nFiles <- length(files)
  # --- read the index -------------------------------------------
  INDEX <- readRDS( file.path(path, "index.rds"))
  nIndex <- nrow(INDEX)
  warn <- 0
  # --- Identify index entries with no file ---------------------
  for( i in 1:nIndex) {
    if( !(INDEX$filename[i] %in% files )) {
      print(paste0(INDEX$filename[i], " does not exist" ))
      warn <- warn + 1
    }
  }
  # --- Identify files not in the index ---------------------------
  for(i in 1:nFiles ) {
    if( !(files[i] %in% c("index.rds", "history.txt", INDEX$filename) ) ) {
      print(paste0(files[i]," is not in the index" ))
      warn <- warn + 1
    }
  }
  # --- Is everything OK ---------------------------------------
  if( warn == 0 ) {
    print("No Inconsistencies found")
  }
}
