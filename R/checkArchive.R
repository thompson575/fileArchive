#' @title checkArchive
#'
#' @description
#' \code{checkArchive} checks that index and archive are consistent.
#'
#' @details
#' \code{checkArchive} checks that index and archive contents are consistent
#' and prints details of any inconsistencies.
#'
#' see \code{\link[=createArchive]{createArchive()}} for details of other archiving functions and of the contents of the index.
#'
#' @param path path to the archive
#'
#' @examples
#' \dontrun{checkArchive("C:/myStore")}
#'
#' @export
#'
checkArchive <- function(path) {

  # --- check arguments
  if( !(is.character(path) & length(path) == 1) ) {
    stop("path must be a single string")
  }

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
      warning(paste0(INDEX$filename[i], " does not exist" ))
      warn <- warn + 1
    }
  }
  # --- Identify files not in the index ---------------------------
  for(i in 1:nFiles ) {
    if( !(files[i] %in% c("index.rds", "history.txt", INDEX$filename) ) ) {
      warning(paste0(files[i]," is not in the index" ))
      warn <- warn + 1
    }
  }
  # --- Is everything OK ---------------------------------------
  if( warn == 0 ) {
    warning("No Inconsistencies found")
  }
}
