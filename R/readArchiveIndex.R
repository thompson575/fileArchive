#' @title readArchiveIndex
#'
#' @description
#' \code{readArchiveIndex} returns the index of an archive.
#'
#' @details
#' \code{readArchiveIndex} returns and the index of an archive as a data frame.
#'
#' @param path path to the archive
#'
#' see help(createArchive) for details of other archiving functions and of the
#' structure of the index.
#'
#' @examples
#' readArchiveIndex('C:/MyStore')
#'
#' @export
#'
readArchiveIndex <- function(path=NULL) {


  if( !file.exists(path) ) {
    stop(paste("archive:", path, "does not exist"))
  }
  if( !file.exists( file.path(path, "index.rds") ) ) {
    stop(paste(path, "is not an archive"))
  }
  filename <- file.path(path, 'index.rds')
  if (!file.exists(filename)){
    stop(paste(filename, 'does not exist'))
  }
  readRDS(file=filename)
}
