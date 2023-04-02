#' @title removeFromArchive
#'
#' @description
#' \code{removeFromArchive} deletes files from the archive.
#'
#' @details
#' \code{removeFromArchive} deletes file from an archive.
#'
#' see help(createArchive) for details of other archiving functions and of the
#' structure of the index.
#'
#' @param path path to the data archive
#' @param id  id number(s) of entries to be deleted
#'
#' @examples
#' removeFromArchive(path='MyStore', id=32 )
#'
#' @export
#'
removeFromArchive <- function(path, id) {

  if( !file.exists(path) ) {
    stop(paste("archive:", path, "does not exist"))
  }
  if( !file.exists( file.path(path, "index.rds") ) ) {
    stop(paste(path, "is not an archive"))
  }

  INDEX <- readRDS( file.path(path, "index.rds"))
  i <- which( INDEX$id == id )
  if( length(i) != 1 ) {
    stop("Cannot drop an id that is not in the index")
  }
  cat( paste( "\nRemoval:\n",
              "  id:", INDEX$id[i], "\n",
              "  name:", INDEX$name[i], "\n",
              "  tags:", INDEX$tags[i], "\n",
              "  filename:", INDEX$filename[i], "\n",
              "  datetime:", INDEX$datetime[i], "\n"),
      file = file.path(path, "history.txt"),
      append=TRUE)
  filename <- INDEX$filename[id]
  INDEX <- INDEX[-id, ]
  unlink( file.path(path, filename) )
  saveRDS(INDEX ,file=file.path(path, 'index.rds'))
}
