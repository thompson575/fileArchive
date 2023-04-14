#' @title removeFromArchive
#'
#' @description
#' \code{removeFromArchive} deletes a file from the archive.
#'
#' @details
#' \code{removeFromArchive} deletes a file from an archive, adjusts the index
#'  and notes the deletion in the history file.
#'
#' see \code{\link[=createArchive]{createArchive()}} for details of other archiving functions and of the
#' structure of the index.
#'
#' @param path path to the archive
#' @param id  id number of the file to be deleted
#'
#' @examples
#' \dontrun{removeFromArchive('C:/MyStore', id=32 )}
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
  unlink( file.path(path, INDEX$filename[i]) )
  INDEX <- INDEX[-i, ]
  saveRDS(INDEX ,file=file.path(path, 'index.rds'))
}
