#' @title removeFromArchive
#'
#' @description
#' \code{removeFromArchive} deletes a file from the archive.
#'
#' @details
#' \code{removeFromArchive} deletes one or more files from an archive, adjusts the index
#'  and notes the deletion in the history file.
#'
#' see \code{\link[=createArchive]{createArchive()}} for details of other archiving functions and of the
#' structure of the index.
#'
#' @param path path to the archive
#' @param id  id number(s) of the file(s) to be deleted
#'
#' @examples
#' \dontrun{removeFromArchive('C:/MyStore', id=32 )}
#'
#' @export
#'
removeFromArchive <- function(path, id) {
  # --- check arguments
  if( !(is.character(path) & length(path) == 1) ) {
    stop("path must be a single string")
  }
  if( !is.numeric(id)) {
    stop("id not numeric")
  }

  # --- check path
  if( !file.exists(path) ) {
    stop(paste("archive:", path, "does not exist"))
  }
  if( !file.exists( file.path(path, "index.rds") ) ) {
    stop(paste(path, "is not an archive"))
  }

  INDEX <- readRDS( file.path(path, "index.rds"))
  # --- each id
  for( j in seq_along(id)) {
    i <- which( INDEX$id == id[j] )
    if( length(i) == 0 ) {
      # --- id not in the index
      warning(paste("id", id[j], "not in the index"))
    } else if( length(i) > 1 ) {
      # --- corrupt index
      stop("multiple index entries with the same id")
    } else {
      # --- id located as row i
      cat( paste( "\nRemoval:\n",
                  "  id:", INDEX$id[i], "\n",
                  "  name:", INDEX$name[i], "\n",
                  "  tag:", INDEX$tag[i], "\n",
                  "  filename:", INDEX$filename[i], "\n",
                  "  datetime:", INDEX$datetime[i], "\n"),
           file = file.path(path, "history.txt"),
           append=TRUE)
      unlink( file.path(path, INDEX$filename[i]) )
      INDEX <- INDEX[-i, ]
    }
  }
  saveRDS(INDEX ,file=file.path(path, 'index.rds'))
}
