#' @title replaceTags
#'
#' @description
#' \code{replaceTags} deletes files from the archive.
#'
#' @details
#' \code{replaceTags} deletes file from an archive.
#'
#' see help(createArchive) for details of other archiving functions and of the
#' structure of the index.
#'
#' @param path path to the data archive
#' @param id  id number(s) of entries to be deleted
#' @param newtags  new tags to replace the olds ones
#'
#' @examples
#' replaceTags(path='MyStore', id=32, newtags="some new tags" )
#'
#' @export
#'
replaceTags <- function(path, id, newtags) {

  if( !file.exists(path) ) {
    stop(paste("archive:", path, "does not exist"))
  }
  if( !file.exists( file.path(path, "index.rds") ) ) {
    stop(paste(path, "is not an archive"))
  }

  INDEX <- readRDS( file.path(path, "index.rds"))
  i <- which( INDEX$id == id )
  if( length(i) != 1 ) {
    stop("Cannot replace the tags of an id that is not in the index")
  }
  if( substr(newtags, 1, 1) == "+" ) {
    substr(newtags, 1, 1) <- " "
    newtags <- paste0(INDEX$tags[i], newtags)
  }
  cat( paste( "\nTag Replacement:\n",
              "  id:", INDEX$id[i], "\n",
              "  name:", INDEX$name[i], "\n",
              "  old tags:", INDEX$tags[i], "\n",
              "  new tags:", newtags, "\n",
              "  filename:", INDEX$filename[i], "\n",
              "  datetime:", INDEX$datetime[i], "\n"),
       file = file.path(path, "history.txt"),
       append=TRUE)
  INDEX$tags[i] <- newtags
  saveRDS(INDEX ,file=file.path(path, 'index.rds'))
}
