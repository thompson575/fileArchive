#' @title replaceTag
#'
#' @description
#' \code{replaceTag} replaces the tag in a selected file.
#'
#' @details
#' \code{replaceTag} replaces the tag of a selected file(s) and
#' notes the change in the history file. If the new tag is not
#' a single string then it is collapsed into one using " , "
#' (space comma space) as the separator.
#' When the collapsed new tag begin "+" it is
#' added to the end of the existing tag.
#'
#' see \code{\link[=createArchive]{createArchive()}} for details of other archiving functions and of the
#' structure of the index.
#'
#' @param path path to the data archive
#' @param id  id number(s) of the tag that is to be replaced
#' @param newtag the replacement tag
#'
#' @examples
#' \dontrun{replaceTag(path='MyStore', id=32, newtag="some tag")}
#'
#' @export
#'
replaceTag <- function(path, id, newtag) {
  # --- check arguments
  if( !(is.character(path) & length(path) == 1) ) {
    stop("path must be a single string")
  }
  newtag <- paste(newtag, collapse = " , ")
  if( !is.character(newtag) ) {
    stop("unable to collapse newtag to a single string")
  }
  if( !is.numeric(id) ) {
    stop("id not numeric")
  }

  # --- check if is an archive
  if( !file.exists(path) ) {
    stop(paste("archive:", path, "does not exist"))
  }
  if( !file.exists( file.path(path, "index.rds") ) ) {
    stop(paste(path, "is not an archive"))
  }

  append <- FALSE
  if( substr(newtag, 1, 1) == "+" ) {
    substr(newtag, 1, 1) <- " "
    append <- TRUE
  }
  # --- replace the tag
  INDEX <- readRDS( file.path(path, "index.rds"))
  for( j in seq_along(id)) {
    i <- which( INDEX$id == id[j] )
    if( length(i) != 1 ) {
      warning(paste("id", id[j], "is not in the index"))
    } else {
      if( append ) {
        thistag <- paste0(INDEX$tag[i], newtag)
      } else {
        thistag <- newtag
      }
      cat( paste( "\nTag Replacement:\n",
                  "  id:", INDEX$id[i], "\n",
                  "  name:", INDEX$name[i], "\n",
                  "  old tag:", INDEX$tag[i], "\n",
                  "  new tag:", thistag, "\n",
                  "  filename:", INDEX$filename[i], "\n",
                  "  datetime:", INDEX$datetime[i], "\n"),
           file = file.path(path, "history.txt"),
           append=TRUE)
      INDEX$tag[i] <- thistag
    }
  }
  saveRDS(INDEX ,file=file.path(path, 'index.rds'))
}
