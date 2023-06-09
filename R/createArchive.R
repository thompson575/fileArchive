#' @title createArchive
#'
#' @description
#' \code{createArchive} creates an archive folder with an index.
#'
#' @details
#' \code{createArchive} creates an archive folder with an empty index and
#' a history file that automatically documents changes to the archive.
#'
#' The archive folder may already exist, but if so, it must be empty.
#'
#' \itemize{
#'   \item copyToArchive() copies a file to the archive.
#'   \item removeFromArchive() removes a file from the archive and deletes it.
#'   \item checkArchive() looks for inconsistencies between the folder content and the index.
#'   \item replaceTag() replaces the old tag with new one.
#' }
#'
#' The index contains
#' \itemize{
#'   \item id       ... an automatically assigned identification number: 1, 2, 3, etc.
#'   \item name     ... string identifier assigned by the user
#'   \item tag      ... a single string of tags
#'   \item filename ... filename used in the archive
#'   \item datetime ... date and time when saved
#' }
#'
#' @param path path to the new archive
#'
#' @examples
#' createArchive('C:/myStore')
#'
#' @export
#'
createArchive <- function(path) {
  # --- check argument
  if( !(is.character(path) & length(path) == 1) ) {
    stop("path must be a single string")
  }

  # --- Does archive already exist? --------------------------------
  if( file.exists(path) & length(list.files(path,
                                            all.files=TRUE,
                                            include.dirs = TRUE,
                                            no.. = TRUE)) > 0 ) {
    stop("Folder exists and is not empty")
  } else {
    # --- Does not exist .. create -----------------------------
    if( !file.exists(path) ) dir.create(path)
    data.frame(     id       = integer(),
                    tag      = character(),
                    name     = character(),
                    filename = character(),
                    date     = character(),
                    stringsAsFactors=FALSE
    ) -> INDEX
    saveRDS(INDEX, file=file.path(path, 'index.rds'))
    cat(paste("Archive:", path, "\n"),
        file = file.path(path, "history.txt"))
    cat(paste("Created:", Sys.time(), "\n"),
        file = file.path(path, "history.txt"),
        append=TRUE)
  }
}
