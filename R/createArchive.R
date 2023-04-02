#' @title createArchive
#'
#' @description
#' \code{createArchive} creates an archive folder with an index.
#'
#' @details
#' \code{createArchive} creates an archive folder with an empty index.
#'
#' The folder may already exist, but it must be empty.
#'
#' \itemize{
#'   \item copyToArchive() copies a file to the archive.
#'   \item moveToArchive() moves a file into the archive.
#'   \item dropFromArchive() removes a file from the archive and deletes it.
#'   \item readArchiveIndex() returns the index of an archive as a tibble
#'   \item editTags() enables editing tags.
#' }
#'
#' The index contains
#' \itemize{
#'   \item id ... an identification number 1, 2, 3, etc.
#'   \item name ... a name given to the saved object
#'   \item tags ... single string of tags
#'   \item filename ... name of file within the archive
#'   \item datetime ... date and time when saved
#' }
#'
#' @param path path to this archive
#'
#' @examples
#' createArchive('C:/myStore')
#'
#' @export
#'
createArchive <- function(path) {

  # --- Does archive already exist? --------------------------------
  if( exists(path) & length(list.files(path, all.files=TRUE, include.dirs = TRUE)) != 0 ) {
    stop("Folder exists and is not empty")
  } else {
    # --- Does not exist .. create -----------------------------
    if( !exists(path) ) dir.create(path)
    data.frame(     id=integer(),
                    name=character(),
                    tags=character(),
                    filename=character(),
                    date=character(),
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
