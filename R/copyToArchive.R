#' @title copyToArchive
#'
#' @description
#' \code{copyToArchive} copies a file to an existing archive.
#'
#' @details
#' \code{copyToArchive} copies a file to the archive, adds it to the index and
#' records the transaction in the history file.
#' The name used in the index need not be unique. Multiple files with the same
#' identification name are treated as versions of the same data. If replace=TRUE
#' all files with that index name are deleted before the new file is copied to
#' the archive.
#' If there is already a file in the archive with the same filename as the
#' new file, the new file is given a replacement filename composed of the old
#' filename plus a version number.
#'
#' see \code{\link[=createArchive]{createArchive()}} for details of other archiving functions and of the
#' structure of the index.
#'
#' @param path path to the archive
#' @param file path of the file that is to be copied
#' @param name name used to identify the file in the index
#' @param tag  single string of information about the file
#' @param replace delete existing entries with the same name (TRUE) or
#'             add to archive as a new version (FALSE). Defaults to FALSE
#'
#' @examples
#' \dontrun{copyToArchive(path = "C:/Project/archive",
#'               file    = "C:/temp/myFile.rds",
#'               name    = "New Results",
#'               tag     = "some interesting findings",
#'               replace = TRUE)
#'          }
#'
#' @export
#'
copyToArchive <- function(path, file, name, tag="", replace=FALSE) {
  # --- check arguments
  if( !(is.character(path) & length(path) == 1) ) {
    stop("path must be a single string")
  }
  if( !(is.character(file) & length(file) == 1) ) {
    stop("file must be a single string")
  }
  tag <- paste(tag, collapse = " , ")
  if( !is.character(tag) ) {
    stop("unable to collapse tag to a single string")
  }
  # --- check that archive exists
  if( !file.exists(path) ) {
    stop(paste("archive:", path, "does not exist"))
  }
  if( !file.exists( file.path(path, "index.rds") ) ) {
    stop(paste(path, "is not an archive"))
  }
  # --- Read the index   ------------------------------------------
  INDEX <- readRDS( file.path(path, "index.rds") )
  # --- Delete old versions ---------------------------------------
  if( replace ) {
    i <- 0
    while( i < nrow(INDEX) ) {
      i <- i + 1
      if( INDEX$name[i] == name ) {
        cat( paste( "\nRemoval:\n",
                    "  id:", INDEX$id[i], "\n",
                    "  name:", INDEX$name[i], "\n",
                    "  tag:", INDEX$tag[i], "\n",
                    "  filename:", INDEX$filename[i], "\n",
                    "  datetime:", INDEX$datetime[i], "\n"),
             file = file.path(path, "history.txt"),
             append=TRUE)
        # --- previous version found --------------------------------
        unlink(file.path(path, INDEX$file[i]), force=TRUE, expand=FALSE)
        # --- drop from index ---------------------------------------
        INDEX <- INDEX[-i, ]
        i <- i - 1
      }
    }
  }

  # --- Create unique filename for new object -----------------------
  filename <- basename(file)
  root     <- sub('\\.([[:alnum:]]+)$', '', filename)
  exten    <- gsub(root, '', filename)
  if( file.exists( file.path(path, filename)) ) {
     i <- 0
     testname <- filename
     while( file.exists( file.path(path, testname) )) {
       i <- i + 1
       testname <- paste0(root, "_v", i, exten)
     }
     filename <- testname
  }
  # --- write file to archive ---------------------------------------
  file.copy(file, file.path(path, filename))
  # --- update the index --------------------------------------------
  INDEX <- rbind(INDEX,
                 data.frame(id=ifelse( nrow(INDEX) == 0, 1, max(INDEX$id)+1),
                            name=name,
                            tag=tag,
                            filename=filename,
                            datetime=Sys.time(),
                            stringsAsFactors=FALSE) )
  # --- save updated index --------------------------------------------
  saveRDS(INDEX ,file=file.path(path, "index.rds"))

  i <- nrow(INDEX)
  cat( paste( "\nAddition:\n",
              "  id:", INDEX$id[i], "\n",
              "  name:", INDEX$name[i], "\n",
              "  tag:", INDEX$tag[i], "\n",
              "  filename:", INDEX$filename[i], "\n",
              "  datetime:", INDEX$datetime[i], "\n"),
       file = file.path(path, "history.txt"),
       append=TRUE)
  return(invisible(filename))
}
