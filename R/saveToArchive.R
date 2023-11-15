#' @title saveToArchive
#'
#' @description
#' \code{saveToArchive} save an R object to an existing archive.
#'
#' @details
#' \code{saveToArchive} saves an object to the archive, adds it to the index and
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
#' @param x R object to be save
#' @param file path+name of the file that is to be used to save the object
#' (path must point to an archive).
#' Currently support rds and csv files
#' @param name name used to identify the file in the index. Defaults to the name of the file to
#' be saved without its extension.
#' @param tag  single string of information about the file
#' @param replace delete existing entries with the same name (TRUE) or
#'             add to archive as a new version (FALSE). Defaults to FALSE
#'
#' @examples
#' \dontrun{saveToArchive(myData,
#'               file    = "C:/archive/myFile.rds",
#'               name    = "New Results",
#'               tag     = "some interesting findings",
#'               replace = TRUE)
#'          }
#'
#' @export
#'
saveToArchive <- function(x, file, name="", tag="", replace=FALSE) {
  # --- check arguments
  if( !(is.character(file) & length(file) == 1) ) {
    stop("file must be a single string")
  }
  tag <- paste(tag, collapse = " , ")
  if( !is.character(tag) ) {
    stop("unable to collapse tag to a single string")
  }
  # --- extract archive path from file -----------------------------
  path <- dirname(file)
  # --- check that archive exists
  if( !file.exists(path) ) {
    stop(paste("archive:", path, "does not exist"))
  }
  if( !file.exists( file.path(path, "index.rds") ) ) {
    stop(paste(path, "is not an archive"))
  }
  # --- Name missing ----------------------------------------------
  if( name == "" ) {
    name <- sub('\\..*$', '', basename(file))
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
  if( !(exten %in% c(".rds", ".csv")) ) {
    stop("Extension not supported")
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
        unlink(file.path(path, INDEX$filename[i]), force=TRUE, expand=FALSE)
        # --- drop from index ---------------------------------------
        INDEX <- INDEX[-i, ]
        i <- i - 1
      }
    }
  }
  # --- write file to archive ---------------------------------------
  if( exten == ".rds") {
    saveRDS(x, file)
  } else if( exten == ".csv" ) {
    write.csv(x, file)
  }
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
  cat( paste( "\nAddition (saved):\n",
              "  id:", INDEX$id[i], "\n",
              "  name:", INDEX$name[i], "\n",
              "  tag:", INDEX$tag[i], "\n",
              "  filename:", INDEX$filename[i], "\n",
              "  datetime:", INDEX$datetime[i], "\n"),
       file = file.path(path, "history.txt"),
       append=TRUE)
  return(invisible(filename))
}
