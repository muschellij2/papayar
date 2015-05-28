#' @title View images with Papaya
#'
#' @description Writes temporary images out from nifti objects or passes character filenames
#' of images to papaya JS viewer
#' @param L list of arguments passed to papaya using params
#' @param outdir output directory for index and all to go
#' @param daemon Argument passed to \code{\link[servr]{server_config}}
#' @param close_on_exit Should the server close once the function finishes? 
#' @param sleeper Time in seconds to sleep if \code{close_on_exit = TRUE}.
#' This allows the server to start up.
#' @export
#' @importFrom servr httd
#' @return NULL
pass_papaya <- function(
  L = NULL,
  outdir = NULL,
  daemon = ifelse(is.null(getOption("viewer")), TRUE, FALSE),
  close_on_exit = TRUE,
  sleeper = 3
  ){
  ##################
  #Create temporary directory for things to go
  ##################    
  if (is.null(outdir)){
    outdir = tempfile()
  }
  if (!file.exists(outdir)){
    dir.create(outdir)
  }
  
  ##################
  # Copy over the requirements for papaya
  ##################  
  files = c("index.html", "papaya.css", "papaya.js")
  files = sapply(files, system.file, package="papayar")
  file.copy(files, to = outdir, overwrite = TRUE)
  
  ##################
  # Reading in the index file to add to
  ##################
  index.file = file.path(outdir, "index.html")
  index = readLines(index.file)
  line = grep("var params", index)
  stopifnot(length(line) == 1)
  top = index[seq(line)]
  bottom = index[seq(line + 1, length(index))]
  
  nL = names(L)
  
  start = paste0('params["images"] = [')
  end = '];'
  cmd = ""
  if (length(L$images) > 0){
    cmd = paste0(start, L$images, end)
  }
  
  ##################
  # Writing out index.html to use
  ##################
  index = c(top, cmd, bottom)
  writeLines(text = index, con = index.file)
  # cat(index.file)
  ##################
  # browsing the file
  ##################
  viewer <- getOption("viewer")
  daemon_name = httd(outdir, daemon = daemon, browser = TRUE)
  if (close_on_exit){
    Sys.sleep(sleeper)
    on.exit({
      servr::daemon_stop(daemon_name)
    })
  }
  return(index.file)
}