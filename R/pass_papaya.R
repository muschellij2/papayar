#' @title View images with Papaya
#'
#' @description Writes temporary images out from nifti objects or passes character filenames
#' of images to papaya JS viewer
#' @param L list of arguments passed to papaya using params
#' @param outdir output directory for index and all to go
#' @param force_browser Force the browsing in the browser 
#' (as opposed to RStudio Viewer)
#' @param height passed to \code{\link[rstudio]{viewer}}, try "maximize"
#' @export
#' @importFrom rstudioapi viewer
#' @importFrom servr httd
#' @return NULL
pass_papaya <- function(
  L = NULL,
  outdir = NULL,
  force_browser = FALSE,
  height = NULL
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
  daemon = httd(outdir, daemon = TRUE)
  if (is.null(viewer) | force_browser){
    utils::browseURL(index.file)
  } 
  Sys.sleep(1)
  servr::daemon_stop(daemon)
  
#   if (is.null(viewer) | force_browser){
#     cat("# Not in the viewer\n")
#     httd(outdir)
#     utils::browseURL(index.file)
#   } else {
#     cat("# In the viewer\n")
#     rstudioapi::viewer(index.file, height=height)
#   }  
  return(index.file)
}