#' @title Embed images with Papaya
#'
#' @description Writes temporary images out from nifti objects or passes character filenames
#' of images to papaya JS viewer
#' @param images character filenames or \code{nifti} objects to be viewed
#' @export
#' @importFrom fslr checkimg
#' @importFrom oro.nifti is.nifti nifti
#' @return Output html
embed_papaya <- function(
  images, # character filenames or \code{nifti} objects to be viewed
  outdir = NULL
){
  #####################
  # Make sure they are nifti
  #####################
  if (is.nifti(images)){
    images = list(images)
  }
  #   images = sapply(images, checkimg, check_type = TRUE)
  images = sapply(images, checkimg)
  # range
  #####################
  # Have to copy to temporary directory for js to work it seemed
  #####################
  if (is.null(outdir)){
    outdir = tempfile()
  }
  if (!file.exists(outdir)){
    dir.create(outdir)
  }
  
  #####################
  # Copying image to output directory
  #####################
  outfiles = file.path(outdir, basename(images))
  file.copy(images, to = outfiles, overwrite = TRUE)
  images = basename(outfiles)
  images = paste0('"', images, '"')
  images = paste(images, collapse = ", ")
  L = list()
  L$images = images
  
  ##################
  # Copy over the requirements for papaya
  ##################  
  index.file <- system.file("embed.html", package="papayar")
  
  ##################
  # Reading in the index file to add to
  ##################
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
  cssfile = system.file("papaya.css", package="papayar")
  jsfile = system.file("papaya.js", package="papayar")
  css = sprintf(
    '<link rel="stylesheet" type="text/css" href="%s?version=0.7&build=744" />\n', 
                cssfile)
  js = sprintf(
    '<script type="text/javascript" src="%s?version=0.7&build=744"></script>\n', 
               jsfile)
  
  index = c(css, js, top, cmd, bottom)
  return(index)
}