#' @title Embed images with Papaya
#'
#' @description Writes temporary images out from nifti objects or passes character filenames
#' of images to papaya JS viewer
#' @param images character filenames or \code{nifti} objects to be viewed
#' @param outdir output directory for index and all to go
#' @importFrom neurobase checkimg
#' @importFrom oro.nifti is.nifti nifti
#' @return Output html
#' @import htmltools
#' @export
embed_papaya <- function(
  images, # character filenames or \code{nifti} objects to be viewed
  outdir = NULL
){
  L = get_papaya_version()
  build = L$build
  # version = L$version
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
  
  outfiles = file.path(outdir, basename(c(cssfile, jsfile)))
  file.copy(c(cssfile, jsfile), to = outfiles, overwrite = TRUE)
  
  css = sprintf(
    paste0('<link rel="stylesheet" type="text/css" href="%s?build=', 
           build, '" />\n'), 
    basename(cssfile))
  js = sprintf(
    paste0('<script type="text/javascript" src="%s?build=', 
           build, '"></script>\n'), 
    basename(jsfile))
  
  index = c(css, js, top, cmd, bottom)
  index = HTML(index)
  return(index)
}