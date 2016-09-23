#' @title View images with Papaya
#'
#' @description Writes temporary images out from nifti objects or passes character filenames
#' of images to papaya JS viewer
#' @param images character filenames or \code{nifti} objects to be viewed
#' @param outdir output directory for index and all to go 
#' @param ... Options to be passed to \code{\link{pass_papaya}}
#' @export
#' @importFrom neurobase checkimg
#' @importFrom oro.nifti is.nifti nifti
#' @return Output directory where index.html, js, and copied nii.gz files
#' @examples \dontrun{
#' library(neurobase)
#' x = nifti(img = array(rnorm(100^3), dim= rep(100, 3)), dim=rep(100, 3), datatype=16)
#' thresh = datatyper(x > 1)
#' index.file = papaya(list(x, thresh))
#' }
papaya <- function(
  images, # character filenames or \code{nifti} objects to be viewed
  outdir = NULL,
  ...
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
  ##################
  #Create temporary directory for things to go
  ##################    
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
  fc = file.copy(images, to = outfiles, overwrite = TRUE)
  if (!all(fc)){
    stop("# Some files passed in are not found!")
  }

  images = basename(outfiles)
  images = paste0('"', images, '"')
  images = paste(images, collapse = ", ")
  L = list()
  L$images = images
  index.file = pass_papaya(L, outdir = outdir, ...)
  return(index.file)
}