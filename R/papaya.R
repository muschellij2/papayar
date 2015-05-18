#' @title View images with Papaya
#'
#' @description Writes temporary images out from nifti objects or passes character filenames
#' of images to papaya JS viewer
#' @param images character filenames or \code{nifti} objects to be viewed
#' @export
#' @importFrom fslr checkimg
#' @importFrom oro.nifti is.nifti
#' @return Output directory where index.html, js, and copied nii.gz files
papaya <- function(
  images # character filenames or \code{nifti} objects to be viewed
  ){
  #####################
  # Make sure they are nifti
  #####################
  if (is.nifti(images)){
    images = list(images)
  }
  images = sapply(images, checkimg)
  
  #####################
  # Have to copy to temporary directory for js to work it seemed
  #####################
  outdir = tempfile()
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
  L$images = images
  pass_papaya(L, outdir = outdir)
  return(outdir)
}