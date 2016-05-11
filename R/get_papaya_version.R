
#' @title Get Papaya Version
#' @description Reads the papaya.js file installed and determines version and build
#' @return List of build and version, both characters
#' @export
get_papaya_version = function() {
  jsfile = system.file("papaya.js", 
                       package = "papayar")  
  line = readLines(jsfile, n = 1)
  separate = strsplit(line, ",")[[1]]
  separate = gsub('"', "", separate)
  version = grep("PAPAYA_VERSION_ID", separate, value = TRUE)
  build = grep("PAPAYA_BUILD_NUM", separate, value = TRUE)
  version = gsub(".*=(.*)", "\\1", version)
  build = gsub(".*=(.*)", "\\1", build)
  L = list(version = version, build = build)
  return(L)
}


#' @title Update Papaya build version from GitHub
#' @description Updates the papaya version in the papayar package to the most 
#' current on GitHub
#' @param type Type of release.  Standard is default
#' @param verbose Should download progress be shown?
#'
#' @return Result of \code{\link{get_papaya_version}} after downloading
#' @export
#' @importFrom utils download.file
update_papaya_build = function(type = c("standard", "minimal", "nodicom", 
                                        "nojquery", "standard-with-atlas-local", 
                                        "standard-with-atlas"), verbose = TRUE) {
  type = match.arg(type)
  files = c("index.html", "papaya.css", "papaya.js")
  url = paste0(
    "http://raw.githubusercontent.com/rii-mango/Papaya/master/release/current/", 
  type, "/")
  dest = system.file(files, 
                     package = "papayar")
  bn = basename(dest)
  files = intersect(files, bn)
  files = paste0(url, files)
  ifile = 1 
  # download the files
  for (ifile in seq_along(files)) {
    download.file(files[ifile], dest[ifile], quiet = !verbose)
  }

  L = get_papaya_version()
  return(L)
}