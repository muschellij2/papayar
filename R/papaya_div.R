#' @title Papaya Div element output
#' @description Get the necessary div output for embedding a papaya image
#' @return Character string
#' @export
#'
#' @examples
#' papaya_div()
papaya_div = function(){
  index.file <- system.file("embed.html", package="papayar")
  x = readLines(index.file)
  x = x[length(x)]
  return(x)
}