#' Convert Font Size to Points
#'
#' This function converts a given font size, specified either as a numeric value (in points) or
#' as a descriptive character value (e.g., "小四", "三号"), into its corresponding point size.
#' The function supports both single and multiple font size conversions by using an internal
#' mapping table to match descriptive font sizes to numeric point values.
#'
#' @param font_size A numeric value representing the font size in points, or a character string
#' corresponding to a predefined font size (e.g., "小四", "三号"). Multiple values can be provided
#' as a vector.
#'
#' @return A numeric value or a vector of numeric values representing the font size(s) in points.
#'
#' @examplesIf FALSE
#' get_font_size(12) # returns 12
#' get_font_size("小四") # returns 12
#' get_font_size(c(11, "小四", "14", "四号")) # returns corresponding point sizes
#'
#' @export
get_font_size <- function(font_size = "小四") {

  result <- sapply(font_size, function(x){
    if(is.numeric(x)){
      x <- x
    } else if (is.character(x)) {

      if( !suppressWarnings(is.na(as.numeric(x))) ) {
        x <- as.numeric(x)
      }else{
        # utils::data(font_size_mapping, package = "officerEasy", envir = environment())
        font_size_mapping <- get("font_size_mapping", envir = asNamespace("officerEasy"))
        if( ! x %in% font_size_mapping$font_size_name)stop("'font_size' must in one of '",paste0(font_size_mapping$font_size_name, collapse = ", "),"'")
        x <- font_size_mapping$size_in_points[ font_size_mapping$font_size_name %in% x ]
      }

    }else{
      stop("'x' must be an integer value or one of '",paste0(font_size_mapping$font_size_name, collapse = ", "),"'")
    }

  }, simplify = TRUE)
  names(result) <- NULL
  return(result)
}
