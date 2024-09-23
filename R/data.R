#' Chinese Font Size and Corresponding Point Size
#'
#' This dataset provides the mapping between common Chinese font size names (e.g., 小四, 三号)
#' and their corresponding size in points. It is useful for setting font sizes in documents
#' created with the `officer` package.
#'
#' @format A data frame with 4 rows and 2 variables:
#' \describe{
#'   \item{font_size_name}{A character vector indicating the name of the Chinese font size (e.g., 小四, 三号).}
#'   \item{size_in_points}{A numeric vector indicating the corresponding size in points.}
#' }
#'
#' @examples
#' data(font_size_mapping)
#' print(font_size_mapping)
#'
"font_size_mapping"
