#' Add Empty Lines to a Word Document
#'
#' This function adds a specified number of empty lines to a Word document using the `officer` package.
#'
#' @param doc An `rdocx` object representing the Word document.
#' @param n An integer specifying the number of empty lines to add. Default is 1.
#'
#' @return The modified `rdocx` object with the added empty lines.
#'
#' @details The function iterates through the specified number of empty lines and adds them to the document. Each empty line is added as a new paragraph with an empty string.
#'
#' @examplesIf FALSE
#' library(officer)
#' doc <- read_docx()
#' doc <- body_add_empty_line(doc, n = 3) # 添加3个空白行
#' print(doc, target = "empty_lines_example.docx")
#'
#' @export
#'
body_add_empty_line <- function(doc, n = 1) {

  for (i in seq_len(n)) {
    doc <- officer::body_add_par(x = doc, value = "")
  }
  return(doc)
}

