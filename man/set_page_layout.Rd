% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/set_page_layout.R
\name{set_page_layout}
\alias{set_page_layout}
\title{Set Custom Cover Page Layout}
\usage{
set_page_layout(
  page_width = NULL,
  page_height = NULL,
  page_orient = "portrait",
  section_type = "nextPage",
  top = 0,
  bottom = 0,
  left = 0,
  right = 0,
  header = 0,
  footer = 0
)
}
\arguments{
\item{page_width}{The width of the page in inches.}

\item{page_height}{The height of the page in inches.}

\item{page_orient}{Page orientation, either 'landscape', either 'portrait'.}

\item{section_type}{Section type. It defines how the contents of the section will be placed relative to the previous section.
Available types are "continuous" (begins the section on the next paragraph),
"evenPage" (begins on the next even-numbered page), "nextColumn" (begins on the next column on the page), "nextPage" (begins on the following page), "oddPage" (begins on the next odd-numbered page).}

\item{top}{The top margin in inches. Default is 0.}

\item{bottom}{The bottom margin in inches. Default is 0.}

\item{left}{The left margin in inches. Default is 0.}

\item{right}{The right margin in inches. Default is 0.}

\item{header}{The header margin in inches. Default is 0.}

\item{footer}{The footer margin in inches. Default is 0.}
}
\value{
A \code{prop_section} object containing the layout properties for the cover page.
}
\description{
This function creates section properties for a cover page layout in a Word document. It allows customization of page dimensions, margins, headers, and footers, and is adaptable to various page sizes.
}
\details{
This function is useful for creating a cover page layout with custom page sizes and margins. The \code{page_width} and \code{page_height} parameters are optional; if not provided, the current settings of the Word document will be used.
}
\examples{
\dontshow{if (FALSE) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
library(officer)
cover_layout <- set_page_layout(page_width= 8.27, page_height = 11.69)
doc <- read_docx() \%>\%
  body_add_par("Cover Page Title", style = "heading 1") \%>\%
  body_set_default_section(cover_layout) \%>\%
  print(target = "A4_cover_page.docx")

# Using the image as the cover
doc <- read_docx() \%>\%
  body_set_default_section( cover_sect_properties ) \%>\%
  body_add_img(src = "./input/cover.jpg", width = 8.27, height = 11.69) \%>\%
  body_end_block_section(value = block_section(property = cover_sect_properties)) # 分节设置
\dontshow{\}) # examplesIf}
}
