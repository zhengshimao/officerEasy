% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_color_bar_text.R
\name{get_color_bar_text}
\alias{get_color_bar_text}
\title{Generate a formatted color bar text table}
\usage{
get_color_bar_text(
  text,
  text_color = "white",
  bg_color = "#006699",
  font_name_ch = "宋体",
  font_name_en = "Times New Roman",
  font_size = 12,
  bar_width = cm2inche(16),
  bar_height = cm2inche(2),
  unit = "in",
  padding_top = 0,
  padding_bottom = 0,
  padding_left = 0,
  padding_right = 0
)
}
\arguments{
\item{text}{A character vector to be displayed in the color bar.}

\item{text_color}{A character string specifying the color of the text. Default is "white".}

\item{bg_color}{A character string specifying the background color of the color bar. Default is "#006699".}

\item{font_name_ch}{A character string specifying the font name for Chinese text. Default is "宋体".}

\item{font_name_en}{A character string specifying the font name for English text. Default is "Times New Roman".}

\item{font_size}{A numeric value indicating the font size for the text. Default is 12.}

\item{bar_width}{A numeric value representing the width of the table. The default is 16 cm, converted to inches.}

\item{bar_height}{A numeric value representing the height of the table. The default is 2 cm, converted to inches.}

\item{unit}{A character string indicating the unit of measurement for width and height. Default is "in" (inches).}

\item{padding_top}{A numeric value indicating the top padding of the text within the cell. Default is 0. unit is pts (points).}

\item{padding_bottom}{A numeric value indicating the bottom padding of the text within the cell. Default is 0. unit is pts (points).}

\item{padding_left}{A numeric value indicating the left padding of the text within the cell. Default is 0. unit is pts (points).}

\item{padding_right}{A numeric value indicating the right padding of the text within the cell. Default is 0. unit is pts (points).}
}
\value{
A \code{flextable} object with the formatted color bar text.
}
\description{
This function creates a \code{flextable} object with customizable background color, text color, font settings, and padding. It is useful for generating color bars with text in Word documents.
}
\details{
The \code{padding} parameters allow for precise control over the spacing between the text and the cell borders, which is useful for adjusting the appearance of the text within the color bar.
}
\examples{
\dontshow{if (FALSE) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
library(flextable)
library(officer)
get_color_bar_text("Example Text")
text_bar <- get_color_bar_text("示例文本", font_name_ch = "微软雅黑")
read_docx() \%>\% body_add_flextable(text_bar, align = "left") \%>\%
  print(target = "color_bar_text_Example1.docx")
text_bar <- get_color_bar_text("Example Text 示例文本",
                                font_name_ch = "微软雅黑",font_name_en = "Times New Roman")
read_docx() \%>\% body_add_flextable(text_bar, align = "left") \%>\%
  print(target = "color_bar_text_Example2.docx")
\dontshow{\}) # examplesIf}
}
