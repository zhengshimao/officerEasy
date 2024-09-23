#' Generate a formatted color bar text table
#'
#' This function creates a `flextable` object with customizable background color, text color, font settings, and padding. It is useful for generating color bars with text in Word documents.
#'
#' @param text A character vector to be displayed in the color bar.
#' @param text_color A character string specifying the color of the text. Default is "white".
#' @param bg_color A character string specifying the background color of the color bar. Default is "#006699".
#' @param font_name_ch A character string specifying the font name for Chinese text. Default is "宋体".
#' @param font_name_en A character string specifying the font name for English text. Default is "Times New Roman".
#' @param font_size A numeric value indicating the font size for the text. Default is 12.
#' @param bar_width A numeric value representing the width of the table. The default is 16 cm, converted to inches.
#' @param bar_height A numeric value representing the height of the table. The default is 2 cm, converted to inches.
#' @param unit A character string indicating the unit of measurement for width and height. Default is "in" (inches).
#' @param padding_top A numeric value indicating the top padding of the text within the cell. Default is 0. unit is pts (points).
#' @param padding_bottom A numeric value indicating the bottom padding of the text within the cell. Default is 0. unit is pts (points).
#' @param padding_left A numeric value indicating the left padding of the text within the cell. Default is 0. unit is pts (points).
#' @param padding_right A numeric value indicating the right padding of the text within the cell. Default is 0. unit is pts (points).
#'
#' @return A `flextable` object with the formatted color bar text.
#'
#' @details The `padding` parameters allow for precise control over the spacing between the text and the cell borders, which is useful for adjusting the appearance of the text within the color bar.
#'
#' @examplesIf FALSE
#' library(flextable)
#' library(officer)
#' get_color_bar_text("Example Text")
#' text_bar <- get_color_bar_text("示例文本", font_name_ch = "微软雅黑")
#' read_docx() %>% body_add_flextable(text_bar, align = "left") %>%
#'   print(target = "color_bar_text_Example1.docx")
#' text_bar <- get_color_bar_text("Example Text 示例文本",
#'                                 font_name_ch = "微软雅黑",font_name_en = "Times New Roman")
#' read_docx() %>% body_add_flextable(text_bar, align = "left") %>%
#'   print(target = "color_bar_text_Example2.docx")
#'
#' @export
get_color_bar_text <- function(text,
                               text_color = "white",
                               bg_color = "#006699",
                               font_name_ch = "宋体", # "宋体"用 Unicode 表示为 "\u5b8b\u4f53"
                               font_name_en = "Times New Roman",
                               font_size = 12,
                               bar_width = cm2inche(16),
                               bar_height = cm2inche(2),
                               unit = "in",
                               padding_top = 0, # 段落前距离
                               padding_bottom = 0, # 段落后距离
                               padding_left = 0,
                               padding_right = 0
                               ){
  data.frame(Title = text) %>%
    flextable::flextable() %>%
    flextable::set_table_properties(layout = "fixed") %>%
    flextable::border_remove() %>%
    flextable::width(j= 1, width = bar_width,unit = unit) %>%
    flextable::height(i= 1, height = bar_height,unit = unit) %>%
    flextable::align(align = "left", part = "body") %>%
    flextable::bg(bg = bg_color, part = "body") %>%
    flextable::font(fontname = font_name_en, eastasia.family = font_name_ch, part = "body") %>%
    flextable::fontsize(size = font_size, part = "body") %>%
    flextable::color(color = text_color, part = "body") %>%
    flextable::delete_part(part = "header") %>% # 删掉表头
    flextable::padding(padding.top = padding_top, # 单元格内，段落前后间距，左右距离边缘的距离，单位磅
                       padding.bottom = padding_bottom,
                       padding.left = padding_left,
                       padding.right = padding_right,
                       part = "all")
}

