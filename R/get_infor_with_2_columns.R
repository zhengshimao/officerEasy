#' Create a Two-Column Information Table
#'
#' This function generates a two-column table using the `flextable` package,
#' where each column contains text with customizable formatting options.
#' 1st column (`x`) typically represents descriptions, and 2nd column (`y`)
#' represents corresponding values. The table allows customization of font sizes,
#' alignment, borders, cell dimensions, and padding.
#'
#' @param x contents for 1st column.
#' @param y contents for 2nd column.
#' @param y_border_color Border color for 2nd column (default is "black").
#' @param y_border_style Border style for 2nd column (default is "solid"). see \link[officer]{fp_border}.
#' @param y_border_width Border width for 2nd column (default is 1). see \link[officer]{fp_border}.
#' @param x_align 1st column. text alignment - a single character value, expected value is one of 'left', 'right', 'center', 'justify'. 'distributed’ alignment is not possible.
#' @param y_align 2nd column. text alignment - a single character value, expected value is one of 'left', 'right', 'center', 'justify'. 'distributed' alignment is not possible.
#' @param x_fontsize Font size for 1st column text.
#' @param y_fontsize Font size for 2nd column text.
#' @param font_family_ch Font family for Chinese characters (default is "宋体").
#' @param font_family_en Font family for English characters (default is "Times New Roman").
#' @param font_family_cs Font family for complex script Unicode range.
#' @param cell_height Cell height in inches (default is cm2inche(c(1,1,1,1))). Temporarily invalid settings.
#' @param cell_width Cell width in inches (default is cm2inche(c(2,5))).
#' @param cell_unit Unit for cell width, one of "in", "cm", "mm".
#' @param padding_top Padding at the top of the cell (default is 0). Adjust the cell height of the table.
#' @param padding_bottom Padding at the bottom of the cell (default is 0). Adjust the cell height of the table.
#' @param padding_left Padding on the left side of the cell (default is 0).
#' @param padding_right Padding on the right side of the cell (default is 0).
#'
#' @return A `flextable` object representing the customized two-column table.
#'
#' @examplesIf FALSE
#' x <- c("Name", "School", "Grade", "Date")
#' y <- c("Xiaoming", "Yangguang School", "Grade 3", "2024-08-28")
#' table <- get_infor_with_2_columns(x, y)
#' read_docx() %>% body_add_flextable(table, align = "center") %>%
#'   print(target = "infor_table_Example1.docx")
#'
#' @export
get_infor_with_2_columns <- function(x,
                                     y,
                                     y_border_color = "black",
                                     y_border_style = "solid",
                                     y_border_width = 1,
                                     x_align = "center",
                                     y_align = "center",
                                     x_fontsize = 14,
                                     y_fontsize = x_fontsize,
                                     font_family_ch = "宋体",
                                     font_family_en = "Times New Roman",
                                     font_family_cs = font_family_en,
                                     cell_height = cm2inche(c(1,1,1,1)), # 数值设置无效。
                                     cell_width = cm2inche(c(2,5)),
                                     cell_unit = "in",
                                     padding_top = 0, # 段落前距离 # 用来控制表格高度
                                     padding_bottom = 0, # 段落后距离 # 用来控制表格高度
                                     padding_left = 0,
                                     padding_right = 0 ){

  stopifnot(length(x) == length(y))
  df <- data.frame("Description" = x, "Value" = y)

  if(length(cell_height) == 1) cell_height <- rep(cell_height, times = nrow(df))
  stopifnot(length(cell_height) == nrow(df))

  if(length(cell_width) == 1) cell_width <- rep(cell_width, times = ncol(df))
  stopifnot(length(cell_width) == ncol(df))

  ft <- flextable::flextable(df) %>% flextable::border_remove() %>% flextable::set_table_properties(layout = "fixed")
  ft <- ft %>%
    flextable::border(border = officer::fp_border(width = 0,color = "transparent")) %>% # 无边框表格
    flextable::border(j =2,
                      border.bottom = officer::fp_border(color = y_border_color,
                                                style = y_border_style,
                                                width = y_border_width), # 整数，不是英寸
                      part = "all") %>% # 对第2列表格边框添加
    flextable::fontsize(j = 1,size = x_fontsize, part = "body") %>% # 表格内字体大小 # 15 小三号；16 三号
    flextable::fontsize(j = 2,size = y_fontsize, part = "body") %>% # 表格内字体大小 # 15 小三号；16 三号
    flextable::font(i = seq_len(nrow(df)), j = seq_len(ncol(df)),
                    fontname = font_family_en,
                    cs.family = font_family_cs,
                    hansi.family = font_family_ch, eastasia.family = font_family_ch
                    ) %>%
    flextable::height(i = seq_len(nrow(df)),height = cell_height,part = "body", unit = cell_unit ) %>%
    flextable::width(j = seq_len(ncol(df)), width = cell_width, unit = cell_unit ) %>% # 每一列的宽度
    flextable::align(j = 1, align = x_align, part = "body") %>% # 表格内对其方式
    flextable::align(j = 2, align = y_align, part = "body") %>% # 表格内对其方式
    flextable::padding(padding.top = padding_top, # 单元格内，段落前后间距，左右距离边缘的距离，单位磅
                       padding.bottom = padding_bottom,
                       padding.left = padding_left,
                       padding.right = padding_right,
                       part = "all") %>%
    flextable::delete_part(part = "header")
  return(ft)
}
