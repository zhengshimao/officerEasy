#' Create a Formatted Paragraph
#'
#' This function generates a formatted paragraph for use in Word documents. It allows customization of text content, font properties, alignment, padding, and line spacing.
#'
#' @param ... Some text representing the content of the paragraph.
#' @param tab A character pasted at the beginning of text text. Default is a tab character (`"\t"`).
#' @param n_tab An integer specifying the number of tab characters.
#' @param font_size A numeric value or vector indicating the font size. Default is 12.
#' @param font_family_ch A character string specifying the font family for Chinese characters. Default is "宋体".
#' @param font_family_en single character value. Specifies the font to be used to format characters in the Unicode range (U+0000-U+007F). Default is "Times New Roman".
#' @param font_family_cs optional font to be used to format characters in a complex script Unicode range. For example, Arabic text might be displayed using the "Arial Unicode MS" font.
#' @param bold 	is bold. NULL or a numeric vector with same length to all text.
#' @param italic is italic. NULL or a numeric vector with same length to all text.
#' @param color text color. a character or a vector with same length to all text. Default is "black".
#' @param vertical_align single character value or a vector specifying font vertical alignments. Expected value is one of the following : default 'baseline' or 'subscript' or 'superscript'.
#' @param underlined is underlined. NULL or a numeric vector with same length to all text.
#' @param shading_color shading color - a single character value or a vector specifying a valid color (e.g. "#000000" or "black").
#' @param text_align text alignment - a single character value, expected value is one of 'left', 'right', 'center', 'justify'.
#' @param padding_left A numeric value specifying the left padding of the paragraph.
#' @param padding_right A numeric value specifying the right padding of the paragraph.
#' @param padding_top A numeric value specifying the top padding of the paragraph.
#' @param padding_bottom A numeric value specifying the bottom padding of the paragraph.
#' @param line_spacing line spacing, 1 is single line spacing, 2 is double line spacing.
#'
#' @return A `fpar` object representing the formatted paragraph.
#'
#' @details This function uses `fp_text` to set text formatting options such as font size, color, and bold style, and `fp_par` to set paragraph formatting options including alignment, padding, and line spacing. The result is a `fpar` object that can be used with functions from the `officer` package to create rich text content in Word documents.
#'
#' @examplesIf FALSE
#' library(officer)
#' text <- "《黑神话：悟空》是一款由中国游戏公司 Game Science 开发的动作角色扮演游戏（ARPG），灵感来自中国古典名著《西游记》。游戏以孙悟空为主角，结合了传统的中国神话元素和现代游戏设计理念，呈现出一个丰富的开放世界。"
#' para <- get_paragraph2(text, tab = " ", n_tab = 8)
#' read_docx() %>% body_add_fpar(para) %>% print("formatted_paragraph.docx")
#'
#' text1 <- "《"
#' text2 <- "黑神话：悟空"
#' text3 <- "》是一款由中国游戏公司"
#' text4 <- "Game Science"
#' text5 <- "开发的动作角色扮演游戏（ARPG），灵感来自中国古典名著《西游记》。游戏以孙悟空为主角，结合了传统的中国神话元素和现代游戏设计理念，呈现出一个丰富的开放世界。"
#' text <- c(text1,text2,text3,text4,text5)
#'
#' # Except for the parameter `tab` and `n_tab`, all parameters in this example can be single values or a vector.
#' para <- get_paragraph2(text, tab = "\t", n_tab = 1,
#'                        font_size = c(12,13,"四号",15,"二号"),
#'                        font_family_ch = "宋体",
#'                        font_family_en = "Times New Roman",
#'                        bold = c(2,4),
#'                        italic = c(4),
#'                        color = c("black","blue","red","green","purple"),
#'                        vertical_align = "baseline",
#'                        underlined = c(4,5),
#'                        shading_color = c("transparent","red","transparent","transparent","transparent")
#'                        )
#' read_docx() %>% body_add_fpar(para) %>% print("formatted_paragraph2.docx")
#' @export
#'
get_paragraph2 <- function(...,
                          tab = "\t",
                          n_tab = 1,
                          font_size = 12,
                          font_family_ch  = "宋体",
                          font_family_en = "Times New Roman",
                          font_family_cs = NULL,
                          bold = NULL,
                          italic = NULL,
                          color = "black",
                          vertical_align = "baseline",
                          underlined = NULL,
                          shading_color = "transparent",
                          text_align = "justify", # 段落文本，两端对齐
                          padding_left = 0,
                          padding_right = 0,
                          padding_top = 0,
                          padding_bottom = 0,
                          line_spacing = 1.25 # 段落内行距
){
  tabs <- rep(tab, times=n_tab) %>% paste0(collapse = "")

  text <- c(...)
  stopifnot(length(text) >= 1)

  font_size <- get_font_size(font_size)

  if( length(font_size) == 1 ) font_size <- rep(font_size, times = length(text))
  if( length(font_family_ch) == 1 ) font_family_ch <- rep(font_family_ch, times = length(text))
  if( length(font_family_en) == 1 ) font_family_en <- rep(font_family_en, times = length(text))
  if( length(font_family_cs) == 1 ) font_family_cs <- rep(font_family_cs, times = length(text))

  font_bold <- rep(FALSE, times = length(text))
  font_bold[bold] <- TRUE

  font_italic <- rep(FALSE, times = length(text))
  font_italic[italic] <- TRUE

  if( length(color) == 1 ) color <- rep(color, times = length(text))
  if( length(vertical_align) == 1 ) vertical_align <- rep(vertical_align, times = length(text))
  if( length(shading_color) == 1 ) shading_color <- rep(shading_color, times = length(text))

  font_underlined <- rep(FALSE, times = length(text))
  font_underlined[underlined] <- TRUE


  text[[1]] <- paste0(tabs,text[[1]], collapse = "")

  ft_all <- list()

  for (i in seq_along(text)) {
    prop <- officer::fp_text(color = color[i],
                             font.size = font_size[i],
                             bold = font_bold[i],
                             italic = font_italic[i],
                             font.family = font_family_en[i], # 单个字符值。指定用于格式化 Unicode 范围内字符的字体（U+0000-U+007F）。通常用于西文字符，如英文和数字。
                             eastasia.family = font_family_ch[i], # 可选的字体，用于格式化东亚 Unicode 范围内的字符。例如，日文文本可能会使用 "MS Mincho" 字体显示。
                             hansi.family = font_family_ch[i], # 可选的字体，用于格式化不属于上述其他类别的 Unicode 范围内的字符。通常用于处理汉字简化字体或其他未分类的字符。
                             cs.family = font_family_cs[i], # 可选的字体，用于格式化复杂脚本 Unicode 范围内的字符。例如，阿拉伯语文本可能会使用 "Arial Unicode MS" 字体显示。
                             vertical.align = vertical_align[i],
                             underlined = font_underlined[i],
                             shading.color = shading_color[i]
                             )
    ft <- officer::ftext(text = text[i],
                         prop = prop)
    ft_all[[i]] <- ft
  }


  fp_p <- officer::fp_par(text.align = text_align,     # 段落对齐
                          padding.left = padding_left, # 所有行首的填充
                          padding.top = padding_top,
                          padding.right = padding_right,
                          padding.bottom = padding_bottom,
                          line_spacing = line_spacing # 1.5倍行距
  )
  paragraph <- do.call(officer::fpar, c(ft_all, list(fp_p = fp_p)))
  # paragraph <- officer::fpar(ft, fp_p = fp_p)
  return(paragraph)
}

