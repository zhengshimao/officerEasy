#' Set Custom Cover Page Layout
#'
#' This function creates section properties for a cover page layout in a Word document. It allows customization of page dimensions, margins, headers, and footers, and is adaptable to various page sizes.
#'
#' @param page_width The width of the page in inches.
#' @param page_height The height of the page in inches.
#' @param page_orient Page orientation, either 'landscape', either 'portrait'.
#' @param section_type Section type. It defines how the contents of the section will be placed relative to the previous section.
#' Available types are "continuous" (begins the section on the next paragraph),
#' "evenPage" (begins on the next even-numbered page), "nextColumn" (begins on the next column on the page), "nextPage" (begins on the following page), "oddPage" (begins on the next odd-numbered page).
#' @param top The top margin in inches. Default is 0.
#' @param bottom The bottom margin in inches. Default is 0.
#' @param left The left margin in inches. Default is 0.
#' @param right The right margin in inches. Default is 0.
#' @param header The header margin in inches. Default is 0.
#' @param footer The footer margin in inches. Default is 0.
#'
#' @return A `prop_section` object containing the layout properties for the cover page.
#'
#' @details This function is useful for creating a cover page layout with custom page sizes and margins. The `page_width` and `page_height` parameters are optional; if not provided, the current settings of the Word document will be used.
#'
#' @examplesIf FALSE
#' library(officer)
#' cover_layout <- set_page_layout(page_width= 8.27, page_height = 11.69)
#' doc <- read_docx() %>%
#'   body_add_par("Cover Page Title", style = "heading 1") %>%
#'   body_set_default_section(cover_layout) %>%
#'   print(target = "A4_cover_page.docx")
#'
#' # Using the image as the cover
#' doc <- read_docx() %>%
#'   body_set_default_section( cover_sect_properties ) %>%
#'   body_add_img(src = "./input/cover.jpg", width = 8.27, height = 11.69) %>%
#'   body_end_block_section(value = block_section(property = cover_sect_properties)) # 分节设置
#'
#' @export
set_page_layout <- function(  page_width= NULL,
                              page_height = NULL,
                              page_orient = "portrait", # portrait 纵向， landscape 为横向
                              section_type = "nextPage", # continuous 分节符，连续页 # nextPage 下一页开始
                              top = 0,    # 上边距 # 单位为英寸
                              bottom = 0, # 下边距
                              left = 0,   # 左边距
                              right = 0,  # 右边距
                              header = 0, # 页眉
                              footer = 0  # 页脚
                            ){
# 封面部分页面布局
cover_sect_properties <- officer::prop_section(
  page_size = officer::page_size(orient = page_orient,
                                 width = page_width, height = page_height
                                 ), type = section_type,

  page_margins = officer::page_mar(top = 0,    # 上边距 # 单位为英寸
                                   bottom = 0, # 下边距
                                   left = 0,   # 左边距
                                   right = 0,  # 右边距
                                   header = 0, # 页眉
                                   footer = 0,  # 页脚
                                   gutter = 0
                                   )
)
return(cover_sect_properties)
}


#' Set A4 Cover Page Layout
#'
#' This function sets up the layout for an A4-sized cover page in a Word document. It configures the page dimensions and margins specifically for A4 paper size.
#'
#' @param page_width The width of the page in inches. Default is 8.27 inches, which is the standard width for A4 paper.
#' @param page_height The height of the page in inches. Default is 11.69 inches, which is the standard height for A4 paper.
#' @param top The top margin in inches. Default is 0.
#' @param bottom The bottom margin in inches. Default is 0.
#' @param left The left margin in inches. Default is 0.
#' @param right The right margin in inches. Default is 0.
#' @param header The header margin in inches. Default is 0.
#' @param footer The footer margin in inches. Default is 0.
#'
#' @return A `prop_section` object with the layout properties for the A4 cover page.
#'
#' @details This function is a wrapper around the `set_page_layout` function, pre-configured for A4-sized paper. It is useful for generating cover pages in A4 format with specific margins and layout settings.
#'
#' @examplesIf FALSE
#' library(officer)
#' cover_layout <- set_A4_cover_layout()
#' doc <- read_docx() %>%
#'   body_add_par("Cover Page Title", style = "heading 1") %>%
#'   body_set_default_section(cover_layout) %>%
#'   print(target = "A4_cover_page.docx")
#'
#' # Using the image as the cover
#' doc <- read_docx() %>%
#'   body_set_default_section( cover_sect_properties ) %>%
#'   body_add_img(src = "./input/cover.jpg", width = 8.27, height = 11.69) %>%
#'   body_end_block_section(value = block_section(property = cover_sect_properties)) # 分节设置
#'
#' @export
set_A4_cover_layout <- function( page_width= 8.27,
                                 page_height = 11.69,
                                 top = 0,    # 上边距 # 单位为英寸
                                 bottom = 0, # 下边距
                                 left = 0,   # 左边距
                                 right = 0,  # 右边距
                                 header = 0, # 页眉
                                 footer = 0  # 页脚
                                 ){

  cover_layout <- set_page_layout(page_width= page_width,
                                   page_height = page_height,
                                   top = top,    # 上边距 # 单位为英寸
                                   bottom = bottom, # 下边距
                                   left = left,   # 左边距
                                   right = right,  # 右边距
                                   header = header, # 页眉
                                   footer = footer  # 页脚
                                   )
  return(cover_layout)

}

#' Set B5 Cover Page Layout
#'
#' This function configures a section layout for a B5-sized cover page in a Word document. It sets the page dimensions, margins, headers, and footers using the standard B5 size.
#'
#' @param page_width The width of the page in inches. Default is 6.93 inches (B5 width).
#' @param page_height The height of the page in inches. Default is 9.84 inches (B5 height).
#' @param top The top margin in inches. Default is 0.
#' @param bottom The bottom margin in inches. Default is 0.
#' @param left The left margin in inches. Default is 0.
#' @param right The right margin in inches. Default is 0.
#' @param header The header margin in inches. Default is 0.
#' @param footer The footer margin in inches. Default is 0.
#'
#' @return A `prop_section` object containing the layout properties for the B5 cover page.
#'
#' @details This function is a wrapper around `set_page_layout`, specifically for creating a B5-sized cover page layout. It sets the page size to the B5 standard dimensions and allows for customization of the page margins, headers, and footers.
#'
#' @examplesIf FALSE
#' library(officer)
#' cover_layout <- set_B5_cover_layout()
#' doc <- read_docx() %>%
#'   body_add_par("B5 Cover Page Title", style = "heading 1") %>%
#'   body_set_default_section(cover_layout) %>%
#'   print(target = "B5_cover_page.docx")
#'
#' @export
set_B5_cover_layout <- function( page_width= 6.93,
                                 page_height = 9.84,
                                 top = 0,    # 上边距 # 单位为英寸
                                 bottom = 0, # 下边距
                                 left = 0,   # 左边距
                                 right = 0,  # 右边距
                                 header = 0, # 页眉
                                 footer = 0  # 页脚
){

  cover_layout <- set_page_layout(page_width= page_width,
                                   page_height = page_height,
                                   top = top,    # 上边距 # 单位为英寸
                                   bottom = bottom, # 下边距
                                   left = left,   # 左边距
                                   right = right,  # 右边距
                                   header = header, # 页眉
                                   footer = footer  # 页脚
  )
  return(cover_layout)

}

#' Set A3 Cover Page Layout
#'
#' This function configures a section layout for an A3-sized cover page in a Word document. It sets the page dimensions, margins, headers, and footers using the standard A3 size.
#'
#' @param page_width The width of the page in inches. Default is 11.7 inches (A3 width).
#' @param page_height The height of the page in inches. Default is 16.5 inches (A3 height).
#' @param top The top margin in inches. Default is 0.
#' @param bottom The bottom margin in inches. Default is 0.
#' @param left The left margin in inches. Default is 0.
#' @param right The right margin in inches. Default is 0.
#' @param header The header margin in inches. Default is 0.
#' @param footer The footer margin in inches. Default is 0.
#'
#' @return A `prop_section` object containing the layout properties for the A3 cover page.
#'
#' @details This function is a wrapper around `set_page_layout`, specifically for creating an A3-sized cover page layout. It sets the page size to the A3 standard dimensions and allows for customization of the page margins, headers, and footers.
#'
#' @examplesIf FALSE
#' library(officer)
#' cover_layout <- set_A3_cover_layout()
#' doc <- read_docx() %>%
#'   body_add_par("A3 Cover Page Title", style = "heading 1") %>%
#'   body_set_default_section(cover_layout) %>%
#'   print(target = "A3_cover_page.docx")
#'
#' @export
set_A3_cover_layout <- function( page_width= 11.7,
                                 page_height = 16.5,
                                 top = 0,    # 上边距 # 单位为英寸
                                 bottom = 0, # 下边距
                                 left = 0,   # 左边距
                                 right = 0,  # 右边距
                                 header = 0, # 页眉
                                 footer = 0  # 页脚
){

  cover_layout <- set_page_layout(page_width= page_width,
                                   page_height = page_height,
                                   top = top,    # 上边距 # 单位为英寸
                                   bottom = bottom, # 下边距
                                   left = left,   # 左边距
                                   right = right,  # 右边距
                                   header = header, # 页眉
                                   footer = footer  # 页脚
  )
  return(cover_layout)

}


