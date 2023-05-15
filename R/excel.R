#------------------------------------------------
#' mast_excel_lists
#'
#' \code{mast_excel_lists}
#'
#' @description Reads sheets from excel file and converts
#'   into a list, with named sheet per list.
#' @param sheets Name of sheets in xlsx file. Default = `NULL`. Takes
#'   priority over regex
#' @param pattern Regex for sheets to be used. Default = `NULL`
#' @param ... Other arguments passed to [readxl::read_xlsx]
#'
#' @inheritParams readxl::read_xlsx
#' @importFrom readxl read_xlsx
#' @export

mast_excel_lists <- function(path, sheets = NULL, pattern = NULL, ...) {

  # checks on args
  assert_file_exists(path)
  assert_null_and_func(sheets, assert_single_string)
  assert_null_and_func(pattern, assert_single_string)

  # how are we getting the sheets
  if (is.null(sheets) && is.null(pattern)) {
    sheets <- readxl::excel_sheets(path)
  } else if (!is.null(sheets)) {
    all_sheets <- readxl::excel_sheets(path)
    sheets <- all_sheets[sheets %in% all_sheets]
  } else if (is.null(sheets) && !is.null(pattern)) {
    sheets <- readxl::excel_sheets(path)
    sheets <- grep(pattern, sheets, value = TRUE)
  }

  # stop if nothing found
  if (length(sheets) == 0) {
    stop("No sheets found using sheets/pattern arguments")
  }

  # create our list
  targetxl_list <- lapply(sheets, function(x) {
    readxl::read_xlsx(path, sheet = x, ...)
  }) %>% stats::setNames(sheets)

  return(targetxl_list)

}
