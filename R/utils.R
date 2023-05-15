`%||%` <- function(a, b) { # nolint
  if (is.null(a)) b else a
}


#------------------------------------------------
#' read_mast_file
#'
#' \code{read_mast_file} read mast package file
#'
#' @description Load a file from within the inst/extdata folder of the
#'   [mast] package. File extension must be one of .csv, .rds, or .xlsx
#'
#' @param name Name of a file within the inst/extdata folder.
#' @param sheet Name of sheet in xlsx file. Default = `NULL`
#'
#' @importFrom utils read.csv
#' @importFrom readxl read_xlsx
#' @keywords internal

read_mast_file <- function(name, sheet = NULL) {

  # check that valid file extension
  ext <- strsplit(name, "\\.")[[1]]
  ext <- ext[length(ext)]
  if (is.element(ext, c("csv", "rds", "xlsx")) == FALSE) {
    stop("file extension not valid")
  }

  # get full file path
  path <- system.file("extdata/", name, package = "mast", mustWork = TRUE)

  # read in file
  if (ext == "rds") {
    ret <- readRDS(path)
  } else if (ext == "csv") {
    ret <- utils::read.csv(file = path, header = TRUE, sep = ",")
  } else {
    ret <- readxl::read_xlsx(path, sheet = sheet)
  }

  return(ret)

}

#' @noRd
#' @keywords internal
quiet_message <- function(msg) {

  if (mast_loud()) {
    message(msg)
  }

}

#' @noRd
#' @keywords internal
mast_loud <- function(){
  if (Sys.getenv("MAST_LOUD") == "TRUE") {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' @noRd
#' @keywords internal
mast_set_loud <- function() {
  Sys.setenv("MAST_LOUD" = "TRUE")
}

#' @noRd
#' @keywords internal
mast_set_quiet <- function() {
  Sys.setenv("MAST_LOUD" = "FALSE")
}

#' @noRd
#' @keywords internal
match_clean <- function(a,b, quiet=TRUE){
  a <- gsub("[[:punct:][:space:]]","",tolower(stringi::stri_trans_general(a, "latin-ascii")))
  b <- gsub("[[:punct:][:space:]]","",tolower(stringi::stri_trans_general(b, "latin-ascii")))
  ret <- match(a,b)
  if(sum(is.na(ret)>0)){
    dists <- stringdist::seq_distmatrix(lapply(a,utf8ToInt),lapply(b,utf8ToInt))
    ret[is.na(ret)] <- apply(dists[which(is.na(ret)),,drop=FALSE],1,which.min)
    if(!quiet){
      print(unique(cbind(a,b[ret])))
    }
  }
  return(ret)
}

#------------------------------------------------
#' Save Figures Nicely
#'
#' \code{save_figs} saves ggplot and similar figures to file
#'
#' @description Saves ggplot and similar figures to both a pdf and
#'   to a png with the same size. PDF can then be easily edited after
#'   if needed for quick or complicated figure annotation.
#'
#' @param name Name of saved file (no file ending required). E.g. "my_plot"
#' @param fig ggplot or similar object
#' @param width Width of figure in inches. Default = 6
#' @param height Height of figure in inches. Default = 6
#' @param root Directory where figures are saved. Default is :
#'   `file.path(here::here(), "analysis/plots")`
#'
#' @importFrom utils read.csv
#' @importFrom readxl read_xlsx
#' @importFrom grDevices dev.off pdf
#' @keywords internal
save_figs <- function(name,
         fig,
         width = 6,
         height = 6,
         root = file.path(here::here(), "analysis/plots")) {

  dir.create(root, showWarnings = FALSE)
  fig_path <- function(name) {paste0(root, "/", name)}

  cowplot::save_plot(filename = fig_path(paste0(name,".png")),
                     plot = fig,
                     base_height = height,
                     base_width = width)

  pdf(file = fig_path(paste0(name,".pdf")), width = width, height = height)
  print(fig)
  dev.off()

}
