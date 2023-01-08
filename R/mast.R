#' \pkg{mast} Munging and samrat tools
#'
#' Package for data munging (cleaning, collating etc) related
#' to the samrat package.
#'
#' @docType package
#' @keywords internal
#' @name mast
#'
#'
"_PACKAGE"

globalVariables(c("livelihood_substrings"))

# package variables
livelihood_substrings <- list(
  "livelihood" = c("livel", "lz", "LHZ", "lhz", "LZ",
                   "Livel", "lhood", "Lhood"),
  "agriculturalists" = c("gric", "cultur", "farm", "cultiv",
                         "Farm", "Cultur", "Cultiv"),
  "pastoralists" = c("past", "Past", "herd", "Herd", "cattle", "Cattle"),
  "agropastoralists" = c("grop", "grip", "groP", "griP",
                         "gro P", "gri P", "gro-p", "gri-p"),
  "riverine" = c("river", "River", "riv", "Riv"),
  "fishing" = c("fish", "Fish"),
  "urban" = c("urb", "Urb", "city", "City", "town", "Town"),
  "displaced" = c("displ", "IDP", "camp", "intern", "Displ",
                  "idp", "Camp", "PoC", "poc", "POC"),
  "refugee" = c("Ref", "ref", "gee")
)
