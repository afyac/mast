#' Admin matching function
#'
#' @param admins Admins to match
#' @param country Country to match admins against. Default = "SOM"
#'
admin_match <- function(admins, country = "SOM") {

 if(country == "SOM") {

   mtch <- match(text_simplify(admins), text_simplify(som_admins[, 1]))
   new <- som_admins[mtch, 2]

   if(any(is.na(new))) {
     message("Direct Matches Not Found. May use fuzzy matching")
     missing <- admins[is.na(new)]
     mtchs <- match_clean(missing, som_admins[, 2], quiet = !mast_loud())
     new[is.na(new)] <- som_admins[mtchs, 2]
   }

 }

  return(new)

}

#' @noRd
text_simplify <- function(x) {

  gsub(
    "[[:punct:][:space:]]",
    "",
    tolower(stringi::stri_trans_general(x, "latin-ascii"))
  )

}
