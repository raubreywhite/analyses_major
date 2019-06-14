only_letters <- function(x) { stringr::str_to_lower(gsub("[^[:alpha:]]", "", x)) }

check_ref_to_new <- function(xref, xnew){
  fail <- FALSE
  error_msg <- c()
  # reference files missing in new export
  missing_in_new <- xref[!xref %in% xnew]
  extra_in_new <- xnew[!xnew %in% xref]
  
  return(list(
    missing_in_new=missing_in_new,
    extra_in_new=extra_in_new
  ))
}
