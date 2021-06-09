#' Generate subject IDs
#'
#' Provides sequential numbered subject IDs for a specified number of subjects.
#'
#' @param n_subjects The number of subjects.
#' @param prefix The prefix of the IDs. "S" by default.
#'
#' @return A character vector of length `n_subjects`.
#' @export
#'
#' @examples
#' generate_subject_ids(100)
#' generate_subject_ids(20, "subj-")
#'
#' @importFrom stringr str_c str_pad
generate_subject_ids <- function(n_subjects, prefix = "S") {
  stringr::str_c(
    prefix,
    stringr::str_pad(seq(1:n_subjects), width = nchar(n_subjects),
                     side = "left", pad = "0")
  )
}
