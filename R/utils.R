generate_subject_ids <- function(n_subjects, prefix = "S") {
  stringr::str_c(
    prefix,
    stringr::str_pad(seq(1:n_subjects), width = nchar(n_subjects),
                     side = "left", pad = "0")
  )
}
