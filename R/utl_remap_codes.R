#' Recode Old Values To New Values
#'
#' @param x
#' @param mapping
#' @param default
#' @param missing_code
#' @param warn_unmapped
#'
#' @return
#' @export
#'
#' @examples
remap_vec <- function(x,
                      mapping,
                      default = NA,
                      missing_code = NA,
                      warn_unmapped = TRUE) {

  # Evaluate tidy expression inside mutate()
  x <- rlang::eval_tidy(x)

  # Check mapping
  if (is.null(names(mapping)) || any(names(mapping) == "")) {
    stop("`mapping` must be a named vector: c('old'='new')")
  }

  x_chr <- as.character(x)

  # warn about unmapped
  if (warn_unmapped) {
    unmapped <- setdiff(unique(x_chr), names(mapping))
    unmapped <- unmapped[!is.na(unmapped)]
    if (length(unmapped) > 0) {
      warning("Unmapped values found: ", paste(unmapped, collapse = ", "))
    }
  }

  out <- dplyr::recode(
    x_chr,
    !!!mapping,
    .default = as.character(default)
  )

  # missing code
  out[is.na(x)] <- as.character(missing_code)

  # auto-convert mapping result to numeric if appropriate
  if (all(!is.na(suppressWarnings(as.numeric(mapping))))) {
    out <- suppressWarnings(as.numeric(out))
  }

  out
}

