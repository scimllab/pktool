#' Create metadata definition
#'
#' @param df
#' @param include_levels
#' @param exclude_vars
#'
#' @return
#' @export
#'
#' @examples
create_data_definition <- function(df, include_levels = TRUE, exclude_vars = NULL) {
  if (!is.data.frame(df)) stop("Input must be a data frame")

  # Basic info
  df_def <- data.frame(
    Variable  = names(df),
    Type    = sapply(df, function(x) class(x)[1]),
    N_missing = sapply(df, function(x) sum(is.na(x))),
    N_unique  = sapply(df, function(x) length(unique(x))),
    stringsAsFactors = FALSE
  )

  # Optional: Levels for factor or character columns
  if (include_levels) {
    df_def$Levels <- sapply(df, function(x) {
      if (is.factor(x) || is.character(x)) {
        lvls <- unique(x)
        lvls <- lvls[!is.na(lvls)]
        paste(lvls, collapse = ", ")
      } else {
        NA
      }
    })
  } else {
    df_def$Levels <- NA
  }

  # Optional: min/max for numeric columns
  df_def$Min <- sapply(df, function(x) if (is.numeric(x)) min(x, na.rm=TRUE) else NA)
  df_def$Max <- sapply(df, function(x) if (is.numeric(x)) max(x, na.rm=TRUE) else NA)

  # Exclude certain variables from Levels/Min/Max
  if (!is.null(exclude_vars)) {
    df_def$Levels[df_def$Variable %in% exclude_vars] <- NA
    df_def$Min[df_def$Variable %in% exclude_vars] <- NA
    df_def$Max[df_def$Variable %in% exclude_vars] <- NA
  }

  return(df_def)
}
