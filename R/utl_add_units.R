#' Add Units
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
add_units_to_varnames <- function(df) {

  # Get all column names
  vars <- names(df)

  # Find unit columns (ending with 'U')
  unit_cols <- vars[grepl("U$", vars)]

  for (u in unit_cols) {
    base <- sub("U$", "", u) # corresponding variable without U

    if (base %in% vars) {
      # Get first non-missing unit value
      unit_val <- df[[u]][!is.na(df[[u]])][1]

      # Skip if no unit found
      if (!is.null(unit_val) && nzchar(unit_val)) {
        # Rename the variable to include unit
        new_name <- paste0(base, " (", unit_val, ")")
        names(df)[names(df) == base] <- new_name
      }
    }
  }

  # Drop unit columns
  if((is.null(debug))){
    df <- df[, !grepl("U$", names(df)), drop = FALSE]
  }

  return(df)
}
