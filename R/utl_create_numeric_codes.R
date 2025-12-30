
#' Create Numeric Codes For Covariates
#'
#' @param df
#' @param user_orders
#' @param auto_cols
#' @param start
#' @param suffix
#' @param overwrite
#' @param drop_original
#' @param default_code
#' @param missing_code
#' @param sort_auto
#'
#' @return
#' @export
#'
#' @examples
map_columns_hybrid <- function(df,
                               user_orders = list(),
                               auto_cols = NULL,
                               start = 1,
                               suffix = "N",
                               overwrite = FALSE,
                               drop_original = FALSE,
                               default_code = 99L,
                               missing_code = 0L,
                               sort_auto = TRUE) {

  # Helper to create numeric code mapping
  create_code_map <- function(vec, start_index) {
    codes <- seq(start_index, length(vec) + start_index - 1)
    setNames(codes, vec)
  }

  # Internal function to map a single column
  map_column <- function(df, col_name, values) {

    # Remove NA values from mapping
    values_clean <- values[!is.na(values)]
    code_map <- create_code_map(values_clean, start)
    num_col <- paste0(col_name, suffix)

    # Create numeric _N column
    df <- df %>%
      mutate("{num_col}" := case_when(
        is.na(.data[[col_name]]) ~ missing_code,
        TRUE ~ recode(as.character(.data[[col_name]]),
                      !!!code_map,
                      .default = default_code)
      ))

    # Optionally overwrite original column with "code=value"
    if (overwrite) {
      df <- df %>%
        mutate("{col_name}" := case_when(
          is.na(.data[[col_name]]) ~ paste0(missing_code,"=MISSING"),
          TRUE ~ recode(as.character(.data[[col_name]]),
                        !!!setNames(paste0(code_map,"=",values_clean), values_clean),
                        .default = paste0(default_code,"=OTHER"))
        ))
    }

    # Drop original column if requested
    if (drop_original) {
      df <- df %>% select(-all_of(col_name))
    }

    df
  }

  # 1. Handle user-specified orders (respect order)
  if (length(user_orders) > 0) {
    for (col_name in names(user_orders)) {
      df <- map_column(df, col_name, user_orders[[col_name]])
    }
  }

  # 2. Handle automated columns
  if (!is.null(auto_cols)) {
    for (col_name in auto_cols) {
      vals <- unique(df[[col_name]])
      if (sort_auto) vals <- sort(vals, na.last = TRUE)
      df <- map_column(df, col_name, vals)
    }
  }

  return(df)
}


