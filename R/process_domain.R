#' Process Domain
#'
#' @param file_name
#' @param projpath
#' @param datetime_var
#' @param convert_datetime_fun
#' @param filter_expr
#' @param rename_list
#' @param drop_vars
#' @param mutate_expr
#'
#' @return
#' @export
#'
#' @examples
process_domain <- function(file_name,
                           projpath,
                           datetime_var = NULL,
                           convert_datetime_fun = convert_datetime,
                           filter_expr=NULL,
                           rename_list = NULL,
                           drop_vars=NULL,
                           mutate_expr =NULL) {

  df <- read_xpt(file.path(projpath, file_name))

  if (!is.null(datetime_var)) {
    df <- df %>%
      mutate("{datetime_var}N" := convert_datetime_fun(.data, datetime_var))
  }

  if (!is.null(rename_list)) {
    df <- df %>% rename(!!!rename_list)
  }
  if (!is.null(drop_vars)) {
    df <- df %>% select(-all_of(drop_vars))
  }

  if (!is.null(mutate_expr)) {
    df <- df %>% mutate(!!!mutate_expr)
  }
  if (!is.null(filter_expr)) {
    df <- df %>% filter(!!!filter_expr)
  }
  return(df)
}
