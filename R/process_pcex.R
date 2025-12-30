#' Make PC EX
#'
#' @param pc
#' @param ex
#' @param cols_to_fill
#'
#' @return
#' @export
#'
#' @examples
make_pcex <- function(pc, ex,
                      cols_to_fill = c("DOSFRM", "DOSFRQ", "ROUTE", "EXDOSE", "EXDOSU")) {

  pcex <- dplyr::bind_rows(pc, ex)

  # Fill forward by subject (custom cumsum grouping used in your code)
  pcex <- pcex %>%
    dplyr::group_by(USUBJID) %>%
    dplyr::arrange(USUBJID, TIME, dplyr::desc(EVID), .by_group = TRUE) %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(cols_to_fill),
        ~ {
          g <- cumsum(!is.na(.x))
          ave(.x, g, FUN = function(v) v[!is.na(v)][1])
        },
        .names = "{.col}_filled"
      )
    ) %>%
    dplyr::ungroup()

  # 3. Automatically rename: X_filled → new X name
  # remove _filled suffix and convert to the base variable name
  filled_names <- paste0(cols_to_fill, "_filled")

  # new names: drop EX prefix if present
  new_names <- gsub("^EX", "", cols_to_fill) # EXDOSE → DOSE, EXDOSU → DOSU

  # Build rename list dynamically
  rename_list <- setNames(filled_names, new_names)

  # 4. Drop original variables and rename the filled ones
  pcex <- pcex %>%
    dplyr::select(-dplyr::all_of(cols_to_fill)) %>%
    dplyr::rename(!!!rename_list)

  pcex<-compute_TAD_from_PCTPT(pcex)
  pcex<-derive_NTIME(pcex)
  return(pcex)
}
