#-------------------------------------------------------------------------------
#' Baseline Covariates
#'
#' @param df
#' @param group_col
#' @param date_col
#' @param ref_date_col
#' @param required_col
#' @param condition
#'
#' @return
#' @export
#'
#' @examples
basechar_old <- function(df, group_col, date_col, ref_date_col, required_col, condition = NULL) {
  # Capture user-supplied columns as symbols
  group_col <- ensym(group_col)
  date_col <- ensym(date_col)
  ref_date_col <- ensym(ref_date_col)
  required_col <- ensym(required_col)
  # capture the user-supplied expression
  cond <- enquo(condition)

  # apply filter only if a condition was supplied
  if (!quo_is_null(cond)) {
    df <- df %>% filter(eval_tidy(cond, data = df))
  }

  df <- df %>%
    mutate(
      !!date_col := parse_date_time(!!date_col, orders = c("ymd HMS", "ymd HM", "ymd")),
      !!ref_date_col := parse_date_time(!!ref_date_col, orders = c("ymd HMS", "ymd HM", "ymd"))

    ) %>%
    group_by(across(all_of(group_col))) %>%
    # compute the latest date per group that is <= ref_date
    mutate(

      # Consider only rows where required_col is NOT NA and date <= ref_date
      latest_date_allowed = max(.data[[date_col]][!is.na(.data[[required_col]]) &
                                                    .data[[date_col]] <= .data[[ref_date_col]]],
                                na.rm = TRUE),
      # Flag only if the row is latest and required_col is not missing
      is_latest = .data[[date_col]] == latest_date_allowed & !is.na(.data[[required_col]]) &
        .data[[date_col]] <= .data[[ref_date_col]]
    ) %>%
    ungroup() %>%
    select(-latest_date_allowed) # optional: remove helper column

  return(df)
}

#-------------------------------------------------------------------------------
#' Baseline Covariates
#'
#' @param df
#' @param group_col
#' @param date_col
#' @param ref_date_col
#' @param required_col
#' @param analysis_col
#' @param method
#' @param condition
#'
#' @return
#' @export
#'
#' @examples
basechar <-function(df,
         group_col,
         date_col,
         ref_date_col,
         required_col,
         analysis_col = NULL,
         method = c("max_rowid", "avg_analysis"),
         condition = NULL) {

  method <- match.arg(method)

  group_col <- ensym(group_col)
  date_col <- ensym(date_col)
  ref_date_col <- ensym(ref_date_col)
  required_col <- ensym(required_col)
  analysis_col <- enquo(analysis_col)
  cond <- enquo(condition)

  if (!quo_is_null(cond)) {
    df <- df %>% filter(eval_tidy(cond, data = df))
  }

  df %>%
    mutate(
      !!date_col := parse_date_time(!!date_col, orders = c("ymd HMS", "ymd HM", "ymd")),
      !!ref_date_col := parse_date_time(!!ref_date_col, orders = c("ymd HMS", "ymd HM", "ymd")),
      .row_id = row_number()
    ) %>%
    group_by(across(all_of(group_col))) %>%
    mutate(
      eligible =
        !is.na(.data[[required_col]]) &
        .data[[date_col]] <= .data[[ref_date_col]]
    ) %>%

    # ---- method: max row id ----
  {
    if (method == "max_rowid") {
      mutate(
        .,
        max_row_id = max(.row_id[eligible], na.rm = TRUE),
        is_latest = eligible & .row_id == max_row_id
      )
    } else {
      if (quo_is_null(analysis_col)) {
        stop("analysis_col must be supplied when method = 'avg_analysis'")
      }
      mutate(
        .,
        avg_analysis_value = mean(.data[[analysis_col]][eligible], na.rm = TRUE),
        is_latest = FALSE
      )
    }
  } %>%
    ungroup() %>%
    select(-eligible, -.row_id, -any_of("max_row_id"))
}


#-------------------------------------------------------------------------------
#'creatinine in mg/dL in the formula
#'
#' @param SEX
#' @param AGE
#' @param WEIGHT
#' @param WEIGHTU
#' @param CREAT
#' @param CREATU
#'
#' @return
#' @export
#'
#' @examples
cg_creatinine_clearance <- function( SEX, AGE, WEIGHT, WEIGHTU, CREAT, CREATU) {
  # --- Weight conversion ---
  # --- Weight conversion (if lb → kg) ---
  WEIGHT_kg <- ifelse(tolower(WEIGHTU) == "lb",
                      WEIGHT * 0.45359237,
                      WEIGHT)

  # --- Creatinine conversion to µmol/L ---
  CREAT_umol <- ifelse(
    tolower(CREATU) == "mg/dl",
    CREAT * 88.4,
    ifelse(
      tolower(CREATU) == "mmol/l",
      CREAT * 1000, # mmol/L → µmol/L
      CREAT # already in µmol/L
    )
  )

  # --- Female adjustment (SEX == 1) ---
  adj <- ifelse(SEX == 1, 0.85, 1)

  # Cockcroft–Gault formula using CREAT in µmol/L and WEIGHT in kg
  result <- (140 - AGE) * WEIGHT * adj / (72 * (CREAT_umol / 88.4))
  round(result, 1)
}


#-------------------------------------------------------------------------------

#' VS Processing
#'
#' @param projpath
#' @param sourcedata
#' @param fdose
#'
#' @return
#' @export
#'
#' @examples
process_vs <- function(projpath, sourcedata, fdose) {
  vs <- read_xpt(file.path(projpath, sourcedata, "vs.xpt")) %>%
    mutate(VSDTCN = convert_datetime(.data, "VSDTC")) %>%
    left_join(fdose, by = "USUBJID")

  vswt <- vs %>%
    filter(VSTESTCD == "WEIGHT") %>%
    basechar(USUBJID, VSDTC, FDOSEDTN, VSSTRESN) %>%
    filter(is_latest) %>%
    rename(WEIGHT = VSSTRESN, WEIGHTU = VSSTRESU) %>%
    select(USUBJID, starts_with("WEIGHT"))

  vsht <- vs %>%
    filter(VSTESTCD == "HEIGHT") %>%
    basechar(USUBJID, VSDTC, FDOSEDTN, VSSTRESN) %>%
    filter(is_latest) %>%
    rename(HEIGHT = VSSTRESN, HEIGHTU = VSSTRESU) %>%
    select(USUBJID, starts_with("HEIGHT"))

  list(vswt = vswt, vsht = vsht)
}


#-------------------------------------------------------------------------------

# LB Processing
#' Title
#'
#' @param projpath
#' @param sourcedata
#' @param fdose
#'
#' @return
#' @export
#'
#' @examples
process_lb <- function(projpath, sourcedata, fdose) {
  lb <- read_xpt(file.path(projpath, sourcedata, "lb.xpt")) %>%
    mutate(LBDTCN = convert_datetime(.data, "LBDTC")) %>%
    left_join(fdose, by = "USUBJID")

  lb %>%
    filter(str_like(toupper(LBTEST), "%CREATININE%")) %>%
    basechar(USUBJID, LBDTCN, FDOSEDTN, LBSTRESN) %>%
    filter(is_latest) %>%
    rename(CREAT = LBSTRESN, CREATU = LBSTRESU) %>%
    select(USUBJID, starts_with("CREAT"))
}
