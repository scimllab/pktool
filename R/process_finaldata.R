#' Default columns to compute
#'
#' @param df
#' @param study
#'
#' @return
#' @export
#'
#' @examples
default_mutate <- function(df, study = 111) {
  df %>%
    mutate(
      STUDY  = study,
      PTNM  = {
        id_digits <- str_replace_all(USUBJID, "[^0-9]", "")
        as.numeric(paste0(study,str_sub(id_digits, -4)))
      },
      ANALYTE = if_else(EVID == 0, PCTESTCD, EXTRT),
      TYPE  = "PK",
      CRCL  = cg_creatinine_clearance(SEX, AGE, WEIGHT, WEIGHTU, CREAT, CREATU),
      CRCLU  = "mL/min",
      AMT   = if_else(EVID == 1, DOSE * 1000, NA_real_),
      AMTU  = "ng",
      BLQFL  = if_else(str_detect(PCORRES, "BLQ"), 1, 0),
      DV   = if_else(BLQFL == 1 & ATAFD < 0, 0, PCSTRESN),
      DVU   = PCSTRESU,
      DVC   = PCORRES,
      DVCU  = PCORRESU,
      DATE  = coalesce(PCDTC, EXSTDTC),
      ATAD  = if_else(EVID==0,as.numeric(difftime(PCDTCN, MEXSTDTCN, units = "hours")),as.numeric(difftime(EXSTDTCN, MEXSTDTCN, units = "hours")))
    )
}


#' Derive Modeling Related Variables
#'
#' @param df
#' @param study
#' @param user_mutate
#' @param select_cols
#' @param filter_expr
#'
#' @return
#' @export
#'
#' @examples
process_final_data <- function(df,
                            study = 111,
                            user_mutate = NULL,
                            select_cols = keepvars,
                            filter_expr = NULL) {

  # user sepcified fileter
  if (!is.null(filter_expr)) {
    df <- df %>% filter(!!!filter_expr)
  }

  # Apply default mutate
  df <- df %>% default_mutate(study)

  # Apply user-specified mutate if provided
  if (!is.null(user_mutate)) {
    df <- df %>% mutate(!!!user_mutate)
  }

  # Select columns safely
  #df <- df %>% select(any_of(select_cols))%>%arrange(USUBJID,ATAFD,TIME)

  return(df)
}
