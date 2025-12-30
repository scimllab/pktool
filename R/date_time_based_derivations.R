#-------------------------------------------------------------------------------
#' Convert to numeric date time
#'
#' @param data
#' @param column
#'
#' @return
#' @export
#'
#' @examples
convert_datetime <- function(data, column) {
  lubridate::parse_date_time(
    data[[column]],
    orders = c("ymd HMS", "ymd HM", "ymd", "ymd_HM")
  )
}

#-------------------------------------------------------------------------------

#' Create numeric date and time variable
#'
#' @param .data dataset name
#' @param date date variable name
#' @param type date variable type
#'
#' @return convert character date time variable to numeric
#' @export
#'
#' @examples
#' @importFrom dplyr mutate filter select
datenum<-function(.data,date,type=""){
  dtc=sym(date)
  dtn=sym(paste0(date,"N"))
  if(type=='yymmddThhmm'){
    .data%>%mutate({{dtn}}:=ymd_hm({{dtc}}),MERGEDTN={{dtn}})
  }
  else if (type=='yymmdd'){
    .data%>%mutate({{dtn}}:=ymd_hm(paste0({{dtc}},"T00:01")),MERGEDTN={{dtn}})
  }
  else {.data%>%mutate({{dtn}}:=NA,MERGEDTN={{dtn}})}
}

#-------------------------------------------------------------------------------

# compute first dose
#' Title
#'
#' @param df
#' @param subject_var
#' @param date_var
#'
#' @return
#' @export
#'
#' @examples
compute_first_dose <- function(df, subject_var = "USUBJID", date_var = "EXSTDTCN") {
  df %>%
    group_by(across(all_of(subject_var))) %>%
    arrange(across(all_of(subject_var)), across(all_of(date_var))) %>%
    mutate(FDOSEDTN = dplyr::first(.data[[date_var]])) %>%
    distinct(across(all_of(subject_var)), FDOSEDTN) %>%
    ungroup()
}

#-------------------------------------------------------------------------------
#' Compute Lead In
#'
#' @param df
#' @param subject_var
#' @param date_var
#' @param lead_n
#' @param new_var
#'
#' @return
#' @export
#'
#' @examples
compute_lead <- function(df, subject_var = "USUBJID", date_var = "EXSTDTCN", lead_n = 1, new_var = "EXSTDTCN_LEAD") {
  df %>%
    group_by(across(all_of(subject_var))) %>%
    mutate("{new_var}" := lead(.data[[date_var]], n = lead_n)) %>%
    ungroup()
}

#-------------------------------------------------------------------------------

#' Actual Time Derivation
#'
#' @param df
#' @param join_df
#' @param by
#' @param date_var
#' @param ref_var
#' @param extra_mutate
#'
#' @return
#' @export
#'
#' @examples
join_and_compute_time <- function(df,
                                  join_df,
                                  by = "USUBJID",
                                  date_var = "EXSTDTCN",
                                  ref_var = "FDOSEDTN",
                                  extra_mutate = NULL) {

  # Ensure join_df has the reference column
  if (!ref_var %in% colnames(join_df)) {
    stop(glue::glue("Reference column '{ref_var}' not found in join_df"))
  }

  # Join the reference data
  df_joined <- df %>% left_join(join_df, by = by)

  # Check that the joined reference column exists
  if (!ref_var %in% colnames(df_joined)) {
    stop(glue::glue("After join, column '{ref_var}' not found in df_joined"))
  }

  # Compute ATAFD, EVID, TIME
  df_joined <- df_joined %>%
    mutate(
      !!sym("ATAFD") := as.numeric(difftime(.data[[date_var]], .data[[ref_var]], units = "hours"))
    )

  # Apply any extra user-defined mutate expressions
  if (!is.null(extra_mutate)) {
    df_joined <- df_joined %>% mutate(!!!extra_mutate)
  }

  df_joined
}


#-------------------------------------------------------------------------------

#' Derive TAD
#'
#' @param data
#' @param pctpt_var
#' @param pc_dt_var
#' @param ex_dt_var
#' @param ref_dt_var
#' @param atafd_var
#'
#' @return
#' @export
#'
#' @examples
compute_TAD_from_PCTPT <- function(
    data,
    pctpt_var = "PCTPT",
    pc_dt_var = "PCDTCN",
    ex_dt_var = "EXSTDTCN",
    ref_dt_var = "MEXSTDTCN",
    atafd_var = "ATAFD"
) {

  pctpt  <- rlang::sym(pctpt_var)
  pc_dt  <- rlang::sym(pc_dt_var)
  ex_dt  <- rlang::sym(ex_dt_var)
  ref_dt <- rlang::sym(ref_dt_var)
  atafd  <- rlang::sym(atafd_var)

  data %>%
    mutate(
      #-----------------------------------------------------------
      # 0. Missing PCTPT flag
      #-----------------------------------------------------------
      pctpt_missing = is.na(!!pctpt),

      #-----------------------------------------------------------
      # 1. Extract Flags
      #-----------------------------------------------------------
      prefl = if_else(!pctpt_missing & str_detect(!!pctpt, regex("PRE[:print:]?DOSE", TRUE)), 1, 0),
      hrfl  = if_else(!pctpt_missing & str_detect(!!pctpt, regex("hour|hr", TRUE)), 1, 0),
      minfl = if_else(!pctpt_missing & str_detect(!!pctpt, regex("min", TRUE)), 1, 0),

      #-----------------------------------------------------------
      # 2. Extract hours and minutes safely
      #-----------------------------------------------------------
      hr_txt = if_else(hrfl == 1,
                       str_extract(!!pctpt, regex("\\d*\\.?\\d+(?=\\s*(hour|hr))", TRUE)),
                       NA_character_),
      hr_val = if_else(!is.na(hr_txt) & str_detect(hr_txt, "^\\d*\\.?\\d+$"),
                       as.numeric(hr_txt), NA_real_),

      min_txt = if_else(minfl == 1,
                        str_extract(!!pctpt, regex("\\d*\\.?\\d+(?=\\s*min)", TRUE)),
                        NA_character_),
      min_val = if_else(!is.na(min_txt) & str_detect(min_txt, "^\\d*\\.?\\d+$"),
                        as.numeric(min_txt), NA_real_),

      #-----------------------------------------------------------
      # 3. Extract pure decimal PCTPT values (only numeric strings)
      #-----------------------------------------------------------
      pctpt_num_txt = if_else(!pctpt_missing &
                                str_detect(!!pctpt, "^\\d*\\.?\\d+$"),
                              !!pctpt,
                              NA_character_),
      pctpt_num = if_else(!is.na(pctpt_num_txt),
                          as.numeric(pctpt_num_txt),
                          NA_real_),

      #-----------------------------------------------------------
      # 4. NEW: Determine which datetime to use as event datetime
      #      event_dt = PCDTCN if available; else EXSTDTCN
      #-----------------------------------------------------------
      event_dt = case_when(
        !is.na(!!pc_dt) ~ !!pc_dt,
        !is.na(!!ex_dt) ~ !!ex_dt,
        TRUE      ~ as.POSIXct(NA)
      ),

      #-----------------------------------------------------------
      # 5. NEW: Compute TAD when PCTPT is missing
      #-----------------------------------------------------------
      tad_from_dates = case_when(
        pctpt_missing &
          !is.na(event_dt) & !is.na(!!ref_dt) ~
          as.numeric(as.Date(event_dt) - as.Date(!!ref_dt)) * 24,
        TRUE ~ NA_real_
      ),

      #-----------------------------------------------------------
      # 6. Final TAD Computation
      #-----------------------------------------------------------
      TAD = case_when(
        # Priority 1: Missing PCTPT logic
        !is.na(tad_from_dates)      ~ tad_from_dates,

        # Priority 2: PCTPT expressions
        prefl == 1            ~ 0,
        hrfl == 1 & minfl == 1      ~ hr_val + min_val / 60,
        hrfl == 1 & minfl == 0      ~ hr_val,
        hrfl == 0 & minfl == 1      ~ min_val / 60,
        !is.na(pctpt_num)        ~ pctpt_num,

        TRUE               ~ NA_real_
      )
    ) %>%
    select(-hr_txt, -min_txt, -pctpt_num_txt, -pctpt_missing)
}


#-------------------------------------------------------------------------------

#' Derive NTIME
#'
#' @param data
#' @param tad_var
#' @param pc_dt_var
#' @param ex_dt_var
#' @param mex_dt_var
#' @param fdose_var
#'
#' @return
#' @export
#'
#' @examples
derive_NTIME <- function(
    data,
    tad_var = "TAD",
    pc_dt_var = "PCDTCN",
    ex_dt_var = "EXSTDTCN",
    mex_dt_var = "MEXSTDTCN",
    fdose_var = "FDOSEDTN"
) {

  tad  <- sym(tad_var)
  pc_dt <- sym(pc_dt_var)
  ex_dt <- sym(ex_dt_var)
  mex_dt <- sym(mex_dt_var)
  fdt  <- sym(fdose_var)

  data %>%
    mutate(
      # determine event datetime (PC takes precedence, else EX)
      event_dt = case_when(
        !is.na(!!pc_dt) ~ !!pc_dt,
        !is.na(!!ex_dt) ~ !!ex_dt,
        TRUE      ~ as.POSIXct(NA)
      ),

      # date difference in days
      day_diff = as.numeric(as.Date(event_dt) - as.Date(!!fdt)),

      # NTIME calculation
      NTIME = case_when(
        # Case 1: TAD is not missing AND MEXSTDTCN differs from FDOSEDTN (or MEXSTDTCN missing)
        !is.na(!!tad) & (is.na(!!mex_dt) | (!!mex_dt != !!fdt)) & !is.na(event_dt) ~ day_diff * 24 + !!tad,

        # Case 2: TAD is not missing but MEXSTDTCN equals FDOSEDTN → just use TAD
        !is.na(!!tad) & !is.na(!!mex_dt) & !!mex_dt == !!fdt ~ !!tad,

        # Case 3: TAD is missing → fallback to date diff * 24
        is.na(!!tad) & !is.na(event_dt) ~ day_diff * 24,

        # else
        TRUE ~ NA_real_
      )
    ) %>%
    select(-event_dt, -day_diff)
}

