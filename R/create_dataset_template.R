#' Create Dataset Template Function
#'
#' @param study
#' @param studyid
#' @param substudy
#' @param projpath
#'
#' @return
#' @export
#'
#' @examples
run_study_pipeline <- function(
    study  = "PK-01",
    studyid = 1101,
    substudy=NULL,
    projpath=projpath
) {

  debug<-NULL
  # -------------------------------------------------------------------
  # Process EX domain
  # -------------------------------------------------------------------
  ex <- process_domain(
    "ex.xpt", projpath,
    datetime_var = "EXSTDTC",
    rename_list =   c(
      DOSFRM = "EXDOSFRM",
      ROUTE = "EXROUTE",
      DOSFRQ = "EXDOSFRQ",
      DOSE = "EXDOSE",
      DOSEU = "EXDOSU"
    ),
    drop_vars  = c("EPOCH", "EXSEQ"),
    filter_expr = NULL
  )

  fdose <- compute_first_dose(ex)
  ex <- compute_lead(ex)

  ex <- join_and_compute_time(
    df     = ex,
    join_df   = fdose,
    by     = "USUBJID",
    date_var  = "EXSTDTCN",
    ref_var   = "FDOSEDTN",
    extra_mutate = quos(
      EVID = 1,
      TIME = ATAFD,
      MEXSTDTCN=EXSTDTCN
    )
  )

  # -------------------------------------------------------------------
  # Process PC domain
  # -------------------------------------------------------------------
  pc <- process_domain(
    "pc.xpt", projpath,
    datetime_var = "PCDTC",
    filter_expr =
  )

  pc <- join_and_compute_time(
    df     = pc,
    join_df   = fdose,
    by     = "USUBJID",
    date_var  = "PCDTCN",
    ref_var   = "FDOSEDTN",
    extra_mutate = quos(
      ATAFD = as.numeric(difftime(PCDTCN, FDOSEDTN, units = "hours")),
      EVID = 0,
      TIME = if_else(ATAFD <= 0, 0, ATAFD)
    )
  )

  pclast<-pc%>%filter(!(is.na(PCSTRESN)))%>%group_by(USUBJID)%>%arrange(USUBJID,PCDTCN)%>%mutate(lastPCDTCN=last(PCDTCN))%>%filter(PCDTCN==lastPCDTCN)%>%select(USUBJID,lastPCDTCN)%>%distinct()
  ex<-inner_join(ex,pclast,by = join_by(USUBJID))%>%filter(EXSTDTCN<=lastPCDTCN)%>%select(-lastPCDTCN)
  # -------------------------------------------------------------------
  # DM
  # -------------------------------------------------------------------
  dm <- process_domain("dm.xpt", projpath)

  # -------------------------------------------------------------------
  # VS → Weight / Height
  # -------------------------------------------------------------------
  vs <- process_domain("vs.xpt", projpath, datetime_var = "VSDTC")
  vs <- left_join(vs, fdose, by = join_by(USUBJID))

  vswt <- vs %>%
    filter(VSTESTCD == "WEIGHT") %>%
    basechar("USUBJID", "VSDTC", "FDOSEDTN", "VSSTRESN") %>%
    filter(is_latest) %>%
    rename(WEIGHT = VSSTRESN, WEIGHTU = VSSTRESU) %>%
    select(USUBJID, starts_with("WEIGHT"))

  vsht <- vs %>%
    filter(VSTESTCD == "HEIGHT") %>%
    basechar("USUBJID", "VSDTC", "FDOSEDTN", "VSSTRESN") %>%
    filter(is_latest) %>%
    rename(HEIGHT = VSSTRESN, HEIGHTU = VSSTRESU) %>%
    select(USUBJID, starts_with("HEIGHT"))

  # -------------------------------------------------------------------
  # LB → Creatinine
  # -------------------------------------------------------------------
  lb <- process_domain("lb.xpt", projpath, datetime_var = "LBDTC")
  lb <- left_join(lb, fdose, by = join_by(USUBJID))

  lbcreat <- lb %>%
    filter(str_like(toupper(LBTEST), "%CREATININE%")) %>%
    basechar("USUBJID", "LBDTCN", "FDOSEDTN", "LBSTRESN") %>%
    filter(is_latest) %>%
    rename(CREAT = LBSTRESN, CREATU = LBSTRESU) %>%
    select(USUBJID, starts_with("CREAT"))

  # -------------------------------------------------------------------
  # PC + EX combined
  # -------------------------------------------------------------------
  pcex <- make_pcex(pc, ex, cols_to_fill =  c("DOSFRM", "DOSFRQ", "ROUTE", "DOSE", "DOSEU", "MEXSTDTCN"))


  # -------------------------------------------------------------------
  # Final Join
  # -------------------------------------------------------------------
  final <- list(pcex, vswt, vsht, lbcreat) %>%
    reduce(left_join, by = "USUBJID")

  final<-inner_join(final,dm, by = "USUBJID")
  #final<-flag_dates_after_latest(final,"USUBJID","PCDTCN","EXSTDTCN")

  # -------------------------------------------------------------------
  # User-defined mutation + default column selection
  # -------------------------------------------------------------------

  final %>%
    mutate(
      STUDY  = 1,
      PTNM  = {
        id_digits <- str_replace_all(USUBJID, "[^0-9]", "")
        as.numeric(paste0(1,str_sub(id_digits, -4)))
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


  default_select_cols =c(
    "STUDYID","STUDY","USUBJID","PTNM","ANALYTE","TIME","ATAFD","DATE",
    "EVID","DV","DVU","DVC","DVCU","BLQFL","ARM","DOSE",
    "DOSEU","AMT","DOSFRM","DOSFRQ","ROUTE",
    "WEIGHT","WEIGHTU","SEX","RACE",
    "AGE","AGEU","HEIGHT","HEIGHTU","CREAT","CREATU","CRCL","CRCLU")


  final<-final %>% select(any_of(default_select_cols))%>%arrange(USUBJID,ATAFD,TIME)

  # -------------------------------------------------------------------
  # Apply user ordering + automatic ordering
  # -------------------------------------------------------------------

  user_orders=NULL
  auto_cols   = c("ARM","DOSFRQ","ROUTE","SEX","RACE","ETHNIC","COUNTRY","POP", "FORM")
  poppkdata <-map_columns_hybrid(final,user_orders, auto_cols, overwrite = F,drop_original = F)
  auto_cols   = "USUBJID"
  poppkdata <-map_columns_hybrid(final,NULL, auto_cols)

  poppkdata<-poppkdata%>%filter(!is.na(TIME))%>%select(STUDY, USUBJID, PTNM, DATE, TIME, NTIME, TAD, ATAD, starts_with("DV"), starts_with("BLQFL"),EVID, DOSE,DOSEU, AMT, AMTU, ARMN, DOSFRQN, ROUTEN, WEIGHT,WEIGHTU, SEXN, RACEN, ETHNICN, COUNTRYN, AGE,AGEU, HEIGHT,HEIGHTU, CRCL,CRCLU, POPN, FORMN, USUBJIDN)

  poppkdatacsv<-add_units_to_varnames(poppkdata)

  write.csv(poppkdatacsv, file.path(projpath, "derived", "poppkdata.csv"), row.names = FALSE, na = "")


  # Create data definition
  #metadata <- create_data_definition(final, exclude_vars = c("USUBJID","DATE","DVC"))

  #final <- add_units_to_varnames(final)

  # -------------------------------------------------------------------
  # Save output
  # -------------------------------------------------------------------
  outfile <- save_study_data(
    study  = study,
    substudy = substudy,
    final  = final,
    projpath = projpath
  )

  return(list(
    final   = final,
    output   = outfile,
    fdose   = fdose,
    ex     = ex,
    pc     = pc,
    lbcreat  = lbcreat,
    pcex    = pcex,
    study_path = projpath2,
    metadata  = metadata,
    vs = vs,
    dm= dm,
    lb =lb
  ))

}
