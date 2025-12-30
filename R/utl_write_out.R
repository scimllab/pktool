#' write out study data
#'
#' @param study
#' @param substudy
#' @param final
#' @param projpath
#'
#' @return
#' @export
#'
#' @examples
save_study_data <- function(study, substudy = NULL, final, projpath) {

  # ----- 1. Create list and store data -----
  #dfs <- list()
  #dfs[[study]] <- final

  # ----- 2. Build dynamic object name -----
  if (!is.null(substudy) && nzchar(substudy)) {
    objname <- paste0(study, substudy)
  } else {
    objname <- study
  }

  # Assign the data frame to that name
  #assign(objname, dfs[[study]], envir = .GlobalEnv)
  assign(objname, final, envir = .GlobalEnv)
  final_o <- add_units_to_varnames(final)

  # ----- 3. Build output file path safely -----
  if (!is.null(substudy) && nzchar(substudy)) {
    outfile <- file.path(projpath, "derived", paste0(study, substudy, ".csv"))
  } else {
    outfile <- file.path(projpath, "derived", paste0(study, ".csv"))
  }

  # Ensure the output folder exists
  if (!dir.exists(dirname(outfile))) {
    dir.create(dirname(outfile), recursive = TRUE)
  }

  # ----- 4. Write CSV -----
  #write.csv(dfs[[study]], outfile, row.names = FALSE, na = "")
  write.csv(final_o, outfile, row.names = FALSE, na = "")

  return(outfile)
}
