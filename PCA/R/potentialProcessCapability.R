#'
#'This is used total Potential Process Capability in R
#'
#'@return potential process capability
#'@export

ppclist <- function (data, LSL = NA, USL = NA, meanST = 0, sdTotal =1){
  if (is.na(LSL) & is.na(USL)) {
    stop("No specification limits provided")
  }
  if (!is.numeric(data)) {
    stop("Incorrect vector data")
  }

  cp <- (USL - LSL) / (6 * sdTotal)
  cpu <- (USL - meanST) / (3 * sdTotal)
  cpl <- (meanST - LSL) / (3 * sdTotal)
  cpk <- min(cpl,cpu)

  result <- list(cp = cp, cpu = cpu, cpl = cpl, cpk = cpk)
  return(result)
}
