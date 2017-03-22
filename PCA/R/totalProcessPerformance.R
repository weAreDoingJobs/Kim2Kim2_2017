#'
#'This is used total Process Performance in R
#'
#'@return total standard devtiation
#'@export

tpplist <- function (data, LSL = NA, USL = NA, meanST = 0, sdOver = 1){
  if (is.na(LSL) & is.na(USL)) {
    stop("No specification limits provided")
  }
  if (!is.numeric(data)) {
    stop("Incorrect vector data")
  }

  pp <- (USL - LSL) / (6 * sdOver)
  ppu <- (USL - meanST) / (3 * sdOver)
  ppl <- (meanST - LSL) / (3 * sdOver)
  ppk <- min(ppl,ppu)
  result <- list(pp = pp, ppu = ppu, ppl = ppl, ppk = ppk)
  return(result)
}
