#'
#'This is testing page
#'
#'@return ss.rr function example
#'@export
#'
msa.ss.rr <- function(){
  library(SixSigma)
  result <- ss.rr(time1, prototype, operator, data = ss.data.rr,
                  sub = "Six Sigma Paper Helicopter Project",
                  alphaLim = 0.05, errorTerm = "interaction")
  print(result)

}

