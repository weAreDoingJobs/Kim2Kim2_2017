#'
#'This is used for getting total standard deviation.
#'@param c(split, rowSplit, colSplit, nST, mean, na.rm = FALSE, c4 = 1)
#'@return total standard devtiation for n!=1
#'@export
sdOverAll <- function(split, rowSplit, colSplit, nST, mean, na.rm = FALSE, c4 = 1){
  subpow <- 0
  for(i in 1:rowSplit){
    for(j in 1:colSplit){
      subpow <- subpow + (split[i,j]-mean)^2
    }
  }
  overallBefore <- sqrt(subpow/(nST-1))
  result<- overallBefore/c4
  return(result)
}

#`
#'This is used for getting total standard deviation.(when n ==1)
#'@param c(split, rowSplit, colSplit, nST, mean, na.rm = FALSE, c4 = 1)
#'@return total standard devtiation for n==1
#'@export
sdOverAll2 <- function(split, rowSplit, colSplit, nST, mean, na.rm = FALSE, c4 = 1){
  subpow <- 0
  for(i in 1:rowSplit){
    for(j in 1:colSplit){
      subpow <- subpow + (split[i,j]-mean)^2
    }
  }
  overallBefore <- sqrt(subpow/nST-1)
  result<- overallBefore/c4
  return(result)
}
#'
#'This is used for spliting data by group
#'@param c(data,group)
#'@return splited data
#'@export
splitby <- function (data, group){

  reshape <- function(df, nrow, ncol, byrow = TRUE) data.frame(matrix(as.matrix(df), nrow, ncol, byrow = byrow))
  ndata <- length(data)
  ncolumn <- floor(ndata/group)
  result <- reshape(df = data, nrow = group, ncol = ncolumn)
  return(result)
}

#'
#'This is used for subtract max to min
#'@param data
#'@return max - min
#'@export
rangeMinMax <- function (data){

  result <- range(data)[2]-range(data)[1]
  return(result)
}

#'
#'This is used to get Sp
#'@param data
#'@return sp
#'@export
getSp <- function (split, rowSplit, colSplit, nST, group){
  n <- nST / group
  subpow <- 0
  for(i in 1:rowSplit){
    for(j in 1:colSplit){
      subpow <- subpow + (split[i,j] - split[i,colSplit+1])^2
    }
  }
  sp <- subpow/(group*(n-1))
  retrun()
}
