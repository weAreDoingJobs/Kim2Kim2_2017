#'
#'@param xST, LSL = NA,USL = NA,Target = NA, group = 1,alpha = 0.05,f.na.rm = TRUE, f.main = "Six Sigma Capability Analysis Study", f.sub = ""
#'@return pca.normDist
#'@export

#xST data$variable
#(LSL = NA) 하한
#(USL = NA) 상한
#(Target = NA) 타겟
#(group = 1) 몇개의 군이 있는지 = k
#(f.na.rm = TRUE) NA 값 있을 시 제외하고 통계 계산
#(withinSd1 = 1) 군내 표준편차 추정 방식 (1: R_bar, 2: S_bar, 3: 합동표준편차 방식)
#(withinSd2 = 1) 군내 표준편차 추정 방식 (1: 이동범위의 평균 방식, 2: 이동범위의 중위수 방식, 3: MSSD의 제곱근 방식)
pca.normdist <- function(xST, LSL = NA,USL = NA,Target = NA, group = 1,
                        f.na.rm = TRUE, withinSd1 =1, withinSd2 = 1) {
  library(qcc)
  if(is.na(Target)){
    stop("Target is needed.")
  }
  if(is.na(LSL) & is.na(USL)){
    stop("No specification limits provided.")
  }
  if(!(withinSd1 == 1 ) & !(withinSd1 ==2) & !(withinSd1 ==3)){
    stop("'withinSd1 message")
  }
  if(!(withinSd2 == 1 ) & !(withinSd2 ==2) & !(withinSd2 ==3)){
    stop("'withinSd2 message")
  }
  if(group <=0){
    stop("Group must be positive integer.")
  }

  #변수 정의 (meanST = 공정평균, nST = 전체 데이터 갯수=N, n = 군내 표본 갯수(n), xST : vector 형태로 전환)
  meanST <- mean(xST, na.rm = f.na.rm)
  nST <- length(xST[!is.na(xST)])
  n <- nST / group
  xST <- as.vector(xST)

  split <- NA
  rowSplit <- NA
  colSplit <- NA
  sr <- list(NA)
  overallC4 <- NA
  sdOver <- NA
  sdW <- NA

  #부분군 크기가 2이상인 경우
  if(n >=2){
    #데이터 군별로 쪼개기
    split <- splitby(data = xST, group = group)
    rowSplit <- nrow(split)
    colSplit <- ncol(split)

    #측정 데이터 평균, 범위, 표준편차
    split$mean <- apply(split, 1,mean)
    split$range <- apply(split[1:colSplit],1,rangeMinMax)
    split$sd <- apply(split[1:colSplit] ,1,sd)
    #각 값들의 평균 구하기
    sr <- list(mean = mean(split[,colSplit+1]),
               range = mean(split[,colSplit+2]),sd = mean(split[,colSplit+3]))

    #sdOver = OVERALL S.D., ccc>c4 사용
    if(!is.na(ccc[n,1])){
      overallC4 <- ccc[n,1]
    }else{
      overallC4 <- 4*(nST -1)/(4*nST -3)
    }
    sdOver <- sdOverAll(split = split, rowSplit = rowSplit, colSplit = colSplit,
                   nST = nST, mean = sr$mean, na.rm = f.na.rm, c4 = overallC4)

    #군내 표준편차 추정
    if(withinSd1 ==1){                        # R 방식
      sdW <- sd.R(data = split[1:colSplit])
    }else if(withinSd1 ==2){                  # S 방식
      sdW <- sd.S(data = split[1:colSplit])
    }else{                                    # 합동표준편차방식
      sp <- getSp(split = split, rowSplit= rowSplit, colSplit = colSplit, nST = nST, group = group)
      tempc4 <- group*(n-1)+1
      sdW <- sp/ccc[tempc4,1] # ccc>c4 사용
    }

  } # 부분군 크기가 2이상인 경우(end)

  #부분군 크기가 1인 경우 (N = k = group)
  if(n ==1){

    #데이터 군별로 쪼개기
    split <- splitby(data = xST, group = 20)
    names(split) <- "X1"
    rowSplit <- nrow(split)
    colSplit <- ncol(split)

    #측정 데이터 이동범위(moving range), 연속차(successive differences)
    split$MR <- c(NA,abs(split[2:rowSplit, colSplit] - split[1:(rowSplit-1), colSplit]))
    split$sucdiff <-c(NA,split[2:rowSplit, colSplit] - split[1:(rowSplit-1), colSplit])

    #각 값들의 평균 구하기
    sr <- list(mean = mean(split[,1]),MR = sum(split[,colSplit+1],na.rm = TRUE)/(group-1))

    #sdOver = OVERALL S.D., ccc>c4 사용
    overallC4 <- ccc[nST,1]
    sdOver <- sdOverAll2(split = split, rowSplit = rowSplit, colSplit = colSplit, nST = nST,
                    mean = sr$mean, na.rm = f.na.rm, c4 = overallC4)


  #군내 표준편차 추정
  if(withinSd2 ==1){                          # 이동범위의 평균 방식
    sdW <- sr$MR/ccc[2,3] # ccc>d2
  }else if(withinSd2 ==2){                    # 이동범위의 중위수 방식
    sdW <- median(split$MR,na.rm = TRUE)/ccc[2,5] # ccc>d4
  }else{                                      # MSSD의 제곱근 방식
    disq <- sum((split$sucdiff)^2)
    sdwup <- sqrt(disq/(2*(rowSplit-1)))
    sdW <- sdwup / ccc[rowSplit,1] # ccc>c4
  }


  }# 부분군 크기가 1인 경우(end)

  cplist <- ppclist(data = xST,LSL = LSL, USL = USL, meanST = meanST, sdTotal = sdW) #cp, cpu, cpl, cpk 해결해야함
  pplist <- tpplist(data = xST,LSL = LSL, USL = USL, meanST = meanST, sdOver = sdOver) #pp, ppu, ppl, ppk
  totallist <- list(cplist,pplist) # cplist + pplist

  print(totallist)
}
