#'
#'@param xST xLT= NA, LSL = NA,USL = NA,Target = NA, group = 1,alpha = 0.05, f.na.rm = TRUE, f.main = "Six Sigma Capability Analysis Study", f.sub = ""
#'@return pca.withBet
#'@export

#xST data$variable
#(LSL = NA) 하한
#(USL = NA) 상한
#(Target = NA) 타겟
#(group = 1) 몇개의 군이 있는지 = k
#(f.na.rm = TRUE) NA 값 있을 시 제외하고 통계 계산
#(withinSD = 1) 군내 표준편차 추정 방식 (1: R_bar, 2: S_bar, 3: 합동표준편차 방식)
#(betSd = 1) 군간 표준편차 추정 방식 (1: 이동범위의 평균 방식, 2: 이동범위의 중위수 방식, 3: MSSD의 제곱근 방식)
pca.withBet <- function(xST, LSL = NA,USL = NA,Target = NA, group = 1,
                        f.na.rm = TRUE, withinSd =1, betSd = 1) {
  library(qcc)
  if(is.na(Target)){
    stop("Target is needed.")
  }
  if(is.na(LSL) & is.na(USL)){
    stop("No specification limits provided.")
  }
  if(!(withinSd == 1 ) & !(withinSd ==2) & !(withinSd ==3)){
    stop("'withinSd message")
  }
  if(!(betSd == 1 ) & !(betSd ==2)){
    stop("betSd message")
  }

  #변수 정의 (meanST = 공정평균, nST = 전체 데이터 갯수=N, n = 군내 표본 갯수(n))
  meanST <- mean(xST, na.rm = f.na.rm)
  nST <- length(xST[!is.na(xST)])
  n <- nST / group

  #데이터 군별로 쪼개기
  xST <- as.vector(xST)
  split <- splitby(data = xST, group = group)
  rowSplit <- nrow(split)
  colSplit <- ncol(split)
  #측정 데이터 평균, 이동범위, 범위, 표준편차
  split$mean <- apply(split, 1,mean)
  split$MR <- c(NA,abs(split[2:rowSplit, colSplit+1] - split[1:(rowSplit-1), colSplit+1]))
  split$range <- apply(split[1:colSplit],1,rangeMinMax)
  split$sd <- apply(split[1:colSplit] ,1,sd)
  #각 값들의 평균 구하기
  sr <- list(mean = mean(split[,colSplit+1]), MR = sum(split[,colSplit+2],na.rm = TRUE)/(group-1),
             range = mean(split[,colSplit+3]),sd = mean(split[,colSplit+4]))

  #군내 표준편차 추정
  sdW <- NA
  if(withinSd ==1){#R 방식
    sdW <- sd.R(data = split[1:colSplit])
  }else if(withinSd ==2){#S 방식
    sdW <- sd.S(data = split[1:colSplit])
  }else{#합동표준편차방식
    sp <- getSp(split = split, rowSplit= rowSplit, colSplit = colSplit, nST = nST, group = group)
    tempc4 <- group*(n-1)+1
    sdW <- sp/ccc[tempc4,1] # ccc>c4 사용
  }

  #군간 표준편차 추정
  sdX <- NA
  if(betSd ==1){#이동범위의 평균 방식
    sdX <- sr$MR/ccc[2,3] # ccc>d2
  }else{#이동범위의 중위수 방식
    sdX <- median(split$MR,na.rm = TRUE)/ccc[2,5] # ccc>d4
  }

  sdB <- sqrt((sdX^2)-(sdW/n))
  sdTotal <- sqrt(sdW^2 + sdB^2) #sdTotal = TOTAL S.D.
  sdOver <- sdOverAll(split,rowSplit = rowSplit, colSplit = colSplit,
                      nST = nST, mean =sr$mean, na.rm = f.na.rm, c4 = ccc[n,1]) #sdOver = OVERALL S.D., ccc>c4 사용
  cplist <- ppclist(data = xST,LSL = LSL, USL = USL, meanST = meanST, sdTotal = sdTotal) #cp, cpu, cpl, cpk
  pplist <- tpplist(data = xST,LSL = LSL, USL = USL, meanST = meanST, sdOver = sdOver) #pp, ppu, ppl, ppk
  totallist <- list(cplist,pplist) # cplist + pplist

  print(totallist)
}
