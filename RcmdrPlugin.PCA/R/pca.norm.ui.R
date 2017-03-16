#'
#'This is for SixSigma > ss.study.ca : RcmdrPlugin.PCA
#'
#'@return ss.study.ca result
#'@export

pca.norm.ui <- function(){
  library(SixSigma)
  initializeDialog(title = gettextRcmdr("Process Capability Analysis(Normal Distribution)"))
  #defaults
  defaults <- list(xLTnName = "NA", xLTmeanName = "NA", xLTsdName = "NA",
                   lslName= "NA", uslName = "NA",tarName = "NA")
  dialog.values <- getDialog("pca.norm.ui", defaults)

  #Frame
  xLTFrame <- tkframe(top)
  lsluslFrame <- tkframe(top)
  tarFrame <- tkframe(top)

  #xST SELECTION BOX (only numeric)
  xBox <- variableListBox(top, Numeric(), title = gettextRcmdr("Var (pick one)"))

  UpdateModelNumber()
  #xLT input box (only number)
  xLTnName <- tclVar(dialog.values$xLTnName)
  xLTnField <- ttkentry(xLTFrame, width = "20", textvariable = xLTnName)
  xLTmeanName <- tclVar(dialog.values$xLTmeanName)
  xLTmeanField <- ttkentry(xLTFrame, width = "20", textvariable = xLTmeanName)
  xLTsdName <- tclVar(dialog.values$xLTsdName)
  xLTsdField <- ttkentry(xLTFrame, width = "20", textvariable = xLTsdName)

  lslName <- tclVar(dialog.values$lslName)
  lslField <- ttkentry(lsluslFrame, width = "20", textvariable = lslName)
  uslName <- tclVar(dialog.values$uslName)
  uslField <- ttkentry(lsluslFrame, width = "20", textvariable = uslName)

  tarName <- tclVar(dialog.values$tarName)
  tarField <- ttkentry(tarFrame, width = "20", textvariable = tarName)

  #ok 반응
  onOK <- function() {
    closeDialog()
    #변수 받기
    x <- getSelection(xBox)
    nName <- round(as.numeric(trim.blanks(tclvalue(xLTnName))))
    meanName <- round(as.numeric(trim.blanks(tclvalue(xLTmeanName))))
    sdName <- round(as.numeric(trim.blanks(tclvalue(xLTsdName))))
    LSL <- round(as.numeric(trim.blanks(tclvalue(lslName))))
    USL <- round(as.numeric(trim.blanks(tclvalue(uslName))))
    target <- round(as.numeric(trim.blanks(tclvalue(tarName))))
    xLTcode <- ""

    if (length(x) == 0) {
      UpdateModelNumber(-1)
      errorCondition(recall = pca.norm.ui, message = gettextRcmdr("You must select a variable."))
      return()
    }
    if (!is.numeric(nName) || nName <=0 ){

      errorCondition(recall = pca.norm.ui, message = gettextRcmdr("You must put number bigger than 0."))
      return()
    }
    if (!is.numeric(meanName)) {

      errorCondition(recall = pca.norm.ui, message = gettextRcmdr("You must put numeric value."))
      return()
    }
     if (!is.numeric(sdName) || sdName <= 0 ) {

      errorCondition(recall = pca.norm.ui, message = gettextRcmdr("You must put numeric value bigger than 0."))
      return()
    }
    if (!is.numeric(LSL)) {

      errorCondition(recall = pca.norm.ui, message = gettextRcmdr("You must put numeric value."))
      return()
    }
    if (!is.numeric(USL)) {

      errorCondition(recall = pca.norm.ui, message = gettextRcmdr("You must put numeric value."))
      return()
    }
    if(is.NA(LSL) && is.na(USL)){

      errorCondition(recall = pca.norm.ui, message = gettextRcmdr("You must put numeric value either LSL or USL."))
      return()
    }
    if (!is.numeric(target)) {

      errorCondition(recall = pca.norm.ui, message = gettextRcmdr("You must put numeric value."))
      return()
    }
    xLTcode <- as.character(paste(nName,",",meanName,",",sdName))
    command <-paste("ss.study.ca( xST = ",x,", xLT = rnorm(",xLTcode,"),LSL = ",LSL,
                    ", USL = ", USL ,"Target = ",target," )", sep = "")
    doItAndPrint(command)
    tkdestroy(top)
    tkfocus(CommanderWindow())
  }
  #UI구성
  OKCancelHelp(helpSubject = "hello")

  tkgrid(getFrame(xBox), sticky = "nw")

  tkgrid(labelRcmdr(xLTFrame, text = gettextRcmdr("   N ")), xLTnField, sticky = "w")
  tkgrid(labelRcmdr(xLTFrame, text = gettextRcmdr("MEAN ")), xLTmeanField, sticky = "w")
  tkgrid(labelRcmdr(xLTFrame, text = gettextRcmdr("S.D. ")), xLTsdField, sticky = "w")
  tkgrid(xLTFrame, sticky = "w")

  tkgrid(labelRcmdr(lsluslFrame, text = gettextRcmdr(" LSL ")), lslField, sticky = "w")
  tkgrid(labelRcmdr(lsluslFrame, text = gettextRcmdr(" USL ")), uslField, sticky = "w")
  tkgrid(lsluslFrame, sticky = "w")

  tkgrid(labelRcmdr(tarFrame, text = gettextRcmdr("Target ")), tarField, sticky = "w")
  tkgrid(tarFrame, sticky = "w")

  tkgrid(buttonsFrame, columnspan = 2, sticky = "w")

  dialogSuffix(rows = 4, columns = 3)
}
