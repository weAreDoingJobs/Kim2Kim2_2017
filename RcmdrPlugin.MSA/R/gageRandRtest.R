#'
#'This is testing page
#'
#'@return gage r&r example
#'@export
#'
gageRandRtest <- function(){

  #result <- ss.rr(time1, prototype, operator, data = ss.data.rr,
  #                sub = "Six Sigma Paper Helicopter Project",
  #                alphaLim = 0.05, errorTerm = "interaction")

  ##
  library(SixSigma)

  initializeDialog(title = gettextRcmdr("Gage R&R Study (Crossed)"))

  xBox <- variableListBox(top, Numeric(), title = gettextRcmdr("Var (pick one)"))
  yBox <- variableListBox(top, Factors(), title = gettextRcmdr("part (pick one)"))
  zBox <- variableListBox(top, Factors(), title = gettextRcmdr("appr (pick one)"))
  UpdateModelNumber()

#ok 반응
   onOK <- function() {
    x <- getSelection(xBox)
    y <- getSelection(yBox)
    z <- getSelection(zBox)
    closeDialog()
    if (length(x) == 0) {
      UpdateModelNumber(-1)
      errorCondition(recall = gageRandRtest, message = gettextRcmdr("You must select a variable."))
      return()
    }
    if (length(y) != 1) {
      UpdateModelNumber(-1)
      errorCondition(recall = gageRandRtest, message = gettextRcmdr("You must select  one factor."))
      return()
    }
    if (length(z) != 1) {
      UpdateModelNumber(-1)
      errorCondition(recall = gageRandRtest, message = gettextRcmdr("You must select  one factor."))
      return()
    }

    command <-paste("ss.rr( var = ",x,", part = ",y,",appr = ",z,", data = ", ActiveDataSet()," )", sep = "")
    doItAndPrint(command)
    tkdestroy(top)
    tkfocus(CommanderWindow())
   }
   #UI구성
  OKCancelHelp(helpSubject = "hello")

  tkgrid(getFrame(xBox),getFrame(yBox),getFrame(zBox), sticky = "nw")
  tkgrid(buttonsFrame, columnspan = 2, sticky = "w")

  dialogSuffix(rows = 5, columns = 10)
}
