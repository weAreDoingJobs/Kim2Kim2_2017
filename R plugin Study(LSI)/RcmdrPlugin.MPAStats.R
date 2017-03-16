
library(RcmdrPlugin.MPAStats)
singleSampleTTest2
function ()
{
  defaults <- list(initial.x = NULL, initial.alternative = "two.sided",
                   initial.level = ".95", initial.plots = "No", initial.mu = "0.0")
  dialog.values <- getDialog("singleSampleTTest2", defaults)
  initializeDialog(title = gettextRcmdr("Single-Sample t-Test"))
  xBox <- variableListBox(top, Numeric(), title = gettextRcmdr("Variable (pick one)"),
                          initialSelection = varPosn(dialog.values$initial.x, "numeric"))
  
  
  radioButtons(top, name = "plots", buttons = c("Yes", "No"),
               values = c("Yes", "No"), labels = gettextRcmdr(c("Yes","No")),
               title = gettextRcmdr("Plot?"), initialValue = dialog.values$initial.plot)
  

  
  onOK <- function() {
    x <- getSelection(xBox)
    if (length(x) == 0) {
      errorCondition(recall = singleSampleTTest2, message = gettextRcmdr("You must select a variable."))
      return()
    }
    alternative <- as.character(tclvalue(alternativeVariable))
    level <- tclvalue(confidenceLevel)
    mu <- tclvalue(muVariable)
    plots <- as.character(tclvalue(plotsVariable))
    putDialog("singleSampleTTest2", list(initial.x = x, initial.alternative = alternative,
                                         initial.level = level, initial.plots = plots, initial.mu = mu))
    closeDialog()
    doItAndPrint(paste("t.test1 <- t.test(", ActiveDataSet(),
                       "$", x, ", alternative='", alternative, "', mu=",
                       mu, ", conf.level=", level, ")", sep = ""))
    doItAndPrint("t.test1")
    if (plots == "Yes") {
      doItAndPrint(paste("graphtest <- ", ActiveDataSet(),
                         "$", x, sep = ""))
      doItAndPrint(paste("hist(graphtest, xlab='", ActiveDataSet(),
                         "$", x, "', main='Histogram of ", ActiveDataSet(),
                         "$", x, "')", sep = ""))
    }
    doItAndPrint("singleTTestWords(t.test1)")
    tkdestroy(top)
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject = "t.test", reset = "singleSampleTTest2")
  #라디오 버튼
  radioButtons(top, name = "alternative", buttons = c("twosided",
                                                      "less", "greater"), values = c("two.sided", "less", "greater"),
               labels = gettextRcmdr(c("Population mean != mu0", "Population mean < mu0",
                                       "Population mean > mu0")), title = gettextRcmdr("Alternative Hypothesis"),
               initialValue = dialog.values$initial.alternative)
  
  

  
  muFrame <- tkframe(top)
  
  muVariable <- tclVar(dialog.values$initial.mu)
  muField <- ttkentry(muFrame, width = "8", textvariable = muVariable)
  
  tkgrid(labelRcmdr(muFrame, text = gettextRcmdr("Null hypothesis: mu = ")),
         muField, sticky = "w")
  
  tkgrid(muFrame, sticky = "w")
  
  
  
  confidenceFrame <- tkframe(top)
  confidenceLevel <- tclVar(dialog.values$initial.level)
  confidenceField <- ttkentry(confidenceFrame, width = "6",
                              textvariable = confidenceLevel)
  
  muField <- ttkentry(muFrame, width = "8", textvariable = muVariable)
  tkgrid(getFrame(xBox), sticky = "nw")
  tkgrid(labelRcmdr(top, text = ""), sticky = "w")
  
  tkgrid(labelRcmdr(confidenceFrame, text = gettextRcmdr("Confidence Level: ")),
         confidenceField, sticky = "w")
  tkgrid(confidenceFrame, sticky = "w")
  tkgrid(alternativeFrame, top, sticky = "nw")
