


msa.ss.rr.ui <- function ()
{
  initializeDialog(title = gettextRcmdr("Gage R&R"))
  labelsFrame <- tkframe(top)
  variablesFrame <- tkframe(top)
  .variable <- Variables()
  .numeric <- Numeric()
  statFrame <- tkframe(labelsFrame)
  xBox <- variableListBox(variablesFrame, .variable, title = gettextRcmdr("id variable (pick one)"))
  yBox <- variableListBox(variablesFrame, .numeric, title = gettextRcmdr("Effect size (ES) (pick one)"))
  aBox <- variableListBox(variablesFrame, .variable, title = gettextRcmdr("N variable (pick one)"))
  UpdateModelNumber()
  statVar <- tclVar(gettextRcmdr(".50 "))
  statFrame <- tkframe(labelsFrame)
  statEntry <- ttkentry(statFrame, width = "5", textvariable = statVar)
  tkgrid(labelRcmdr(statFrame, text = gettextRcmdr("estimated correlation btwn outcome measures"),
                    fg = "blue"), sticky = "w")
  tkgrid(statEntry, sticky = "w")
  tkgrid(statFrame, labelRcmdr(labelsFrame, text = " Default is .50  (Wampold, 1997)  "),
         sticky = "w")
  modelName <- tclVar(paste("ag.", getRcmdr("modelNumber"),
                            sep = ""))
  modelFrame <- tkframe(top)
  model <- ttkentry(modelFrame, width = "20", textvariable = modelName)
  onOK <- function() {
    x <- getSelection(xBox)
    y <- getSelection(yBox)
    a <- getSelection(aBox)
    closeDialog()
    if (0 == length(x)) {
      UpdateModelNumber(-1)
      errorCondition(recall = MetaGcmd, message = gettextRcmdr("You must select an id variable."))
      return()
    }
    if (0 == length(a)) {
      UpdateModelNumber(-1)
      errorCondition(recall = MetaGcmd, message = gettextRcmdr("No sample (N) variable selected."))
      return()
    }
    if (0 == length(y)) {
      UpdateModelNumber(-1)
      errorCondition(recall = MetaGcmd, message = gettextRcmdr("You must select a ES variable."))
      return()
    }
    value <- trim.blanks(tclvalue(modelName))
    if (!is.valid.name(value)) {
      UpdateModelNumber(-1)
      errorCondition(recall = MetaGcmd, message = sprintf(gettextRcmdr("\"%s\" is not a valid name."),
                                                          value))
      return()
    }
    stat <- trim.blanks(tclvalue(statVar))
    stat <- paste(" ", stat, "", sep = "")
    command <- paste("aggC(", x, ", ", y, ", ", a, ", cor = ",
                     stat, " , data=", ActiveDataSet(), ")", sep = "")
    command <- paste(value, " <- ", command, sep = "")
    doItAndPrint(command)
  }
  OKCancelHelp(helpSubject = "agg", model = TRUE)
  tkgrid(labelRcmdr(modelFrame, text = gettextRcmdr("Enter name for data.frame:")),
         model, sticky = "w")
  tkgrid(modelFrame, sticky = "w")
  tkgrid(getFrame(yBox), labelRcmdr(variablesFrame, text = "    "),
         getFrame(xBox), getFrame(aBox), sticky = "nw")
  tkgrid(variablesFrame, sticky = "w")
  tkgrid(labelsFrame, sticky = "w")
  tkgrid(buttonsFrame, stick = "w")
  tkgrid.configure(helpButton, sticky = "e")
  dialogSuffix(rows = 7, columns = 7)
}
