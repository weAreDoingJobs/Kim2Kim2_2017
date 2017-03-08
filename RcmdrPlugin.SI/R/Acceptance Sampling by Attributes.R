#'
#'no what
#'
#'@return asa
#'@export

ASA <- function ()
{
  initializeDialog(title = gettextRcmdr("Acceptance Sampling by Attributes"))
  defaults <- list(AQLVar = "1", ROLLTPDVar = 10000, alphaVar = "0.05", betaVar = 0.10, lotSizeVar = 10)
  dialog.values <- getDialog("ASA", defaults)



   AQLVar <- tclVar(dialog.values$AQLVar)
  AQLEntry <- tkentry(top, width = "6", textvariable = AQLVar)
  ROLLTPDVar <- tclVar(dialog.values$ROLLTPDVar)
  ROLLTPDEntry <- tkentry(top, width = "6", textvariable = ROLLTPDVar)
  alphaVar <- tclVar(dialog.values$alphaVar)
  alphaEntry <- tkentry(top, width = "6", textvariable = alphaVar)
  betaVar <- tclVar(dialog.values$betaVar)
  betaEntry <- tkentry(top, width = "6", textvariable = betaVar)
  betaVar <- tclVar(dialog.values$betaVar)
  betaEntry <- tkentry(top, width = "6", textvariable = betaVar)
  lotSizeVar <- tclVar(dialog.values$lotSizeVar)
  lotSizeEntry <- tkentry(top, width = "6", textvariable = lotSizeVar)

  onOK <- function() {
    closeDialog()
    AQL <- round(as.numeric(tclvalue(AQLVar)))
    if (is.na(AQL) || n <= 0) {
      errorCondition(recall = ASA,
                     message = "AQL must be a positive integer.")
      return()
    }
    ROLLTPD <- round(as.numeric(tclvalue(ROLLTPDVar)))
    if (is.na(ROLLTPD) || ROLLTPD <= 0) {
      errorCondition(recall = ASA,
                     message = "ROL or LTPD must be a positive integer.")
      return()
    }
    alpha <- round(as.numeric(tclvalue(alphaVar)))
    if (is.na(alpha) || alpha < 0 || alpha > 1) {
      errorCondition(recall = ASA,
                     message = "Producer risk must be a positive integer.")
      return()
    }
    beta <- round(as.numeric(tclvalue(betaVar)))
    if (is.na(beta) || beta < 0 || beta > 1) {
      errorCondition(recall = ASA,
                     message = "Consumer risk must be between 0 to 1.")
      return()
    }
    lotSize <- round(as.numeric(tclvalue(lotSizeVar)))
    if (is.na(lotSize) || lotSize < 0 || lotSize > 1) {
      errorCondition(recall = ASA,
                     message = "Producer risk must be a positive integer.")
      return()
    }

    putDialog("ASA", lapply(list(AQLVar = AQL,ROLLTPDVar = ROLLTPD, alphaVar = alpha, betaVar= beta,lotSizeVar = lotSize), as.character))
    command <- "hello"
    doItAndPrint(command)
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject = "ASA", reset = "ASA",apply = "ASA")
  #UI
  tkgrid(tklabel(top, text = "Acceptable quality level(AQL)"), AQLEntry, sticky = "e")
  tkgrid(tklabel(top, text = "Rejectable quality level (RQL or LTPD)"), ROLLTPDEntry, sticky = "e")
  tkgrid(tklabel(top, text = "Producer Risk(alpha)"), alphaEntry, sticky = "e")
  tkgrid(tklabel(top, text = "Consumer Risk Risk(beta)"), betaEntry, sticky = "e")
  tkgrid(tklabel(top, text = "Lot Size"), lotSizeEntry, sticky = "e")

  tkgrid(buttonsFrame, sticky = "w", columnspan = 2)

  dialogSuffix(rows = 6, columns = 2, focus = AQLEntry)
}
