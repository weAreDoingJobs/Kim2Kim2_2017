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

  #Language of texts in the corpus
  ##types <- c(pf = "pass/Fail (pf)", def  = "Number of defectives (def)")
  ##tclType <- tclVar(types[gettextRcmdr("pf")])

  ##comboType <- ttkcombobox(top, width = 20, textvariable = tclType,state = "readonly", values = types)

  #Language of texts in the corpus
  languages2 <- c(pf = "pass / fail", nd = "Number of defects")
  tclLang2 <- tclVar(languages2[gettextRcmdr("pf")])

  comboLang2 <- ttkcombobox(top, width = 20, textvariable = tclLang2,state = "readonly", values = languages2)


  #Language of texts in the corpus
  languages <- c(dr = "Defect Rate ", dro = "Defect ratio ", ndm = "Number of Defects per million ")
  tclLang <- tclVar(languages[gettextRcmdr("dr")])

  comboLang <- ttkcombobox(top, width = 20, textvariable = tclLang, state = "readonly", values = languages)

  # value 받기
  AQLVar <- tclVar(dialog.values$AQLVar)
  AQLEntry <- tkentry(top, width = "6", textvariable = AQLVar)
  ROLLTPDVar <- tclVar(dialog.values$ROLLTPDVar)
  ROLLTPDEntry <- tkentry(top, width = "6", textvariable = ROLLTPDVar)
  alphaVar <- tclVar(dialog.values$alphaVar)
  alphaEntry <- tkentry(top, width = "6", textvariable = alphaVar)
  betaVar <- tclVar(dialog.values$betaVar)
  betaEntry <- tkentry(top, width = "6", textvariable = betaVar)
  lotSizeVar <- tclVar(dialog.values$lotSizeVar)
  lotSizeEntry <- tkentry(top, width = "6", textvariable = lotSizeVar)

  onOK <- function() {
    closeDialog()
    AQL <- round(as.numeric(tclvalue(AQLVar)))
    if (is.na(AQL) || AQL <= 0) {
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

  tkgrid(labelRcmdr(top, text = gettextRcmdr("Type of measurement :")),sticky = "w", pady = 10)
  tkgrid(comboLang2, sticky = "w", pady = 10, row = 1, column = 1,columnspan = 2)

  tkgrid(labelRcmdr(top, text = gettextRcmdr("Unit of quality level :")),sticky = "w", pady = 10)
  tkgrid(comboLang, sticky = "w", pady = 10, row = 3, column = 1,columnspan = 2)

  tkgrid(tklabel(top, text = "Acceptable quality level(AQL)"), AQLEntry, sticky = "e")
  tkgrid(tklabel(top, text = "Rejectable quality level (RQL or LTPD)"), ROLLTPDEntry, sticky = "e")
  tkgrid(tklabel(top, text = "Producer Risk(alpha)"), alphaEntry, sticky = "e")
  tkgrid(tklabel(top, text = "Consumer Risk Risk(beta)"), betaEntry, sticky = "e")
  tkgrid(tklabel(top, text = "Lot Size"), lotSizeEntry, sticky = "e")
  tkgrid(buttonsFrame, sticky = "w", columnspan = 2)

  tkgrid.configure(AQLEntry, sticky = "w")
  tkgrid.configure(ROLLTPDEntry, sticky = "w")
  tkgrid.configure(alphaEntry, sticky = "w")
  tkgrid.configure(betaEntry, sticky = "w")
  tkgrid.configure(lotSizeEntry, sticky = "w")

  dialogSuffix(rows = 10, columns = 2, focus = comboLang2)
}
