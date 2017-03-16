 library(RcmdrPlugin.MA)
 CatCompcmd
function () 
{
  initializeDialog(title = gettextRcmdr("Categorical moderation (single predictor)"))
  
  #모델 이름 입력하기
  modelName <- tclVar(paste("catmod.", getRcmdr("modelNumber"), 
                            sep = ""))
  modelFrame <- tkframe(top)
  model <- ttkentry(modelFrame, width = "20", textvariable = modelName)
  UpdateModelNumber()
  
  #박스 3개
  variablesFrame <- tkframe(top)
  .variable <- Variables()
  .numeric <- Numeric()
  .factor <- Factors()
  xBox <- variableListBox(variablesFrame, .factor, title = gettextRcmdr("Moderator (pick one)"))
  yBox <- variableListBox(variablesFrame, .numeric, title = gettextRcmdr("Effect size (ES) (pick one)"))
  zBox <- variableListBox(variablesFrame, .numeric, title = gettextRcmdr("ES Variance (pick one)"))
  
  #하위셋 표현식
  subsetBox()
  onOK <- function() {
    
    #박스 3개
    x <- getSelection(xBox)
    y <- getSelection(yBox)
    z <- getSelection(zBox)
    closeDialog()
    if (0 == length(y)) {
      UpdateModelNumber(-1)
      errorCondition(recall = CatModcmd, message = gettextRcmdr("You must select an effect size variable."))
      return()
    }
    if (0 == length(x)) {
      UpdateModelNumber(-1)
      errorCondition(recall = CatModcmd, message = gettextRcmdr("No moderator variables selected."))
      return()
    }
    if (0 == length(z)) {
      UpdateModelNumber(-1)
      errorCondition(recall = CatModcmd, message = gettextRcmdr("No variance variables selected."))
      return()
    }
    if (is.element(y, x)) {
      UpdateModelNumber(-1)
      errorCondition(recall = CatModcmd, message = gettextRcmdr("Response and explanatory variables must be         \n                                                            different."))
      return()
    }
    #박스 3개 end
    
    #하위셋 표현식
    subset <- tclvalue(subsetVariable)
    if (trim.blanks(subset) == gettextRcmdr("<all valid cases>") || 
        trim.blanks(subset) == "") {
      subset <- ""
      putRcmdr("modelWithSubset", FALSE)
    }
    else {
      subset <- paste(", subset=", subset, sep = "")
      putRcmdr("modelWithSubset", TRUE)
    }
    #하위셋 표현식 end
    
    #모델 이름 입력하기 
    value <- trim.blanks(tclvalue(modelName))
    if (!is.valid.name(value)) {
      UpdateModelNumber(-1)
      errorCondition(recall = CatModcmd, message = sprintf(gettextRcmdr("\"%s\" is not a valid name."), 
                                                           value))
      return()
    }
    if (is.element(value, listLinearModels())) {
      if ("no" == tclvalue(checkReplace(value, type = gettextRcmdr("Model")))) {
        UpdateModelNumber(-1)
        linearRegressionModel()
        return()
      }
    }
    #모델 이름 입력하기 end
    
    modelFR <- as.character(tclvalue(modelFRVariable))
    meta <- ActiveDataSet()
    command <- paste("macat(", y, ",  var=", z, ",  mod=", 
                     x, ", data=", ActiveDataSet(), ",method='", modelFR, 
                     "')", sep = "")
    command <- paste(value, " <- ", command, sep = "")
    doItAndPrint(command)
    command2 <- (paste(value))
    doItAndPrint(command2)
  }
  
  
  OKCancelHelp(helpSubject = "macat", model = TRUE)
  
  #모델 라디오 버튼
  radioButtons(name = "modelFR", buttons = c("Fixed", "Random"), 
               values = c("fixed", "random"), labels = gettextRcmdr(c("fixed", 
                                                                      "random")), title = gettextRcmdr("model"))
  
  #모델이름 입력하기 
  tkgrid(modelFRFrame, sticky = "w")
  tkgrid(labelRcmdr(modelFrame, text = gettextRcmdr("Enter name for model:")), 
         model, sticky = "w")
  tkgrid(modelFrame, sticky = "w")
  
  #박스 3개
  tkgrid(getFrame(yBox), labelRcmdr(variablesFrame, text = "    "), 
         getFrame(zBox), getFrame(xBox), sticky = "nw")
  tkgrid(variablesFrame, sticky = "w")
  
  #하위셋 표현식
  tkgrid(subsetFrame, sticky = "w")
  tkgrid(buttonsFrame, stick = "w")
  
  tkgrid.configure(helpButton, sticky = "e")
  dialogSuffix(rows = 5, columns = 1)
}