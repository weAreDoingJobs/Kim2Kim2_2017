---
title: "RcmdrPlugin.epack"
author: "이상인"
date: "2017년 3월 8일"
output: html_document
---

```{r}
library(RcmdrPlugin.epack)
ls("package:RcmdrPlugin.epack")
```

```{r}
bcMod
{
    initializeDialog(title = gettextRcmdr("Box Cox Transformation"))
    xBox <- variableListBox(top, Numeric(), title = gettextRcmdr("Variable (pick one)"))
    onOK <- function() {
        x <- getSelection(xBox)
        if (length(x) == 0) {
            errorCondition(recall = bcMod, message = gettextRcmdr("You must select a variable."))
            return()
        }
        closeDialog()
        doItAndPrint(paste("bc2(", ActiveDataSet(), "$", x, ")", 
            sep = ""))
        tkdestroy(top)
        tkfocus(CommanderWindow())
    }
    OKCancelHelp(helpSubject = "boxcox")
    tkgrid(getFrame(xBox), sticky = "nw")
    tkgrid(buttonsFrame, columnspan = 2, sticky = "w")
    dialogSuffix(rows = 4, columns = 2)
}
```

