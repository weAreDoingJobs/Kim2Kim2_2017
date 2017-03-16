---
title: "RcmrPlugin_temis"
author: "이상인"
date: "2017년 3월 10일"
output: html_document
---
```{r}
library(RcmdrPlugin.temis)
ls("package:RcmdrPlugin.temis")
```


```{r}

function () 
{
  initializeDialog(title = .gettext("Import Corpus"))
  setState <- function(...) {
    if (tclvalue(sourceVariable) %in% c("dir", "file", "alceste")) {
      tkconfigure(comboEnc, state = "normal")
      if (tclvalue(tclEnc) == "UTF-8") 
        tclvalue(tclEnc) <- autoEnc
    }
    else {
      tkconfigure(comboEnc, state = "disabled")
      if (tclvalue(tclEnc) == autoEnc) 
        tclvalue(tclEnc) <- "UTF-8"
    }
  }
  #라디오 버튼
  radioButtons(name = "source", buttons = c("dir", "file", 
                                            "factiva", "lexisnexis", "europresse", "alceste", "twitter"), 
               labels = c(.gettext("Directory containing plain text files"), 
                          .gettext("Spreadsheet file (CSV, XLS, ODS...)"), 
                          .gettext("Factiva XML or HTML file(s)"), .gettext("LexisNexis HTML file(s)"), 
                          .gettext("Europresse HTML file(s)"), .gettext("Alceste file(s)"), 
                          .gettext("Twitter search")), title = .gettext("Load corpus from:"), 
               right.buttons = FALSE, command = setState)
  
  #File encoding
  autoEnc <- .gettext("detect automatically")
  tclEnc <- tclVar(autoEnc)
  comboEnc <- ttkcombobox(top, width = 20, textvariable = tclEnc, 
                          values = c(autoEnc, iconvlist()))
  
  tk2tip(comboEnc, sprintf(.gettext("Most probable encodings for this language:\n%s"), 
                           .langToEncs(.gettext("en"))))
  
  #Language of texts in the corpus
  languages <- c(da = "Dansk (da)", de = "Deutsch (de)", en = "English (en)", 
                 es = "Espanol (es)", fi = "Suomi (fi)", fr = "Francais (fr)", 
                 hu = "Magyar (hu)")
  tclLang <- tclVar(languages[.gettext("en")])
  
  comboLang <- ttkcombobox(top, width = 20, textvariable = tclLang, 
                           state = "readonly", values = languages)
  
  
  tkbind(comboLang, "<<ComboboxSelected>>", function() {
    rawLang <- tclvalue(tclLang)
    lang <- substring(rawLang, nchar(rawLang) - 2, nchar(rawLang) - 1)
    tk2tip(comboEnc, sprintf(.gettext("Most probable encodings for this language:\n%s"), 
                             .langToEncs(lang)))
  })
  
  
  #checkbox
  checkBoxes(frame = "processingFrame", boxes = c("lowercase", 
                                                  "punctuation", "digits", "stopwords", "stemming", "customStemming"), 
             initialValues = c(1, 1, 1, 0, 1, 0), labels = c(.gettext("Ignore case"), 
                                                             .gettext("Remove punctuation"), .gettext("Remove digits"), 
                                                             .gettext("Remove stopwords"), .gettext("Apply stemming"), 
                                                             .gettext("Edit stemming manually")), title = .gettext("Text processing:"))
  
  
  
  #Texgt splitting
  tclChunks <- tclVar(0)
  tclNParagraphs <- tclVar(1)
  chunksButton <- ttkcheckbutton(top, variable = tclChunks, 
                                 text = .gettext("Split texts into smaller documents"), 
                                 command = function() {
                                   if (tclvalue(tclChunks) == 1) 
                                     tkconfigure(chunksSlider, state = "active")
                                   else tkconfigure(chunksSlider, state = "disabled")
                                 })
 
  
  
   chunksSlider <- tkscale(top, from = 1, to = 20, showvalue = TRUE, 
                          variable = tclNParagraphs, resolution = 1, orient = "horizontal", 
                          state = "disabled")
  
   
  #onOK
  
  onOK <- function() {
    source <- tclvalue(sourceVariable)
    twitter <- source == "twitter"
    lowercase <- tclvalue(lowercaseVariable) == 1
    punctuation <- tclvalue(punctuationVariable) == 1
    digits <- tclvalue(digitsVariable) == 1
    stopwords <- tclvalue(stopwordsVariable) == 1
    stemming <- tclvalue(stemmingVariable) == 1
    customStemming <- tclvalue(customStemmingVariable) == 1
    rawLang <- tclvalue(tclLang)
    lang <- substring(rawLang, nchar(rawLang) - 2, nchar(rawLang) - 
                        1)
    enc <- tclvalue(tclEnc)
    if (enc == autoEnc) 
      enc <- ""
    if (enc != "" && !enc %in% iconvlist()) {
      .Message(.gettext("Unsupported encoding: please select an encoding from the list."), 
               "error", parent = top)
      return()
    }
    closeDialog()
    setBusyCursor()
    on.exit(setIdleCursor())
    if (stemming) {
      if (!.checkAndInstall("SnowballC", 
                            .gettext("Package SnowballC is needed to perform stemming.
                                     Do you want to install it?\n\nThis requires a working Internet connection."))) 
        return()
    }
    objects <- c("corpus", "corpusVars", "dtm", "wordsDtm", 
                 "lengthsDtm", "voc", "coocs", "termFreqs", "absTermFreqs", 
                 "varTermFreqs", "freqTerms", "specTerms", "docSeries", 
                 ".last.table", "corpusClust", "corpusSubClust", "corpusCa", 
                 "plottingCa")
    if (any(sapply(objects, exists))) {
      if (exists("corpusVars")) {
        putRcmdr(".activeDataSet", NULL)
        Variables(NULL)
        Numeric(NULL)
        Factors(NULL)
        RcmdrTclSet("dataSetName", gettextRcmdr("<No active dataset>"))
        putRcmdr(".activeModel", NULL)
        RcmdrTclSet("modelName", gettextRcmdr("<No active model>"))
        tkconfigure(getRcmdr("dataSetLabel"), foreground = "red")
        tkconfigure(getRcmdr("modelLabel"), foreground = "red")
      }
      doItAndPrint(paste("rm(", paste(objects[sapply(objects, 
                                                     exists)], collapse = ", "), ")", sep = ""))
      gc()
    }
    HTMLSetFile(NULL)
    activateMenus()
    res <- switch(source, dir = importCorpusFromDir(lang, 
                                                    enc), file = importCorpusFromFile(lang, enc), factiva = importCorpusFromFactiva(lang), 
                  lexisnexis = importCorpusFromLexisNexis(lang), europresse = importCorpusFromEuropresse(lang), 
                  alceste = importCorpusFromAlceste(lang, enc), twitter = importCorpusFromTwitter(lang))
    if (!(isTRUE(res) || is.list(res)) || length(corpus) == 
        0) 
      return()
    if (exists("corpusVars")) {
      if (!.selectCorpusVariables(source)) 
        return()
    }
    else {
      doItAndPrint("corpusVars <- data.frame(var1=factor(rep(\"\", length(corpus))), row.names=names(corpus))")
    }
    setBusyCursor()
    on.exit(setIdleCursor())
    doItAndPrint("activeDataSet(\"corpusVars\")")
    doItAndPrint("setCorpusVariables()")
    if (tclvalue(tclChunks) == 1) {
      doItAndPrint(sprintf("corpus <- splitTexts(corpus, %s)", 
                           tclvalue(tclNParagraphs)))
      doItAndPrint("meta(corpus, type=\"corpus\", tag=\"split\") <- TRUE")
    }
    if (!.processTexts(c(twitter = twitter, lowercase = lowercase, 
                         punctuation = punctuation, digits = digits, stopwords = stopwords, 
                         stemming = stemming, customStemming = customStemming, 
                         removeHashtags = res$removeHashtags, removeNames = res$removeNames), 
                       lang)) 
      return()
    doItAndPrint("dtm <- DocumentTermMatrix(dtmCorpus, control=list(tolower=FALSE, wordLengths=c(2, Inf)))")
    doItAndPrint("rm(dtmCorpus)")
    if (!exists("dtm") || is.null(dtm)) {
      return()
    }
    else if (nrow(dtm) == 0 || ncol(dtm) == 0) {
      doItAndPrint("dtm")
      return()
    }
    .buildDictionary(stemming, customStemming, lang)
    .prepareDtm(stopwords, stemming, customStemming, lang)
    gc()
    doItAndPrint(sprintf("meta(corpus, type=\"corpus\", tag=\"language\") <- attr(dtm, \"language\") <- \"%s\"", 
                         lang))
    justDoIt(sprintf("meta(corpus, type=\"corpus\", tag=\"source\") <- \"%s\"", 
                     res$source))
    doItAndPrint(sprintf("meta(corpus, type=\"corpus\", tag=\"processing\") <- attr(dtm, \"processing\") <- c(lowercase=%s, punctuation=%s, digits=%s, stopwords=%s, stemming=%s, customStemming=%s, twitter=%s, removeHashtags=%s, removeNames=%s)", 
                         lowercase, punctuation, digits, stopwords, stemming, 
                         customStemming, twitter, ifelse(is.null(res$removeHashtags), 
                                                         NA, res$removeHashtags), ifelse(is.null(res$removeNames), 
                                                                                         NA, res$removeNames)))
    doItAndPrint("corpus")
    doItAndPrint("dtm")
    activateMenus()
    tkfocus(CommanderWindow())
  }
  
  #UI
  OKCancelHelp(helpSubject = "importCorpusDlg")
  tkgrid(sourceFrame, columnspan = 1, sticky = "w", pady = 6)
  tkgrid(labelRcmdr(top, text = .gettext("Unit of quality level :")), 
         sticky = "w", pady = 6)
  
  tkgrid(comboLang, sticky = "ew", pady = 6, row = 1, column = 1, 
         columnspan = 2)
  tkgrid(labelRcmdr(top, text = .gettext("File encoding:")), 
         sticky = "w", pady = 6)
  tkgrid(comboEnc, sticky = "ew", pady = 6, row = 2, column = 1, 
         columnspan = 2)
  
  tkgrid(.titleLabel(top, text = .gettext("Text splitting:")), 
         sticky = "ws", pady = c(12, 6))
  tkgrid(chunksButton, sticky = "w", pady = 6, columnspan = 3)
  
  tkgrid(labelRcmdr(top, text = .gettext("Size of new documents:")), 
         chunksSlider, labelRcmdr(top, text = .gettext("paragraphs")), 
         sticky = "w", pady = 6)
  tkgrid(processingFrame, columnspan = 3, sticky = "w", pady = 6)
  tkgrid(buttonsFrame, columnspan = 3, sticky = "ew", pady = 6)
  tkgrid.columnconfigure(top, 0, pad = 12)
  tkgrid.columnconfigure(top, 1, pad = 12)
  tkgrid.columnconfigure(top, 2, pad = 12)
  dialogSuffix(focus = comboLang)
}
```

