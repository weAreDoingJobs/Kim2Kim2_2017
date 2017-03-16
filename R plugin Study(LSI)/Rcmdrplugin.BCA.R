---
title: "BCA plugin"
author: "이상인"
date: "2017년 3월 8일"
output: html_document
---

```{r}
library(RcmdrPlugin.BCA)
ls("package:RcmdrPlugin.BCA")
```

```{r}
kcentroidsClustering
function () 
{
    .activeDataSet <- ActiveDataSet()
    initializeDialog(title = gettextRcmdr("K-Centroids Clustering"))
    dataFrame <- tkframe(top)
    xBox <- variableListBox(dataFrame, Numeric(), selectmode = "", title = gettextRcmdr("Variables (pick one or more)"))
    subsetBoxBCA(dataFrame)
    assignFrame <- tkframe(dataFrame)
    assignClusters <- tclVar("0")
    assignCB <- tkcheckbutton(assignFrame)
    tkconfigure(assignCB, variable = assignClusters)
    assignName <- tclVar("KCentroids")
    assignField <- tkentry(assignFrame, width = "15", textvariable = assignName)
    radioButtons(name = "method", buttons = c("kmn", "kmd", "neuralgas"), 
        labels = gettextRcmdr(c("K-Means", "K-Medians", "Neural Gas")), 
        title = gettextRcmdr("Clustering Method"))
    optionsFrame <- tkframe(top)
    clusterNumber <- tclVar("2")
    clusterNumSlider <- tkscale(optionsFrame, from = 2, to = 10, 
        showvalue = TRUE, variable = clusterNumber, resolution = 1, 
        orient = "horizontal")
    seedNumber <- tclVar("10")
    seedNumSlider <- tkscale(optionsFrame, from = 1, to = 20, 
        showvalue = TRUE, variable = seedNumber, resolution = 1, 
        orient = "horizontal")
    summaryClusters <- tclVar("1")
    summaryCB <- tkcheckbutton(optionsFrame)
    tkconfigure(summaryCB, variable = summaryClusters)
    plotCls2d <- tclVar("1")
    plot2dCB <- tkcheckbutton(optionsFrame)
    tkconfigure(plot2dCB, variable = plotCls2d)
    plotCls3d <- tclVar("0")
    plot3dCB <- tkcheckbutton(optionsFrame)
    tkconfigure(plot3dCB, variable = plotCls3d)
    plotFrame <- tkframe(optionsFrame)
    plotPts <- tclVar("1")
    plotPtsCB <- tkcheckbutton(plotFrame)
    tkconfigure(plotPtsCB, variable = plotPts)
    plotCnt <- tclVar("0")
    plotCntCB <- tkcheckbutton(plotFrame)
    tkconfigure(plotCntCB, variable = plotCnt)
    onOK <- function() {
        x <- getSelection(xBox)
        nvar <- length(x)
        subset <- trim.blanks(tclvalue(subsetVariable))
        clusMethod <- tclvalue(methodVariable)
        nClusters <- tclvalue(clusterNumber)
        seeds <- tclvalue(seedNumber)
        clusterSummary <- tclvalue(summaryClusters)
        clsPlot2d <- tclvalue(plotCls2d)
        clsPlot3d <- tclvalue(plotCls3d)
        ptsPlot <- tclvalue(plotPts)
        cntPlot <- tclvalue(plotCnt)
        clusterAssign <- tclvalue(assignClusters)
        clusterVariable <- trim.blanks(tclvalue(assignName))
        if (clusterAssign == "1" & !is.valid.name(clusterVariable)) {
            errorCondition(recall = kcentroidsClustering, message = sprintf(gettextRcmdr("\"%s\" is not a valid name."), 
                clusterVariable))
            return()
        }
        closeDialog()
        if (clusterAssign == "1") {
            if (is.element(clusterVariable, Variables())) {
                if ("no" == tclvalue(checkReplace(clusterVariable))) {
                  kcentroidsClustering()
                  return()
                }
            }
        }
        if (length(x) == 0) {
            errorCondition(recall = kcentroidsClustering, message == 
                gettextRcmdr("No variables selected."))
            return()
        }
        varFormula <- paste(x, collapse = " + ")
        vars <- paste(x, collapse = ",", sep = "")
        .activeDataSet <- ActiveDataSet()
        dset <- if (trim.blanks(subset) == gettextRcmdr("<all valid cases>")) 
            .activeDataSet
        else {
            paste(.activeDataSet, "[", .activeDataSet, "$", subset, 
                ", ]", sep = "")
        }
        xmat <- paste("model.matrix(~-1 + ", varFormula, ", ", 
            dset, ")", sep = "")
        details <- "FUN = kcca, family = kccaFamily(\"kmeans\")"
        if (clusMethod == "kmd") {
            details <- "FUN = kcca, family = kccaFamily(\"kmedians\")"
        }
        else if (clusMethod == "neuralgas") {
            details <- "FUN = cclust, dist = \"euclidean\", method = \"neuralgas\""
        }
        command <- paste("stepFlexclust(", xmat, ", k = ", nClusters, 
            ", nrep = ", seeds, ", ", details, ")", sep = "")
        doItAndPrint(paste(".cluster", "<-", command))
        if (clusterSummary == "1") {
            doItAndPrint("summary(.cluster)")
            doItAndPrint("print(.cluster@centers) # Cluster Centroids")
        }
        if ((clsPlot2d == "1" | clsPlot3d == "1") & ptsPlot == 
            "0" & cntPlot == "0") {
            Message(gettextRcmdr("Bi-plot(s) contains no points or centroids."), 
                type = "warning")
        }
        ptsOpt <- "FALSE"
        if (ptsPlot == "1") 
            ptsOpt <- "TRUE"
        cntOpt <- "FALSE"
        if (cntPlot == "1") 
            cntOpt <- "TRUE"
        if (clsPlot2d == "1") {
            plotCmd2d <- paste("bpCent(prcomp(", xmat, "), clusters(.cluster), data.pts = ", 
                ptsOpt, ", centroids = ", cntOpt, ", xlabs = as.character(clusters(.cluster)))", 
                sep = "")
            justDoIt(plotCmd2d)
            logger(plotCmd2d)
        }
        if (clsPlot3d == "1") {
            plotCmd3d <- paste("bpCent3d(prcomp(", xmat, "), clusters(.cluster), data.pts = ", 
                ptsOpt, ", centroids = ", cntOpt, ", xlabs = as.character(clusters(.cluster)))", 
                sep = "")
            justDoIt(plotCmd3d)
            logger(plotCmd3d)
            .Tcl("update")
            activateMenus()
            rgl::rgl.bringtotop()
        }
        if (clusterAssign == "1") {
            assignCommand <- paste(.activeDataSet, "$", clusterVariable, 
                " <- as.factor(clusters(.cluster))", sep = "")
            justDoIt(assignCommand)
            logger(assignCommand)
            activeDataSet(.activeDataSet)
        }
        justDoIt(paste("remove(.cluster)"))
        logger(paste("remove(.cluster)"))
        activateMenus()
        tkfocus(CommanderWindow())
    }
    OKCancelHelp(helpSubject = "kcca")
    tkgrid(getFrame(xBox), sticky = "nw")
    tkgrid(subsetFrame, sticky = "w")
    tkgrid(tklabel(assignFrame, text = gettextRcmdr("Assign clusters to\nthe data set         ")), 
        assignCB, sticky = "w")
    tkgrid(tklabel(assignFrame, text = gettextRcmdr("Assignment variable: ")), 
        assignField, sticky = "w")
    tkgrid(assignFrame, sticky = "w")
    tkgrid(tklabel(optionsFrame, text = gettextRcmdr("Number of clusters:")), 
        clusterNumSlider, sticky = "sw")
    tkgrid(tklabel(optionsFrame, text = gettextRcmdr("Number of starting seeds:")), 
        seedNumSlider, sticky = "sw")
    tkgrid(tklabel(optionsFrame, text = gettextRcmdr("Print cluster summary")), 
        summaryCB, sticky = "w")
    tkgrid(tklabel(optionsFrame, text = gettextRcmdr("2D bi-plot of clusters")), 
        plot2dCB, sticky = "w")
    tkgrid(tklabel(optionsFrame, text = gettextRcmdr("3D bi-plot of clusters")), 
        plot3dCB, sticky = "w")
    tkgrid(tklabel(plotFrame, text = gettextRcmdr("Plot points")), 
        plotPtsCB, tklabel(plotFrame, text = gettextRcmdr("Plot centroids")), 
        plotCntCB, sticky = "w")
    tkgrid(plotFrame, columnspan = 2, sticky = "w")
    tkgrid(dataFrame, tklabel(top, text = "  "), methodFrame, 
        optionsFrame, sticky = "nw")
    tkgrid(buttonsFrame, columnspan = 3, sticky = "w")
    dialogSuffix(rows = 3, columns = 4)
}
```

