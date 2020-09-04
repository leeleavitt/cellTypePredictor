#' Funciton to create labeled data from a single experiment
#' @param dat is th labeled data
#' @param cells a selective subset of neurons to input into the function
#' @param windowSizeMin This is the size of the window to view the peak in minutes
#' @param tType chracter or character vector this is the trace types to include to improve training.
#' @param winMin this is the minimum window size to include for the windows. 
#' @param winMax this is the maximum window size to include for the window size.
#' @param rdName the actual name of the experiment
labeledDataMaker <- function(dat, cells = cellSelect, windowSizeMin = 5, tType = "blc", winMin = 1.2, winMax = 5, winSel = T){
    # Lets create the window region
    wr <- dat$w.dat$wr1
    # always remove the '' window
    levs <- setdiff(unique(dat$w.dat$wr1), "")

    # Lets find the window sizes to include,
    x1s <- tapply(dat$w.dat[,"Time"],as.factor(wr),min)
    x2s <- tapply(dat$w.dat[,"Time"],as.factor(wr),max)
    cat("\nThese are the windows sorted:\n")
    print(levs)

    windowLen <- x2s - x1s
    winCol <- names(windowLen[windowLen < winMax & windowLen > winMin])

    if(winSel){
        cat('\nSelect windows to add to the labeled data\n')
        winCol <- select.list(levs, preselect = winCol,  multiple = T, title = "Select windows", graphics = T)
    }else{
        winCol <- levs
    }

    winStart <- sort(x1s[winCol])
    winEnd <- winStart + windowSizeMin

    cat("\nThese are included windows:\n")
    print(names(winStart))

    # Test the window size since we are losing some data. due to differing window sizes
    windowSize <- c()
    for(i in 1:length(winStart)){
        windowLogic <-  dat$w.dat$Time > winStart[i] & 
                        dat$w.dat$Time < winEnd[i]

        windowSize[i] <- dim(dat$t.dat[-1][windowLogic, ])[1]
    }

    # Now what we are trying to test is, if the difference between the window size min and max 
    # are too large we know one of these windows cannot encompass the entire region of the
    # trace
    # If this is the case then we need to drop the windows that are greater than the
    # difference of 10, then select the minimum value
    windowSizeMax <- max(windowSize)

    # This is difference in window size, returns a negative number
    # Here we want logic defining windows that have a greater distance 
    # than 10 to remove.
    windowLogic <- (windowSizeMax - windowSize) < 10

    winStart <- winStart[windowLogic]

    windowSizeMin <- min(windowSize)
    
    startPoints <- Reduce(c,lapply(winStart, function(x) which(dat$t.dat$Time == x, arr.ind=T) ))
    endPoints <- startPoints + windowSizeMin


    allPulses <- data.frame()
    allLabels <- data.frame()
    for(i in 1:length(winStart)){

        # This will allow me to collect multiple traces.
        for(j in 1:length(tType)){
            pulseToScore <- as.data.frame(t(dat[[ tType[j] ]][-1][startPoints[i]:endPoints[i], cells]))
            colnames(pulseToScore) <- 1:dim(pulseToScore)[2]

            newRowName <- paste0(rdName, "_", row.names(pulseToScore), "_", names(winStart[i]),"_", tType[j])
            row.names(pulseToScore) <- newRowName

            tryCatch({
                allPulses <- rbind(allPulses, pulseToScore)
                binLogic <- TRUE
            }, error = function(e){
                cat("\nCould Not add: ", names(winStart[i]), "\n")
                cat("Error was:\n")
                print(e)
                winCol <<- setdiff(winCol, names(winStart[i]))
                binLogic <<- FALSE
            })

            if(binLogic){
                labelToAdd <- dat$bin[cells, names(winStart)[i], drop = FALSE]
                row.names(labelToAdd) <- newRowName
                colnames(labelToAdd) <- 'labels'
                allLabels <- rbind(allLabels, labelToAdd)
            }
        }
    }
    cat("\nThe dimension of the scored neurons traces are: \n")
    print(dim(allPulses))
    cat("\nThe labels are\n")
    print(dim(allLabels))
    cat("\nIs the labels and the traces identifiers identical?\n")
    print(identical(row.names(allPulses), row.names(allLabels)))
    cat("\nYou have a total of: ", dim(allLabels)[1], "labeled data points")
    cat("\nThe break down of the labels are\n")
    print(summary(as.factor(allLabels$labels)))

    write.csv(allPulses, file = "features.csv")
    write.csv(allLabels, file = "labels.csv")
}

#' Function to check if the labels have performed well. This function doesn't return anything
#' But instead provides an assessment of the quality of the model compared to the test data.
labelChecker <- function(dat, cells = goodCells, windowSizeMin = 3, tType = "t.dat", winMin = 1.2, winMax = 5, rdName = rd_Name, model = "./VRC.h5", winSel = T, featureWindows = 12){
    keras <- reticulate::import('keras')
    model <- keras::load_model_hdf5(model)
    pyPharm <- reticulate::import('python_pharmer')

    # Lets create the window region
    wr <- dat$w.dat$wr1
    # always remove the '' window
    levs <- setdiff(unique(dat$w.dat$wr1), "")

    # Lets find the window sizes to include,
    x1s <- tapply(dat$w.dat[,"Time"],as.factor(wr),min)
    x2s <- tapply(dat$w.dat[,"Time"],as.factor(wr),max)
    cat("\nThese are the windows sorted:\n")
    print(levs)

    windowLen <- x2s - x1s
    winCol <- names(windowLen[windowLen < winMax & windowLen > winMin])

    if(winSel){
        cat('\nSelect windows to add to the labeled data\n')
        winCol <- select.list(levs, preselect = winCol,  multiple = T, title = "Select windows", graphics = T)
    }else{
        winCol <- levs
    }
    winStart <- sort(x1s[winCol])
    winEnd <- winStart + windowSizeMin

    cat("\nThese are included windows:\n")
    print(names(winStart))

    # Test the window size since we are losing some data. due to differing window sizes
    windowSize <- c()
    for(i in 1:length(winStart)){
        windowLogic <-  dat$w.dat$Time > winStart[i] & 
                        dat$w.dat$Time < winEnd[i]

        windowSize[i] <- dim(dat$t.dat[-1][windowLogic, ])[1]
    }

    # Now what we are trying to test is, if the difference between the window size min and max 
    # are too large we know one of these windows cannot encompass the entire region of the
    # trace
    # If this is the case then we need to drop the windows that are greater than the
    # difference of 10, then select the minimum value
    windowSizeMax <- max(windowSize)

    # This is difference in window size, returns a negative number
    # Here we want logic defining windows that have a greater distance 
    # than 10 to remove.
    windowLogic <- (windowSizeMax - windowSize) < 10

    winStart <- winStart[windowLogic]

    windowSizeMin <- min(windowSize)
    
    startPoints <- Reduce(c,lapply(winStart, function(x) which(dat$t.dat$Time == x, arr.ind=T) ))
    endPoints <- startPoints + windowSizeMin


    # Here we are collecting the windows that were scored
    # These are all the smaller windows that were ensured to be correct
    modelBin <- dat$bin[goodCells, names(winStart)]
    modelBin[modelBin == 1] <- 0

    for(i in 1:length(winStart)){
        pulseToScore <- as.data.frame(t(dat[[tType]][-1][startPoints[i]:endPoints[i],goodCells]))
        featureFrame <- pyPharm$featureMaker2(pulseToScore, featureWindows)
        
        modelBin[,names(winStart)[i]] <- model$predict_classes(featureFrame)
    }

    # Real bin
    realBin <- dat$bin[goodCells, names(winStart)]

    # Summary of the results from training.
    modelBinMat <- as.matrix(modelBin)
    dim(modelBinMat) <- prod(dim(modelBinMat))

    realBinMat <- as.matrix(dat$bin[goodCells, names(winStart)])
    dim(realBinMat) <- prod(dim(realBinMat))

    dim(realBinMat)

    binComp <- cbind(modelBinMat, realBinMat)
    compSumm <- apply(binComp, 1, paste, collapse = "")
    modelVReal <- summary(as.factor(compSumm))

    cat("\nThe summary of the model vs the actual scores")
    cat("\nModel vs. Real\n")
    print(modelVReal)

    percentMisClass0 <- modelVReal['01']/(modelVReal["00"] + modelVReal['01']) * 100
    percentMisClass1 <- modelVReal['10']/(modelVReal["11"] + modelVReal['10']) * 100

    cat("\nThe percent misclassified for 0's are:\n")
    print(round(percentMisClass0, digits = 2))

    cat("\nThe percent misclassified for 1's are:\n")
    print(round(percentMisClass1, digits = 2))

    # Now compare the bin mats to see which window is scoring better or worse
    modelBinColSum <- colSums(modelBin)
    realBinColSum <- colSums(realBin)

    modelVReal <- cbind(realBinColSum, modelBinColSum)
    modelRealDiff <- apply(modelVReal, 1, diff)

    modelVReal <- cbind(modelVReal, modelRealDiff)
    #(modelVReal[1] + modelVReal[2])

    cat("\nModel vs Real Bin summary\n")
    print(modelVReal)
}

# Now that we have the labels being made we need to create an uncertainty matrix
# dat <- tmpRD
# cells = goodCells
# windowSizeMin = 3
# tType = "t.dat"
# winMin = 1.2
# winMax = 5
# rdName = rd_Name
# model = "./VRC.h5"
# winSel = T
labelBinder <- function(dat, windowSizeMin = 3, tType = "t.dat", winMin = 1.2, winMax = 5, model = "./VRC.h5", winSel = T, featureWindows = 24){
    keras <- reticulate::import('keras')
    model <- keras::load_model_hdf5(model)
    pyPharm <- reticulate::import('python_pharmer')

    # Lets create the window region
    wr <- dat$w.dat$wr1
    levs <- setdiff(unique(dat$w.dat$wr1), "")

    # Lets find the window sizes to include,
    x1s <- tapply(dat$w.dat[,"Time"],as.factor(wr),min)
    x2s <- tapply(dat$w.dat[,"Time"],as.factor(wr),max)

    # Perform a test to include windows of the right sizes
    windowLen <- x2s - x1s
    winCol <- names(windowLen[windowLen < winMax & windowLen > winMin])

    # Provides the user with control over the windows includesd
    if(winSel){
        cat('\nSelect windows to add to the labeled data\n')
        winCol <- select.list(levs, preselect = winCol,  multiple = T, title = "Select windows", graphics = T)
    }else{
        winCol <- levs
    }
    winStart <- sort(x1s[winCol])
    winEnd <- winStart + windowSizeMin

    # Test the window size since we are losing some data. due to differing window sizes
    windowSize <- c()
    for(i in 1:length(winStart)){
        windowLogic <-  dat$w.dat$Time > winStart[i] & 
                        dat$w.dat$Time < winEnd[i]

        windowSize[i] <- dim(dat$t.dat[-1][windowLogic, ])[1]
    }

    windowSizeMax <- max(windowSize)
    windowLogic <- (windowSizeMax - windowSize) < 10
    if(length(winStart[!windowLogic])> 0 ){
        cat("\nThese windows were not scored\n")
        print(names(winStart[!windowLogic]))
        cat("\nAnd these windows are defined to not be scored\n")
        print(setdiff(levs, names(winStart[windowLogic])))
    }

    # This is the way we set the maximun windo size to the minimum window size
    # for all window regions.
    winStart <- winStart[windowLogic]
    windowSizeMini <- min(windowSize)

    startPoints <- Reduce(c,lapply(winStart, function(x) which(dat$t.dat$Time == x, arr.ind=T) ))
    endPoints <- startPoints + windowSizeMini

    # Create the uncertainty matrix and add the binary scoring to the data frame
    uncertainMat <- data.frame(matrix(nrow = dim(dat$c.dat)[1], ncol = length(winStart)))
    for(i in 1:length(winStart)){
        pulseToScore <- as.data.frame(t(dat[[tType]][-1][startPoints[i]:endPoints[i],]))
        featureFrame <- pyPharm$featureMaker2(pulseToScore, featureWindows)
        
        probs <- model$predict(featureFrame)
        classes <- model$predict_classes(featureFrame)

        uncertainty <- sqrt(probs[,1]^2 + probs[,2]^2)
        uncertainMat[,i] <- uncertainty

        dat$bin[,names(winStart)[i]] <- classes
    }

    names(uncertainMat) <- names(winStart)
    row.names(uncertainMat) <- row.names(dat$c.dat)
    dat$uncMat <- uncertainMat

    # A plot to show the uncertainty per response type.
    dev.new(width = 8, height = 4)
    par(bty = 'l', mar = c(7,6,4,2))
    bpDim <- boxplot(
        uncertainMat,
        xaxt = "n",
        border = NA,
        boxfill = 'gray90',
        medcol = 'black',
        medlty = 1,
        medlwd =2,
        whisklty = 1,
        whisklwd = 2,
        whiskcol = 'black',
        staplelty = 1, 
        staplelwd = 2,
        staplecol = 'black',
        main = 'Uncertainty per response',
        ylab = 'Class probabilities\nsqrt(Sum of squares)'
    )

    # stripchart(
    #     uncertainMat, 
    #     vertical = T,
    #     jitter = .2,
    #     method = 'jitter',
    #     pch = 18,
    #     cex = .2,
    #     col = rgb(0,0,0,.2),
    #     add = T
    #     )

    par(xpd = T)
    text(
        seq(1,length(winStart), by=1),
        par('usr')[3] - yinch(.1),
        names(winStart),
        adj = 1,
        srt = 90,
        cex = .7
    )

    return(dat)
}

mainDir <- "./lukeVRCModels/"
setwd(mainDir)

expName <- list.files(pattern = "^RD[.]", full.names = T, recursive = T)

rd_Name <- sub(".Rdata", '', rev(strsplit(expName, "/")[[1]])[1])
tmpRD <- get(load(expName))

# First get rid of the drops
goodCells <- tmpRD$c.dat$id[tmpRD$bin$drop != 1]
length(goodCells)

# This makes our labeled data for a single experiment.
labeledDataMaker(tmpRD, goodCells, windowSizeMin = 3,  winMax = 5, tType = c('t.dat','blc'))

# At this point run the model, Here i simply move to python to run this.
# the model is located in this directory as amckModel.py

# Now i need to compare the scoring to Lukes original scoring
labelChecker(tmpRD, goodCells, windowSizeMin = 3, tType = c("blc"), featureWindows = 24)

# Now we will use labelBinder to add the labels to the experiment.
# this also creates a uncertainty matrix which we will use in the peakfunc view
# This function take the dat as an input and returns it with the new labels, and the
# uncertainty matrix.
tmpRD <- labelBinder(tmpRD, windowSizeMin = 3, tType = 'blc')





