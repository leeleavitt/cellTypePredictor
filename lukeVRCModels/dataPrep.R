mainDir <- "./lukeVRCModels/"
setwd(mainDir)

expName <- list.files(pattern = "^RD[.]", full.names = T, recursive = T)

rd_Name <- sub(".Rdata", '', rev(strsplit(expName, "/")[[1]])[1])
tmpRD <- get(load(expName))

# First get rid of the drops
goodCells <- tmpRD$c.dat$id[tmpRD$bin$drop != 1]
length(goodCells)

#' Funciton to create labeled data from a single experiment
#' @param dat is th labeled data
#' @param cells a selective subset of neurons to input into the function
#' @param windowSizeMin This is the size of the window to view the peak in minutes
#' @param tType chracter or character vector this is the trace types to include to improve training.
#' @param winMin this is the minimum window size to include for the windows. 
#' @param winMax this is the maximum window size to include for the window size.
#' @param rdName the actual name of the experiment
labeledDataMaker <- function(dat, cells = cellSelect, windowSizeMin = 3, tType = "blc", winMin = 1.2, winMax = 2, rdName = rd_Name){
    # Lets create the window region
    wr <- tmpRD$w.dat$wr1
    # always remove the '' window
    levs <- setdiff(unique(dat$w.dat$wr1), "")

    # Lets find the window sizes to include,
    x1s <- tapply(dat$w.dat[,"Time"],as.factor(wr),min)
    x2s <- tapply(dat$w.dat[,"Time"],as.factor(wr),max)
    cat("\nThese are the windows sorted:\n")
    print(levs)

    windowLen <- x2s - x1s
    winCol <- names(windowLen[windowLen < winMax & windowLen > winMin])

    cat('\nSelect windows to add to the labeled data\n')
    winCol <- select.list(levs, preselect = winCol,  multiple = T, title = "Select windows", graphics = T)

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
    windowSizeMin <- min(windowSize)

    windowSizeDiff <- windowSizeMax - windowSizeMin

    if(windowSizeDiff < 10){
        endPoint <- windowSizeMin
    }else{
        windowSize
    }

    startPoints <- Reduce(c,lapply(winStart, function(x) which(dat$t.dat$Time == x, arr.ind=T) ))


    allPulses <- data.frame()
    allLabels <- data.frame()
    for(i in 1:length(winStart)){
        windowLogic <-  dat$w.dat$Time > winStart[i] & 
                        dat$w.dat$Time < winEnd[i]

        # This will allow me to collect multiple traces.
        for(j in 1:length(tType)){
            pulseToScore <- as.data.frame(t(dat[[ tType[j] ]][-1][windowLogic, cells]))
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


labeledDataMaker(tmpRD, goodCells, winMax = 5)
