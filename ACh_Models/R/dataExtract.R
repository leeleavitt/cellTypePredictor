getwd()
mainDir <- "./ACh_Models/data/achResponses/"
setwd(mainDir)
tmpRD <- get(load(list.files(pattern = "^RD")))

# Load up the scored data. this is what will be placed into datExport
cellsClean <- get(load("cleanAndScoredACH.RData"))

levs <- grep("^ACH", names(tmpRD$bin), ignore.case = T, value = T)

length(cellsClean) * length(levs)

#' Function to export data from an experiment/dat. Before passing into this function the experiment
#' needs to be cleaned up significantly. This means while using tcd ensure the scores are accurate
#' make sure to drop weird cells. Once again scores need to be PERFECT. The output is a list
#' @param dat RD.experiment 
#' @param cells specified cells to collect for training data if NA all cells are included
#' @param levs the window regions specified
#' @param tType the trace for the feature space examples include "blc", "t.dat", or c("blc", "t.dat")
#' @param windowSize size of feature space/trace in minutes
#' @param dropRM boolean. If TRUE drops are removed
#' @examples
#' \dontrun{
#' # Here we are loading the specified cells we selected and cleaned up
#' cellsClean <- get(load("cleanAndScoredACH.RData"))
#' # Here we are selecting the window regions to collect for features
#' levs <- grep("^ACH", names(tmpRD$bin), ignore.case = T, value = T)
#' # Now we run the function as below.
#' collected <- datExport(tmpRD, cellsClean, levs)
#' write.csv(collected$allPulses, file = "features.csv")
#' write.csv(collected$allLabels, file = "labels.csv")
#' }
#' @export
datExport <- function(dat, cells = NA, levs, tType = "blc", windowSize = 4, dropRM = T){
    rdName <- deparse(substitute(dat))

    # Removing drops to attempt to improve the scoring
    if(dropRM){
        dropLogic <- dat$bin$drop == 0
        cellsClean <- dat$c.dat$id[dropLogic]
    }

    # Grab all cells if the input is NA
    if(is.na(cells)){
        cells<- dat$c.dat$id
    }

    cells <- intersect(cells, cellsClean)


    # Here we are collecting the windows that were scored
    # These are all the smaller windows that were ensured to be correct
    wr <- dat$w.dat$wr1
    if(is.na(levs)){
        levs <- setdiff(unique(dat$w.dat$wr1), "")
        levs <- grep("^[kK]", levs, value = T)
        levs <- levs[-11]
    }

    # Quick test to make sure the windows are large enough for us to use
    # a time of 1.2 minutes is default here
    x1s <- tapply(dat$w.dat[,"Time"], as.factor(wr), min)
    x2s <- tapply(dat$w.dat[,"Time"], as.factor(wr), max)
    windowLen <- x2s - x1s
    windowLen <- windowLen[levs]
    winCol <- names(windowLen[windowLen > 1.2])

    winStart <- sort(x1s[winCol])
    winEnd <- winStart + windowSize

    # now we work through all the windows and determine the number of data points per window
    # we choose the maximum number of points from the selected window regions
    dfSizes <- c()
    for(i in 1:length(winStart)){
        windowLogic <-  dat$w.dat$Time > winStart[i] & 
                        dat$w.dat$Time < winEnd[i]

        dfSizes[i] <- dim(as.data.frame(t(dat[[tType[1]]][windowLogic,cells])))[2]
    }
    endSize <- max(dfSizes)

    # collect the features and labels
    allPulses <- data.frame()
    allLabels <- data.frame()
    for(i in 1:length(winStart)){
        winStartIndex <-  min(which(dat$w.dat$Time > winStart[i] , arr.ind = T))
        windowLogic <- winStartIndex : (winStartIndex + endSize -1)
                        
        for(j in 1:length(tType)){
            pulseToScore <- as.data.frame( t(dat[[tType[j]]][windowLogic, cells]))
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
    all <- list(allPulses = allPulses, allLabels = allLabels)
    return(all)
}

collected <- datExport(tmpRD, cellsClean, levs)

write.csv(collected$allPulses, file = "features.csv")
write.csv(collected$allLabels, file = "labels.csv")
