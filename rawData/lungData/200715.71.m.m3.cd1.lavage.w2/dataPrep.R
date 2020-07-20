# Here we are going to prepare the training Data
getwd()
rdName <- ls(pattern = "^RD[.]")
tmpRD <- get(load("./rawData/lungData/200715.71.m.m3.cd1.lavage.w2/RD.200715.71.m.m3.cd1.lavage.w2.Rdata"))

# For this project we have multiple options to bolster our training dataset.
# To augment the traces to beef up ones or zeros we can use variou trace types.
# blc, t.dat, are the first two obvious examples. But, TraceBrewer has more traces produced.

# In this experiment (which you can see in the history), has been scored upto 1100
scoredNeurons <- 1:1100

# Here we are collecting the windows that were scored
# These are all the smaller windows that were ensured to be correct
wr <- tmpRD$w.dat$wr1
levs <- setdiff(unique(tmpRD$w.dat$wr1), "")

x1s <- tapply(tmpRD$w.dat[,"Time"],as.factor(wr),min)
x2s <- tapply(tmpRD$w.dat[,"Time"],as.factor(wr),max)
windowLen <- x2s - x1s
winCol <- names(windowLen[windowLen < 2 & windowLen > 1.2])

winStart <- sort(x1s[winCol])
winEnd <- winStart + 4

allPulses <- data.frame()
allLabels <- data.frame()
for(i in 1:length(winStart)){
    windowLogic <-  tmpRD$w.dat$Time > winStart[i] & 
                    tmpRD$w.dat$Time < winEnd[i]
    
    pulseToScore <- as.data.frame(t(tmpRD$blc[windowLogic,scoredNeurons+1]))
    colnames(pulseToScore) <- 1:dim(pulseToScore)[2]

    newRowName <- paste0(rdName, "_", row.names(pulseToScore), "_", names(winStart[i]))
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
        labelToAdd <- tmpRD$bin[scoredNeurons, names(winStart)[i], drop = FALSE]
        row.names(labelToAdd) <- newRowName
        colnames(labelToAdd) <- 'labels'
        allLabels <- rbind(allLabels, labelToAdd)
    }

}

cat("\nThe dimension of the scored neurons traces are: \n")
dim(allPulses)
cat("\nThe labels are\n")
dim(allLabels)
cat("\nIs the labels and the traces identifiers identical?\n")
identical(row.names(allPulses), row.names(allLabels))
cat("\nYou have a total of: ", dim(allLabels)[1], "labeled data points")
cat("\nThe break down of the labels are\n")
summary(as.factor(allLabels$labels))

write.csv(allPulses, file = "features.csv")
write.csv(allLabels, file = "labels.csv")