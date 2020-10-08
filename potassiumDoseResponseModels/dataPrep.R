# Here we are going to prepare the training Data
getwd()
mainDir <- "./potassiumDoseResponseModels/200917.31.f.p1 dx332 k  dose response/"
tmpRD <- get(load(paste0(mainDir,"RD.200917.31.f.p1.Rdata")))
rdName <- ls(pattern = "^RD[.]")

# For this project we have multiple options to bolster our training dataset.
# To augment the traces to beef up ones or zeros we can use variou trace types.
# blc, t.dat, are the first two obvious examples. But, TraceBrewer has more traces produced.

## 1 Select window regions decide the sizes
windowSize <- 3
#tType <- c("t.dat", "blc")
tType <- c("blc")

scoredNeurons <- tmpRD$c.dat$id

# Removing drops to attempt to improve the scoring
dropLogic <- tmpRD$bin$drop == 0
scoredNeurons <- tmpRD$c.dat$id[dropLogic]

# Here we are collecting the windows that were scored
# These are all the smaller windows that were ensured to be correct
wr <- tmpRD$w.dat$wr1
levs <- setdiff(unique(tmpRD$w.dat$wr1), "")
levs <- grep("^[kK]", levs, value = T)
levs <- levs[-11]

x1s <- tapply(tmpRD$w.dat[,"Time"],as.factor(wr),min)
x2s <- tapply(tmpRD$w.dat[,"Time"],as.factor(wr),max)
windowLen <- x2s - x1s
windowLen <- windowLen[levs]

winCol <- names(windowLen[windowLen < 2 & windowLen > 1.2])

winStart <- sort(x1s[winCol])
winEnd <- winStart + windowSize

dfSizes <- c()
for(i in 1:length(winStart)){
    windowLogic <-  tmpRD$w.dat$Time > winStart[i] & 
                    tmpRD$w.dat$Time < winEnd[i]

    dfSizes[i] <- dim(as.data.frame(t(tmpRD[[tType[1]]][windowLogic,scoredNeurons])))[2]
}
endSize <- max(dfSizes)

## 2 collect the features and labels
allPulses <- data.frame()
allLabels <- data.frame()
for(i in 1:length(winStart)){
    print(i)
    winStartIndex <-  min(which(tmpRD$w.dat$Time > winStart[i] , arr.ind = T))
    windowLogic <- winStartIndex : (winStartIndex + endSize -1)

                    
    for(j in 1:length(tType)){
        pulseToScore <- as.data.frame(t(tmpRD[[tType[j]]][windowLogic,scoredNeurons]))
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
            labelToAdd <- tmpRD$bin[scoredNeurons, names(winStart)[i], drop = FALSE]
            row.names(labelToAdd) <- newRowName
            colnames(labelToAdd) <- 'labels'
            allLabels <- rbind(allLabels, labelToAdd)
        }
    }
}

## 3 Give me a run down of the features and labels breadown
cat("\nThe dimension of the scored neurons traces are: \n")
print(dim(allPulses))
cat("\nThe labels are\n")
print(dim(allLabels))
cat("\nIs the labels and the traces identifiers identical?\n")
print(identical(row.names(allPulses), row.names(allLabels)))
cat("\nYou have a total of: ", dim(allLabels)[1], "labeled data points")
cat("\nThe break down of the labels are\n")
print(summary(as.factor(allLabels$labels)))

## 4 Save it up for the python
write.csv(allPulses, file = paste0(mainDir, "features.csv"))
write.csv(allLabels, file = paste0(mainDir, "labels.csv"))

## 5 Make augmented data
oneDAugmentJit <- function(dat,cells, response){
    winStartIndex <-  min(which(dat$w.dat$Time > winStart[response] , arr.ind = T))
    windowLogic <- winStartIndex : (winStartIndex + 89)


    pulsesToScore <- t(dat[[tType]][windowLogic, cells])

    jitAmt <- c(
        0.002, 0.002, 0.002, 0.002, 0.002, 0.002, 0.002, 0.002, 0.002, 0.002,
        0.0025, 0.0025, 0.0025, 0.0025, 0.0025,0.0025, 0.0025, 0.0025, 0.0025, 0.0025,
        0.003, 0.003, 0.003, 0.003, 0.003, 0.003, 0.003, 0.003, 0.003, 0.003,
        0.0035, 0.0035, 0.0035, 0.0035, 0.0035,0.0035, 0.0035, 0.0035, 0.0035, 0.0035
        # 0.004, 0.004, 0.004, 0.004,
        # 0.0045, 0.0045, 0.0045, 0.0045
    )

    augmentedData <- matrix(nrow = 0, ncol = dim(pulsesToScore)[2])
    augmentedDataLabels <- data.frame()

    for(i in 1:length(jitAmt)){
        augment1 <- apply(pulsesToScore, 2, jitter, amount  = jitAmt[i])
        newRowName <- paste0(rdName, "_", cells, "_", names(winStart[response]),"_", paste0('augment',i))
        row.names(augment1) <- newRowName

        augmentedData <-rbind(augmentedData, augment1)

        labelToAdd <- dat$bin[cells, response, drop = FALSE]
        row.names(labelToAdd) <- newRowName
        colnames(labelToAdd) <- 'labels'
        augmentedDataLabels <- rbind(augmentedDataLabels, labelToAdd)
    }
    return(list(features = augmentedData, labels = augmentedDataLabels))
}
## 5 Make augmented data

oneDAugmentJit <- function(dat,cells, response){
    winStartIndex <-  min(which(dat$w.dat$Time > winStart[response] , arr.ind = T))
    windowLogic <- winStartIndex : (winStartIndex + 89)


    pulsesToScore <- t(dat[[tType]][windowLogic, cells])

    jitAmt <- c(
        0.002, 0.002, 0.002, 0.002, 0.002, 0.002, 0.002, 0.002, 0.002, 0.002,
        0.0025, 0.0025, 0.0025, 0.0025, 0.0025,0.0025, 0.0025, 0.0025, 0.0025, 0.0025,
        0.003, 0.003, 0.003, 0.003, 0.003, 0.003, 0.003, 0.003, 0.003, 0.003,
        0.0035, 0.0035, 0.0035, 0.0035, 0.0035,0.0035, 0.0035, 0.0035, 0.0035, 0.0035
        # 0.004, 0.004, 0.004, 0.004,
        # 0.0045, 0.0045, 0.0045, 0.0045
    )

    augmentedData <- matrix(nrow = 0, ncol = dim(pulsesToScore)[2])
    augmentedDataLabels <- data.frame()

    for(i in 1:length(jitAmt)){
        augment1 <- apply(pulsesToScore, 2, jitter, amount  = jitAmt[i])
        newRowName <- paste0(rdName, "_", cells, "_", names(winStart[response]),"_", paste0('augment',i))
        row.names(augment1) <- newRowName

        augmentedData <-rbind(augmentedData, augment1)

        labelToAdd <- dat$bin[cells, response, drop = FALSE]
        row.names(labelToAdd) <- newRowName
        colnames(labelToAdd) <- 'labels'
        augmentedDataLabels <- rbind(augmentedDataLabels, labelToAdd)
    }
    return(list(features = augmentedData, labels = augmentedDataLabels))
}

oneDAugmentshift <- function(dat,cells, response){
    winStartIndex <-  min(which(dat$w.dat$Time > winStart[response] , arr.ind = T))
    windowLogic <- winStartIndex : (winStartIndex + 89)

    pulsesToScore <- t(dat[[tType]][windowLogic, cells])


    # Second method move pulse bak 1 or two points
    pulseToScore3 <- c(pulseToScore[1], pulseToScore[-length(pulseToScore)])
    pulseToScore4 <- c(pulseToScore3[1], pulseToScore3[-length(pulseToScore3)])
    pulseToScore5 <- c(pulseToScore4[1], pulseToScore4[-length(pulseToScore4)])

    augmentedData <- matrix(nrow = 0, ncol = dim(pulsesToScore)[2])
    augmentedDataLabels <- data.frame()

    for(i in 1:length(jitAmt)){
        augment1 <- c(pulseToScore[1], pulseToScore[-length(pulseToScore)])
        augment1 <- apply(pulsesToScore, 2, jitter, amount  = jitAmt[i])
        newRowName <- paste0(rdName, "_", cells, "_", names(winStart[response]),"_", paste0('augment',i))
        row.names(augment1) <- newRowName

        augmentedData <-rbind(augmentedData, augment1)

        labelToAdd <- dat$bin[cells, response, drop = FALSE]
        row.names(labelToAdd) <- newRowName
        colnames(labelToAdd) <- 'labels'
        augmentedDataLabels <- rbind(augmentedDataLabels, labelToAdd)
    }
    return(list(features = augmentedData, labels = augmentedDataLabels))
}

# This 10mM pulses are of low abundance
k10Logic <- tmpRD$bin["K.10mM"] == 1
cellsToAugment <- tmpRD$c.dat$id[k10Logic]
augDat1 <- oneDAugmentJit(tmpRD, cellsToAugment, "K.10mM")

# We also want to grab all of the first 20mM potassium repsonses 
# that are below a max hight of 0.07
cellSelLogic <- tmpRD$bin["K.20mM"] == 1 &
    tmpRD$scp["K.20mM.max"] < 0.2

cellSel <- tmpRD$c.dat$id[cellSelLogic]
augDat2 <- oneDAugmentJit(tmpRD, cellSel, "K.20mM")

dim(augDat2$features)
## 4 Save it up for the python
write.csv(rbind(augDat2$features, augDat1$features), file = paste0(mainDir, "augfeatures.csv"))
write.csv(rbind(augDat2$labels, augDat1$labels), file = paste0(mainDir, "auglabels.csv"))

write.csv(rbind(augDat2$features, augDat1$features), file = paste0(mainDir, "augfeatures.csv"))
write.csv(rbind(augDat2$labels, augDat1$labels), file = paste0(mainDir, "auglabels.csv"))



