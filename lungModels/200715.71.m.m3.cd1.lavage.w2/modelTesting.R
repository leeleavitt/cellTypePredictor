getwd()
tmpRD <- get(load("./rawData/lungData/200715.71.m.m3.cd1.lavage.w2/RD.200715.71.m.m3.cd1.lavage.w2.Rdata"))
rdName <- ls(pattern = "^RD[.]")

model <- keras::load_model_hdf5("./rawData/lungData/200715.71.m.m3.cd1.lavage.w2/lung.h5")
pyPharm <- reticulate::import('python_pharmer')

# In this experiment (which you can see in the history), has been scored upto 1100
scoredNeurons <- 1:1100
windowSize <- 3
featureWindows <- 6
tType <- c("blc")

# Here we are collecting the windows that were scored
# These are all the smaller windows that were ensured to be correct
wr <- tmpRD$w.dat$wr1
levs <- setdiff(unique(tmpRD$w.dat$wr1), "")

x1s <- tapply(tmpRD$w.dat[,"Time"],as.factor(wr),min)
x2s <- tapply(tmpRD$w.dat[,"Time"],as.factor(wr),max)
windowLen <- x2s - x1s
winCol <- names(windowLen[windowLen < 2 & windowLen > 1])

winStart <- sort(x1s[winCol])
winEnd <- winStart + windowSize

allPulses <- data.frame()
allLabels <- data.frame()

modelBin <- tmpRD$bin[scoredNeurons, winCol]
modelBin[modelBin == 1] <- 0

for(i in 1:length(winStart)){
    print(i)
    windowLogic <-  tmpRD$w.dat$Time > winStart[i] & 
                    tmpRD$w.dat$Time < winEnd[i]

    pulseToScore <- as.data.frame(t(tmpRD[[tType]][windowLogic,scoredNeurons+1]))
    featureFrame <- pyPharm$featureMaker2(pulseToScore, featureWindows)
    
    modelBin[,names(winStart)[i]] <- model$predict_classes(featureFrame)
}

# Summary of the results from training.
modelBinMat <- as.matrix(modelBin)
dim(modelBinMat) <- prod(dim(modelBinMat))

realBinMat <- as.matrix(tmpRD$bin[scoredNeurons, winCol])
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
print(percentMisClass0)

cat("\nThe percent misclassified for 1's are:\n")
print(percentMisClass1)


#Apply the model to everything to see how well it performs
for(i in 1:length(winStart)){
    windowLogic <-  tmpRD$w.dat$Time > winStart[i] & 
                    tmpRD$w.dat$Time < winEnd[i]

    pulseToScore <- as.data.frame(t(tmpRD[[tType]][windowLogic,-1]))
    featureFrame <- pyPharm$featureMaker2(pulseToScore, featureWindows)
    
    tmpRD$bin[,names(winStart)[i]] <- model$predict_classes(featureFrame)
}


bob <- tcd(tmpRD, sample(tmpRD$c.dat$id)) 



