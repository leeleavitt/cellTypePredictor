mainDir <- "./potassiumDoseResponseModels/200917.31.f.p1 dx332 k  dose response/"
tmpRD <- get(load(paste0(mainDir,"RD.200917.31.f.p1.Rdata")))
rdName <- ls(pattern = "^RD[.]")

model <- keras::load_model_hdf5(paste0(mainDir, "kdr.h5"))
pyPharm <- reticulate::import('python_pharmer')

# In this experiment (which you can see in the history), has been scored upto 1100
scoredNeurons <- tmpRD$c.dat$id

# Removing drops to attempt to improve the scoring
dropLogic <- tmpRD$bin$drop == 0
scoredNeurons <- tmpRD$c.dat$id[dropLogic]

windowSize <- 3
featureWindows <- 6
featureWindows <- 12
tType <- c("blc")

# Here we are collecting the windows that were scored
# These are all the smaller windows that were ensured to be correct
wr <- tmpRD$w.dat$wr1
levs <- setdiff(unique(tmpRD$w.dat$wr1), "")
levs <- grep("^[kK]", levs, value = T)

x1s <- tapply(tmpRD$w.dat[,"Time"],as.factor(wr),min)
x2s <- tapply(tmpRD$w.dat[,"Time"],as.factor(wr),max)
windowLen <- x2s - x1s
windowLen <- windowLen[levs]

winCol <- names(windowLen[windowLen < 2 & windowLen > 1.2])

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

    pulseToScore <- as.data.frame(t(tmpRD[[tType]][windowLogic,scoredNeurons]))
    featureFrame <- pyPharm$featureMaker2(pulseToScore, featureWindows)
    
    modelBin[,names(winStart)[i]] <- model$predict_classes(featureFrame)
}

# Summary of the results from training.
modelBinMat <- as.matrix(modelBin)
dim(modelBinMat) <- prod(dim(modelBinMat))

realBinMat <- as.matrix(tmpRD$bin[scoredNeurons, winCol])
dim(realBinMat) <- prod(dim(realBinMat))

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

# We also need to compare per response success
realBin <- tmpRD$bin[scoredNeurons, winCol]
dim(modelBin)
dim(realBin)

binComp <- realBin
binComp[binComp == 1] <- 0
for(i in 1:dim(realBin)[2]){
    binComp[,i] <- as.factor(paste0(realBin[,i], modelBin[,i]))
}

binSumMat <- matrix(nrow = 4, ncol = 11)
for(i in 1:dim(binComp)[2]){
    binCompSumm <- summary(binComp[,i])
    binSumMat[,i] <- binCompSumm
}
colnames(binSumMat) <- colnames(realBin)
row.names(binSumMat) <-names(binCompSumm)
binSumMat



# tmpRD
# Make an uncertainty matrix, or collect it.
if(is.null(tmpRD$uncMat)){
    uncertainMat <- data.frame(matrix(nrow = dim(tmpRD$c.dat)[1], ncol = 0))
    row.names(uncertainMat) <- row.names(tmpRD$c.dat)
}else{
    uncertainMat <- tmpRD$uncMat
}


#Apply the model to everything to see how well it performs
for(i in 1:length(winStart)){
    windowLogic <-  tmpRD$w.dat$Time > winStart[i] & 
                    tmpRD$w.dat$Time < winEnd[i]

    pulseToScore <- as.data.frame(t(tmpRD[[tType]][windowLogic,-1]))
    featureFrame <- pyPharm$featureMaker2(pulseToScore, featureWindows)
    
    tmpRD$bin[,names(winStart)[i]] <- model$predict_classes(featureFrame)

    # add to the uncMat
    probs <- model$predict(featureFrame)
    uncertainty <- sqrt(probs[,1]^2 + probs[,2]^2)
    uncertainMat[names(winStart)[i]] <- uncertainty
    
}

tmpRD$uncMat <- uncertainMat

require(procPharm)
bob <- tcd(tmpRD, sample(tmpRD$c.dat$id)) 



