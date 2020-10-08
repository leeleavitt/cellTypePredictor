# Real data export
# This means that 4 min * (60 sec / 1 min) * (1 frame / 2 sec)
getwd()
mainDir <- "G:/My Drive/cellTypePredictor/lungMultiModels/"
setwd(mainDir)

expLocs <- list.files(pattern = "RD[.].*Rdata$", recursive = T)
pyPharm <- reticulate::import('python_pharmer')
model <- keras::load_model_hdf5(paste0(mainDir, "lungMulti.h5"))
labelOrig <- c()
labelNew <- c()
for(i in 1:length(expLocs)){
    tmpRD <- get(load(expLocs[i]))
    windowSize <- 4

    featureWindows <- 12
    dat <- tmpRD
    levs <- setdiff(unique(tmpRD$w.dat$wr1), c("", "epad"))
    
    for(j in 1:length(levs)){

        pulse <- levs[j]
        
        winStart <- min( tmpRD$w.dat$Time[dat$w.dat$wr1 == pulse] )
        winEnd <- winStart + windowSize

        pulseLogic <-  tmpRD$w.dat$Time > winStart & 
                            tmpRD$w.dat$Time < winEnd

        pulseToScore <- as.data.frame(t(tmpRD[["blc"]][pulseLogic,-1]))
        featureFrame <- pyPharm$featureMaker2(pulseToScore, featureWindows)
        
        labelNew <- c(labelNew, model$predict_classes(featureFrame))
        labelOrig <- c(labelOrig, tmpRD$bin[,pulse])
    }
}

confusion <-paste0(labelOrig, labelNew)
resultSum <- summary(as.factor(confusion))

zeroResult <- (resultSum["01"] / (resultSum["00"] + resultSum["01"])) * 100

oneResult <- (resultSum["10"] / (resultSum["11"] + resultSum["10"])) * 100

cat("\nThe results of the Neural network is\nOnes:", oneResult, " %\nZeros:", zeroResult, " %\n")






