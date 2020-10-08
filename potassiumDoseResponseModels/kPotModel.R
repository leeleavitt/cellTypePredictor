mainDir <- "./potassiumDoseResponseModels/200917.31.f.p1 dx332 k  dose response/"
tmpRD <- get(load(paste0(mainDir,"RD.200917.31.f.p1.Rdata")))

potassiumModeler <- function(dat){
    model <- keras::load_model_hdf5(paste0(mainDir, "Y:/Computer Setup/R/models/kdr.h5"))
    pyPharm <- reticulate::import('python_pharmer')

    windowSize <- 3
    featureWindows <- 12
    tType <- c("blc")

    # Here we are collecting the windows that were scored
    # These are all the smaller windows that were ensured to be correct
    wr <- dat$w.dat$wr1
    levs <- setdiff(unique(dat$w.dat$wr1), "")
    kWins <- grep("^[kK]", levs, value = T)
    cellTypeK40 <- grep("^[kK][.]40", levs, value = T)[1]
    kWins <- setdiff(kWins, cellTypeK40)

    x1s <- tapply(dat$w.dat[,"Time"],as.factor(wr),min)
    x2s <- tapply(dat$w.dat[,"Time"],as.factor(wr),max)
    windowLen <- x2s - x1s
    windowLen <- windowLen[kWins]

    winCol <- names(windowLen[windowLen < 2 & windowLen > 1.2])

    winStart <- sort(x1s[winCol])
    winEnd <- winStart + windowSize

    # Make an uncertainty matrix, or collect it.
    if(is.null(dat$uncMat)){
        uncertainMat <- data.frame(matrix(nrow = dim(dat$c.dat)[1], ncol = 0))
        row.names(uncertainMat) <- row.names(dat$c.dat)
    }else{
        uncertainMat <- dat$uncMat
    }

    #Apply the model to everything to see how well it performs
    for(i in 1:length(winStart)){
        windowLogic <-  dat$w.dat$Time > winStart[i] & 
                        dat$w.dat$Time < winEnd[i]

        pulseToScore <- as.data.frame(t(dat[[tType]][windowLogic,-1]))
        featureFrame <- pyPharm$featureMaker2(pulseToScore, featureWindows)
        
        dat$bin[,names(winStart)[i]] <- model$predict_classes(featureFrame)

        # add to the uncMat
        probs <- model$predict(featureFrame)
        uncertainty <- sqrt(probs[,1]^2 + probs[,2]^2)
        uncertainMat[names(winStart)[i]] <- uncertainty
        
    }

    dat$uncMat <- uncertainMat

    return(dat)
}
