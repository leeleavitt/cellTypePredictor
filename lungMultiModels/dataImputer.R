getwd()
mainDir <- "G:/My Drive/cellTypePredictor/lungMultiModels/"
setwd(mainDir)

expLocs <- list.files(pattern = "RD[.].*Rdata$", recursive = T)

# With these exerpiments luke scored we have to create a function that 
# Collects all cells and all responses.

tmpRD <- get(load(expLocs[1]))

# Four minute intervals
windowSize <- 4

# Figure our where the windows start and end
levs <- setdiff(unique(tmpRD$w.dat$wr1), "")
windowStarts <- tapply(tmpRD$w.dat$Time, as.factor(tmpRD$w.dat$wr1), min)
windowStarts <- windowStarts[levs]

windowEnds <- tapply(tmpRD$w.dat$Time, as.factor(tmpRD$w.dat$wr1), max)
windowEnds <- windowEnds[levs]

# what is the size of the windows
windowLen <- windowEnds - windowStarts
windowLen <- windowLen[levs]

# Only select windows larger than 1.2
winCol <- names(windowLen[windowLen > 1.2])

# Not all windows are equal. So now we need to check each window regions
winStart <- sort(windowStarts[winCol])
winEnd <- winStart + windowSize

dfSizes <- c()
for(i in 1:length(winStart)){
    windowLogic <-  tmpRD$w.dat$Time > winStart[i] & 
                    tmpRD$w.dat$Time < winEnd[i]

    dfSizes[i] <- dim(as.data.frame(t(tmpRD[["blc"]][windowLogic,])))[2]
}
endSize <- max(dfSizes)
round(endSize, digits = -1)

require(procPharm)
tcd(tmpRD)

summary(as.factor(tmpRD$w.dat$wr1))

6# This means that 4 min * (60 sec / 1 min) * (1 frame / 2 sec)

windowSize * (60/1) * (1/2)

cell <- "X.2498"
pulse <- levs[2]
pulseLogic <- tmpRD$w.dat[,"wr1"] == pulse

cellPulse <- tmpRD[['blc']][pulseLogic, cell]
pulseTime <- tmpRD[['blc']][pulseLogic, "Time"]
xspan <- 5/length(cellPulse)
timeSteps <-  windowSize * (60/1) * (1/2)

targ <- data.frame(pulseTime = seq(min(pulseTime), max(pulseTime), length.out = timeSteps))
xloe <- loess(cellPulse ~ pulseTime, span = xspan)
plot(xloe)
xp <- predict(xloe, newdata = targ)

dim(targ)

#dEGREE OF pOLYNOMIAL
degree <- 20
randAmount <- 1
coef <- c(6)

polyModel <- lm(cellPulse ~ poly(pulseTime, degree))
polyModelPred <- predict(polyModel, targ)

modelNames <- paste0("polyModel", seq(1,20,1))

randomPredictions <- list()
for(i in 1:length(modelNames)){
    polyModelSamp <- polyModel
    coefSamp <- sample(c(2:5))[1:2]
    polyModelSamp$coefficients[coef] <- jitter(polyModelSamp$coefficients[coef], amount = randAmount)
    randomPredictions[[ modelNames[i] ]] <- predict(polyModelSamp, targ)
    assign(modelNames[i], polyModelSamp)
}

# Show the loess smoothing
plot(pulseTime, cellPulse, pch=16, cex=1.2, xlab="time", ylab="Response", col="black", ylim = c(0,1.3))
lines(targ[,1], xp, col="red", cex=.8, lwd =2)

# Show the polymodal function
plot(pulseTime, cellPulse, pch=16, cex=1.2, xlab="time", ylab="Response", col="black", ylim = c(-0.5,1.3))
lines(targ[,1], polyModelPred , col = 'orange', pch=16, cex=.8, lwd=2)

col <- rgb(0,0,1,.2)
colFunc <- colorRampPalette(c("red","blue"))
cols <- colFunc(length(randomPredictions))
for(i in length(randomPredictions):1){
    lines(targ[,1], randomPredictions[[i]], col = cols[i], pch=16, cex=.8, lwd=1)
}


# I wanna try this mars fitting
#install.packages('earth')

randAmount <- .15
coef <- sample(c(1:4))[1:2]
coef = 2

mars1Model <- earth::earth(
  cellPulse ~ pulseTime,
  degree  = 2,
  penalty = -1,
  thresh  = 0,
  nk = 500
)
mars1Model$coefficients

mars1ModelPred <- predict(mars1Model, targ)
plot(pulseTime, cellPulse, pch=16, cex=1.2, xlab="time", ylab="Response", col="black", ylim = c(-0.5,1.3))
lines(targ[,1], mars1ModelPred , col = 'orange', pch=16, cex=.8, lwd=3)

modelNames <- paste0("marsModel", seq(1,20,1))

randomPredictions <- list()
for(i in 1:length(modelNames)){
    marsModelSamp <- mars1Model
    marsModelSamp$coefficients[coef] <- jitter(marsModelSamp$coefficients[coef], amount = randAmount)
    randomPredictions[[ modelNames[i] ]] <- predict(marsModelSamp, targ)
    assign(modelNames[i], marsModelSamp)
}

col <- rgb(0,0,1,.2)
colFunc <- colorRampPalette(c("red","blue"))
cols <- colFunc(length(randomPredictions))
for(i in length(randomPredictions):1){
    lines(targ[,1], randomPredictions[[i]], col = cols[i], pch=16, cex=.8, lwd=1)
}



