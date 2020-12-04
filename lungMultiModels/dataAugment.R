# Snooping
getwd()
mainDir <- "G:/My Drive/cellTypePredictor/lungMultiModels/"
setwd(mainDir)

expLocs <- list.files(pattern = "RD[.].*Rdata$", recursive = T)

# With these exerpiments luke scored we have to create a function that 
# Collects all cells and all responses.

#################################################################
#
#' @param tmpRD  rd.experiment
#' @param levs window regions to apply this to
#' @param degree size of polynomial function
#' @param windowSize size of window region to perform this on
#' @param label the label per window region to apply this to
#' @param degree the size of the polynomial function to apply this to
#' @param augSamps the number of times to augment a single response.
#' @param sdFactor the sdfactor, a factor which multiplies the standard deviation of the coefficient spread of that specific coefficient location. Increasing this value will result in noisier augments
#' @param coefs The coefficients to randomize during augmentation. Increasing this usually results in noisier data
#' @param wins square root of the number of traces to observe if plotit = t
#' @param plotit logical, if true then wins^2 will be plotted to allow you to observe this
responsePolyModelAugmentor<- function(tmpRD, levs = NA, windowSize = 4, label = 1, augSamps = 100, degree = 20, sdFactor = 1.8, coefs = 2:8, wins = 7, parallel = T, plotit = T, saveIt = T){

    numCores <- parallel::detectCores()
    doParallel::registerDoParallel(numCores)
    require(doParallel)

    if(is.na(levs)){
        levs <- setdiff(unique(tmpRD$w.dat$wr1),c("", "epad"))
    }
    windowStarts <- tapply(tmpRD$w.dat$Time, as.factor(tmpRD$w.dat$wr1), min)
    windowStarts <- windowStarts[levs]

    # augmentedFeatures <- foreach(m = 1:length(levs), .combine = rbind, .packages="foreach") %do% { 
    augmentedFeatures <- data.frame()
    for(m in 1:length(levs)){
        pulse <- levs[m]
        cat("\nPulse", m, " of", length(levs), ": ", pulse, "\n")
        
        # Only augment cells that are of the specified label
        cells <- tmpRD$c.dat$id[tmpRD$bin[pulse] == label]

        # Obtain the window region based on the window size in minutes
        winStart <- windowStarts[pulse]
        winEnd <- winStart + windowSize
        
        pulseLogic <-  tmpRD$w.dat$Time > winStart & 
                        tmpRD$w.dat$Time < winEnd
        
        pulseTime <- tmpRD[['blc']][pulseLogic, "Time"]
        
        # This creates the specific array to output.
        timeSteps <-  windowSize * (60/1) * (1/2)
        targ <- data.frame(
            pulseTime = seq(
                min(pulseTime), 
                max(pulseTime), 
                length.out = timeSteps
            )
        )

        # For each cell fit the trace to a specific polynomial
        # Collect all the coefficients from all input cells
        # perform the modeling, and save the coefficients
        coefList = list()
        for(i in 1:length(cells)){
        #coefList <- foreach(i = 1:length(cells)) %do% {
            cell = cells[i]
            cellPulse <- tmpRD[['blc']][pulseLogic, cell]
            polyModel <- lm(cellPulse ~ poly(pulseTime, degree))
            coefList[[cell]] <- polyModel$coefficients
        }
        coefDf <- Reduce(rbind, coefList)

        # Now what we will do is create synthetic responsese by randomizing
        # the coefficients The amount of randomization is dictated by
        # the sdfactor, a factor which multiplies the standard deviation of the 
        # coefficient spread of that specific coefficient
        # and which coefficients are randomized.
        if(plotit == T){
            dev.new(width = 15, height  = 15)
            par(mfrow = c(wins, wins))

            for(i in sample(1:length(cells))){
                if(i < wins^2){
                    # Compute a model!
                    polyModel <- lm(tmpRD[['blc']][pulseLogic, cells[i]] ~ poly(pulseTime, degree))
                    origCoef <- polyModel$coefficients

                    par(mar = c(0,0,0,0), bg = "gray70", xaxt = 'n', yaxt = 'n')
                    tryCatch({
                        plot(
                            pulseTime, 
                            tmpRD[['blc']][pulseLogic, cells[i]], 
                            pch=16, 
                            cex=1.2, 
                            xlab="time", 
                            ylab="Response", 
                            ylim = c(-0.5,1.3), 
                            main = pulse, 
                            col= "black",
                            type = 'l',
                            lwd = 2)
                        
                        samples = 50
                        cols <- rainbow( n = samples)
                        for(j in 1:samples){
                            #coefSamp <- coefDfBpInfo[3,]
                            coefSamp <- origCoef
                            for(k in coefs){
                                coefSamp[k] <- sample(
                                    seq(
                                    origCoef[k] - ( sdFactor*sd(coefDf[,k]) ), 
                                    origCoef[k] + ( sdFactor*sd(coefDf[,k]) ), 
                                    length.out = 100
                                    )
                                )[1]
                            }

                            polyModel$coefficients <- coefSamp
                            polyModelPred <- predict(polyModel, newdata = targ)

                            # Show the polymodal function
                            lines(targ[,1], polyModelPred , col = cols[j], pch=16, cex=.8, lwd=1)
                        }
                        lines(
                            pulseTime, 
                            tmpRD[['blc']][pulseLogic, cells[i]], 
                            cex=1.2, 
                            col= "black",
                            type = 'l',
                            lwd = 2)
                    }, error = function(e) NULL)
                }
            }
        }

        #   This can be very time consuming.
        if(saveIt){
            if(!parallel){
                #For each cell create a random model output the specifed number of augsamps requested
                augmentedFeaturesToAdd <- data.frame()
                for(i in 1:length(cells)){
                    if(i%%50 == 0){
                        cat("Currently on cell ", i, " of ", length(cells), " cells\n")
                    }

                    # Compute a model!
                    polyModel <- lm(tmpRD[['blc']][pulseLogic, cells[i]] ~ poly(pulseTime, degree))
                    origCoef <- polyModel$coefficients
                    
                    # Now perform my coefficient randomizer
                    # Start with a augment sampler
                    newFeatures <- data.frame()
                    for(j in 1:augSamps){
                    #polyModelPreds <- foreach(j = 1:augSamps, .combine = rbind, .packages="foreach") %do% {
                        coefSamp <- origCoef
                        #coefSamp <- foreach(k = coefs, .combine = c, .packages="foreach") %:% {
                        for(k in coefs){
                            coefSamp[k] <- sample(
                                seq(
                                origCoef[k] - ( sdFactor*sd(coefDf[,k]) ), 
                                origCoef[k] + ( sdFactor*sd(coefDf[,k]) ), 
                                length.out = 100
                                )
                            )[1]
                        }

                        polyModel$coefficients <- coefSamp
                        polyModelPred <- predict(polyModel, newdata = targ)
                        newFeatures <- rbind(newFeatures, polyModelPred)
                        colnames(newFeatures) <- seq(1, timeSteps)
                    }
                    augmentedFeaturesToAdd <- rbind(augmentedFeaturesToAdd, newFeatures)
                }
                #newFeatures1
                #str(newFeatures1)
                augmentedFeatures <- rbind(augmentedFeatures, augmentedFeaturesToAdd)
            }else{
                #For each cell create a random model output the specifed number of augsamps requested
                augmentedFeaturesToAdd <- foreach(i = 1:length(cells), .combine = rbind, .packages="foreach") %dopar% { 
                    # Compute a model!
                    polyModel <- lm(tmpRD[['blc']][pulseLogic, cells[i]] ~ poly(pulseTime, degree))
                    origCoef <- polyModel$coefficients
                    
                    # Now perform my coefficient randomizer
                    # Start with a augment sampler 
                    polyModelPreds <- foreach(j = 1:augSamps, .combine = rbind, .packages="foreach") %do% {
                        coefSamp <- origCoef
                        #coefSamp <- foreach(k = coefs, .combine = c, .packages="foreach") %:% {
                        for(k in coefs){
                            coefSamp[k] <- sample(
                                seq(
                                origCoef[k] - ( sdFactor*sd(coefDf[,k]) ), 
                                origCoef[k] + ( sdFactor*sd(coefDf[,k]) ), 
                                length.out = 100
                                )
                            )[1]
                        }

                        polyModel$coefficients <- coefSamp
                        polyModelPred <- predict(polyModel, newdata = targ)
                        polyModelPred
                    }
                    polyModelPreds
                }
                augmentedFeatures <- rbind(augmentedFeatures, augmentedFeaturesToAdd)
            }
        }
    }

    if(saveIt){
        augmentedLabels <- rep(label, dim(augmentedFeatures)[1])
        return(list(augmentedFeatures =augmentedFeatures, augmentedLabels = augmentedLabels))
    }
}


allAugmentedFeatures <- data.frame()
allAugmentedLabels <- c()
for(i in 1:length(expLocs)){
    tmpRD <- get(load(expLocs[i]))
    augmentedData <- responsePolyModelAugmentor(tmpRD, sdFactor = 1.1, coefs = 2:8, augSamps = 5, plotit = F, parallel = F)
    
    allAugmentedFeatures <- rbind(allAugmentedFeatures, augmentedData$augmentedFeatures)
    allAugmentedLabels <- c(allAugmentedLabels, augmentedData$augmentedLabels)
}

write.csv(allAugmentedFeatures, "augfeatures.csv")
write.csv(allAugmentedLabels, "auglabels.csv")



#######################
# Testing
tmpRD <- get(load(expLocs[1]))
graphics.off()

levs <- setdiff(unique(tmpRD$w.dat$wr1),c("", "epad"))
cat("\n This is how many augmented data should come through ", sum(colSums(tmpRD$bin[levs]) * 10), " \n")

cat("\n For 10 augment samples per trace, normal\n")
print(system.time(augmentedData <- responsePolyModelAugmentor(tmpRD, sdFactor = 2, coefs = 2:4, augSamps = 10, plotit = T, parallel = F)))

cat("\n For 10 augment samples per trace, parallelized\n")
print(system.time(augmentedData <- responsePolyModelAugmentor(tmpRD, sdFactor = 2, coefs = 2:4, augSamps = 10, plotit = F, parallel = T)))

cat("\n For 100 augment samples per trace, normal\n")
print(system.time(augmentedData <- responsePolyModelAugmentor(tmpRD, sdFactor = 2, coefs = 2:4, augSamps = 100, plotit = F, parallel = F)))

cat("\n For 100 augment samples per trace, parallel\n")
print(system.time(augmentedData <- responsePolyModelAugmentor(tmpRD, sdFactor = 2, coefs = 2:4, augSamps = 100, plotit = F, parallel = T)))

cat("\n For 100 augment samples per trace, normal\n")
print(system.time(augmentedData <- responsePolyModelAugmentor(tmpRD, sdFactor = 2, coefs = 2:4, augSamps = 1000, plotit = F, parallel = F)))

cat("\n For 100 augment samples per trace, parallel\n")
print(system.time(augmentedData <- responsePolyModelAugmentor(tmpRD, sdFactor = 2, coefs = 2:4, augSamps = 1000, plotit = F, parallel = T)))

cat("\n This is how many augmented came through ", dim(augmentedData[[1]]), " \n")

