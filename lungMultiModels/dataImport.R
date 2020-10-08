#####################################################################
# Snooping
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


#######################################################################################
# Real data export
# This means that 4 min * (60 sec / 1 min) * (1 frame / 2 sec)
getwd()
mainDir <- "G:/My Drive/cellTypePredictor/lungMultiModels/"
setwd(mainDir)

expLocs <- list.files(pattern = "RD[.].*Rdata$", recursive = T)

# With these exerpiments luke scored we have to create a function that 
# Collects all cells and all responses.
#' Function takes an RD.expeirment, specifc windows, and returns 
#' traces, that have been imputed for equal points per minute
#' also returns the labels of the imputed data(name)
responseImputer <- function(dat, levs = NA, windowSize = 4, datName = NA){
    
    if(is.na(datName)){
        rdName <- deparse(substitute(dat))
    }else{
        rdName <- datName
    }

    if(is.na(levs)){
        levs <- setdiff(unique(tmpRD$w.dat$wr1), "")
    }

    timeSteps <-  windowSize * (60/1) * (1/2)

    responseMat <- as.data.frame(matrix(
        nrow = length(dat$c.dat$id) * length(levs),
        ncol = timeSteps
    ))

    labelMat <- as.data.frame(matrix(
        nrow = length(dat$c.dat$id) * length(levs),
        ncol = 1
    ))

    count <- 1
    for(i in 1:length(dat$c.dat$id)){
        for(j in 1:length(levs)){
            cell <- dat$c.dat$id[i]
            pulse <- levs[j]
            
            winStart <- min( dat$w.dat$Time[dat$w.dat$wr1 == pulse] )
            winEnd <- winStart + windowSize

            pulseLogic <-  tmpRD$w.dat$Time > winStart & 
                                tmpRD$w.dat$Time < winEnd

            cellPulse <- dat[['blc']][pulseLogic, cell]
            pulseTime <- dat[['blc']][pulseLogic, "Time"]
            
            xspan <- 5/length(cellPulse)

            targ <- data.frame(
                pulseTime = seq(
                    winStart, 
                    winEnd,  
                    length.out = timeSteps)
            )

            xloe <- loess(
                cellPulse ~ pulseTime, 
                span = xspan,
                control=loess.control(surface="direct")
            )

            xp <- predict(xloe, newdata = targ)
            responseMat[count,] <- xp

            newRowNames <- paste0(c(rdName, pulse, cell), collapse = "_")

            row.names(responseMat)[count] <- newRowNames
            
            labelMat[count,] <- dat$bin[cell,pulse]
            row.names(labelMat)[count] <- newRowNames

            count <- count + 1
        }
    }

    return(list(features = responseMat, labels = labelMat))
}


totalFeatureData <- data.frame()
totalLabelData <- data.frame()
for(i in 1:length(expLocs)){
    print(i)
    print(expLocs[i])
    tmpRD <- get(load(expLocs[i]))
    imputedData <- responseImputer(tmpRD, datName = sub(".Rdata",'',expLocs[i]))
    
    print(summary(as.factor(imputedData[[2]][,1]))  )

    totalFeatureData <- rbind(totalFeatureData, imputedData[['features']])
    totalLabelData <- rbind(totalLabelData, imputedData[['labels']])
}

dim(totalFeatureData)
dim(totalLabelData)

summary(as.factor(totalLabelData[,1]))  

write.csv(totalFeatureData, 'features.csv')
write.csv(totalLabelData, "labels.csv")



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

responsePolyModelAugmentor<- function(tmpRD, levs = NA, windowSize = 4, label = 1, augSamps = 100, degree = 20, sdFactor = 1.8, coefs = 2:8, wins = 7, parallel = T, plotit = F, saveIt = T){

    numCores <- parallel::detectCores()
    doParallel::registerDoParallel(numCores)
    require(doParallel)

    if(is.na(levs)){
        levs <- setdiff(unique(tmpRD$w.dat$wr1),c("", "epad"))
    }
    windowStarts <- tapply(tmpRD$w.dat$Time, as.factor(tmpRD$w.dat$wr1), min)
    windowStarts <- windowStarts[levs]

    #augmentedFeatures <- foreach(m = 1:length(levs), .combine = rbind, .packages="foreach") %do% {
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

            for(i in 1:length(cells)){
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
                #colnames(newFeatures) <- seq(1, timeSteps)
                #newFeatures <- data.frame()
                #newFeatures1 <- foreach(i = 1:length(cells), .combine = rbind, .packages="foreach") %dopar% { 
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
                augmentedFeatures <- data.frame()
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
                        newFeatures <- rbind(newFeatures, polyModelPred)
                        colnames(newFeatures) <- seq(1, timeSteps)
                        newFeatures
                    }
                    augmentedFeaturesToAdd
                }
                augmentedFeatures <- rbind(augmentedFeatures, augmentedFeaturesToAdd)
            }
            augmentedLabels <- rep(label, dim(augmentedFeatures)[1])
            return(list(augmentedFeatures =augmentedFeatures, augmentedLabels = augmentedLabels))
        }
    }
}

levs <- setdiff(unique(tmpRD$w.dat$wr1),c("", "epad"))
sum(colSums(tmpRD$bin[levs]) * 10)
augmentedData <- responsePolyModelAugmentor(tmpRD, sdFactor = 2, coefs = 2:4, augSamps = 10, plotit = T )

dim(augmentedData[[1]])


sum(colSums(tmpRD$bin[1:7]))* 100
#To Augment this data, we ill now 
# 1 calcuate the distribution of coefficient values across all 1 responses
# 2 For each cell perform an initial fit,
# 3 randomize the first 5 coefficients 100 times and collect these as augmented data






modelNames <- paste0("polyModel", seq(1,20,1))
    randomPredictions <- list()
    for(i in 1:length(modelNames)){
        polyModelSamp <- polyModel
        coefSamp <- sample(c(2:5))[1:2]
        polyModelSamp$coefficients[coef] <- jitter(polyModelSamp$coefficients[coef], amount = randAmount)
        randomPredictions[[ modelNames[i] ]] <- predict(polyModelSamp, targ)
        assign(modelNames[i], polyModelSamp)
    }

