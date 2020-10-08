# Now I wanna augment some of the k10 pulses and k20 pulses
# X.244 k10
i = "K.10mM"
windowLogic <-  tmpRD$w.dat$Time > winStart[i] & 
                tmpRD$w.dat$Time < winEnd[i]


pulseToScore <- t(tmpRD[[tType]][windowLogic,"X.244"])

# First method Add slight amount of jitter
pulseToScore2 <- jitter( t(tmpRD[[tType]][windowLogic,"X.244"]),amount = .002)

# Second method move pulse bak 1 or two points
pulseToScore3 <- c(pulseToScore[1], pulseToScore[-length(pulseToScore)])
pulseToScore4 <- c(pulseToScore3[1], pulseToScore3[-length(pulseToScore3)])
pulseToScore5 <- c(pulseToScore4[1], pulseToScore4[-length(pulseToScore4)])

# third

str(pulseToScore)
plot(tmpRD[[tType]][windowLogic,"Time"], pulseToScore,  type = 'l', lwd = 3)
lines(tmpRD[[tType]][windowLogic,"Time"], pulseToScore2,  type = 'l', col = 'blue', lwd =2)
lines(tmpRD[[tType]][windowLogic,"Time"], pulseToScore3,  type = 'l', col = 'red', lwd = 2)
lines(tmpRD[[tType]][windowLogic,"Time"], pulseToScore4,  type = 'l', col = 'red', lwd = 2)
lines(tmpRD[[tType]][windowLogic,"Time"], pulseToScore5,  type = 'l', col = 'red', lwd = 2)

# With so little K 10 we will add a lot of jittered responses to the data

oneDAugment <- function(dat, response){
    k10Logic <- dat$bin[response] == 1
    cellsToAugment <- dat$c.dat$id[k10Logic]

    windowLogic <-  dat$w.dat$Time > winStart[response] & 
                    dat$w.dat$Time < winEnd[response]

    pulsesToScore <- t(dat[[tType]][windowLogic, cellsToAugment])

    jitAmt <- c(
        0.002, 0.002, 0.002, 0.002,
        0.0025, 0.0025, 0.0025, 0.0025, 
        0.003, 0.003, 0.003, 0.003, 
        0.0035, 0.0035, 0.0035, 0.0035,
        0.004, 0.004, 0.004, 0.004,
        0.0045, 0.0045, 0.0045, 0.0045
    )

    augmentedData <- matrix(nrow = 0, ncol = dim(pulsesToScore)[2])
    augmentedDataLabels <- data.frame()

    for(i in 1:length(jitAmt)){
        augment1 <- apply(pulsesToScore, 2, jitter, amount  = jitAmt[i])
        newRowName <- paste0(rdName, "_", cellsToAugment, "_", names(winStart[response]),"_", paste0('augment',i))
        row.names(augment1) <- newRowName

        augmentedData <-rbind(augmentedData, augment1)

        labelToAdd <- dat$bin[cellsToAugment, response, drop = FALSE]
        row.names(labelToAdd) <- newRowName
        colnames(labelToAdd) <- 'labels'
        augmentedDataLabels <- rbind(augmentedDataLabels, labelToAdd)
    }
    return(list(features = augmentedData, labels = augmentedDataLabels))
}

augDat <- oneDAugment(dat, "K.10mM")



