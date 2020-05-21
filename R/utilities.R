# function to check if correct cell types are present.
cellTypeChecker <- function(rdExps){
    # Now search through each experiment to see if they have the required cell types
    cellTypes <- c('L1', 'L2', 'L3', 'L4', 'L5', 'L6', 'G7', 'G8', 'G9', 'G10', 'R11', 'R12', 'R13', 'N14', 'N15', 'N16')

    goodRdExps <- c()
    for(i in 1:length(rdExps) ){
        # get the data
        tmpRD <- get(rdExps[i])
        # Some experiments have spelled cell.types as cell_types
        cellTypeName <- grep('^cell[._]types$', names(tmpRD))
        if(length(cellTypeName) > 0){
            # Are the cell types i've list above in this experiment?
            # first ask the logic
            correctCtLogic <- names(tmpRD[[ cellTypeName ]]) %in% cellTypes
            # Does the cell type match the correct length?
            correctCt <- names(tmpRD[[ cellTypeName ]])[correctCtLogic]
            if(length(correctCt) != 0 & length(correctCt) == length(cellTypes)){
                print(paste(rdExps[i], '= good: ', length(correctCt)))
                goodRdExps <- c(goodRdExps, rdExps[i])
            }else{print(paste(rdExps[i], '= bad: ', length(correctCt)))}
        }else{
            print(paste(rdExps[i], '= bad: no cell Types'))
        }
    }
    return(goodRdExps)
}

# function to add cell types to the bin dataframe.
cellTypeBinner <- function(rdExps){
    for( i in 1:length(rdExps)){
        # Get the experiment
        tmpRD <- get(rdExps[i])
        # Some experiments have spelled cell.types as cell_types, get it right
        cellTypeName <- grep('^cell[._]types$', names(tmpRD), value = TRUE)
        
        # These are the cell types we are gonna work with
        cellTypes <- c('L1', 'L2', 'L3', 'L4', 'L5', 'L6', 'G7', 'G8', 'G9', 'G10', 'R11', 'R12', 'R13', 'N14', 'N15', 'N16')

        # Grab only the cell types we are interested in
        rdCellType <- tmpRD[[ cellTypeName ]][ cellTypes ]

        
        # Add cell_types to the bin dataframe
        tmpRD$bin[,'cell_types'] <- NA
        # Itteratively update the binary data frame to contain the new labels.
        for(j in 1:length(rdCellType)){
            tmpRD$bin[rdCellType[[j]], 'cell_types'] <- j
        }

        assign(rdExps[i], tmpRD, envir = .GlobalEnv)
    }
}

# Function to make trace labeled Data
rdExps = goodExps
pulseToFind = 'r3j'
label = 'cell_types'
folderName = './trainingData/cellTypeData/r3j/'
traceLabeledDataMaker <- function(rdExps, pulseToFind, label, folderName){

    traceCollection <- matrix(nrow = 0, ncol = 120)
    labelCollection <- matrix(nrow = 0, ncol = 1)
    if(pulseToFind != "r3j"){
        for( i in 1:length(rdExps) ){
            tryCatch({
                tmpRD <- get( rdExps[i] )

                pulseStart <- min(grep(pulseToFind, tmpRD$w.dat$wr1))
                startTime <- tmpRD$w.dat$Time[pulseStart]
                endTime <- min(tmpRD$w.dat$Time[tmpRD$w.dat$Time >= (startTime + 4)])

                startEnd<- which(tmpRD$w.dat$Time == startTime | tmpRD$w.dat$Time == endTime, arr.ind = T)

                onlyCtLogic <- !is.na(tmpRD$bin[,label])
                cellNames <- tmpRD$c.dat$id[onlyCtLogic]
                rowNames <- paste0(rdExps[i], '_', cellNames)

                traceSelection <- t(tmpRD$blc[startEnd[1]:startEnd[2], cellNames])
                row.names(traceSelection) <- rowNames

                traceCollection <- rbind(traceCollection, traceSelection[,1:120] )

                # Collect Labels
                labelToAdd <- tmpRD$bin[cellNames, label, drop=F]
                row.names(labelToAdd) <- rowNames
                labelCollection <- rbind(labelCollection,  labelToAdd)
            }, error = function(e) print(paste("Cannot add", rdExps[i])))
        }
    }else{
        r3JTraces <- list()
        for( i in 1:length(rdExps) ){
            tryCatch({
                tmpRD <- get( rdExps[i] )
                
                winRegs <- unique(tmpRD$w.dat$wr1)
                r3JLoc <- grep('[rR](3|[I]{3})[jJ]', winRegs)[1]
                kBeforeR3JLoc <- r3JLoc - 1
                kAfterR3JLoc <- r3JLoc + 1
                r3JStart <- min(which(tmpRD$w.dat$wr1 == winRegs[kBeforeR3JLoc] , arr.ind=T))
                r3JEnd <- max(which(tmpRD$w.dat$wr1 == winRegs[kAfterR3JLoc] , arr.ind=T))

                onlyCtLogic <- !is.na(tmpRD$bin[,label])
                cellNames <- tmpRD$c.dat$id[onlyCtLogic]
                rowNames <- paste0(rdExps[i], '_', cellNames)

                traceSelection <- tmpRD$blc[r3JStart:r3JEnd, cellNames]
                colnames(traceSelection) <- rowNames
                r3JTraces[[ rdExps[i] ]] <- traceSelection

                # Collect Labels
                labelToAdd <- tmpRD$bin[cellNames, label, drop=F]
                row.names(labelToAdd) <- rowNames
                labelCollection <- rbind(labelCollection,  labelToAdd)
                #print(paste("Can add", rdExps[i]))
            }, error = function(e) print(paste("Cannot add", rdExps[i])))
        }

        maxSeqLength <- min(Reduce(c, lapply(r3JTraces, function(x) dim(x)[1] )))
        reducedSeq <- list() # list of the points to extract
        for(i in 1:length(rdExps)){
            maxDim <- dim(r3JTraces[[ rdExps[i] ]])[1]
            # make the reduced sequence, basically deleting data points
            reducedSeq <- ceiling(seq(1, maxDim, length.out = maxSeqLength))
            r3JTraces[[ rdExps[i] ]] <- r3JTraces[[ rdExps[i] ]][reducedSeq,]
        }
        traceCollection <- t(Reduce(cbind, r3JTraces))
    }
    invisible(dir.create(folderName, FALSE))
    print(dim(traceCollection))
    print(dim(labelCollection))
    write.csv(traceCollection, file = paste0(folderName, '/traces.csv'))
    write.csv(labelCollection, file = paste0(folderName, '/labels.csv'))
}


