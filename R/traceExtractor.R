###############################################################
###############################################################
###############################################################
# find all files
fileLocation <- '../rawData/multiClassData/'
# Only load from microscope 3
files <- list.files(fileLocation, '[mM]3')

# load in all files, thank god for all my ram
if(length(ls(pattern = 'RD.')) < 1){
    for(i in files){
        print(i)
        load( paste0(fileLocation,i) )
    }
}

# Find all experiments in the workspace
rdExps <- ls(pattern ='RD[.]')

# Now search through each experiment to see if they have the required cell types
cellTypes <- c('L1', 'L2', 'L3', 'L4', 'L5', 'L6', 'G7', 'G8', 'G9', 'G10', 'R11', 'R12', 'R13', 'N14', 'N15', 'N16')

goodRdExps <- c()
for(i in 1:length(rdExps) ){
    # get the data
    tmpRD <- get(rdExps[i])
    # Some experiments have spelled cell.types as cell_types
    cellTypeName <- grep('^cell[._]types$', names(tmpRD))
    
    # Are the cell types i've list above in this experiment?
    # first ask the logic
    correctCtLogic <- names(tmpRD[[ cellTypeName ]]) %in% cellTypes
    # Does the cell type match the correct length?
    correctCt <- names(tmpRD[[ cellTypeName ]])[correctCtLogic]
    if(length(correctCt) != 0 & length(correctCt) == length(cellTypes)){
        print(paste('good: ', length(correctCt)))
        goodRdExps <- c(goodRdExps, rdExps[i])
    }else{print(paste('bad: ', length(correctCt)))}
}

rdExps <- setdiff(goodRdExps, "RD.190612.38.f.M3.W1.ConROI_Y5yM13L")

# # Need to search through for the right window regions
# # This is a test to make sure everything is in the right places
# 
# for(i in 1:length(rdExps) ){
#     # get the data
#     tmpRD <- get(rdExps[i])
#     winRegs <- unique(tmpRD$w.dat$wr1)
#     logic <- sapply(regions, function(x) grep(x, winRegs))

#     cat(rdExps[i],'\n')
#     print(logic)  
# }

# Making traces for LSTM inputs
# We will start by composing lists to work with
label <- 'cell_types'
na.rm <- T
range <- 20
regions <- c('*[rR].*[jJ].*', '*[aA][iI][tT][cC].*', '*[mM][eE][nN][tT].*', '*[cC][aA][pP].*', '*[kK].*40.*')

r3JTraces <- list()
amckTraces <- list()
mainLabelArray <-  matrix(nrow=0, ncol=1)
for(i in 1:length(rdExps) ){
    # Get data
    tmpRD <- get(rdExps[i])

    # Define the NA's as 0's this is a Bold move
    if(na.rm){
        naLogic <- is.na(tmpRD$bin[,label])
        tmpRD$bin <- tmpRD$bin[!naLogic, ]
        tmpRD$c.dat <- tmpRD$c.dat[!naLogic, ]
    }else{
        tmpRD$bin[naLogic, label] <- 0
    }

    # This checks to see if we will be able to observe the cell like in the image prepartation
    # Gather the center x and y data
    cellXValue <- tmpRD$c.dat$center.x.simplified
    cellYValue <- tmpRD$c.dat$center.y.simplified

    canView <- 
        0 < (cellXValue - range) & 
        (cellXValue + range) < dim(tmpRD$img1)[2] &
        0 < (cellYValue - range) &
        (cellYValue + range) < dim(tmpRD$img1)[1] 
    
    cells <- row.names(tmpRD$c.dat[canView, ])

    # Collect the labels
    subLabelArray <- tmpRD$bin[canView, label, drop = FALSE]
    newRowNames <- paste(
        gsub('[.]', '_', rdExps[i]), 
        sub('[.]', '_', row.names(subLabelArray)),
        sep='__'
    )
    row.names(subLabelArray) <- newRowNames
    mainLabelArray <- rbind(mainLabelArray, subLabelArray)

    # Start on r3J
    # Define window regions
    winRegs <- unique(tmpRD$w.dat$wr1)
    r3JLoc <- grep(regions[1], winRegs)
    kBeforeR3JLoc <- r3JLoc - 1
    kAfterR3JLoc <- r3JLoc + 1
    r3JStart <- min(which(tmpRD$w.dat$wr1 == winRegs[kBeforeR3JLoc] , arr.ind=T))
    r3JEnd <- max(which(tmpRD$w.dat$wr1 == winRegs[kAfterR3JLoc] , arr.ind=T))

    dataToCollect <- tmpRD$blc[r3JStart:r3JEnd, cells]
    
    # Make some nice new column names mirroring the experiment and cell.
    newColNames <- paste(
        gsub('[.]', '_', rdExps[i]), 
        sub('[.]', '_', colnames(dataToCollect)),
        sep='__'
    )
    colnames(dataToCollect) <- newColNames
    
    r3JTraces[[ rdExps[i] ]] <- dataToCollect

    # AMCK now
    aitcLoc <- grep(regions[2], winRegs)
    kLoc <- grep(regions[5], winRegs)

    aitcStart <- min(which(tmpRD$w.dat$wr1 == winRegs[aitcLoc] , arr.ind=T))
    kEnd <- max(which(tmpRD$w.dat$wr1 == winRegs[kLoc] , arr.ind=T))
    dataToCollect <- tmpRD$blc[aitcStart:kEnd, cells]
    colnames(dataToCollect) <- newColNames
    
    amckTraces[[ rdExps[i] ]] <- dataToCollect
}

## R3J lstm data prep
# Look for the shortest sequence, this will be the maximum of allowed points 
maxSeqLength <- min(Reduce(c, lapply(r3JTraces, function(x) dim(x)[1] )))
reducedSeq <- list() # list of the points to extract
for(i in 1:length(rdExps)){
    maxDim <- dim(r3JTraces[[ rdExps[i] ]])[1]
    # make the reduced sequence, basically deleting data points
    reducedSeq <- ceiling(seq(1, maxDim, length.out = maxSeqLength))
    r3JTraces[[ rdExps[i] ]] <- r3JTraces[[ rdExps[i] ]][reducedSeq,]
    print(dim(r3JTraces[[ i ]])[1])
}

newDir <- paste0('./trainingData/',label,'_', 'R3J') 
invisible(dir.create(newDir, FALSE))

bigDataToSave <- t(Reduce(cbind, r3JTraces))

# Names for the files
csvTraceName <- paste0(
    'R3J',
    '_',
    dim(bigDataToSave)[1],
    '.csv'
)

csvLabelName <- paste0(
    'R3J',
    '_',
    dim(bigDataToSave)[1],
    '_',
    'label',
    '.csv'
)

# Save the files
write.csv(
    bigDataToSave,
    file = paste0(newDir,'/',csvTraceName)
)

write.csv(
    mainLabelArray, 
    file = paste0(newDir,'/', csvLabelName)
)


## AMCK lstm data prep
# Look for the shortest sequence, this will be the maximum of allowed points 
maxSeqLength <- min(Reduce(c, lapply(amckTraces, function(x) dim(x)[1] )))
reducedSeq <- list() # list of the points to extract
for(i in 1:length(rdExps)){
    maxDim <- dim(r3JTraces[[ rdExps[i] ]])[1]
    # make the reduced sequence, basically deleting data points
    reducedSeq <- ceiling(seq(1, maxDim, length.out = maxSeqLength))
    amckTraces[[ rdExps[i] ]] <- amckTraces[[ rdExps[i] ]][reducedSeq,]
    print(dim(amckTraces[[ i ]])[1])
}

newDir <- paste0('./trainingData/',label,'_', 'AMCK') 
invisible(dir.create(newDir, FALSE))

bigDataToSave <- t(Reduce(cbind, amckTraces))
print(dim(bigDataToSave))
csvName <- paste0(
    'AMCK',
    '_',
    dim(bigDataToSave)[1],
    '.csv'
)

csvLabelName <- paste0(
    'AMCk',
    '_',
    dim(bigDataToSave)[1],
    '_',
    'label',
    '.csv'
)
write.csv(
    bigDataToSave,
    file = paste0(newDir,'/', csvName)
)

write.csv(
    mainLabelArray, 
    file = paste0(newDir,'/', csvLabelName)
)




# # What would deleting the points do? Would things line up well? From the looks of it YES!
# plot(
#     seq(1,length(r3JTraces[[3]][,1])),
#     r3JTraces[[3]][,1], 
#     ylim = c( min(r3JTraces[[1]]), max(r3JTraces[[1]]) ),
#     type='l'
# )

# cols <- rgb(0,0,0,.1)
# lapply(r3JTraces[[4]],
#     function(x) lines(seq(1,length(x) ), x, col = cols)
# )

# newCoors <- ceiling(seq(1, 374, length.out = 225))
# cols <- rgb(1,0,0,.1)
# lapply(r3JTraces[[1]],
#     function(x) lines(seq(1,length(x[newCoors]) ), x[newCoors], col = cols)
# )








# {
#     # AMCK now
#     aitcLoc <- grep(regions[2], winRegs)
#     kLoc <- grep(regions[5], winRegs)

#     aitcStart <- min(which(tmpRD$w.dat$wr1 == winRegs[aitcLoc] , arr.ind=T))
#     kEnd <- max(which(tmpRD$w.dat$wr1 == winRegs[kLoc] , arr.ind=T))

#     amckTrace <- tmpRD$blc[aitcStart:kEnd, cells]

#     megaTrace <- rbind(r3JTrace, amckTrace) 

#     cat(rdExps[i], "\n")
#     print(dim(megaTrace))
# }



