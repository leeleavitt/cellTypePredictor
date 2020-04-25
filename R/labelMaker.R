###############################################################
###############################################################
###############################################################
# find all files
fileLocation <- './rawData/multiClassData/'
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

# Now wee need to add these cells types to the binary collumn.
# We will make our lives easier and just call it cell_types
for( i in 1:length(rdExps)){
    # Get the experiment
    tmpRD <- get(rdExps[i])
    # Some experiments have spelled cell.types as cell_types, get it right
    cellTypeName <- grep('^cell[._]types$', names(tmpRD), value = TRUE)
    
    # Grab only the cell types we are interested in
    rdCellType <- tmpRD[[ cellTypeName ]][ cellTypes ]
    
    # Add cell_types to the bin dataframe
    tmpRD$bin[,'cell_types'] <- NA
    # Itteratively update the binary data frame to contain the new labels.
    for(j in 1:length(rdCellType)){
        tmpRD$bin[rdCellType[[j]], 'cell_types'] <- j
    }
    assign(rdExps[i], tmpRD)
}

# Make the labels. This need to be concactible
label <- c('gfp.bin', 'cy5.bin')
range = 20
mainLabelArray <-  matrix(nrow=0, ncol=1)
na.rm = TRUE
for(i in 1:length(rdExps) ){
    # Get data
    tmpRD <- get(rdExps[i])

    # Define the NA's as 0's this is a Bold move
    if(na.rm){
        naLogic <- is.na(tmpRD$bin[,'cell_types'])
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
    subLabelArray <- tmpRD$bin[canView, label, drop = FALSE ]
    subLabelArray <- apply(subLabelArray, 1, paste, collapse='')
    newLevels <- sort(unique(subLabelArray))
    print(newLevels)
    subLabelArray <- as.integer(factor(subLabelArray, levels = newLevels))
    subLabelArray <- as.data.frame(subLabelArray)

    # Make new row names
    newRowNames <- paste(
        gsub('[.]', '_', rdExps[i]), 
        sub('[.]', '_', row.names(subLabelArray)),
        sep='__'
    )
    row.names(subLabelArray) <- newRowNames
    colnames(subLabelArray) <- paste0(label, collapse = '_')
    mainLabelArray <- rbind(mainLabelArray, subLabelArray)
}

csvLabelName <- paste0(
    './trainingData/',
    paste0(label, collapse = '_'),
    '_',
    'label',
    '.csv'
)

write.csv(mainLabelArray, file = csvLabelName)