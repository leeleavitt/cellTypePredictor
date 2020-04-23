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

# Looking at all images
for( i in 1:length(rdExps)){    
    rdName <- rdExps[i]
    cat('\n', rdName)
    
    tmpRd <- get( rdExps[i] )

    cat('\nimage2 dim: ', dim(tmpRd$img2) )
    cat('\nimage3 dim: ', dim(tmpRd$img3) )   
    cat('\nimage4 dim: ', dim(tmpRd$img4) )
    cat('\nimage8 dim: ', dim(tmpRd$img8) )
}


# Now wee need to add these cells types to the binary collumn.
# We will make our lives easier and just call it cell_types
rdExps <- goodRdExps

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

source("./R/imageExtractor.R")
imageExtractor(rdExps, 20, 'cell_types', 'img2', c(1,2,3) )

