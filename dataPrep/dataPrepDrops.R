###############################################################
###############################################################
###############################################################
# find all files
fileLocation <- './rawData/dropData/'
# Only load from microscope 3
files <- list.files(fileLocation)

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
    if(length(cellTypeName) != 0){
        # Are the cell types i've list above in this experiment?
        # first ask the logic
        correctCtLogic <- names(tmpRD[[ cellTypeName ]]) %in% cellTypes
        # Does the cell type match the correct length?
        correctCt <- names(tmpRD[[ cellTypeName ]])[correctCtLogic]
        if(length(correctCt) != 0 & length(correctCt) == length(cellTypes)){
            print(paste('good: ', length(correctCt)))
            goodRdExps <- c(goodRdExps, rdExps[i])
        }else{print(paste('bad: ', length(correctCt)))}
    }else{
        print("No cell types")
    }
}

rdExps <- goodRdExps
#rdExps <- setdiff(goodRdExps, "RD.190612.38.f.M3.W1.ConROI_Y5yM13L")

# Looking at all images
for( i in 1:length(rdExps)){    
    rdName <- rdExps[i]
    cat('\n', rdName)
    
    tmpRD <- get( rdExps[i] )

    cat('\nimage2 dim: ', dim(tmpRD$img2) )
    cat('\nimage3 dim: ', dim(tmpRD$img3) )   
    cat('\nimage4 dim: ', dim(tmpRD$img4) )
    cat('\nimage8 dim: ', dim(tmpRD$img8) )
}

# Now wee need to add these cells types to the binary collumn.
# We will make our lives easier and just call it cell_types

for( i in 1:length(rdExps)){
    i=4
    # Get the experiment
    tmpRD <- get(rdExps[i])
    # Some experiments have spelled cell.types as cell_types, get it right
    cellTypeName <- grep('^cell[._]types$', names(tmpRD), value = TRUE)
    
    if(length(cellTypeName) >=0){
        # Grab only the cell types we are interested in
        rdCellType <- tmpRD[[ cellTypeName ]]
        neurons <- rdCellType$neurons 
        notNeurons <- setdiff(tmpRD$c.dat$id, neurons)

        tmpRD$bin[notNeurons,'drop']<- NA
        tmpRD$bin$drop
        
        # Add cell_types to the bin dataframe
        tmpRD$bin[,'cell_types'] <- NA
        # Itteratively update the binary data frame to contain the new labels.
        for(j in 1:length(rdCellType)){
            tmpRD$bin[rdCellType[[j]], 'cell_types'] <- j
        }
        assign(rdExps[i], tmpRD)
    }else{
         
    }
}

source("./R/imageExtractor.R")
# imageExtractor(rdExps, 20, 'drop', 'img8', c(1,2,3) )

# # Image 1 is a total bright feild overlay
# imageExtractor(rdExps, 20, 'cell_types', 'img1', c(1,2,3) )
# # Image 2 is the gfp cy5 and sometime dapi
# imageExtractor(rdExps, 20, 'cell_types', 'img2', c(1,2,3) )
# # Image 3 is ib4 only/red label
imageExtractor(rdExps, 20, 'cy5.bin', 'img3', c(1) )
# Image 5 is the gfp only/green label
imageExtractor(rdExps, 20, 'gfp.bin', 'img4', c(2) )
# #Image 8 is the roi and the dapi
# imageExtractor(rdExps, 20, 'drop', 'img8', c(1, 2, 3) )

# Traces are more complicated so just use this script
source("./R/traceExtractor.R")

