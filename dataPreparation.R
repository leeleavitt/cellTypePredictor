# # NOTES Images
# img2 <- 'ib4 and gfp'
# img3 <- 'ib4 only'
# img4 <- 'gfp only'
# img8 <- 'dapi and label'

## Looking at all images
# for( i in 1:length(rdExperiments)){    
#     rdName <- rdExperiments[i]
#     cat('\n', rdName)
    
#     tmpRd <- get( rdExperiments[i] )

#     cat('\nimage2 dim: ', dim(tmpRd$img2) )
#     cat('\nimage3 dim: ', dim(tmpRd$img3) )   
#     cat('\nimage4 dim: ', dim(tmpRd$img4) )
# }


# This is where I live
mainDir <- getwd()

# Go to the raw data
setwd('./rawData/')

# find all files
files <- list.files()

# load in all files, thank god for all my ram
for(i in files){
    print(i)
    load(i)
}

require(procPharm)
rdExperiments<- ls(pattern = 'RD.')

# Only work with experiments from microscope 3
m3Experiments <- grep('[.]m3[.]', rdExperiments, value=TRUE)


# Time to start constructing the images to feed into the neural networks
# i need to save the data as numpy arrays to load into python for the main fun
install.packages("RcppCNPy")

# lets start with defining the ib4

# define the range, label and image
range <- 20
label <- 'drop'
image <- 'img8'
#channel <- 1

#slice is the size of the range plus 1
slice <- (range * 2) + 1
# First make our main array to fill stuff in wiht
mainImageArray <- array(dim = c(0, slice, slice, 3))
dims <- dim(mainImageArray)
mainLabelArray <- labels <- matrix(nrow=0, ncol=1)
for( i in 1:length(m3Experiments) ){
    tmpRd <- get(m3Experiments[i])
    
    # Define the NA's as 0's this is a Bold move
    naLogic <- is.na(tmpRd$bin[,label])
    tmpRd$bin[naLogic, label] <- 0
    
    # Gather the center x and y data
    cellXValue <- tmpRd$c.dat$center.x.simplified
    cellYValue <- tmpRd$c.dat$center.y.simplified

    # This checks to see if we will be able to observe the cell
    canView <- 
        0 < (cellXValue - range) & 
        (cellXValue + range) < dim(tmpRd[[ image ]] )[2] &
        0 < (cellYValue - range) &
        (cellYValue + range) < dim(tmpRd[[ image ]])[1] 
    
    # Collect the labels
    subLabelArray <- tmpRd$bin[canView, label, drop = FALSE]
    newRowNames <- paste(
        gsub('[.]', '_', m3Experiments[i]), 
        sub('[.]', '_', row.names(subLabelArray)),
        sep='__'
    )
    row.names(subLabelArray) <- newRowNames
    mainLabelArray <- rbind(mainLabelArray, subLabelArray)

    # Now we will collect all the images and add it to the array
    subImageArray <- array(dim = c(length(tmpRd$bin[ , label][canView]), slice, slice, 3) )
    for( j in 1:length(tmpRd$bin[ , label][canView]) ){
        xLeft <- cellXValue[canView][j] - range
        xRight <- cellXValue[canView][j] + range

        yTop <- cellYValue[canView][j] - range
        yBottom <- cellYValue[canView][j] + range

        subImageArray[j, , , ] <- tmpRd[[ image ]][yTop:yBottom,xLeft:xRight,]
    }
    mainImageArray <- abind::abind(mainImageArray, subImageArray, along = 1)

    # We also need to track the labels
}

mainImageArrayReshape <- mainImageArray
dims <- dim(mainImageArray)
dim(mainImageArrayReshape) <- c(prod(dims[1:4]))

dim(mainImageArray)
dim(mainLabelArray)

# Little function to view the cell
plot(0,0, xlim = c(0, 10), ylim = c(10,0))
rasterImage(
    mainImageArray[20, , ,],
    0, 10, 10, 0,
)
RcppCNPy::npySave("randmat.npy", mainImageArrayReshape)

setwd('./')
source("./R/imageExtractor.R")

# # NOTES Images
# img2 <- 'ib4 and gfp'
# img3 <- 'ib4 only'
# img4 <- 'gfp only'
# img8 <- 'dapi and label'

# # Do not use
# 'RD.190828.40.m1.w1.5bdbd'
# Go to the raw data

###############################################################
###############################################################
###############################################################
# find all files
fileLocation <- './rawData/multiClassData/'
# Only load from microscope 3
files <- list.files(fileLocation, '[mM]3')

# load in all files, thank god for all my ram
for(i in files){
    print(i)
    load( paste0(fileLocation,i) )
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
    cellTypeName <- grep('^cell[._]types$', names(tmpRD))
    
    # Grab only the cell types we are interested in
    rdCellType <- tmpRD[[ cellTypeName ]][ cellTypes ]
    
    # Add cell_types to the bin dataframe
    tmpRD$bin['cell_types'] <- 'NA'
    # Itteratively update the binary data frame to contain the new labels.
    for(i in 1:length(rdCellType)){
        tmpRD$bin[rdCellType[[i]], ] <- i
    }
    assign(rdExps[i], tmpRD)
}

tmpRD <- get(rdExps[i])
names(tmpRD$bin)
source("./R/imageExtractor.R")
imageExtractor(rdExps, 20, 'cell_types', 'img2', c(1,2,3) )


# Only work with experiments from microscope 3
rdExperiments<- ls(pattern = 'RD.')
m3Experiments <- grep('[.]m3[.]', rdExperiments, value=TRUE)
source("./R/imageExtractor.R")
imageExtractor(m3Experiments, 20, 'cy5.bin', 'img3', 1)
imageExtractor(m3Experiments, 20, 'gfp.bin', 'img4', 2)
imageExtractor(m3Experiments, 20, 'drop', 'img8', c(1,2,3))
