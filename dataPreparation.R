# # NOTES Images
# img2 <- 'ib4 and gfp'
# img3 <- 'ib4 only'
# img4 <- 'gfp only'
# img8 <- 'dapi and label'

# for( i in 1:length(rdExperiments)){    
#     rdName <- rdExperiments[i]
#     cat('\n', rdName)
    
#     tmpRd <- get( rdExperiments[i] )

#     cat('\nimage2 dim: ', dim(tmpRd$img2) )
#     cat('\nimage3 dim: ', dim(tmpRd$img3) )   
#     cat('\nimage4 dim: ', dim(tmpRd$img4) )
# }

# # Do not use
# 'RD.190828.40.m1.w1.5bdbd'

# toScore <- c('gfp', 'ib4', 'drop')
# for( i in 1:length(m3Experiments) ){
#     tmpRd <- get(m3Experiments[i])

#     rapply('grep')


#     cat('\n',m3Experiments[i])
#     cat('\n', rev( names(tmpRd$bin) ) )
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
fileLocation <- './rawData/'
files <- list.files(fileLocation)

# load in all files, thank god for all my ram
for(i in files){
    print(i)
    load( paste0(fileLocation,i) )
}

# Only work with experiments from microscope 3
rdExperiments<- ls(pattern = 'RD.')
m3Experiments <- grep('[.]m3[.]', rdExperiments, value=TRUE)
source("./R/imageExtractor.R")
imageExtractor(m3Experiments, 20, 'cy5.bin', 'img3', 1)
imageExtractor(m3Experiments, 20, 'gfp.bin', 'img4', 2)
imageExtractor(m3Experiments, 20, 'drop', 'img8', c(1,2,3))
