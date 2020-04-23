# define the range, label and image
range <- 20
label <- 'cy5.bin'
image <- 'img3'
channel <- c(1,2,3)
imageExtractor <- function(experiments, range = 20, label = 'cy5.bin', image = 'img3', channel = 1){
    #slice is the size of the range plus 1
    slice <- (range * 2) + 1
    # First make our main array to fill stuff in wiht
    mainImageArray <- array(dim = c(0, slice, slice, length(channel)))
    mainLabelArray <- labels <- matrix(nrow=0, ncol=1)
    for( i in 1:length(experiments) ){
        tmpRd <- get(experiments[i])
        
        print(names(tmpRd$bin))
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
            gsub('[.]', '_', experiments[i]), 
            sub('[.]', '_', row.names(subLabelArray)),
            sep='__'
        )
        row.names(subLabelArray) <- newRowNames
        mainLabelArray <- rbind(mainLabelArray, subLabelArray)

        # Now we will collect all the images and add it to the array
        subImageArray <- array(
            dim = c(
                length(tmpRd$bin[ , label][canView]), 
                slice, 
                slice, 
                length(channel)
            ) 
        )
        for( j in 1:length(tmpRd$bin[ , label][canView]) ){
            xLeft <- cellXValue[canView][j] - range
            xRight <- cellXValue[canView][j] + range

            yTop <- cellYValue[canView][j] - range
            yBottom <- cellYValue[canView][j] + range

            subImageArray[j, , , ] <- tmpRd[[ image ]][yTop:yBottom,xLeft:xRight, channel]
        }
        mainImageArray <- abind::abind(mainImageArray, subImageArray, along = 1)
    }

    mainImageArrayReshape <- mainImageArray
    dims <- dim(mainImageArray)
    dim(mainImageArrayReshape) <- c(prod(dims[1:4]))

    numpyName <- paste0(
        dim(mainImageArray)[1],
        '_',
        slice,
        '_',
        slice,
        '_',
        length(channel),
        '_',
        label,
        '.npy'
    )

    csvName <- paste0(
        label,
        '_',
        dim(mainImageArray)[1],
        '.csv'
    )

    RcppCNPy::npySave(
        paste0('./expData/', numpyName), 
        mainImageArrayReshape
    )
    write.csv(
        mainLabelArray, 
        file = paste0('./expData/', csvName)
    )
}
