# define the range, label and image
experiments <- rdExps

range = 20 
label = c('gfp.bin', 'cy5.bin')
image = 'img2'
channel = c(1,2,3)
#' Function to gather images of each cell and also collect specified labels of those cells. This outputs a numpy array that can be easily loaded into python. See py/main.py where the numpy array is unpacked into its original dimensions
#' @param experiments is a character vector of experiment names
#' @param range this is the image area to create the image of the cell. From the cells center.x and center.y location the range extends up and and down this results in the image being 2*range
#' @param label this is the collumn name taken from the binary dataframe which defines
#' @param image this is the name of the image taken from the RD.experiment
#' @param channel this is the rgb color channel to collect can be a single numeric value, or a vector of numeric intergers ex. c(1,2,3)
#' @param na.rm logical. If this is TRUE values defined as NA will be removed, if FALSE the values are changed to be zero.
#' @param csvExport boolean, if true then a csv will be output to the current working directory, if false the object will be returned.
imageExtractor <- function(experiments, range = 20, label = 'cy5.bin', image = 'img3', channel = 1, na.rm = T, csvExport = T){
    #slice is the size of the range plus 1
    slice <- (range * 2) + 1
    # First make our main array to fill stuff in wiht
    mainImageArray <- array(dim = c(0, slice, slice, length(channel)))
    mainLabelArray <-  matrix(nrow=0, ncol=1)
    for( i in 1:length(experiments) ){
        tmpRD <- get(experiments[i], globalenv())
        
        # Define the NA's as 0's this is a Bold move
        if(na.rm){
            tmpRD$bin <- tmpRD$bin[ ,label, drop = FALSE]
            tmpRD$bin <- na.omit(tmpRD$bin)

            tmpRD$c.dat <- tmpRD$c.dat[row.names(tmpRD$bin),]
        }else{
            naLogic <- is.na(tmpRD$bin[,label])
            tmpRD$bin[naLogic, label] <- 0
        }
        
        # Gather the center x and y data
        cellXValue <- tmpRD$c.dat$center.x.simplified
        cellYValue <- tmpRD$c.dat$center.y.simplified

        # This checks to see if we will be able to observe the cell
        canView <- 
            0 < (cellXValue - range) & 
            (cellXValue + range) < dim(tmpRD[[ image ]] )[2] &
            0 < (cellYValue - range) &
            (cellYValue + range) < dim(tmpRD[[ image ]])[1] 
        
        # Collect the labels
        subLabelArray <- tmpRD$bin[canView, label, drop = FALSE]
        subLabelArray <- apply(subLabelArray, 1, paste, collapse='')
        newLevels <- sort(unique(subLabelArray))
        print(newLevels)
        subLabelArray <- factor(subLabelArray, levels = newLevels)
        subLabelArray <- as.data.frame(subLabelArray)

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
                dim(tmpRD$c.dat[canView, ])[1], 
                slice, 
                slice, 
                length(channel)
            ) 
        )
        for( j in 1:dim(tmpRD$c.dat[canView, ])[1] ){
            xLeft <- cellXValue[canView][j] - range
            xRight <- cellXValue[canView][j] + range

            yTop <- cellYValue[canView][j] - range
            yBottom <- cellYValue[canView][j] + range

            subImageArray[j, , , ] <- tmpRD[[ image ]][yTop:yBottom,xLeft:xRight, channel]
        }
        mainImageArray <- abind::abind(mainImageArray, subImageArray, along = 1)
    }

    mainImageArrayReshape <- mainImageArray
    dims <- dim(mainImageArray)
    dim(mainImageArrayReshape) <- c(prod(dims[1:4]))

    if(csvExport){
        numpyName <- paste0(
            dim(mainImageArray)[1],
            '_',
            slice,
            '_',
            slice,
            '_',
            length(channel),
            '_',
            paste0(label, collapse = '_'),
            '.npy'
        )

        csvName <- paste0(
            paste0(label, collapse = '_'),
            '_',
            dim(mainImageArray)[1],
            '_',
            'label',
            '.csv'
        )

        newDir <- paste0('./trainingData/',paste0(label, collapse = '_'),'_', image,'_', paste(channel, collapse='')) 
        invisible(dir.create(newDir, FALSE))

        invisible(RcppCNPy::npySave(
            paste0(newDir,'/', numpyName), 
            mainImageArrayReshape
        ))
        write.csv(
            mainLabelArray, 
            file = paste0(newDir,'/', csvName)
        )
    }else{
        return(mainImageArrayReshape)

    }
    cat("\nCompleted making the numpy array and labels")
    cat("\nTotal examples: ", dim(mainImageArray)[1],'\n')
}
