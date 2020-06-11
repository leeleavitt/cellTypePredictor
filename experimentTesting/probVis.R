pulseToFind = '[aA][iI][tT][cC]'
modelLoc = './modelMakers/marioModels/models/aitc.h5'

# Function to model the trace input
multiTraceModeler <- function(tmpRD, pulseToFind, modelLoc){
    require(reticulate)
    mainpy <- import_from_path('main', './py')

    tryCatch({
        # Collect the trace to send into the model
        if(pulseToFind != 'r3j'){
            pulseStart <- min(grep(pulseToFind, tmpRD$w.dat$wr1))
            startTime <- tmpRD$w.dat$Time[pulseStart]
            endTime <- min(tmpRD$w.dat$Time[tmpRD$w.dat$Time >= (startTime + 4)])
            startEnd<- which(tmpRD$w.dat$Time == startTime | tmpRD$w.dat$Time == endTime, arr.ind = T)
            traceSelection <- t(tmpRD$blc[startEnd[1]:startEnd[2], -1])
        }else{
            winRegs <- unique(tmpRD$w.dat$wr1)
            r3JLoc <- grep('[rR](3|[I]{3})[jJ]', winRegs)[1]
            kBeforeR3JLoc <- r3JLoc - 1
            kAfterR3JLoc <- r3JLoc + 1
            r3JStart <- min(which(tmpRD$w.dat$wr1 == winRegs[kBeforeR3JLoc] , arr.ind=T))
            r3JEnd <- max(which(tmpRD$w.dat$wr1 == winRegs[kAfterR3JLoc] , arr.ind=T))
            traceSelection <- t(tmpRD$blc[r3JStart:r3JEnd, -1])
        }
        
        ## Put into the model
        model <- keras::load_model_hdf5(modelLoc)
        pred <- as.matrix(traceSelection)
        pred <- mainpy$featureMaker(pred, as.integer(10))
        modelName <- strsplit(rev(strsplit(modelLoc, '/')[[1]])[1],'[.]')[[1]][1]
        prob <- model$predict(pred)
        row.names(prob) <- tmpRD$c.dat$id
        tmpRD$model[[ modelName ]] <- prob
        return(tmpRD)
    }, error = function(e) print(paste("Cannot model", modelName)))
}

# image = 'img1'
# channel = c(1,2,3)
# range = 20
# modelLoc = './modelMakers/marioModels/models/image.h5'
multiImageModeler <- function(tmpRD, image = 'img3', channel = 1, range = 20, modelLoc = './modelMakers/marioModels/models/image.h5'){
        tryCatch({
        # Collect all images to send into the model
        slice <- (range * 2) + 1
        
        # Gather the center x and y data
        cellXValue <- tmpRD$c.dat$center.x.simplified
        cellYValue <- tmpRD$c.dat$center.y.simplified

        # This checks to see if we will be able to observe the cell
        canView <- 
            0 < (cellXValue - range) & 
            (cellXValue + range) < dim(tmpRD[[ image ]] )[2] &
            0 < (cellYValue - range) &
            (cellYValue + range) < dim(tmpRD[[ image ]])[1] 
        
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

        ## Put into the model
        model <- keras::load_model_hdf5(modelLoc)
        modelName <- strsplit(rev(strsplit(modelLoc, '/')[[1]])[1],'[.]')[[1]][1]
        predLabels <- model$predict(subImageArray)
        
        probMat <- matrix(nrow = length(tmpRD$c.dat$id), ncol = dim(predLabels)[2])
        probMat[canView, ] <- predLabels
        row.names(probMat) <- tmpRD$c.dat$id

        tmpRD$model[[ modelName ]] <- probMat
        return(tmpRD)
    }, error=function(e) print(paste("Cannot model", modelName)))

}

# function to add cell types to the bin dataframe.
cellTypeBinner <- function(tmpRD){
    # Some experiments have spelled cell.types as cell_types, get it right
    cellTypeName <- grep('^cell[._]types$', names(tmpRD), value = TRUE)
    
    # These are the cell types we are gonna work with
    cellTypes <- c('L1', 'L2', 'L3', 'L4', 'L5', 'L6', 'G7', 'G8', 'G9', 'G10', 'R11', 'R12', 'R13', 'N14', 'N15', 'N16')

    # Grab only the cell types we are interested in
    rdCellType <- tmpRD[[ cellTypeName ]][ cellTypes ]

    
    # Add cell_types to the bin dataframe
    tmpRD$c.dat[,'cell_types'] <- NA
    # Itteratively update the binary data frame to contain the new labels.
    for(j in 1:length(rdCellType)){
        tmpRD$c.dat[rdCellType[[j]], 'cell_types'] <- j
    }

    return(tmpRD)
}

# Visualizer
readkeygraph <- function(prompt){
    getGraphicsEvent(prompt = prompt, 
                 onMouseDown = NULL, onMouseMove = NULL,
                 onMouseUp = NULL, onKeybd = onKeybd,
                 consolePrompt = "uh")
    Sys.sleep(0.01)
    return(keyPressed)
}

onKeybd <- function(key){
    keyPressed <<- key
}

#cell <- "X.23"
probVis <- function(tmpRD, cell){
    classNames <- c('L1', "L2", "L3", "L4", "L5", "L6", "G7", "G8", "G9", 'G10', 'R11', 'R12', 'R13', 'N14', 'N15', 'N16')
    datName <- deparse(substitute(tmpRD))
    correctLabel <- tmpRD$c.dat[cell,'cell_types']

    # Combine all models
    allModels <- lapply(tmpRD$model, function(x) x[cell,])
    rowNames <- names(allModels)    
    allModels <- Reduce(rbind, allModels)
    row.names(allModels) <- rowNames
    allModels <- allModels[rev(row.names(allModels)),]

    # Start plotting
    brewerCols <- RColorBrewer::brewer.pal(n = dim(allModels)[1], 'Dark2')[c(2,3,4,6,7)]
    cols <- rev(c(brewerCols, c('cadetblue3', 'seagreen4', 'red3')))
    par(mar = c(5,4,4,4))
    bpDims <- barplot(
        as.matrix(allModels), 
        beside = F, 
        col = cols,
        ylim = c(0,10),
        xaxt = 'n',
        main = paste0(datName, " : ", cell),
        border = NA
    )

    par(xpd=T)
    legend(
        par('usr')[2],
        par('usr')[4],
        legend = rev(row.names(allModels)),
        fill = rev(cols),
        #horiz = T,
        bty='n',
        border = NA,
        cex=.9
    )

    text(
        apply(as.matrix(bpDims), 1, mean),
        par('usr')[3] -yinch(.2),
        classNames,
        col = ifelse(correctLabel == seq(1,16), 'red', 'black'),
        font = ifelse(correctLabel == seq(1,16), 2, 1),
        cex = ifelse(correctLabel == seq(1,16), 1.5, 1)
    )
}

probVisWalker <- function(tmpRD, cells){
    graphics.off()

    dev.new(width =10, height = 4)
    traceWindow <- dev.cur()

    dev.new(width=10, height=4)
    probWindow<- dev.cur()

    dev.set(traceWindow)

    keyPressed <- 'z'
    cell.i <- 1
    while(keyPressed != 'q'){
        keyPressed <- readkeygraph("[press any key to continue]")
        if(keyPressed == 'Up'){
            cell.i <- cell.i + 1
        }

        if(keyPressed == 'Down'){
            cell.i <- cell.i - 1
        }
        if(cell.i < 1){
            cell.i <- length(cells)
        }


        dev.set(probWindow)
        probVis(tmpRD, cells[cell.i])
        
        dev.set(traceWindow)
        PeakFunc7(tmpRD, cells[cell.i], 'blc', zf=20)
    }
}
