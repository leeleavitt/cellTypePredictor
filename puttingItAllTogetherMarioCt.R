# What we need to do is load in the model,
# 1: predict the gfp
#   If gfp, then calculate the max of the sum of all classes within g7, g8, g9 g10
require(reticulate)
mainpy <- import_from_path('main', './py')

# List all the files
models <- c(
    './modelMakers/marioModels/models/r3j.h5',
    './modelMakers/marioModels/models/aitc.h5',
    './modelMakers/marioModels/models/menth.h5',
    './modelMakers/marioModels/models/caps.h5',
    './modelMakers/marioModels/models/k40.h5',
    './modelMakers/marioModels/models/gfp.h5',
    './modelMakers/marioModels/models/ib4.h5',
    './modelMakers/marioModels/models/image.h5'
)
cat("These are the Models:\n")
cat(models, sep='  ')

# List of all data for predicting in order of the models
predData <- c(
    './trainingData/cellTypeData/r3j/traces.csv',
    './trainingData/cellTypeData/aitc/traces.csv', 
    './trainingData/cellTypeData/menth/traces.csv',
    './trainingData/cellTypeData/caps/traces.csv',
    './trainingData/cellTypeData/k40/traces.csv',
    './trainingData/cellTypeData/cell_types_img4_2/13465_41_41_1_cell_types.npy',
    './trainingData/cellTypeData/cell_types_img3_1/13465_41_41_1_cell_types.npy',
    './trainingData/cellTypeData/cell_types_img1_123/13465_41_41_3_cell_types.npy'
)

# Grab the correct Classes to play with
labData <- c(
    './trainingData/cellTypeData/r3j/labels.csv',   
    './trainingData/cellTypeData/aitc/labels.csv', 
    './trainingData/cellTypeData/menth/labels.csv',
    './trainingData/cellTypeData/caps/labels.csv',
    './trainingData/cellTypeData/k40/labels.csv',
    './trainingData/cellTypeData/cell_types_img4_2/cell_types_13465_label.csv',
    './trainingData/cellTypeData/cell_types_img3_1/cell_types_13465_label.csv',
    './trainingData/cellTypeData/cell_types_img1_123/cell_types_13465_label.csv'
)

# Problem is that some experiments don't have the data required to test all models
labList <- list()
for( i in 1:length(labData) ){
    labList[[ labData[i] ]] <- read.csv(labData[i], row.names = 1)
}

modelNames <- c('r3J','aitc', 'menth', 'caps', 'k40', 'gfp', 'ib4', 'image')
predictedClasses <- list()
INV
# AMCK model
for(i in 1:5){
    model <- invisible(keras::load_model_hdf5(models[i]))
    pred <- data.frame(data.table::fread(predData[i], header = T), row.names=1)
    pred <- as.matrix(pred)
    pred <- mainpy$featureMaker(pred, as.integer(10))
    predictedClasses[[ modelNames[i] ]] <- model$predict(pred)
    row.names(predictedClasses[[ modelNames[i] ]]) <- row.names( labList[[ i ]] )
}

# image models
for(i in 6:8){
    model <- invisible(keras::load_model_hdf5(models[i]))
    image <- RcppCNPy::npyLoad(predData[i])
    imgDim <- as.integer(strsplit(rev(strsplit(predData[i], '/')[[1]])[1],'_')[[1]][1:4])
    dim(image) <- imgDim
    predictedClasses[[ modelNames[i] ]] <- model$predict(image)
    row.names( predictedClasses[[ modelNames[i] ]] ) <- row.names( labList[[ i ]] )
}


#########################################################################
classNames <- c('L1', "L2", "L3", "L4", "L5", "L6", "G7", "G8", "G9", 'G10', 'R11', 'R12', 'R13', 'N14', 'N15', 'N16')

# Find unique neurons
listRowNames <- lapply(labList, function(x) row.names(x))
uniqueNuerons <- uniqueNeurons <- Reduce(intersect, listRowNames)
# load in labels from first model that i know contains most examples
labels <- read.csv(labData[1], row.names = 1)
labelSet <- labels [uniqueNeurons, ,drop=F]

# Lets see how this does without binary guidance
allClassComp <- data.frame(matrix(nrow = dim(labelSet)[1], ncol = 3))
colnames(allClassComp) <- c('correctLabel', 'predictedLabel', 'topModel')

modelFrames <- list()
for( i in 1:dim(labelSet)[1]){
    # Create the data frame of all collected models
    modelFrame <- data.frame(
        matrix(
            nrow = length(predictedClasses), 
            ncol = dim(predictedClasses[[1]])[2]
    ))
    row.names(modelFrame) <- names(predictedClasses)
    
    # Loop through each model and collect the cells scores
    for(j in 1:length(predictedClasses)){
        modelFrame[j, ] <- predictedClasses[[j]][row.names(labelSet)[i],]
    }

    # Compute the sum of all models
    predictedClass <- apply(modelFrame, 2, sum)
    # Which class was predicted correctly?
    classPred <- which.max(predictedClass)
    # Which model dominated
    modelMax <- which.max(modelFrame[,classPred])

    allClassComp[i, 1] <- labelSet[i,]
    allClassComp[i, 2] <- as.integer(classPred)
    allClassComp[i, 3] <- names(predictedClasses)[modelMax]

    modelFrames[[ row.names(labelSet)[i] ]] <- modelFrame
}

allClassCompTable <- table(allClassComp[1] == allClassComp[2])
allClassCompSuccess <-  round(100 * (allClassCompTable['TRUE']/sum(allClassCompTable) ), digits=2)
cat('\nAll class success is\n')
cat(allClassCompSuccess, " %\n")

dev.new(width = 7, height = 4)

bpDim <- barplot(
    summary(as.factor(allClassComp$topModel)),
    main = paste0('All Class Success: ',allClassCompSuccess, '%\nTotal examples ', dim(allClassComp)[1]) , 
    border = NA, 
    ylim= c(0, max(summary(as.factor(allClassComp$topModel)))*1.4 )
)

text(bpDim,
    summary(as.factor(allClassComp$topModel)) + yinch(.2),
    summary(as.factor(allClassComp$topModel)),
    srt=90
)

###################################################################
# Which classes are most mis classified?
mostCommonLabels <- summary(as.factor(allClassComp$correctLabel))
labs <- as.integer(names(mostCommonLabels))

misClassInfo <- data.frame(matrix(nrow = length(labs), ncol = 3))
misClassInfo[,1] <- mostCommonLabels

classMisClasses <- list()
for(i in 1:length(labs)){
    # Identify the number of correctly identified neurons
    labClassSel <- allClassComp$correctLabel == labs[i]
    selectedNeurons <- allClassComp[labClassSel,]
    
    # What other classes was this class assigned to? and why?
    newName <- paste0(classNames[i], ' : ', dim(selectedNeurons)[1])
    classMisClass <- sort(summary(as.factor(selectedNeurons$predictedLabel)), TRUE)[1:3]
    classMisClassNames <- as.integer(names(classMisClass))
    names(classMisClass) <- classNames[classMisClassNames]
    classMisClasses[[ newName ]] <- list()
    classMisClasses[[ newName ]][[ 'misClassClasses' ]] <- classMisClass
    
    barLabels <- c()
    for(j in 1:length(classMisClassNames)){
        logic <- selectedNeurons$predictedLabel == classMisClassNames[j]
        toAdd <- sort(summary(as.factor(selectedNeurons[logic, 'topModel'])), TRUE)[1]
        barLabels[j] <- paste0(names(toAdd), " : ", toAdd)
    }
    classMisClasses[[ newName ]][[ 'barLabels' ]] <- barLabels

    correct <- table(selectedNeurons$correctLabel == selectedNeurons$predictedLabel)['TRUE']
    misClassInfo[i,2] <- correct

    modelTopName <- names(which.max(summary(as.factor(selectedNeurons$topModel))))
    modelTopNum <- max(summary(as.factor(selectedNeurons$topModel)))
    misClassInfo[i,3] <- paste0(modelTopName, "\n", modelTopNum, " / ", misClassInfo[i,1])
}

################################################################
## Visualize the class misclassification rate
dev.new(width = 10, height = 5)
misClassInfo[is.na(misClassInfo)] <- 0 
bpDims <- barplot(
    misClassInfo[,2] / misClassInfo[,1] * 100,
    ylim = c(0, 100),
    ylab= '% correct', 
    border=NA
)

par(xpd=T)
# Class labels
text(
    bpDims,
    par('usr')[3] - yinch(.5),
    paste0(classNames, "\nn=", misClassInfo[,1]),
    cex=.9
)

# Dominate model
text(
    bpDims -  xinch(.1),
    par('usr')[3] + yinch(.5),    
    misClassInfo[,3],
    cex = .9,
    srt=90
)

# bar percent label
text(
    bpDims -  xinch(.1),
    (misClassInfo[,2] / misClassInfo[,1] * 100) + yinch(.4),
    paste0(round(misClassInfo[,2] / misClassInfo[,1] * 100, digits =0),' %'),
    font = 2,
    srt = 90
)

################################################################
# Visual misclassified to 
dev.new(width = 10, height = 10)
mfrowDim <- ceiling(sqrt(length(classMisClasses)))
par(mfrow = c(mfrowDim, mfrowDim))

for(i in 1:length(classMisClasses)){
    mainName <- names(classMisClasses[i])
    col <- RColorBrewer::brewer.pal(100, 'Accent')[1:3]
    bpDims <- barplot(
        classMisClasses[[i]][[1]],
        main = mainName,
        col = col,
        border=NA
    )

    # Add the model misclass
    text(
        apply(bpDims, 1, 'mean'),
        par('usr')[3]+yinch(.1),
        classMisClasses[[i]][[2]],
        srt=90,
        font=2,
        cex = 1.3, 
        adj = 0
    )
}


################################################################
# Visualize the label scores
dev.new(width = 12, height =20)
par(mfrow=c(4, 1))
for( i in sample(seq(1,length(modelFrames))) [1:4]){
    cols <- RColorBrewer::brewer.pal(n = length(predictedClasses), 'Dark2')
    bpDims <- barplot(
        as.matrix(modelFrames[[ i ]]), 
        beside = F, 
        col = cols,
        ylim = c(0,10),
        xaxt = 'n',
        main = row.names(labelSet)[i],
        border = NA
    )

    legend("top",
        legend = names(predictedClasses),
        fill = cols,
        horiz = T,
        bty='n',
        border = NA)

    par(xpd=T)
    text(
        apply(as.matrix(bpDims), 1, mean),
        par('usr')[3] -yinch(.2),
        classNames,
        col = ifelse(labelSet[i,] == seq(1,16), 'red', 'black'),
        font = ifelse(labelSet[i,] == seq(1,16), 2, 1),
        cex = ifelse(labelSet[i,] == seq(1,16), 1.5, 1)
    )
}