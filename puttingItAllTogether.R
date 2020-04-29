# What we need to do is load in the model,
# 1: predict the gfp
#   If gfp, then calculate the max of the sum of all classes within g7, g8, g9 g10

require(reticulate)
mainpy <- import_from_path('main', './py')

# List all the files
models <- list.files('./models/', full.names = T)
cat("These are the Models:\n")
cat(models, sep='  ')

# List of all data for predicting in order of the models
predData <- c(  
    'cell_types_AMCK/AMCK_7238.csv', 
    'cell_types_img3_1/7238_41_41_1_cell_types.npy',
    'cell_types_img4_2/7238_41_41_1_cell_types.npy',
    'cell_types_img1_123/7238_41_41_3_cell_types.npy',
    'cell_types_R3J/R3J_7238.csv',
    'RAMCK/ramck.csv'
)
predData <- paste0("./trainingData/", predData)

predictedClasses <- list()

# AMCK model
i = 1
model <- invisible(keras::load_model_hdf5(models[i]))
pred <- data.frame(data.table::fread(predData[i], header = T), row.names=1)
pred <- as.matrix(pred)
#pred <- array(pred, c(dim(pred), 1))

pred <- mainpy$featureMaker(pred, as.integer(10))

predictedClasses[[ 'amck' ]] <- model$predict(pred)

# cy5 model
i = 2
model <- invisible(keras::load_model_hdf5(models[i]))
image <- RcppCNPy::npyLoad(predData[i])
imgDim <- as.integer(strsplit(rev(strsplit(predData[i], '/')[[1]])[1],'_')[[1]][1:4])
dim(image) <- imgDim

predictedClasses[[ 'cy5' ]] <- model$predict(image)

# gfp model
i = 3
model <- invisible(keras::load_model_hdf5(models[i]))
image <- RcppCNPy::npyLoad(predData[i])
imgDim <- as.integer(strsplit(rev(strsplit(predData[i], '/')[[1]])[1],'_')[[1]][1:4])
dim(image) <- imgDim

predictedClasses[[ 'gfp' ]] <- model$predict(image)

# label model
i = 4
model <- invisible(keras::load_model_hdf5(models[i]))
image <- RcppCNPy::npyLoad(predData[i])
imgDim <- as.integer(strsplit(rev(strsplit(predData[i], '/')[[1]])[1],'_')[[1]][1:4])
dim(image) <- imgDim

predictedClasses[[ 'image' ]] <- model$predict(image)


# R3J model
i = 5
model <- invisible(keras::load_model_hdf5(models[i]))
pred <- data.frame(data.table::fread(predData[i], header = T), row.names=1)
pred <- as.matrix(pred)
#pred <- array(pred, c(dim(pred), 1))

pred <- mainpy$featureMaker(pred, as.integer(10))

predictedClasses[[ 'r3j' ]] <- model$predict(pred)

# ramck model
i = 6
model <- invisible(keras::load_model_hdf5(models[i]))
pred <- data.frame(data.table::fread(predData[i], header = T), row.names=1)
pred <- as.matrix(pred)
#pred <- array(pred, c(dim(pred), 1))

pred <- mainpy$featureMaker(pred, as.integer(10))

predictedClasses[[ 'ramck' ]] <- model$predict(pred)


#########################################################################
classNames <- c('L1', "L2", "L3", "L4", "L5", "L6", "G7", "G8", "G9", 'G10', 'R11', 'R12', 'R13', 'N14', 'N15', 'N16')

# Grab the correct Classes to play with
corrClass <- read.csv("./trainingData/cell_types_label.csv")
row.names(corrClass) <- corrClass[,1]
corrClass <- corrClass[,-1,drop=F]

# Subset the models into the multiclass vs binary
multiClassModels <- predictedClasses[c(1,4,5,6)]
#multiClassModels <- predictedClasses[c(4,6)]
#multiClassModels <- predictedClasses[c(1,5)]
binaryClassModels <- predictedClasses[c(2,3)]

names(multiClassModels)
# Lets see how this does without binary guidance
allClassComp <- data.frame(matrix(nrow = dim(corrClass)[1], ncol = 3))
colnames(allClassComp) <- c('correctLabel', 'predictedLabel', 'topModel')
for( i in 1:dim(corrClass)[1]){
    # Create the data frame of all collected models
    modelFrame <- data.frame(
        matrix(
            nrow = length(multiClassModels), 
            ncol = dim(multiClassModels[[1]])[2]
    ))
    row.names(modelFrame) <- names(multiClassModels)
    
    # Loop through each model and collect the cells scores
    for(j in 1:length(multiClassModels)){
        modelFrame[j, ] <- multiClassModels[[j]][i,]
    }

    # Compute the sum of all models
    predictedClass <- apply(modelFrame, 2, sum)
    # Which class was predicted correctly?
    classPred <- which.max(predictedClass)
    # Which model dominated
    modelMax <- which.max(modelFrame[,classPred])

    allClassComp[i, 1] <- corrClass[i,]
    allClassComp[i, 2] <- as.integer(classPred)
    allClassComp[i, 3] <- names(multiClassModels)[modelMax]
}

allClassCompTable <- table(allClassComp[1] == allClassComp[2])
allClassCompSuccess <-  100 * (allClassCompTable['TRUE']/sum(allClassCompTable) )
cat('\nAll class success is\n')
cat(round(allClassCompSuccess, digits=2), " %\n")

# Lets see how this does WITH binary guidance
binClassComp <- data.frame(matrix(nrow = dim(corrClass)[1], ncol = 3))
colnames(binClassComp) <- c('correctLabel', 'predictedLabel', 'topModel')
for( i in 1:dim(corrClass)[1]){
    #i=1
    # Is the cell GPF?
    if(which.max(binaryClassModels[['gfp']][i,]) == 2){
        cellTypes <- c(5,6,7,8,9,10)
    }else if(which.max(binaryClassModels[['cy5']][i,]) == 2){
        cellTypes <- c(11,12,13,14)
    }else{
        cellTypes <- c(1, 2, 3, 4, 14, 15, 16)
    }

    # Create the data frame of all collected models
    modelFrame <- data.frame(
        matrix(
            nrow = length(multiClassModels), 
            ncol = dim(multiClassModels[[1]][,cellTypes])[2]
    ))
    row.names(modelFrame) <- names(multiClassModels)
    colnames(modelFrame) <- cellTypes
    
    # Loop through each model and collect the cells scores
    for(j in 1:length(multiClassModels)){
        modelFrame[j, ] <- multiClassModels[[j]][i,cellTypes]
    }

    # Compute the sum of all models
    predictedClass <- apply(modelFrame, 2, sum)
    # Which class was predicted correctly?
    classPred <- names(which.max(predictedClass))
    # Which model dominated
    modelMax <- which.max(modelFrame[,which.max(predictedClass)])

    binClassComp[i, 1] <- corrClass[i,]
    binClassComp[i, 2] <- as.integer(classPred)
    binClassComp[i, 3] <- names(multiClassModels)[modelMax]
}

# Bin 
binClassCompTable <- table(binClassComp[1] == binClassComp[2])
binClassCompSuccess <-  100 * (binClassCompTable['TRUE']/sum(binClassCompTable) )
cat('Binary guided success is\n')
cat(round(binClassCompSuccess, digits=2), " %")

dev.new(width = 8, height = 5)
par(mfrow = c(1, 2))

barplot(
    summary(as.factor(allClassComp$topModel)),
    main = 'All Class Success', 
    border = NA
)
barplot(
    summary(as.factor(binClassComp$topModel)),
    main = 'Bin Class Success',
    border = NA
)

###################################################################
# Which classes are most mis classified?
mostCommonLabels <- summary(as.factor(binClassComp$correctLabel))
labs <- as.integer(names(mostCommonLabels))

misClassInfo <- data.frame(matrix(nrow = length(labs), ncol = 3))

misClassInfo[,1] <- mostCommonLabels

for(i in 1:length(labs)){
    # Identify the number of correctly identified neurons
    labClassSel <- binClassComp$correctLabel == labs[i]
    selectedNeurons <- binClassComp[labClassSel,]
    
    cat('\n', classNames[i], '\n')
    classMisClass <- sort(summary(as.factor(selectedNeurons$predictedLabel)), TRUE)[1:3]
    names(classMisClass) <- classNames[as.integer(names(classMisClass))]
    print(classMisClass)

    correct <- table(selectedNeurons$correctLabel == selectedNeurons$predictedLabel)['TRUE']
    misClassInfo[i,2] <- correct

    modelTopName <- names(which.max(summary(as.factor(selectedNeurons$topModel))))
    modelTopNum <- max(summary(as.factor(selectedNeurons$topModel)))
    misClassInfo[i,3] <- paste0(modelTopName, "\n", modelTopNum, " / ", misClassInfo[i,1])
}

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




# Visualize the label scores

dev.new(width = 12, height =5)
cols <- RColorBrewer::brewer.pal(n = length(multiClassModels), 'Dark2')
bpDims <- barplot(
    as.matrix(modelFrame), 
    beside = T, 
    col = cols,
    ylim = c(0,1),
    xaxt = 'n',
    main = row.names(corrClass)[i],
    border = NA
)

legend("top",
    legend = names(multiClassModels),
    fill = cols,
    horiz = T,
    bty='n',
    border = NA)

par(xpd=T)
text(
    apply(bpDims, 2, mean),
    par('usr')[3] -yinch(.2),
    classNames,
    col = ifelse(corrClass[i,] == seq(1,16), 'red', 'black'),
    font = ifelse(corrClass[i,] == seq(1,16), 2, 1),
    cex = ifelse(corrClass[i,] == seq(1,16), 1.5, 1)
)
