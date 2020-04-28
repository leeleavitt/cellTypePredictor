# What we need to do is load in the model,
# 1: predict the gfp
#   If gfp, then calculate the max of the sum of all classes within g7, g8, g9 g10

require(reticulate)
mainpy <- import_from_path('main', './py')

# List all the files
models <- list.files('./models/', full.names = T)

# List of all data for predicting in order of the models
predData <- c(  'cell_types_AMCK/AMCK_7238.csv', 
    'cell_types_img2_1/7238_41_41_1_cell_types.npy',
    'cell_types_img3_1/7238_41_41_1_cell_types.npy',
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

predictedClasses[[ 'label' ]] <- model$predict(image)


# R3J model
i = 6
model <- invisible(keras::load_model_hdf5(models[i]))
pred <- data.frame(data.table::fread(predData[i], header = T), row.names=1)
pred <- as.matrix(pred)
#pred <- array(pred, c(dim(pred), 1))

pred <- mainpy$featureMaker(pred, as.integer(25))

predictedClasses[[ 'r3j' ]] <- model$predict(pred)

# ramck model
i = 7
model <- invisible(keras::load_model_hdf5(models[i]))
pred <- data.frame(data.table::fread(predData[i], header = T), row.names=1)
pred <- as.matrix(pred)
#pred <- array(pred, c(dim(pred), 1))

pred <- mainpy$featureMaker(pred, as.integer(25))

predictedClasses[[ 'ramck' ]] <- model$predict(pred)
