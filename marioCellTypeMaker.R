###############################################################
###############################################################
###############################################################
source("./R/utilities.R")

# find all files
fileLocation <- './rawData/marioData/'
# Only load from microscope 3
files <- list.files(fileLocation)

# load in all files, thank god for all the ram
if(length(ls(pattern = 'RD.')) < 1){
    for(i in files){
        print(i)
        load( paste0(fileLocation,i) )
    }
}

# Find all experiments in the workspace
rdExps <- ls(pattern ='RD[.]')

# Find the experiments that have all celltypes
goodExps <- cellTypeChecker(rdExps)

# This how we add the cell types to the bin dataFrame
multiCellTypeBinner(goodExps)

# Now we need to make training datasets with cell types are labels for
# all is saved in './trainingData/cellTypeData'
# 1 AITC
# 2 Menthol
# 3 Capsaicin
# 4 40mM K
# 5 GFP
# 6 IB4
# 7 BF.gfp.ib4 overlay
traceLabeledDataMaker(goodExps, 'r3j', 'cell_types', './trainingData/cellTypeData/r3j/')

traceLabeledDataMaker(goodExps, '[aA][iI][tT][cC]', 'cell_types', './trainingData/cellTypeData/aitc/')
traceLabeledDataMaker(goodExps, '[cC][aA][pP][sS]', 'cell_types', './trainingData/cellTypeData/caps/')
traceLabeledDataMaker(goodExps, '[mM][eE][nN][tT][hH]', 'cell_types', './trainingData/cellTypeData/menth/')
traceLabeledDataMaker(goodExps, '[kK][.]40', 'cell_types', './trainingData/cellTypeData/k40/')

source('./R/imageExtractor_v2.R')
imageExtractor(goodExps, range = 20, 'cell_types', 'img1', c(1,2,3), T)
imageExtractor(goodExps, range = 20, 'cell_types', 'img3', c(1), T)
imageExtractor(goodExps, range = 20, 'cell_types', 'img4', c(2), T)


