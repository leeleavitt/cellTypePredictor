require(procPharm)
tmpRD <- get(load("./experimentTesting/RD.200415.43.m.M3.W1.Iq9a.Rdata"))
source('./experimentTesting/probVis.R')

tmpRD <- cellTypeBinner(tmpRD)
tmpRD <- multiTraceModeler(tmpRD, '[aA][iI][tT][cC]', './modelMakers/marioModels/models/aitc.h5')
tmpRD <- multiTraceModeler(tmpRD, '[cC][aA][pP][sS]', './modelMakers/marioModels/models/caps.h5')
tmpRD <- multiTraceModeler(tmpRD, '[mM][eE][nN][tT][hH]', './modelMakers/marioModels/models/menth.h5')
tmpRD <- multiTraceModeler(tmpRD, '[kK][.]40', './modelMakers/marioModels/models/k40.h5')
tmpRD <- multiTraceModeler(tmpRD, 'r3j', './modelMakers/marioModels/models/r3j.h5')

tmpRD <- multiImageModeler(tmpRD, 'img1', c(1,2,3), 20, './modelMakers/marioModels/models/image.h5')
tmpRD <- multiImageModeler(tmpRD, 'img4', c(2), 20, './modelMakers/marioModels/models/gfp.h5')
tmpRD <- multiImageModeler(tmpRD, 'img3', c(1), 20, './modelMakers/marioModels/models/ib4.h5')

source('./experimentTesting/probVis.R')
cells <- sample(c(tmpRD$cell_types$N14, tmpRD$cell_types$R13))
probVisWalker(tmpRD, cells)

###############################################################################
# Playing around with visualization of probabilities predicitng AITC responses
for( i in 1:length(tmpRD$probs)){
    colnames(tmpRD$probs[[i]]) <- paste(names(tmpRD$probs)[i], colnames(tmpRD$probs[[i]]), sep="_")
}

tmpRD$scp <- cbind(tmpRD$scp, Reduce(cbind, tmpRD$probs))
tmpRD$c.dat <- cbind(tmpRD$c.dat, tmpRD$scp)

tmpRD <- cellTypeBinner(tmpRD)

sampleCells <- sample(c(tmpRD$cell_types$R13, tmpRD$cell_types$N14))[1:50]
sampleCells <- sample(c(tmpRD$cell_types$R13))[1:50]

tcd(tmpRD, sampleCells)