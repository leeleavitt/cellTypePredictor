import numpy as np
import matplotlib.pyplot as plt
from py.main import imageLoader

fileName = "./trainingData/cy5.bin_img3_1/13227_41_41_1_cy5.bin.npy"
allImages = imageLoader(fileName)

allImages.shape

plt.figure()
plt.imshow(allImages[19]) 
plt.colorbar()
plt.grid(False)
plt.show()


