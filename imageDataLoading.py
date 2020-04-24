import numpy as np
import matplotlib.pyplot as plt
from py.main import imageLoader

fileName = "./expData/13359_41_41_3_drop.npy"
allImages = imageLoader(fileName)

allImages.shape

plt.figure()
plt.imshow(allImages[19]) 
plt.colorbar()
plt.grid(False)
plt.show()


