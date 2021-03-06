import pandas as pd
import numpy as np
from sklearn.decomposition import PCA

wine = pd.read_csv("D:/DATA SCIENCE/ExcelR/Assignments/PCA/Wine_PCA.py")
wine.describe()

import matplotlib.pyplot as plt
from sklearn.preprocessing import scale

wine.head(5)

# normalize the data

wine_normal = scale(wine)

pca = PCA(n_components = 6)
pca_values = pca.fit_transform(wine_normal)

# The amount of variance that each PCA explains is 
var = pca.explained_variance_ratio_
var
pca.components_[0]

# Cumulative variance 

var1 = np.cumsum(np.round(var,decimals = 4)*100)
var1

# Variance plot for PCA components obtained 
plt.plot(var1,color="red")

# plot between PCA1 and PCA2 
x = pca_values[:,0]
y = pca_values[:,1]
z = pca_values[:2:3]
plt.scatter(x,y,color=["red","blue"])

from mpl_toolkits.mplot3d import Axes3D
Axes3D.scatter(np.array(x),np.array(y),np.array(z),c=["green","blue","red"])


################### Clustering  ##########################
new_df = pd.DataFrame(pca_values[:,0:4])

from sklearn.cluster import KMeans

kmeans = KMeans(n_clusters = 3)
kmeans.fit(new_df)
kmeans.labels_








