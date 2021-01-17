# Tipos-Clientes
Comparación de ventas Vs tipo clientes
#####TIPOS CLIENTES########

library(readr)
clientes <- read_csv("C:/Users/bbarahona/Desktop/clientes.csv")
View(clientes)

#####Escalamiento Multidimensional######
d = dist(clientes[,2:4], method = "euclidean")
fit = cmdscale(d,eig=TRUE, k=2)

x = fit$points[,1] 
y = fit$points[,2]

plot(x,y)
text(x, y, labels = row.names(iris), cex=1)

#Identificaci�n de las Clases
#clase = as.factor(iris$X5)
#plot(x,y,col=c("red","green3","blue")[clase], main = "Iris Dataset Original")

#########Crear Grupos: K-Means########
grupos = kmeans(clientes[,2:4],4)
g1 = grupos$cluster
g2 = grupos$size

# el agrupamiento lo realiza de la siguiente manera:
# grupo 1= 14 elementos (Clientes con Baja Frecuencia)
# grupo 2= 4009 elementos ( Clientes VIP)
# grupo 3= 103 elementos (Clientes Nuevos)
# grupo 4= 721 elementos (VIPs Potenciales)

plot(x,y,col=c("red","green3","blue","black")[g1], main = "clientes Dataset K-Means")

#########Crear Grupos: Jerarquico#########
library("dendextend")
hc = hclust(d, method = "complete" )
clus4 = cutree(hc, 4)

# el agrupamiento lo realiza de la siguiente manera:
# grupo 1= 4 elementos
# grupo 2= 5 elementos
# grupo 3= 46 elementos
# grupo 4= 4792 elementos

dend = as.dendrogram(hc)
dend = color_branches(dend, 4)
colors = c("red", "green3", "blue", "black")
plot(dend, fill = colors[clus4], cex = 0.1 , main = "DHC")

######### Elbow #########
# Crea diferentes valores de k
wi = c()
for (i in 1:10) 
{
  g = kmeans(iris[,2:4],i) 
  wi[i] = g$tot.withinss
}
plot((1:length(wi)),wi, xlab="Numero de Clusters", ylab="Suma Cuadrados Internos", pch=19, col="red", type = "b")

# segun el resultado se deberia tomar de 2 a 3 grupos 

######### Validacion Interna #########
# Indice de Dunn

library(cluster)
library(clValid)
du1 = dunn(d,g1)
du2 = dunn(d,clus3)

# los dos metodos no son buenos ya que el indice de dunn
# en los dos casos es muy bajo, si tubieramos que decidir
# se podria tomar el modelo DHC como el mejor

# Coeficiente de Silueta
sil1 = silhouette(g1,d)
plot(sil1,col=1:3, border=NA)
sil2 = silhouette(clus4,d)
plot(sil2,col=4:6, border=NA)

# segun el diagrama de silueta el metodo DHC tiene mejor promedio 

######### Validacion Externa #########

library(aricode)
library(plyr)
ground = g1
ARI2= ARI(ground,clus4)
AMI2= AMI(ground,clus4)
NMI2= NMI(ground,clus4,variant = c("joint"))
ENT = entropy(ground,clus4)
ARI2 # = 0.10
AMI2 # = 0.094
NMI2 #0.091

# Los valores son estan cercanos a cero es decir que la clasificacion no es buena 

