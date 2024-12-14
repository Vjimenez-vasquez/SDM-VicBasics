##################
#### SCRIPT 1 ####
##################

#1#configurar el area de trabajo#
setwd("C:/Users/HP/Documents/curso_julio")
getwd()

#4#instalar paquetes, solo correr una vez#

#5#install.packages("raster")#
#6#install.packages("rgdal")#
#7#install.packages("SDMPlay")#
#8#install.packages("rJava")#
#9#install.packages("caret")#
#10#install.packages("dismo")#
#11#requires actualization of java https://www.java.com/en/download/manual.jsp#

#12#cargar librerias#

library(raster)
library(rgdal)
library(dismo)
library(SDMPlay)
library(rJava)
library(caret)

#19#cargar los rasters descargados de WORLDCLIM (https://www.worldclim.org/data/worldclim21.html)#

bio1  <- raster("C:/Users/HP/Documents/distribuciones/layers/wc2.1_30s_bio/wc2.1_30s_bio_1.tif")
bio2  <- raster("C:/Users/HP/Documents/distribuciones/layers/wc2.1_30s_bio/wc2.1_30s_bio_2.tif")
bio3  <- raster("C:/Users/HP/Documents/distribuciones/layers/wc2.1_30s_bio/wc2.1_30s_bio_3.tif")
bio4  <- raster("C:/Users/HP/Documents/distribuciones/layers/wc2.1_30s_bio/wc2.1_30s_bio_4.tif")
bio5  <- raster("C:/Users/HP/Documents/distribuciones/layers/wc2.1_30s_bio/wc2.1_30s_bio_5.tif")
bio6  <- raster("C:/Users/HP/Documents/distribuciones/layers/wc2.1_30s_bio/wc2.1_30s_bio_6.tif")
bio7  <- raster("C:/Users/HP/Documents/distribuciones/layers/wc2.1_30s_bio/wc2.1_30s_bio_7.tif")
bio8  <- raster("C:/Users/HP/Documents/distribuciones/layers/wc2.1_30s_bio/wc2.1_30s_bio_8.tif")
bio9  <- raster("C:/Users/HP/Documents/distribuciones/layers/wc2.1_30s_bio/wc2.1_30s_bio_9.tif")
bio10  <- raster("C:/Users/HP/Documents/distribuciones/layers/wc2.1_30s_bio/wc2.1_30s_bio_10.tif")
bio11  <- raster("C:/Users/HP/Documents/distribuciones/layers/wc2.1_30s_bio/wc2.1_30s_bio_11.tif")
bio12  <- raster("C:/Users/HP/Documents/distribuciones/layers/wc2.1_30s_bio/wc2.1_30s_bio_12.tif")
bio13  <- raster("C:/Users/HP/Documents/distribuciones/layers/wc2.1_30s_bio/wc2.1_30s_bio_13.tif")
bio14  <- raster("C:/Users/HP/Documents/distribuciones/layers/wc2.1_30s_bio/wc2.1_30s_bio_14.tif")
bio15  <- raster("C:/Users/HP/Documents/distribuciones/layers/wc2.1_30s_bio/wc2.1_30s_bio_15.tif")
bio16  <- raster("C:/Users/HP/Documents/distribuciones/layers/wc2.1_30s_bio/wc2.1_30s_bio_16.tif")
bio17  <- raster("C:/Users/HP/Documents/distribuciones/layers/wc2.1_30s_bio/wc2.1_30s_bio_17.tif")
bio18  <- raster("C:/Users/HP/Documents/distribuciones/layers/wc2.1_30s_bio/wc2.1_30s_bio_18.tif")
bio19  <- raster("C:/Users/HP/Documents/distribuciones/layers/wc2.1_30s_bio/wc2.1_30s_bio_19.tif")

#39#cargar el raster de elevacion#
elev  <- raster("C:/Users/HP/Documents/distribuciones/layers/wc2.1_30s_elev/wc2.1_30s_elev.tif")

#41#Plotear los rasters#

par(mfrow=c(2,2))
plot(bio1,main="bio 1")
plot(bio6,main="bio 6")
plot(bio12,main="bio 12")
plot(bio19,main="bio 19")

#47#plotear con mayor detalle

par(mfrow=c(1,1))
plot(elev,main="elevation - Peru",xlim=c(-84,-65),ylim=c(-19,2))

#50#Cargar areas/vectores (http://www.diva-gis.org/gdata)#

per_0 <- readOGR("PER_adm/PER_adm0.shp")
plot(per_0,add=TRUE,border="red")
data.frame(names(per_0))

per_1 <- readOGR("PER_adm/PER_adm1.shp")
plot(per_1,add=TRUE)
data.frame(names(per_1))
data.frame(per_1$NAME_1)
plot(per_1[1,],add=TRUE,col="red")
plot(per_1[2,],add=TRUE,col="blue")
plot(per_1[26,],add=TRUE,col="yellow")
plot(per_1[8,],add=TRUE,col="black")

#62#Leer las coordenadas de muestreo (ocurrencias)#
oc <- read.csv("data.csv")
sp1 <- oc[1:24,2:3]
sp2 <- oc[25:47,2:3]
points(sp1, pch = 21, cex = 0.5, bg="red")
points(sp2, pch = 21, cex = 0.5, bg="blue")

#68#agrupar todas las capas en un solo elemento#
layers <- stack(bio1,bio2,bio3,bio4,bio5,bio6,bio7,bio8,bio9,bio10,bio11,bio12,bio13,bio14,bio15,bio16,bio17,bio18,bio19,elev)
names(layers) <- c("bio1","bio2","bio3","bio4","bio5","bio6","bio7","bio8","bio9","bio10","bio11","bio12","bio13","bio14","bio15",
"bio16","bio17","bio18","bio19","elev")

values <- extract(layers,oc[,2:3],method="bilinear")
data2 <- data.frame(oc,values)
#write.csv(data2,file="data_2.csv",row.names=FALSE)#

#74#Ahora vamos a determinar que variables no estan correlacionadas#
data2 <- read.csv("data_2.csv")
numeric <- data2[,4:23]
cor_num <- cor(numeric,numeric)
cor_col <- findCorrelation(cor_num,cutoff=0.8)
ord_cor_col <- sort(cor_col)
numeric_noncor <- numeric[,-c(ord_cor_col)]
numeric_noncor
dim(numeric_noncor)
data.frame(names(numeric_noncor))
#84#Ahi tienes tus variables no correlacionadas: bio2,bio7,bio8,bio15,bio18,bio19#

#85#write.csv(numeric_noncor,file="data_3.csv",row.names=FALSE)
data4 <- data.frame(oc,numeric_noncor)
#87#write.csv(data4,file="data_4.csv",row.names=FALSE)

#88#ANALISIS DE COMPONENTES PRINCIPALES#

data4 <- read.csv("data_4.csv")
data5 <- data4[ ,4:9]
pca <- princomp(data5,scores = TRUE)
names(pca)
biplot(pca)

#normalizacion de la data#
data6 <- log(data4[,4:9])

#96#PCA con princomp#
pca2 <- princomp(data6,scores = TRUE)
summary(pca2)
names(pca2)
biplot(pca2)
pca2$sdev
pca2$scores

#101#plot de la data#
data7 <- pca2$scores
colors <- c(rep("red",24),rep("blue",23))
plot(data7[,1],data7[,2],col=colors)
plot(data7[,1],data7[,2],bg=colors,pch=21,cex=2)
plot(data7[,1],data7[,2],bg=colors,pch=21,cex=2,xlab="PC 1 (86.54%)",ylab="PC 2 (8.9%)",main="princomp")

#107#PCA con prcomp#
pca3 <- prcomp(data6,center = TRUE, scale = FALSE)
summary(pca3)
names(pca3)
biplot(pca3)

pca3$sdev
pca3$rotation
data8 <- pca3$x
plot(data8[,1],data8[,2],bg=colors,pch=21,cex=2,main="prcomp",xlab="PC 1 (86.54%)",ylab="PC 2 (8.89%)")

#116#Preparacion de datos: OUTLYING MEAN INDEX (OMI)Y CANONICAL OUTLYING MEAN INDEX (CANOMI)#

#117#install.packages("adehabitatHS")
#118#install.packages("ade4")

library(ade4)
library(adehabitatHS)

#121#definir el area de trabajo y extraer la data medioambiental
e <- as(extent(-78, -75,-10 ,-5),"SpatialPolygons")
crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
per_1 <- readOGR("PER_adm/PER_adm1.shp")
plot(per_1)
plot(e,add=TRUE,border="red")

oc <- read.csv("data.csv")
sp1 <- oc[1:24,2:3]
sp2 <- oc[25:47,2:3]
points(sp1, pch = 21, cex = 1, bg="red")
points(sp2, pch = 21, cex = 1, bg="blue")

#cortar los raster no correlacionados mas elevacion#
layers <- stack(bio2,bio7,bio8,bio15,bio18,bio19,elev)
r <- crop(layers, e)
names(r) <- c("bio2","bio7","bio8","bio15","bio18","bio19","elev")
plot(r)
plot(r[[5]],main="bio 18")
points(sp1, pch = 21, cex = 1, bg="red")
points(sp2, pch = 21, cex = 1, bg="blue")

#139#guardamos los rasters recortados al area "e"#
#140#writeRaster(r,filename="env.tif",format="GTiff",bylayer=TRUE,suffix=names(r))#
#141#writeRaster(r,filename="env.asc",format="ascii",bylayer=TRUE,suffix=names(r))#

#142#generar data medioambiental para las ocurrencias y para el area de estudio(background)
dots_data <- SDMPlay:::SDMtab(oc[,2:3],r, unique.data = TRUE, same=FALSE, background.nb=500)
dots_data

#144#ploteamos los puntos background obtenidos y las ocurrencias#
plot(r[[7]],main="Elevation")
points(dots_data[1:24,2:3], pch = 21, cex = 1, bg="red")
points(dots_data[25:47,2:3], pch = 21, cex = 1, bg="blue")
points(dots_data[48:547,2:3], pch = 21, cex = 0.5, bg="grey")

#149#generamos la tabla de puntos utilizados(U)#
sp1 <- c(rep(1,24),rep(0,523))
sp2 <- c(rep(0,24),rep(1,23),rep(0,500))
Species <- c(rep("sp1",24),rep("sp2",23),rep("bg",500))

#153#X es la tabla de datos medioambientales totales(547)
X <- dots_data[,4:10]
X

#155#U es la tabla binaria que indica los puntos utilizados por cada especie 
U <- data.frame(sp1,sp2)
U

#157#ejecucion: OUTLYING MEAN INDEX (OMI)#
pc <- dudi.pca(X, scannf=FALSE)
ni <- niche(pc, U, scannf=FALSE)
dots <- ni$ls
ni$eig
ni$tab 
plot(ni)
#162#estimamos los porcentajes de marginalidad extraidos en cada eje#
axis1 <- ni$eig[1]/sum(ni$eig)
axis2 <- ni$eig[2]/sum(ni$eig)

#165#obtener un plot informativo
#install.packages("ggplot")#
library(ggplot2)
p1 <- ggplot(data=dots,aes(x=dots[,1],y=dots[,2],color=Species,shape=Species)) +
geom_hline(yintercept = 0, lty = 2) +
geom_vline(xintercept = 0, lty = 2) +
guides(color=guide_legend(title="Species"),shape=guide_legend(title="Species")) +
scale_shape_manual(values = c(15,16,17)) +
geom_point(alpha = 0.8, size = 2)
p1

p2 <- p1 + stat_ellipse(geom="polygon", aes(fill = Species),
alpha = 0.2, show.legend = FALSE,level = 0.95) +
xlab("axis 1 (82.7%)")+ylab("axis 2 (17.3%)") +
theme_minimal()+theme(panel.grid = element_blank(), 
panel.border = element_rect(fill= "transparent"))
p2

#166#ejecucion: CANONICAL OUTLYING MEAN INDEX (CANOMI)
pc <- dudi.pca(X, scannf=FALSE)
com <- canomi(pc, U, scannf=FALSE)
com
com$eig
dots_c <- as.data.frame(com$ls) 
plot(com)

#167#estimamos los porcentajes de varianza extraidos en cada eje
eje1 <- com$eig[1]/sum(com$eig)
eje2 <- com$eig[2]/sum(com$eig)

p3 <- ggplot(data=dots_c,aes(x=dots_c[,1],y=dots_c[,2],color=Species,shape=Species)) +
geom_hline(yintercept = 0, lty = 2) +
geom_vline(xintercept = 0, lty = 2) +
guides(color=guide_legend(title="Species"),shape=guide_legend(title="Species")) +
scale_shape_manual(values = c(15,16,17)) +
geom_point(alpha = 0.8, size = 2)
p3

p4 <- p3 + stat_ellipse(geom="polygon", aes(fill = Species),
alpha = 0.2, show.legend = FALSE,level = 0.95) +
xlab("axis 1 (77%)")+ylab("axis 2 (22.9%)") +
theme_minimal()+theme(panel.grid = element_blank(), 
panel.border = element_rect(fill= "transparent"))
p4