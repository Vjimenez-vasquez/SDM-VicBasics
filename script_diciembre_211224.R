#################
### SCRIPT 2 ####
#################

#1#configurar el area de trabajo#
setwd("C:/Users/HP/Documents/curso_julio")
getwd()

#2#cargar librerias#
library(raster)
library(rgdal)
library(dismo)
library(SDMPlay)
library(rJava)
library(caret)

#3#leer las ocurrencias#
oc <- read.csv("data.csv")
oc
sp1 <- oc[1:24,2:3]
sp2 <- oc[25:47,2:3]

#4#leer los raster configurados#
bio2  <- raster("TIFs/env_bio2.tif")
bio7  <- raster("TIFs/env_bio7.tif")
bio8  <- raster("TIFs/env_bio8.tif")
bio15  <- raster("TIFs/env_bio15.tif")
bio18  <- raster("TIFs/env_bio18.tif")
bio19  <- raster("TIFs/env_bio19.tif")
elev  <- raster("TIFs/env_elev.tif")

#5#agrupar todas las capas recortadas#
layers <- stack(elev,bio2,bio7,bio8,bio15,bio18,bio19)
plot(layers)

#6#recortar las capas al area entre 0 y 2000 msnm#
plot(layers[[1]])
par(mar=c(0,0,0,0))
layers_2 <- SDMPlay:::delim.area(layers, longmin=-78, longmax=-75,latmin=-10 , latmax=-5, interval=c(0,2000))
plot(subset(layers_2,1),legend.width=0.5, legend.shrink=0.25)
points(sp1, pch = 21, cex = 1, bg="red")
points(sp2, pch = 21, cex = 1, bg="blue")

#7#obtener background de un area delimitada entre 0 y 2000 msnm#
dots_data <- SDMPlay:::SDMtab(oc[,2:3],layers_2, unique.data = TRUE, same=FALSE, background.nb=94)
background <- dots_data[48:141,2:3]
points(background, pch = 21, cex = 0.5, bg="black")

#8#Boosted Regression Trees (BRT) Method#
dots_data_sp1 <- SDMPlay:::SDMtab(sp1,layers_2, unique.data = TRUE, same=FALSE, background.nb=48)
brt_sp1_1 <- SDMPlay:::compute.brt(x=dots_data_sp1, proj.predictors=layers_2, tc=2, lr=0.001, bf=0.75, n.trees=50)
plot(brt_sp1_1$raster.prediction,main="BRT model sp1",legend.args=list(text="HS", side=3, font=2, cex=0.8))

dots_data_sp2 <- SDMPlay:::SDMtab(sp2,layers_2, unique.data = TRUE, same=FALSE, background.nb=46)
brt_sp2_1 <- SDMPlay:::compute.brt(x=dots_data_sp2, proj.predictors=layers_2, tc=2, lr=0.001, bf=0.75, n.trees=50)
plot(brt_sp2_1$raster.prediction,main="BRT model sp2",legend.args=list(text="HS", side=3, font=2, cex=0.8))

par(mfrow=c(1,2))
plot(brt_sp1_1$raster.prediction,main="BRT model sp1",legend.args=list(text="HS", side=3, font=2, cex=0.8))
points(sp1, pch = 21, cex = 0.5, bg="red")
points(dots_data_sp1[25:72,2:3], pch = 21, cex = 0.5, bg="grey")
plot(brt_sp2_1$raster.prediction,main="BRT model sp2",legend.args=list(text="HS", side=3, font=2, cex=0.8))
points(sp2, pch = 21, cex = 0.5, bg="blue")
points(dots_data_sp2[24:69,2:3], pch = 21, cex = 0.5, bg="grey")

#9#obtener una tabla y una grafica de contribuciones para las variables#
names(brt_sp1_1$response)
cont <- brt_sp1_1$response$contributions
b <- barplot(cont[,2], ylab="contribution (%)")
text(b-0.5, par("usr")[3] - 0.025, srt = 45, adj = 1, labels=row.names(cont),cex=1,xpd=T)

#10#obtener curvas de respuesta#
library(dismo)
gbm.plot(brt_sp1_1$response,n.plots=7,cex.axis=0.5, smooth=TRUE)
summary(brt_sp1_1)

#11#evaluacion del modelo a traves de diferentes parametros#
evaluation_sp1 <- SDMPlay:::SDMeval(brt_sp1_1)
evaluation_sp2 <- SDMPlay:::SDMeval(brt_sp2_1)
SDMPlay:::SDMdata.quality(data=dots_data_sp1)
SDMPlay:::SDMdata.quality(data=dots_data_sp2)

#12#BRT modelo para la sp1 con 10 replicas#
dots_data_sp1_1 <- SDMPlay:::SDMtab(sp1,layers_2, unique.data = TRUE, same=FALSE, background.nb=48)
brt_sp1_1 <- SDMPlay:::compute.brt(x=dots_data_sp1_1, proj.predictors=layers_2, tc=2, lr=0.001, bf=0.75, n.trees=50)
dots_data_sp1_2 <- SDMPlay:::SDMtab(sp1,layers_2, unique.data = TRUE, same=FALSE, background.nb=48)
brt_sp1_2 <- SDMPlay:::compute.brt(x=dots_data_sp1_2, proj.predictors=layers_2, tc=2, lr=0.001, bf=0.75, n.trees=50)
dots_data_sp1_3 <- SDMPlay:::SDMtab(sp1,layers_2, unique.data = TRUE, same=FALSE, background.nb=48)
brt_sp1_3 <- SDMPlay:::compute.brt(x=dots_data_sp1_3, proj.predictors=layers_2, tc=2, lr=0.001, bf=0.75, n.trees=50)
dots_data_sp1_4 <- SDMPlay:::SDMtab(sp1,layers_2, unique.data = TRUE, same=FALSE, background.nb=48)
brt_sp1_4 <- SDMPlay:::compute.brt(x=dots_data_sp1_4, proj.predictors=layers_2, tc=2, lr=0.001, bf=0.75, n.trees=50)
dots_data_sp1_5 <- SDMPlay:::SDMtab(sp1,layers_2, unique.data = TRUE, same=FALSE, background.nb=48)
brt_sp1_5 <- SDMPlay:::compute.brt(x=dots_data_sp1_5, proj.predictors=layers_2, tc=2, lr=0.001, bf=0.75, n.trees=50)
dots_data_sp1_6 <- SDMPlay:::SDMtab(sp1,layers_2, unique.data = TRUE, same=FALSE, background.nb=48)
brt_sp1_6 <- SDMPlay:::compute.brt(x=dots_data_sp1_6, proj.predictors=layers_2, tc=2, lr=0.001, bf=0.75, n.trees=50)
dots_data_sp1_7 <- SDMPlay:::SDMtab(sp1,layers_2, unique.data = TRUE, same=FALSE, background.nb=48)
brt_sp1_7 <- SDMPlay:::compute.brt(x=dots_data_sp1_7, proj.predictors=layers_2, tc=2, lr=0.001, bf=0.75, n.trees=50)
dots_data_sp1_8 <- SDMPlay:::SDMtab(sp1,layers_2, unique.data = TRUE, same=FALSE, background.nb=48)
brt_sp1_8 <- SDMPlay:::compute.brt(x=dots_data_sp1_8, proj.predictors=layers_2, tc=2, lr=0.001, bf=0.75, n.trees=50)
dots_data_sp1_9 <- SDMPlay:::SDMtab(sp1,layers_2, unique.data = TRUE, same=FALSE, background.nb=48)
brt_sp1_9 <- SDMPlay:::compute.brt(x=dots_data_sp1_9, proj.predictors=layers_2, tc=2, lr=0.001, bf=0.75, n.trees=50)
dots_data_sp1_10 <- SDMPlay:::SDMtab(sp1,layers_2, unique.data = TRUE, same=FALSE, background.nb=48)
brt_sp1_10 <- SDMPlay:::compute.brt(x=dots_data_sp1_10, proj.predictors=layers_2, tc=2, lr=0.001, bf=0.75, n.trees=50)

#13#promediar las 10 predicciones BRT para sp1#
br_sp1 <- mean(brt_sp1_1$raster.prediction,brt_sp1_2$raster.prediction,brt_sp1_3$raster.prediction,
brt_sp1_4$raster.prediction,brt_sp1_5$raster.prediction,brt_sp1_6$raster.prediction,
brt_sp1_7$raster.prediction,brt_sp1_8$raster.prediction,brt_sp1_9$raster.prediction,brt_sp1_10$raster.prediction)

#14#plotear la prediccion promedio BRT para sp1#
plot(br_sp1,main="BRT Sp1 (10 replicates)")

#15#BRT modelo para la sp2 con 10 replicas#
dots_data_sp2_1 <- SDMPlay:::SDMtab(sp2,layers_2, unique.data = TRUE, same=FALSE, background.nb=46)
brt_sp2_1 <- SDMPlay:::compute.brt(x=dots_data_sp2_1, proj.predictors=layers_2, tc=2, lr=0.001, bf=0.75, n.trees=50)
dots_data_sp2_2 <- SDMPlay:::SDMtab(sp2,layers_2, unique.data = TRUE, same=FALSE, background.nb=46)
brt_sp2_2 <- SDMPlay:::compute.brt(x=dots_data_sp2_2, proj.predictors=layers_2, tc=2, lr=0.001, bf=0.75, n.trees=50)
dots_data_sp2_3 <- SDMPlay:::SDMtab(sp2,layers_2, unique.data = TRUE, same=FALSE, background.nb=46)
brt_sp2_3 <- SDMPlay:::compute.brt(x=dots_data_sp2_3, proj.predictors=layers_2, tc=2, lr=0.001, bf=0.75, n.trees=50)
dots_data_sp2_4 <- SDMPlay:::SDMtab(sp2,layers_2, unique.data = TRUE, same=FALSE, background.nb=46)
brt_sp2_4 <- SDMPlay:::compute.brt(x=dots_data_sp2_4, proj.predictors=layers_2, tc=2, lr=0.001, bf=0.75, n.trees=50)
dots_data_sp2_5 <- SDMPlay:::SDMtab(sp2,layers_2, unique.data = TRUE, same=FALSE, background.nb=46)
brt_sp2_5 <- SDMPlay:::compute.brt(x=dots_data_sp2_5, proj.predictors=layers_2, tc=2, lr=0.001, bf=0.75, n.trees=50)
dots_data_sp2_6 <- SDMPlay:::SDMtab(sp2,layers_2, unique.data = TRUE, same=FALSE, background.nb=46)
brt_sp2_6 <- SDMPlay:::compute.brt(x=dots_data_sp2_6, proj.predictors=layers_2, tc=2, lr=0.001, bf=0.75, n.trees=50)
dots_data_sp2_7 <- SDMPlay:::SDMtab(sp2,layers_2, unique.data = TRUE, same=FALSE, background.nb=46)
brt_sp2_7 <- SDMPlay:::compute.brt(x=dots_data_sp2_7, proj.predictors=layers_2, tc=2, lr=0.001, bf=0.75, n.trees=50)
dots_data_sp2_8 <- SDMPlay:::SDMtab(sp2,layers_2, unique.data = TRUE, same=FALSE, background.nb=46)
brt_sp2_8 <- SDMPlay:::compute.brt(x=dots_data_sp2_8, proj.predictors=layers_2, tc=2, lr=0.001, bf=0.75, n.trees=50)
dots_data_sp2_9 <- SDMPlay:::SDMtab(sp2,layers_2, unique.data = TRUE, same=FALSE, background.nb=46)
brt_sp2_9 <- SDMPlay:::compute.brt(x=dots_data_sp2_9, proj.predictors=layers_2, tc=2, lr=0.001, bf=0.75, n.trees=50)
dots_data_sp2_10 <- SDMPlay:::SDMtab(sp2,layers_2, unique.data = TRUE, same=FALSE, background.nb=46)
brt_sp2_10 <- SDMPlay:::compute.brt(x=dots_data_sp2_10, proj.predictors=layers_2, tc=2, lr=0.001, bf=0.75, n.trees=50)

#16#promediar las 10 predicciones BRT para sp2#
br_sp2 <- mean(brt_sp2_1$raster.prediction,brt_sp2_2$raster.prediction,brt_sp2_3$raster.prediction,
brt_sp2_4$raster.prediction,brt_sp2_5$raster.prediction,brt_sp2_6$raster.prediction,
brt_sp2_7$raster.prediction,brt_sp2_8$raster.prediction,brt_sp2_9$raster.prediction,brt_sp2_10$raster.prediction)

#17#plotear las 10 predicciones BRT para sp2#
br_sp2_all <- stack(brt_sp2_1$raster.prediction,brt_sp2_2$raster.prediction,brt_sp2_3$raster.prediction,
brt_sp2_4$raster.prediction,brt_sp2_5$raster.prediction,brt_sp2_6$raster.prediction,
brt_sp2_7$raster.prediction,brt_sp2_8$raster.prediction,brt_sp2_9$raster.prediction,brt_sp2_10$raster.prediction)

#18#plotear la prediccion promedio BRT para sp2#
plot(br_sp2,main="BRT-SDM Sp2 (10 replicates)",legend.args=list(text="Probability", side=3, font=2, cex=0.8))

#19#plotear ambas predicciones promedio#
par(mfrow=c(1,2))
plot(br_sp1,main="BRT-SDM Sp1 (10 replicates)",legend.args=list(text="Probability", side=3, font=2, cex=0.8))
points(sp1, pch = 21, cex = 0.5, bg="red")
plot(br_sp2,main="BRT-SDM Sp2 (10 replicates)",legend.args=list(text="Probability", side=3, font=2, cex=0.8))
points(sp2, pch = 21, cex = 0.5, bg="blue")

#20#MAXIMUM ENTROPY (MAXENT) METHOD#
#20.1#Primero debemos pegar el aplicativo de MAXENT en la carpeta de java del paquete dismo#
system.file("java", package="dismo")

#20.2#instalamos el paquete ENMeval#
#install.packages("ENMeval")#
library(ENMeval)

#20.3#ebemos estimar dos parametros muy importantes: features(f) y regularization multiplier(rm)#
v = seq(from=0.5,to=4,by=0.5)
f = c("L", "LQ", "H", "LQH", "LQHP", "LQHPT")

#21#generamos 1000 puntos aleatorios (background)#
sp1_dots <- SDMPlay:::SDMtab(sp1,layers_2, unique.data=TRUE, same=FALSE, background.nb=10000)
sp1_bg <- sp1_dots[25:10024,2:3]
plot(layers_2[[1]])
points(sp1, pch = 21, cex = 0.8, bg="red")
points(sp1_bg, pch = 21, cex = 0.2, bg="grey")

#22#evaluamos los dos parametros#
enmeval_results_sp1 <- ENMevaluate(sp1,layers_2, method="randomkfold", kfolds=10, algorithm='maxent.jar', bg.coords=sp1_bg,parallel=TRUE,numCores=4,RMvalues=v,fc=f)
enmeval_results_sp1@results
write.csv(enmeval_results_sp1@results, "enmeval_results_sp1.csv")

sp2_dots <- SDMPlay:::SDMtab(sp2,layers_2, unique.data=TRUE, same=FALSE, background.nb=10000)
sp2_bg <- sp2_dots[24:10023,2:3]
plot(layers_2[[1]])
points(sp2, pch = 21, cex = 1, bg="blue")
points(sp2_bg, pch = 21, cex = 0.1, bg="grey")

enmeval_results_sp2 <- ENMevaluate(sp2,layers_2, method="randomkfold", kfolds=10, algorithm='maxent.jar', bg.coords=sp2_bg,parallel=TRUE,numCores=4,RMvalues=v,fc=f)
enmeval_results_sp2@results
enmeval_results_sp2@models
write.csv(enmeval_results_sp2@results, "enmeval_results_sp2.csv")

#23#cargar los raster generados en MAXENT#
sp1_max <- raster("maxent_mapas/sp1_avg.asc")
sp2_max <- raster("maxent_mapas/sp2_avg.asc")

par(mfrow=c(1,2))
plot(sp1_max,main="MAXENT sp1(10 replicates)")
plot(sp2_max,main="MAXENT sp2(10 replicates)")

#24#compacion visual de los rasters de BRT y MAXENT
par(mfrow=c(1,2))
plot(br_sp1,main="BRT-SDM Sp1 (10 replicates)",legend.args=list(text="Habitat\nSuitability", side=3, font=2, cex=0.8))
points(sp1, pch = 21, cex = 0.5, bg="red")
plot(br_sp2,main="BRT-SDM Sp2 (10 replicates)",legend.args=list(text="Habitat\nSuitability", side=3, font=2, cex=0.8))
points(sp2, pch = 21, cex = 0.5, bg="blue")
plot(sp1_max,main="MAXENT sp1(10 replicates)")
points(sp1, pch = 21, cex = 0.5, bg="red")
plot(sp2_max,main="MAXENT sp2(10 replicates)")
points(sp2, pch = 21, cex = 0.5, bg="blue")

writeRaster(br_sp1,filename="BRT_sp1.tif",format="GTiff")
writeRaster(br_sp2,filename="BRT_sp2.tif",format="GTiff")

#25#configurar la data de sp2 para MAXENT#
dim(sp2_dots)
names(sp2_dots)
a <- sp2_dots[,2:10]
species <- c(rep("sp2",23),rep("background",10000))
b <- data.frame(species,a)
names(b)

write.csv(b[1:23,],file="assay_sp2/sp2_dots.csv",row.names=FALSE)
write.csv(b[24:10023,],file="assay_sp2/background.csv",row.names=FALSE)

#26#configurar la data de sp1 para MAXENT#
dim(sp1_dots)
names(sp1_dots)
c <- sp1_dots[,2:10]
species <- c(rep("sp1",24),rep("background",10000))
d <- data.frame(species,c)
names(d)

write.csv(d[1:24,],file="assay_sp1/sp1_dots.csv",row.names=FALSE)
write.csv(d[25:10024,],file="assay_sp1/background.csv",row.names=FALSE)