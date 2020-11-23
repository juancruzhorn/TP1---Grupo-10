# TP 1
# Computación Científica Actuarial

library(e1071)
library(MASS)
library(lattice)
library(PASWR)
library(car)
library(tidyverse)
library(vtreat)
library(data.table)
library(datos)
library(countrycode)
library(hexbin)
library(fastDummies)
library(corrplot)
library(GGally)
library(modeest)

#1.1

#Importar datset
path="C:/Users/jchorn/Dropbox/UBA/2020/C.C. Actuarial/TPs/TP I"
setwd(path)
Dataset<-read.csv2("flavors_of_cacao.csv",na.strings="")
colSums(is.na(Dataset))
Dataset<-Dataset[,-10] #eliminamos columna 10 que tiene solo valores NA
# para poder analizar los datos de porcentaje, primero tenemos que hacer que sean datos numericos
Dataset$Cocoa.Percent<-as.numeric(str_sub(Dataset$Cocoa.Percent,1,2))/100
#Datos Na
Dataset[Dataset=="ÿ"] <- NA # A los datos faltantes les asignamos la nomenclatura NA asi no genera inconvenientes en el futuro analisis
#Reordenamos la matriz para simplificar su tratamiento, dejando todos los datos numéricos del lado derecho
setcolorder(Dataset,c(1,2,6,8,9,3,4,5,7))

#En las secciones de países normalizaremos los datos y agregaremos una columna con los continentes de origen
#Estas columnas serán utilizadas en consideraciones ulteriores para homogeneizar los datos
Dataset$Company.Location<-countrycode(Dataset$Company.Location,origin="country.name",destination="iso3c")
Dataset$Broad.Bean.Origin<-countrycode(Dataset$Broad.Bean.Origin,origin="country.name",destination="iso3c")
Dataset<-data.frame(Dataset,countrycode(Dataset$Company.Location,origin="iso3c",destination="continent"))
Dataset<-data.frame(Dataset,countrycode(Dataset$Broad.Bean.Origin,origin="iso3c",destination="continent"))
colnames(Dataset)[10]="Continente Compañía"
colnames(Dataset)[11]="Continente Grano"

#Outliers 

# Primero realizamos los gráficos de caja y bigotes para detectar la presencia de Outliars en las columnas del Dataset
par(mfrow=c(2,2))
j<-6:9
for (i in j) {
    columna<-Dataset[,i]
    nombre<-names(Dataset)[i]
    boxplot(columna,main= paste ("Campo",nombre))
}

# Quitamos los outliers del subset de Cocoa.Percent
par(mfrow=c(1,2))
boxplot(Dataset$Cocoa.Percent)

q<-t(matrix(quantile(Dataset[,8])))
longitud<-length(Dataset$Cocoa.Percent)
limiteinferior<-q[,2]-(IQR(Dataset[,8])*1.5)
limitesuperior<-q[,3]+(IQR(Dataset[,8])*1.5)
boxplot.stats(Dataset$Cocoa.Percent)

length(boxplot.stats(Dataset$Cocoa.Percent)$out)
Dataset$Cocoa.Percent[Dataset$Cocoa.Percent<limiteinferior]<-mean(Dataset$Cocoa.Percent)
Dataset$Cocoa.Percent[Dataset$Cocoa.Percent>limitesuperior]<-median(Dataset$Cocoa.Percent)
boxplot(Dataset[,8])

# Quitamos los outliers del subset de Rating
par(mfrow=c(1,2))
boxplot(Dataset$Rating)

q<-t(matrix(quantile(Dataset$Rating)))
longitud<-length(Dataset$Rating)

limiteinferior<-q[,2]-(IQR(Dataset$Rating)*1.5)
limitesuperior<-q[,3]+(IQR(Dataset$Rating)*1.5)
boxplot.stats(Dataset$Rating)


length(boxplot.stats(Dataset$Rating)$out)
Dataset$Rating[Dataset$Rating<limiteinferior]<-mean(Dataset$Rating)
Dataset$Rating[Dataset$Rating>limitesuperior]<-median(Dataset$Rating)
boxplot(Dataset$Rating)

#Realizamos nuevamente el gráfico de caja y bigote para comparar el resultado de la corrección
par(mfrow=c(2,2))
j<-6:9
for (i in j) {
  columna<-Dataset[,i]
  nombre<-names(Dataset)[i]
  boxplot(columna,main= paste ("Campo",nombre))
}

##Ahora que ya no tenemos datos outliers podemos calcular lo solicitado 

par(mfrow=c(2,2))
EDA(Dataset$REF)
EDA(Dataset$Review.Date)
EDA(Dataset$Cocoa.Percent)
EDA(Dataset$Rating)
mfv(Dataset$Rating)
mfv(Dataset$Review.Date)
mfv(Dataset$REF)
mfv(Dataset$Cocoa.Percent)

#1.2 Vamos a realizar gráficos para encontrar relaciones entre las variables.

#Relación entre Rating y el origen de los granos 
pie(table(Dataset$`Continente Grano`),main="Continentes origen del grano")
rat.or.gran <- table(Dataset$`Continente Grano`, Dataset$Rating)
barplot(rat.or.gran, legend= rownames(table(Dataset$`Continente Grano`)))
assocplot(prop.table(table(Dataset$Rating,Dataset$`Continente Grano`)))

#Relación entre Rating y continente de la compañía
pie(table(Dataset$`Continente Compañía`),main="Continentes origen de la compañia")
rat.or.emp <- table(Dataset$`Continente Compañía`, Dataset$Rating)
barplot(rat.or.emp, legend= rownames(table(Dataset$`Continente Compañía`)))
assocplot(prop.table(table(Dataset$Rating,Dataset$`Continente Compañía`)))

#Relación entre Date y REF
ref.date<- hexbin(Dataset$Review.Date, Dataset$REF, xbins=50)
plot(ref.date)
     
#Relación entre Rating y Date
rat.date<- hexbin(Dataset$Review.Date, Dataset$Rating, xbins=50)
plot(rat.date)

# Relación entre Rating y Cocoa percent
rat.per<- hexbin(Dataset$Cocoa.Percent, Dataset$Rating, xbins=50)
plot(rat.per)


###########################################
#Punto 2
#2.1

#2.2
#Por la decisión tomada con outliers y NA volvemos a cargar el Dataset
path="C:/Users/jchorn/Dropbox/UBA/2020/C.C. Actuarial/TPs/TP I"
setwd(path)
Dataset<-read.csv2("flavors_of_cacao.csv",na.strings="")
colSums(is.na(Dataset))
Dataset<-Dataset[,-10] #eliminamos columna 10 que tiene solo valores NA
# para poder analizar los datos de porcentaje, primero tenemos que hacer que sean datos numericos
Dataset$Cocoa.Percent<-as.numeric(str_sub(Dataset$Cocoa.Percent,1,2))/100
#Datos Na
Dataset[Dataset=="ÿ"] <- NA # A los datos faltantes les asignamos la nomenclatura NA asi no genera inconvenientes en el futuro analisis

#Reordenamos la matriz para simplificar su tratamiento, dejando todos los datos numéricos del lado derecho
setcolorder(Dataset,c(1,2,6,8,9,3,4,5,7))

#En las secciones de países normalizaremos los datos y agregaremos una columna con los continentes de origen

Dataset$Company.Location<-countrycode(Dataset$Company.Location,origin="country.name",destination="iso3c")
Dataset$Broad.Bean.Origin<-countrycode(Dataset$Broad.Bean.Origin,origin="country.name",destination="iso3c")
Dataset<-data.frame(Dataset,countrycode(Dataset$Company.Location,origin="iso3c",destination="continent"))
Dataset<-data.frame(Dataset,countrycode(Dataset$Broad.Bean.Origin,origin="iso3c",destination="continent"))
colnames(Dataset)[10]="Continente Compañía"
colnames(Dataset)[11]="Continente Grano"

#Creamos dos dataset para manipular y resguardar el archivo original
Dataset.bin <- Dataset
Dataset.Transformer<-Dataset

#Transformación a binario
Dataset.bin$Rating[Dataset.bin$Rating!=5] <- 0
Dataset.bin$Rating[Dataset.bin$Rating==5] <- 1
#Creación de lista
list.data<-list(Dataset.Transformer,Dataset.bin)

for(i in 10:11)
{
  Dataset.Transformer [is.na( Dataset.Transformer [,i]) , i] <- mean ( Dataset.Transformer$Rating, na.rm = TRUE )
}
Dataset.Transformer [is.na( Dataset.Transformer [,4]) , 4] <- mean ( Dataset.Transformer$Rating, na.rm = TRUE )

#Variable continua
#Transformación variables cualitativas a número -> promedio de rating por subset

#Calculamos los promedios del rating por categoría de variable
promedios.granos.continente<-aggregate(Dataset.Transformer$Rating,by=list(Dataset.Transformer$`Continente Grano`),mean,na.rm=TRUE)
promedios.fabricacion.continente<-aggregate(Dataset.Transformer$Rating,by=list(Dataset.Transformer$`Continente Compañía`),mean,na.rm=TRUE)
promedios.tipo.grano<-aggregate(Dataset.Transformer$Rating,by=list(Dataset.Transformer$Bean.Type),mean,na.rm=TRUE)

# Reemplazaremos los NA por el valor de la media de rating


#Realizamos las transformación de la matriz
Dataset.Transformer[,10][Dataset.Transformer[,10]=="Africa"]<-promedios.fabricacion.continente[1,2]
Dataset.Transformer[,10][Dataset.Transformer[,10]=="Americas"]<-promedios.fabricacion.continente[2,2]
Dataset.Transformer[,10][Dataset.Transformer[,10]=="Asia"]<-promedios.fabricacion.continente[3,2]
Dataset.Transformer[,10][Dataset.Transformer[,10]=="Europe"]<-promedios.fabricacion.continente[4,2]
Dataset.Transformer[,10][Dataset.Transformer[,10]=="Oceania"]<-promedios.fabricacion.continente[5,2]
#La columna era de tipo texto, la coercionamos a valor numérico
Dataset.Transformer$`Continente Compañía`<-as.numeric(Dataset.Transformer$`Continente Compañía`) 

Dataset.Transformer[,11][Dataset.Transformer[,11]=="Africa"]<-promedios.granos.continente[1,2]
Dataset.Transformer[,11][Dataset.Transformer[,11]=="Americas"]<-promedios.granos.continente[2,2]
Dataset.Transformer[,11][Dataset.Transformer[,11]=="Asia"]<-promedios.granos.continente[3,2]
Dataset.Transformer[,11][Dataset.Transformer[,11]=="Europe"]<-promedios.granos.continente[4,2]
Dataset.Transformer[,11][Dataset.Transformer[,11]=="Oceania"]<-promedios.granos.continente[5,2]
Dataset.Transformer$`Continente Grano`<-as.numeric(Dataset.Transformer$`Continente Grano`)

as.data.frame(promedios.tipo.grano)

for (i in  1:40)
     {
      comparativo<-promedios.tipo.grano[i,1]
      print(i)
      print(comparativo)
      Dataset.Transformer[,4][Dataset.Transformer[,4]==comparativo]<-promedios.tipo.grano[i,2]
}
Dataset.Transformer$Bean.Type<-as.numeric(Dataset.Transformer$Bean.Type)

Dataset.Transformer<-Dataset.Transformer[,-c(1,2,3,5,6)]


#######Variable Binaria#######
# Para no generar un gran cantidad de columnas, utilizaremos las columnas agregadas de "Continentes" y omitiremos las referencias a países
Dataset.bin.dummy<-dummy_cols(Dataset.bin,select_columns=c("Bean.Type","Continente Compañía","Continente Grano"),ignore_na=TRUE) %>% select(-c(1,2,3,4,5,10,11))
#Cambiamos los valores NA a 0
for(i in 1:53)
{
  Dataset.bin.dummy [is.na( Dataset.bin.dummy [,i]) , i] <- 0 
}

#Guardado en nueva lista
list.dat.transformadas<-list(Dataset.Transformer,Dataset.bin.dummy)

#2.3
#Generación y armados de particiones

#Modelo binario con variables dummy#
# Partición set binario original
muestra<-sample(c(TRUE,FALSE),nrow(Dataset.bin.dummy),replace=TRUE,prob=c(0.7,0.3))
entrenamiento.bin<-Dataset.bin[muestra,]
prueba.bin<-Dataset.bin[!muestra,]
# Control de partición I
nrow(entrenamiento.bin)+nrow(prueba.bin)

#Partición set binario dummy
entrenamiento.bin.dummy<-Dataset.bin.dummy[muestra,]
prueba.bin.dummy<-Dataset.bin.dummy[!muestra,]
#Control de partición II
nrow(prueba.bin.dummy)+nrow(entrenamiento.bin.dummy)

#Partición set continuo original
muestra<-sample(c(TRUE,FALSE),nrow(Dataset),replace=TRUE,prob=c(0.7,0.3))

#Partición set continuo transformado
entrenamiento.lineal<-Dataset.Transformer[muestra,]
prueba.lineal<-Dataset.Transformer[!muestra,]
# Control de partición
nrow(prueba.lineal)+nrow(entrenamiento.lineal)

#Guardado de listas partidas
list.bin.partío<-list(entrenamiento.bin,prueba.bin)
list.bin.dummy.partío<-list(entrenamiento.bin.dummy,prueba.bin.dummy)
list.lineal<-
list.bin<-list(list.bin.partío,list.bin.dummy.partío)


#2.4
#Regresión lineal - Modelo continuo
modelo.lineal.entrenamiento<-lm(entrenamiento.lineal$Rating~.,data=entrenamiento.lineal)
summary(modelo.lineal.entrenamiento)
ggpairs(entrenamiento.lineal, lower = list(continuous = "smooth"), 
        diag = list(continuous = "bar"), axisLabels = "none")

modelo.lineal.prueba<-lm(prueba.lineal$Rating~.,data=prueba.lineal)
summary(modelo.lineal.prueba)
ggpairs(prueba.lineal, lower = list(continuous = "smooth"), 
        diag = list(continuous = "bar"), axisLabels = "none")

modelo.lineal.poblacion<-lm(Dataset.Transformer$Rating~.,data=Dataset.Transformer)
summary(modelo.lineal.poblacion)
#Gráfico de correlaciones, histogramas y dispersión
ggpairs(Dataset.Transformer, lower = list(continuous = "smooth"), 
        diag = list(continuous = "bar"), axisLabels = "none")

# Armado de histograma de los residuos de la población
par(mfrow=c(1,1))
control.poblacion<-data.frame(modelo.lineal.poblacion$residuals,modelo.lineal.poblacion$fitted.values)
hist(control.poblacion$modelo.lineal.poblacion.residuals,breaks=10,xlab ="Residuos",main="Histograma de residuos")

########Regresión logística - Modelo Binario###########

modelo.log.entrenamiento<-glm(entrenamiento.bin.dummy$Rating~.,data=entrenamiento.bin.dummy,family="binomial")
summary(modelo.log.entrenamiento)
modelo.log.prueba<-glm(prueba.bin.dummy$Rating~.,data=prueba.bin.dummy,family="binomial")
summary(modelo.log.prueba)
modelo.log.poblacion<-glm(Dataset.bin.dummy$Rating~.,data=Dataset.bin.dummy,family="binomial")
summary(modelo.log.poblacion)

#Prueba de ANOVA para el modelo logístico
anova(modelo.log.entrenamiento,test='Chisq')
#Son significativos el Review Dato y tipo de Cacao Blend
