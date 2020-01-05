
#############################################
# Tipología y ciclo de vida de los datos    #
# PRA2                                      #
# Jesica Piñón                              #
#############################################

#===========================================
# Fijamos el directorio de trabajo
#===========================================
setwd("E:/UOC_MASTER/3_SEMESTRE/TIPOLOGIA_CICLO_VIDA_DATOS/PRACS/PRA2")


#===========================================
# Cargamos librerías necesarias -  Se instalan los paquetes automáticamente si la librería no es encontrada!
#===========================================
is.installed <- function(mypkg) is.element(mypkg, installed.packages()[,1])
if (!is.installed("ggplot2")){install.packages("ggplot2",repos="http://cran.r-project.org")}

if (!is.installed("data.table")){install.packages("data.table",repos="http://cran.r-project.org")}

if (!is.installed("rmarkdown")){install.packages("rmarkdown",repos="http://cran.r-project.org")}


if (!is.installed("corrplot")){install.packages("rmarkdown",repos="http://cran.r-project.org")}

suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(rmarkdown))
suppressPackageStartupMessages(library(corrplot))

print ("Librerías cargadas\n")

#===========================================
# Cargamos los datos
#===========================================
data.dt <- as.data.table(read.csv("diamonds.csv", na.strings=""))

#===========================================
# Analizamos la estructura de los datos
#===========================================
# Comprobamos atributos y clases
str(data.dt) 
summary(data.dt)

#===========================================
# Eliminamos variables
#===========================================
# Hay variables que no nos aportan información como la 'X' que es un contador. 
data.dt$X <- NULL

#===========================================
# Eliminamos las filas duplicadas
#===========================================
print(paste0("El número de filas duplicadas es: ",nrow(data.dt[duplicated(data.dt),])))
data.dt <- unique(data.dt)

# Analizamos las variables categóricas un poco más.
unique(data.dt$clarity)
unique(data.dt$cut)
unique(data.dt$color)

#===========================================
# Creación de nuevos atributos
#===========================================
# Creación de la variable 'Vol' - volumen del diamante.
data.dt[,vol:= round(x*y*z,1),]

# Comprobamos límites:
min(data.dt$vol)
# El volumen no puede ser 0; eliminamos esos casos:
data.dt <- data.dt[vol != 0]
min(data.dt$vol)
max(data.dt$vol)

summary(data.dt$vol)

#===========================================
# Eliminamos los atributos innecesarios
#===========================================
data.dt$x <- data.dt$y <- data.dt$z <- NULL

#===========================================
# ACP
#===========================================
acp <- prcomp(data.dt[,c(1,5:6,8)],center=T,scale=T)
print(acp)
summary(acp)

data.dt$vol <- NULL

#===========================================
# Datos perdidos
#===========================================
summary(data.dt) 

colSums(is.na(data.dt)) # No hay valores nulos.


#===========================================
# Outliers
#===========================================
#PRICE
boxplot(data.dt$price)
print (paste0("Representan un porcentaje de ",round((nrow(data.dt[price > 3*sd(data.dt$price)])/nrow(data.dt))*100,2),"% del total de los datos"))

##CARAT
boxplot(data.dt$carat)
print (paste0("Representan un porcentaje de ",round((nrow(data.dt[carat > 3*sd(data.dt$carat)])/nrow(data.dt))*100,2),"% del total de los datos"))

#===========================================
# Guardamos fichero tras realizar la limpieza
#===========================================
write.csv(data.dt,"diamonds_final.csv",row.names=F)

#===========================================
# Test de normalidad de Kolmogorov-Simnorv
#===========================================
cols <- names(data.dt)[c(1,5:7)]
file <- data.dt[,cols,with=F]
for (i in c(1:length(cols))){
  print(apply(file[,i,with=FALSE],2,function(x) ks.test(x,pnorm,mean(x),sd(x),exact=F)))
}

ggplot(data.dt,aes(price))+stat_density()

#===========================================
# Test de homogeneidad de Fligner-Killen
#===========================================
fligner.test(price ~ carat, data=data.dt)
fligner.test(price ~ table, data=data.dt)
fligner.test(price ~ depth, data=data.dt)

#===========================================
# Correlación
#===========================================
data.dt <- as.data.table(data.dt)
table_cor <- cor(data.dt[,cols,with=F],method="spearman",exact=F)
table_cor
corrplot(table_cor,title="Correlación entre variables",mar=c(0,0,5,0),tl.offset = 1)

#===========================================
# Correlación
#===========================================
cor.test(data.dt$price,data.dt$carat,method="spearman",exact=F)

#===========================================
# Regresión lineal
#===========================================
a <- lm(price ~ carat, data=data.dt)
print(paste0("Regresión lineal entre price y carat. r2: ",summary(a)$r.squared))

b <- lm(price ~ cut, data=data.dt)
print(paste0("Regresión lineal entre price y cut. r2: ",summary(b)$r.squared))

c <- lm(price ~ clarity, data=data.dt)
print(paste0("Regresión lineal entre price y clarity. r2: ",summary(c)$r.squared))

d <- lm(price ~ color, data=data.dt)
print(paste0("Regresión lineal entre price y color. r2: ",summary(d)$r.squared))

#===========================================
# Predicción del precio del diamante en función de su peso en quilates.
#===========================================
# Creamos un dataset con valores de peso en quilates.
peso <- data.table(carat=c(0.2,0.3,4.7,3.8))
predict(a,newdata=peso)



#===========================================
# Test de Mann-Whitney
#===========================================
# Carat
# Creamos dos grupos con los precios según tengan un peso u otro en quilates. 
data.carat.low <- data.dt[carat < 3]$price
data.carat.high <- data.dt[carat >= 3]$price
wilcox.test(data.carat.low,data.carat.high,alternative="less")

# Cut
# Creamos dos grupos con los precios según la calidad del tipo de corte. 
data.cut.low <- data.dt[cut != 'Premium']$price
data.cut.high <- data.dt[cut == 'Premium']$price
wilcox.test(data.cut.low,data.cut.high,alternative="less")

# Clarity
# Creamos dos grupos con los precios según tengan una categoría de claridad. 
data.clarity.low <- data.dt[clarity %in% c("I1","SI1","SI2","VS2")]$price
data.clarity.high <- data.dt[clarity %in% c("VS1","V","SI2","VS2")]$price
wilcox.test(data.clarity.low,data.clarity.high,alternative="less")

# Depth
# Creamos dos grupos con los precios según la profundidad del diamante. 
data.depth.low <- data.dt[depth <= 65]$price
data.depth.high <- data.dt[depth > 65]$price
wilcox.test(data.depth.low,data.depth.high,alternative='less')

# Table
# Creamos dos grupos con los precios según tengan un ancho de tabla de diamante. 
data.table.low <- data.dt[table <= 75]$price
data.table.high <- data.dt[table > 75]$price
wilcox.test(data.table.low,data.table.high,alternative='less')