################################################################################

# Ejercicio 1:
library(factoextra)
library(corrplot)
# Cargamos la base de datos:
fp=file.path("C:/Users/Jose Fuentes/Desktop/Posgrado/Tercer Semestre/Aprendizaje estadístico automatizado/Tareas/Tarea 3/Base de datos/student_score.csv")
bd=read.csv(fp)
bd=data.frame(bd)

# THETA
# (I) Calculamos Theta:

# Correlacion entre todas las variables:
cor(bd)

# Definimos Theta:
theta=cov(bd[,3],bd[,5])/(sd(bd[,3])*sd(bd[,5]))
theta

# (II) Estimamos la sd usando Bootstrap:

# Calculamos el valor estimado de la desviacion estandar:
boot=rep(0,4000)
hc=bd
for(i in 1:4000){
  ba=hc[sample(22,22,replace = T),]
  boot[i]=cor(ba[,3],ba[,5])
}    
sd(boot)

# Calculamos la media:
m_es=mean(boot)
m_es

# Calculamos el sesgo:
mean(boot)-theta 

# Graficamos el histograma para nuestro bootstap:
qplot(boot,geom = "histogram",bins=20
)+geom_vline(xintercept = mean(boot))

# Realizamos el scatter plot de nuestro Bootstrap:
plot(boot)


# (III)  Calculamos los  IC's por bootstrap, el naive y el de los cuantiles 
#de la distribucion acumulada empirica:

# Intervalo de confianza de los cuantiles de la distribucion acumulada empirica:
quantile(boot, p=c(0.025, 0.975))


# Hacemos un test de normalidad:
shapiro.test(boot)

# Intervalo de confianza basico:
lq=qnorm(1-0.025)
uq=qnorm(1-0.025)
seb=sd(boot)
l.ic=theta-lq*seb
u.ic=theta+uq*seb
cat("El intervalo de confianza del",95,"% para la media es:(",l.ic,",",u.ic,")\n")

# (IV) Calculamos los intervalos de confianza  a traves de un paquete de computo:

# El estimador para theta sera el siguiente:
theta_fun=function(q,indexes){
  ba=q[sample(22,22,replace = T),]
  boot[indexes]=cor(ba[,3],ba[,5])
  return(boot[indexes])
}

# Empleamos el Bootstrap de R:
g=boot(bd,theta_fun,4000)
sd(g$t)

# Graficamos el histograma del bootstap de R:
qplot(g$t[,1],geom = "histogram",bins=20
)+geom_vline(xintercept = mean(g$t[,1]))

# Realizamos el scatter plot de nuestro Bootstrap:
plot(g$t[,1])
# Realizamos la grafica para el resultado del bootstrap de R:
plot(g)

# Calculamos los IC's:
ci=boot.ci(g, conf=c(0.95),
        type=c("norm","basic", "perc","bca"))

#Graficamos sobre los diferentes ICs sobre los histogramas:
g$t0
plot(g)

hist(g$t,breaks =500,  ylim=c(0,900), col = "lightblue",main = "Histograma con los ICs",xlab ="",ylab = "")
abline(v = g$t0, col="orange", lwd=1)
abline(v = c(ci$normal[,2:3]) , col=c("red"), lwd=2,lty=2)
abline(v = ci$basic[,4:5] , col="blue", lwd=2, lty=2)
abline(v = c(ci$percent[,4:5]) , col=c("black"), lwd=2,lty=2)
abline(v = ci$bca[,4:5] , col="green", lwd=2, lty=2)

# Agregamos las leyendas:
legend("topleft", 
       legend = c("Normal", "Basic","Percent","bca"), 
       col = c("red", "blue","black","green","purple"), 
       pch = c(16,16,16,16,16), 
       bty = "n",
       pt.cex = 0.8, 
       cex = 0.8, 
       text.col = "black", 
       horiz = F ) 

# GAMMA 
# (I) Calculamos Gamma:

# Definimos Gamma:
a=cor(bd)
eig=eigen(a)
gamma=max(eig$values)/min(eig$values)

# (II) Estimamos la sd usando Bootstrap:

# Calculamos el valor estimado de la desviacion estandar:
boot1=rep(0,4000)
maxi=rep(0,4000)
mini=rep(0,4000)
hc=bd
for(i in 1:4000){
  ba=hc[sample(22,22,replace = T),]
  a=cor(ba)
  eig=eigen(a)
  maxi[i]=max(eig$values)
  mini[i]=min(eig$values)
  boot1[i]=max(eig$values)/min(eig$values)
}

sd(boot1)

min(mini)
maxi
max(maxi)
length(which(mini<0.053))

# Calculamos la media:
m_es1=mean(boot1)
m_es1

# Calculamos el sesgo:
mean(boot1)-gamma 

# Graficamos el histograma para nuestro bootstap:
qplot(boot1,geom = "histogram",bins=20
)+geom_vline(xintercept = mean(boot1))

# Realizamos el scatter plot de nuestro Bootstrap:
plot(boot1)


# (III)  Calculamos los  IC's por bootstrap, el naive y el de los cuantiles 
#de la distribucion acumulada empirica:

# Intervalo de confianza de los cuantiles de la distribucion acumulada empirica:
quantile(boot1, p=c(0.025, 0.975))

# Intervalo de confianza basico:
lq=qnorm(1-0.025)
uq=qnorm(1-0.025)
seb=sd(boot1)
l.ic=gamma-lq*seb
u.ic=gamma+uq*seb
cat("El intervalo de confianza del",95,"% para la media es:(",l.ic,",",u.ic,")\n")

# (IV) Calculamos los intervalos de confianza  a traves de un paquete de computo:

# El estimador para theta sera el siguiente:
gamma_fun=function(q,indexes){
  ba=q[sample(22,22,replace = T),]
  a=cor(ba)
  eig=eigen(a)
  boot[indexes]=max(eig$values)/min(eig$values)
  return(boot[indexes])
}

# Empleamos el Bootstrap de R:
g1=boot(bd,gamma_fun,4000)
sd(g1$t)

# Graficamos el histograma del bootstap de R:
qplot(g1$t[,1],geom = "histogram",bins=20
)+geom_vline(xintercept = mean(g1$t[,1]))

# Realizamos el scatter plot de nuestro Bootstrap:
plot(g1$t[,1])

# Realizamos la grafica para el resultado del bootstrap de R:
plot(g1)

# Calculamos wl IC:
ci=boot.ci(g1, conf=c(0.95),
        type=c("norm","basic", "perc","bca"))
hist(g1$t,breaks =500,xlim = c(-100,400),  ylim=c(0,3200), col = "lightblue",main = "Histograma con los ICs",xlab ="",ylab = "")
abline(v = g1$t0, col="orange", lwd=1)
abline(v = c(ci$normal[,2:3]) , col=c("red"), lwd=2,lty=2)
abline(v = ci$basic[,4:5] , col="blue", lwd=2, lty=2)
abline(v = c(ci$percent[,4:5]) , col=c("black"), lwd=2,lty=2)
abline(v = ci$bca[,4:5] , col="green", lwd=2, lty=2)

# Agregamos las leyendas:
legend("topright", 
       legend = c("Normal", "Basic","Percent","bca"), 
       col = c("red", "blue","black","green","purple"), 
       pch = c(16,16,16,16,16), 
       bty = "n",
       pt.cex = 0.8, 
       cex = 0.8, 
       text.col = "black", 
       horiz = F ) 

################################################################################
# Ejercicio 2:

library(MASS)
library(FreqProf)
library(readr)
library(bcaboot)
library(corrplot)
library(dplyr)
library(boot)

######## Modelos a comprobar:

# Cargamos la base de datos:
fp=file.path("C:/Users/Jose Fuentes/Desktop/Posgrado/Tercer Semestre/Aprendizaje estadístico automatizado/Tareas/Tarea 3/Base de datos/diabetes.csv")
diabetes2=read.csv(fp)
diabetes2=data.frame(diabetes2)

# Realizamos el modelo tomando sex como factor:
diabetes2$sex<-as.factor(diabetes2$sex)

# Comprobamos las correlaciones:
C<-cor(select(diabetes2, -sex))
corrplot(C,method="color",addCoef.col="black", type="upper")

# Tabla de contingencia para la variable sexo:
table(dia$prog,dia$sex)
sum(table(dia$prog,dia$sex)[1:107,1])
sum(table(dia$prog,dia$sex)[1:107,2])

sum(table(dia$prog,dia$sex)[108:214,1])
sum(table(dia$prog,dia$sex)[108:214,2])

# Realizamos el 1er modelo:
modelo0<-lm(prog~ . , data = diabetes2 )

# Hacemos el summary:
summary(modelo0)

# Modelo con interaccion:
modelo<-lm((prog)^(0.4646)~ .+sex:bmi , data = diabetes2 )
summary(modelo)

# Cargamos nuevamente el data set sin tomar sex como categorica:
diabetes2=read.csv(fp)
diabetes2=data.frame(diabetes2)

# Comprobamos las correlaciones:
C<-cor(diabetes2)
corrplot(C,method="color",addCoef.col="black", type="upper")

# Modelo aditivo usado en el papper:
modelo1<-lm(prog~ . , data = diabetes2 ) 

# Realizamos el summary:
summary(modelo1) #R^2ad=0.506

# Graficamos:
plot(modelo1)

### Analizamos otros modelos:

# Realizamos el algoritmo de BOX:
bc<-boxcox(modelo1)

# Calculamos el valor de lambda:
lambda <- bc$x[which.max(bc$y)] 
lambda

# Hacemos un 3er modelo:
modelo2<-lm((diabetes2$prog)^(0.4646)~ . , data = diabetes2)

# Hacemos el summary:
summary(modelo2) #No tiene un mejora el  R^2_ajustado.

# Graficamos:
plot(modelo2)

#Modelo con las variables significativas
modelo3<- lm(prog~ sex+bmi+map+tc+ltg, data = diabetes2 )
summary(modelo3)
plot(modelo3)

# Modelo con transformacion sobre las Ys:
modelo3<-lm(prog^(0.4646) ~ sex+bmi+map+tc+ltg, data = diabetes2)
summary(modelo3)


#R^2 ajustado por Bootstrap:

# Cargamos los datos:
fp=file.path("C:/Users/Jose Fuentes/Desktop/Posgrado/Tercer Semestre/Aprendizaje estadístico automatizado/Tareas/Tarea 3/Base de datos/diabetes.csv")
diabetes2=read.csv(fp)
diabetes2=data.frame(diabetes2)

?bcajack
set.seed(1234)

# Tomamos el dataset como matriz:
Xy <- as.matrix(diabetes2)

# Tomamos la funcion sugerida por el paquete:.
rfun <- function(Xy,i) {
  dt=Xy[i,]
  y = dt[, 11] 
  X = dt[, 1:10]
  summary(lm(y~X) )$adj.r.squared
}

# Bootstrap con B=2000:
set.seed(1234)
B=2000
r2boot<- boot(Xy, rfun , R=B)

#Test de Shapiro:
shapiro.test(r2boot$t)

# Porcentaje de observaciones por debajo de 0.507:
length(which(r2boot$t < 0.507))/length(r2boot$t < 0.507) 

# Indices usados:
tabladeindices<-boot.array(r2boot, indices=T) 
plot(r2boot,index=1)

# ICs al 95 con diferentes metodos:
ci.boot<-boot.ci(r2boot, conf= 0.95,
                 type=c("norm","basic", "perc","bca")) 

# ICs hechos por bootstrap del articulo:
z<-bcajack(x = Xy, B = r2boot$t, func = rfun, verbose = F)

#Graficamos:
bcaplot(z) 
z

# ICs generados por el paquete del articulo (automatico):
## n = 442 = 34 * 13
bcajack(x = Xy, B = 2000, func = rfun, m=34, verbose = FALSE)

#Graficamos sobre los diferentes ICs sobre los histogramas:
r2boot$t0
plot(r2boot)

hist(r2boot$t,breaks =35, xlim=c(0.4,0.64), ylim=c(0,140), col = "lightblue",main = "Histograma con los ICs",xlab ="",ylab = "")
abline(v = r2boot$t0, col="black", lwd=1)
abline(v = c(ci.boot$normal[,2:3]) , col=c("red"), lwd=2,lty=2)
abline(v = ci.boot$basic[,4:5] , col="blue", lwd=2, lty=2)
abline(v = c(ci.boot$percent[,4:5]) , col=c("black"), lwd=2,lty=2)
abline(v = ci.boot$bca[,4:5] , col="green", lwd=2, lty=2)
abline(v = c(0.4227,0.5581) , col="purple", lwd=2, lty=2) #bca con el paquete es extra solo para verificar

# Agregamos las leyendas:
legend("topright", 
       legend = c("Normal", "Basic","Percent","bca","bca_artículo"), 
       col = c("red", "blue","black","green","purple"), 
       pch = c(16,16,16,16,16), 
       bty = "n",
       pt.cex = 0.8, 
       cex = 0.8, 
       text.col = "black", 
       horiz = F ) 

# Valores correspondientes a diferentes alphas:
z$lims

ci.boot$

# Ahora hacemos lo anterior con B=5000,10000: 
set.seed(123)

# Fijamos nuestro B:
B2=5000

# Realizamos Bootstrap:
r2boot2<- boot(Xy, rfun , R=B2)

# Guardamos los indicesen una tabla:
tabladeindices2<-boot.array(r2boot2, indices=T)

# Graficamos el Bootstrap:
plot(r2boot2,index=1)

# Calculamos los ICs al 95:
ci.boot2<-boot.ci(r2boot2, conf=0.95,
                  type=c("norm","basic", "perc","bca"))

# Usamos el paquete del articulo:
z2<-bcajack(x = Xy, B = r2boot2$t, func = rfun, verbose = FALSE)

# Graficamos:
bcaplot(z2)

# Comprobamos la media:
z2

# Usamos nuevamente la funcion bcajack:
bcajack(x = Xy, B = 5000, func = rfun, m=34, verbose = FALSE)

# Realizamos el histograma y representamos los ICs:
hist(r2boot2$t, breaks=100, col = "lightblue",main = "Histograma con los ICs",xlab ="",ylab = "")
abline(v = r2boot2$t0, col="black", lwd=1)
abline(v = c(ci.boot2$normal[,2:3]) , col=c("red"), lwd=2,lty=2)
abline(v = ci.boot2$basic[,4:5] , col="blue", lwd=2, lty=2)
abline(v = c(ci.boot2$percent[,4:5]) , col=c("black"), lwd=2,lty=2)
abline(v = ci.boot2$bca[,4:5] , col="green", lwd=2, lty=2)
abline(v = c(0.4253,0.5581) , col="purple", lwd=2, lty=2) #bca con el paquete extra solo para verificar

# Agregamos la leyenda:
legend("topright", 
       legend = c("Normal", "Basic","Percent","bca","bca_artículo"),
       col = c("red", "blue","black","green","purple","purple"),
       pch = c(16,16,16,16,16), 
       bty = "n",
       pt.cex = 0.8, 
       cex = 0.8, 
       text.col = "black", 
       horiz = F ) 

# Comprobamos los valores para diferentes alphas:
z$lims

# Fijamos nuestro valores de B:
B3=10000

# Realizamos Bootstrap para el nuevo valor de B:
r2boot3<- boot(Xy, rfun , R=B3)

# Guadamos los indices con los que estamos trabajando:
tabladeindices3<-boot.array(r2boot3, indices=T)

# Graficamos los resultados del Bootstrap:
plot(r2boot3,index=1)

# Calculamos los ICs al 95:
ci.boot3<-boot.ci(r2boot3, conf=0.95,
                  type=c("norm","basic", "perc","bca"))

# Usamos la funcion bcajack del paquete:
z3<-bcajack(x = Xy, B = r2boot3$t, func= rfun, verbose = FALSE)

# Vemos los limites del IC:
z3$lims

# Graficamos:
bcaplot(z3)

# Corremos nuevamente la funcion para B:
bcajack(x = Xy, B = 10000, func = rfun, m=34, verbose = FALSE)

hist(r2boot3$t, breaks=90, col = "lightblue",main = "Histograma con los ICs",xlab ="",ylab = "")
abline(v = r2boot3$t0, col="black", lwd=1)
abline(v = c(ci.boot3$normal[,2:3]) , col=c("red"), lwd=2,lty=2)
abline(v = ci.boot3$basic[,4:5] , col="blue", lwd=2, lty=2)
abline(v = c(ci.boot3$percent[,4:5]) , col=c("black"), lwd=2,lty=2)
abline(v = ci.boot3$bca[,4:5] , col="green", lwd=2, lty=2)
abline(v = c(0.4275,0.5565) , col="purple", lwd=2, lty=2) #bca con el paquete se solapan con las sacadas por el paquete boot EXtra

# Agregamos la leyenda:
legend("topright", 
       legend = c("Normal", "Basic","Percent","bca","bca_artículo"), 
       col = c("red", "blue","black","green","purple"), 
       pch = c(16,16,16,16,16), 
       bty = "n",
       pt.cex = 0.8, 
       cex = 0.8, 
       text.col = "black", 
       horiz = F ) 

# Prueba extra:
set.seed(1234)

# Fijamos B=50000:
B4=50000

# Realizamos Bootstrap:
r2boot4<- boot(Xy, rfun , R=B4)

# Calculamos los intervalos de confianza:
ci.boot4<-boot.ci(r2boot4, conf=c(0.95),
                  type=c("norm","basic", "perc","bca"))

# Graficamos:
plot(r2boot4,index=1)

# Usamos bcajack:
z4<-bcajack(x = Xy, B = r2boot4$t, func = rfun, verbose = FALSE)

# Graficamos:
bcaplot(z4)

# Los valores para diferentes alphas:
z4$lims

# Corremos nuevamente la funcion:
bcajack(x = Xy, B = 50000, func = rfun, m=34, verbose = FALSE) # por grupos

# Graficamos el histograma con los ICs representados:
hist(r2boot4$t, breaks=90, col = "lightblue",main = "Histograma con los ICs",xlab ="",ylab = "")
abline(v = r2boot4$t0, col="black", lwd=1)
abline(v = c(ci.boot4$normal[,2:3]) , col=c("red"), lwd=2,lty=2)
abline(v = ci.boot4$basic[,4:5] , col="blue", lwd=2, lty=2)
abline(v = c(ci.boot4$percent[,4:5]) , col=c("black"), lwd=2,lty=2)
abline(v = ci.boot4$bca[,4:5] , col="green", lwd=2, lty=2)
abline(v = c(0.4292,0.5579) , col="purple", lwd=2, lty=2) #bca con el paquete se solapan con las sacadas por el paquete boot

# Agregamos la leyenda:
legend("topright", 
       legend = c("Normal", "Basic","Percent","bca","bca_artículo"), 
       col = c("red", "blue","black","green","purple"), 
       pch = c(16,16,16,16,16), 
       bty = "n",
       pt.cex = 0.8, 
       cex = 0.8, 
       text.col = "black", 
       horiz = F ) 

################################################################################

# Ejercicio 3:

library(factoextra)
library(corrplot)

# Cargamos la base de datos:
fp=file.path("C:/Users/Jose Fuentes/Desktop/Posgrado/Tercer Semestre/Aprendizaje estadístico automatizado/Tareas/Tarea 3/Base de datos/diabetes.csv")
dia=read.csv(fp)
dia=data.frame(dia)

# Construimos nuestro modelo sobre los datos de entrenamiento:
lmodel = lm(prog ~ ., data=dia)

# Hacemos el summary:
suma=summary(lmodel)

#Error de prediccion:
error_pred=54.2^2

# Error de prediccion aparente:
sum((dia$prog -lmodel$fitted.values)^2)/442

# Realizamos el modelo lineal generalizado:
glmodel = glm(prog ~ .,family=gaussian, data=dia)

# Tomamos B=200 y k=3:
set.seed(1052)
B=200
err_cv=t(replicate(B, (cv.glm(dia,glmodel,K=3))$delta))
a=(m_err_cv=apply(err_cv, 2, mean))[2]

err_cv=t(replicate(B, (cv.glm(dia,glmodel,K=5))$delta))
b=(m_err_cv=apply(err_cv, 2, mean))[2]

err_cv=t(replicate(B, (cv.glm(dia,glmodel,K=7))$delta))
c=(m_err_cv=apply(err_cv, 2, mean))[2]

err_cv=t(replicate(B, (cv.glm(dia,glmodel,K=10))$delta))
d=(m_err_cv=apply(err_cv, 2, mean))[2]

err1=c(a,b,c,d)


B=300
err_cv=t(replicate(B, (cv.glm(dia,glmodel,K=3))$delta))
a=(m_err_cv=apply(err_cv, 2, mean))[2]

err_cv=t(replicate(B, (cv.glm(dia,glmodel,K=5))$delta))
b=(m_err_cv=apply(err_cv, 2, mean))[2]

err_cv=t(replicate(B, (cv.glm(dia,glmodel,K=7))$delta))
c=(m_err_cv=apply(err_cv, 2, mean))[2]

err_cv=t(replicate(B, (cv.glm(dia,glmodel,K=10))$delta))
d=(m_err_cv=apply(err_cv, 2, mean))[2]

err2=c(a,b,c,d)

B=400
err_cv=t(replicate(B, (cv.glm(dia,glmodel,K=3))$delta))
a=(m_err_cv=apply(err_cv, 2, mean))[2]

err_cv=t(replicate(B, (cv.glm(dia,glmodel,K=5))$delta))
b=(m_err_cv=apply(err_cv, 2, mean))[2]

err_cv=t(replicate(B, (cv.glm(dia,glmodel,K=7))$delta))
c=(m_err_cv=apply(err_cv, 2, mean))[2]

err_cv=t(replicate(B, (cv.glm(dia,glmodel,K=10))$delta))
d=(m_err_cv=apply(err_cv, 2, mean))[2]

err3=c(a,b,c,d)

errrr=c(err1,err2,err3)

k=c("K=3","K=5","K=7","K=10")
k=as.numeric(k)
plot(k, errrr[1:4], type = "b", frame = FALSE, pch = 19, ylim = c(2950,3050), col ="red", xlab = "K", ylab = "Error",lty = 2)
# Add a second line
lines(k, errrr[5:8], pch = 18, col = "blue", type = "b", lty = 2)
lines(k, errrr[9:12], pch = 18, col = "purple", type = "b", lty = 2)
abline(h=2860, col="green")
# Add a legend to the plot
legend("topright", legend=c("B=200", "B=300", "B=400" ),
       col=c("red", "blue", "purple"), lty =c(1:2,1:2), cex=0.8)



# Tomamos B=300 y k=5:
set.seed(1052)
B=300
err_cv=t(replicate(B, (cv.glm(dia,glmodel,K=5))$delta))
(m_err_cv=apply(err_cv, 2, mean))

# Tomamos B=400 y k=10:
set.seed(1052)
B=400
err_cv=t(replicate(B, (cv.glm(dia,glmodel,K=10))$delta))
(m_err_cv=apply(err_cv, 2, mean))

# Complementario:

# Relacion de valores reales vs predichos:
actuals_preds <- data.frame(cbind(actuals=testdata$prog, predicteds=progpredict))  
correlation_accuracy <- cor(actuals_preds) # 72%
head(actuals_preds)

# Precicion
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  

# Aplcimos ahora la funcion step:
#Regresion sin variables explicativas:
RLM.Vacio<-lm(formula= dia$prog~1,dia)

#Hacemos el resumen:
summary(RLM.Vacio)

#Regresion con todas lasvariables explicativas:
RLM.Completo<-lm(formula= dia$prog~.,dia)

#Hacemos el resumen:
summary(RLM.Completo)

#Regresion stepwise:
RLM.stepwise<-step(RLM.Vacio,
                   scope=list(lower=RLM.Vacio,upper=RLM.Completo),
                   direction="both")
#Hacemos el resumen:
summary(RLM.stepwise)

# Error de prediccion aparente para el modelo nuevo:
sum((dia$prog-RLM.stepwise$fitted.values)^2)/442

# Realizamos el modelo lineal generalizado:
glmodel = glm(prog ~ ltg + bmi + map + sex + tc + ldl,family=gaussian, data=dia)

# Tomamos B=200 y k=3:
set.seed(1052)
B=200

# Cambiamos la base de datos:
bd1=dia
bd1=select(dia, -age)
bd1=select(bd1, -tch)
bd1=select(bd1, -glu)
bd1=select(bd1, -hdl)

err_cv=t(replicate(B, (cv.glm(dia,glmodel,K=3))$delta))
(m_err_cv=apply(err_cv, 2, mean))

# Tomamos B=300 y k=5:
set.seed(1052)
B=300
err_cv=t(replicate(B, (cv.glm(dia,glmodel,K=5))$delta))
(m_err_cv=apply(err_cv, 2, mean))

# Tomamos B=400 y k=10:
set.seed(1052)
B=400
err_cv=t(replicate(B, (cv.glm(dia,glmodel,K=10))$delta))
(m_err_cv=apply(err_cv, 2, mean))
