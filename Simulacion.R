library(readxl)
library(tidyverse)
library(boot)
library(effects)
library(leaps)
library(robustbase)
library(minpack.lm)
library(ggplot2)
library(MLmetrics)
library(MASS)
library(rrcov)
library(caret)
#Funcion a tramos para cambio de AQI a Concentracioón
load("DatosAQItoConc.RData")
#interpolation_data <- read_excel("~/Desktop/Noveno Semestre/Simulación de procesos/interpolation data.xlsx")
AQItoConc<-function(aqis){
  solu<-c()
  for (i in 1:length(aqis)){
    if (aqis[i]<=50){
      tempAQI<-interpolation_data$AQI[1:2]
      tempconc<-interpolation_data$Conc[1:2]
      reg<-lm(tempconc~tempAQI)
    }
    else{
      if(aqis[i]>50&aqis[i]<=100){
        tempAQI<-interpolation_data$AQI[3:4]
        tempconc<-interpolation_data$Conc[3:4]
        reg<-lm(tempconc~tempAQI) 
      }
      else{
        if(aqis[i]>100&aqis[i]<=150){
          tempAQI<-interpolation_data$AQI[5:6]
          tempconc<-interpolation_data$Conc[5:6]
          reg<-lm(tempconc~tempAQI) 
        }
        else{
          if(aqis[i]>150&aqis[i]<=200){
            tempAQI<-interpolation_data$AQI[7:8]
            tempconc<-interpolation_data$Conc[7:8]
            reg<-lm(tempconc~tempAQI) 
          }
          
          else{
            if(aqis[i]>200&aqis[i]<=300){
              tempAQI<-interpolation_data$AQI[9:10]
              tempconc<-interpolation_data$Conc[9:10]
              reg<-lm(tempconc~tempAQI) 
            }
            
            else{
              if(aqis[i]>300){
                tempAQI<-interpolation_data$AQI[11:12]
                tempconc<-interpolation_data$Conc[11:12]
                reg<-lm(tempconc~tempAQI) 
              }
            }
          }
        }
      }
    }
    m<-reg$coefficients[2]
    b<-reg$coefficients[1]
    solu[i]<-b+m*aqis[i]
  }
  
  return(solu)
}
Probab<-function(df){
  solu<-c()
  for (i in 1:length(df$DESC_ID_FREC_VIAJE)){
    if (df$DESC_ID_FREC_VIAJE[i]=="Diario"){
      solu<-c(solu,df$FREC_CANTIDAD[i])
    }
    else{
      if(df$DESC_ID_FREC_VIAJE[i]=="Mensual"){
        solu<-c(solu,df$FREC_CANTIDAD[i]/30)
      }
      else{
        if(df$DESC_ID_FREC_VIAJE[i]=="Anual"){
          solu<-c(solu,df$FREC_CANTIDAD[i]/360)
        }
        else{
          solu<-c(solu,df$FREC_CANTIDAD[i]/7)
        }
      }
    }
  }
  return(solu)
}
tabla<-function(df,column,n_cat){
  rang<-(max(df %>% pull(column))-min(df %>% pull(column)))/n_cat
  Tab<-data.frame(Lower_lim=numeric(0),Upper_lim=numeric(0),frequency=numeric(0),Cumulative_freq=numeric(0))
  aux<-min(df %>% pull(column))
  for (i in 1:n_cat) {
    Tab<-rbind(as.matrix(Tab),c(aux,aux+rang,0,0))
    aux<-aux+rang
  }
Tab<-as.data.frame(Tab)
  for (i in 1:n_cat) {
    if (i >n_cat){
    Tab$frequency[i]<-length(which(df %>% pull(column)>=Tab$Lower_lim[i]&df %>% pull(column)<Tab$Upper_lim[i]))/length(df %>% pull(column))
    }
    else{
      Tab$frequency[i]<-length(which(df %>% pull(column)>=Tab$Lower_lim[i]&df %>% pull(column)<=Tab$Upper_lim[i]))/length(df %>% pull(column))
      
    }
  }
  Tab$Cumulative_freq<-cumsum(Tab$frequency)
  return(Tab)
}
rmse.glm = function(m) {
  # m = quote(m.glm)
  
  n = nrow(m$model)
  k = ncol(m$model) - 1
  Y = m$model[,1]
  
  fitted = m$fitted.values
  epsilonhat = Y - fitted
  sigma2hat = sum(epsilonhat^2) / (n-k)
  rmse = sqrt(sigma2hat)
  rsq = cor(Y, fitted)^2
  cbind(rmse, rsq)
}
comedian_matrix <- function(data){
  p <- ncol(data)
  data_prima <- as.matrix(data)
  salida <- matrix(0, p, p)
  for (i in 1:p){
    for (j in 1:p) {
      if (i == j){
        salida[i,j] <- mad(data_prima[,i], constant = 1)^2
      } else {
        salida[i,j] <- median((data_prima[,i]-median(data_prima[,i]))*(data_prima[,j]-median(data_prima[,j])))
      }
    }
  }
  return(salida)
}
comedian_outlier <- function(data){
  n <- nrow(data)
  p <- ncol(data)
  data_prima <- as.matrix(data)
  mad_X <- diag(1/as.vector(apply(data_prima, 2, mad, constant = 1))) # Diagonal matrix with the inverse values of MAD of data.
  DELTA <- mad_X %*% comedian_matrix(data) %*% t(mad_X) # Computing the Median Correlation Matrix of data.
  E <- eigen(DELTA)$vectors # Computing the eigenvectors of the Median Correlation Matrix.
  Q <- solve(mad_X) %*% E
  Z <- t(solve(Q) %*% t(data))
  S <- Q %*% diag(apply(Z, 2, mad, constant = 1)^2) %*% t(Q) # % First estimate of the covariance matrix
  M <- t(Q %*% apply(Z, 2, median))
  RD <- rep(0, n)
  # Computing the Robust Mahalanobis Distance of X and the Cutt-off value
  for (u in 1:n){
    RD[[u]] <- (data_prima[u,]-M) %*% solve(S) %*% t(data_prima[u,]-M)
  }
  CV <- (1.4826*(qchisq(0.95, p)*median(RD)))/qchisq(0.5, p)
  # Result Outputs
  W <- which(RD > CV) # Outliers Detected
  data2 <- data
  data2 <- data2[-c(W),]
  robust_mean <- as.vector(apply(data2, 2, mean)) # Robust mean vector
  robust_cov <- as.matrix(cov(data2)) # Robust covariance matrix
  results_comedian <- list(outs = W, mean = robust_mean, cov = robust_cov)
  return(results_comedian)
}
lm_eqn <- function(df){
  m <- lm(df[,2] ~ df[,1], df)
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq))
}# funcion para armar la formula del parity graph
#Test aprobado (retira el "#" de las dos lineas para volver a hacerlo)
#prueba<-c(12,60,120,230,400)
#AQItoConc(prueba)
#####################################################
#####################################################
#####################################################
#generación  de Parque automotriz
#hacereles grafico
dist_salida_vehic<-rbeta(27,5,1)
P_auto<-c(478000,1337542,1629897)
auts<-c(271000,546768,638333)
mots<-c(139000,710186,893969)
taxis<-c(27000,30000,30143)
bus<-c(7500,6500,6300)
o_bus<-c(8500,11499,16325)
Cam<-c(25000,32589,44827)
por_a<-auts/P_auto
por_m<-mots/P_auto
por_t<-taxis/P_auto
por_b<-bus/P_auto
por_o<-o_bus/P_auto
por_c<-Cam/P_auto
porc_mat<-rbind(por_a,por_m,por_t,por_b,por_o,por_c)
row.names(porc_mat)<-c("Autos","Motos","Taxis","Buses","Otros buses","Camiones")
colnames(porc_mat)<-c(2005,2015,2018)
load("Datosviajes_org.RData")# Carga los datos organizados de viajes
#Aca van los automoviles
autos<-EOD_2017_DatosViajes[which(EOD_2017_DatosViajes$DESC_MODO_TTE_E1=="Auto Particular (Acompañante)"|EOD_2017_DatosViajes$DESC_MODO_TTE_E1=="Particular con pago (Solicitado con plataforma)"| EOD_2017_DatosViajes$DESC_MODO_TTE_E1=="Auto Particular (Conductor)" |EOD_2017_DatosViajes$DESC_MODO_TTE_E1=="Particular con pago (En calle - Sin plataforma)"),]
EOD_2017_DatosViajes<-EOD_2017_DatosViajes[- which(EOD_2017_DatosViajes$DESC_MODO_TTE_E1=="Auto Particular (Acompañante)"|EOD_2017_DatosViajes$DESC_MODO_TTE_E1=="Particular con pago (Solicitado con plataforma)"| EOD_2017_DatosViajes$DESC_MODO_TTE_E1=="Auto Particular (Conductor)" |EOD_2017_DatosViajes$DESC_MODO_TTE_E1=="Particular con pago (En calle - Sin plataforma)"),]
#Aca los taxis
Taxi<-EOD_2017_DatosViajes[which(EOD_2017_DatosViajes$DESC_MODO_TTE_E1=="Taxi Individual (amarillo)"|EOD_2017_DatosViajes$DESC_MODO_TTE_E1=="Taxi colectivo (amarillo)"|EOD_2017_DatosViajes$DESC_MODO_TTE_E1=="Taxi intermunicipal o colectivo (blanco)"),]
EOD_2017_DatosViajes<-EOD_2017_DatosViajes[-which(EOD_2017_DatosViajes$DESC_MODO_TTE_E1=="Taxi Individual (amarillo)"|EOD_2017_DatosViajes$DESC_MODO_TTE_E1=="Taxi colectivo (amarillo)"|EOD_2017_DatosViajes$DESC_MODO_TTE_E1=="Taxi intermunicipal o colectivo (blanco)"),]
#Aca las motos
motos<-EOD_2017_DatosViajes[which(EOD_2017_DatosViajes$DESC_MODO_TTE_E1== "Moto (Acompañante)"|EOD_2017_DatosViajes$DESC_MODO_TTE_E1=="Moto (Conductor)"|EOD_2017_DatosViajes$DESC_MODO_TTE_E1=="MotoTaxi"|EOD_2017_DatosViajes$DESC_MODO_TTE_E1=="Motocarro"),]
EOD_2017_DatosViajes<-EOD_2017_DatosViajes[-which(EOD_2017_DatosViajes$DESC_MODO_TTE_E1== "Moto (Acompañante)"|EOD_2017_DatosViajes$DESC_MODO_TTE_E1=="Moto (Conductor)"|EOD_2017_DatosViajes$DESC_MODO_TTE_E1=="MotoTaxi"|EOD_2017_DatosViajes$DESC_MODO_TTE_E1=="Motocarro"),]
#Aca los buses
Buses<-EOD_2017_DatosViajes 
#construllamos las distribuciones empiricas para las fuentes moviles
Numero_Cat<-100
autos$probab<-Probab(autos)
Dist_autos<-tabla(autos,"probab",Numero_Cat)
motos$probab<-Probab(motos)
Dist_motos<-tabla(motos,"probab",Numero_Cat)
Taxi$probab<-Probab(Taxi)
Dist_Taxi<-tabla(Taxi,"probab",Numero_Cat)
Buses$probab<-Probab(Buses)
Dist_Buses<-tabla(Buses,"probab",Numero_Cat)
#borremos esa base de datos incompleta y carguemosla de nuevo
rm(EOD_2017_DatosViajes)
load("Datosviajes_org.RData")# Carga los datos organizados de viajes
Autos_q_salen<-ceiling(auts[3]*rbeta(27,5,1)*6/10)
Taxis_q_salen<-ceiling(taxis[3]*rbeta(27,5,1))
Motos_q_salen<-ceiling(mots[3]*rbeta(27,5,1)*8/10)
Buses_q_salen<-ceiling((bus[3]+o_bus[3])*rbeta(27,5,1))
Camiones_q_salen<-ceiling(Cam[3]*rbeta(27,5,1))
P_aut_q_sal<-Autos_q_salen+Taxis_q_salen+Motos_q_salen+Buses_q_salen+Camiones_q_salen

#Saquemos las horas de moto, carro y bus de cada dia
Horas_auto<-c()
Horas_moto<-c()
Horas_taxi<-c()
Horas_bus<-c()
Horas_Cam<-c()
set.seed(1)
for (i in 1:length(Autos_q_salen)){
  Horas_auto<-c(Horas_auto,sum(sample(autos$Duración_Viaje,Autos_q_salen[i],TRUE))/60)
  Horas_moto<-c(Horas_moto,sum(sample(motos$Duración_Viaje,Motos_q_salen[i],TRUE))/60)
  Horas_taxi<-c(Horas_taxi,sum(sample(autos$Duración_Viaje,Taxis_q_salen[i],TRUE))/60)
  Horas_bus<-c(Horas_bus,sum(sample(motos$Duración_Viaje,Buses_q_salen[i],TRUE))/60)
}
Horas_Cam<-Camiones_q_salen*8
#Datos de AQI
AQI <- read_excel("Datos AQI.xlsx",sheet = "Datos AQI")
Datos_meteorologicos_mod <- read_excel("Datos meteorologicos_mod.xlsx")
Datos_meteorologicos_mod<-Datos_meteorologicos_mod[-length(Datos_meteorologicos_mod$variable),]
inde<-c()
inde_2<-c()
for (i in 1:length(AQI$date)){
  for( j in Datos_meteorologicos_mod$variable){
    if(AQI$date[i]==j){
      inde<-c(inde,i)
      inde_2<-c(inde_2,which(Datos_meteorologicos_mod==j))
    }
  } 
}
New_AQI<-AQI[inde,]
Datos_meteorologicos_mod<-Datos_meteorologicos_mod[inde_2,]
#Ahora organicemos nuestro data frame
Datos_para_reg<-data.frame(cbind(New_AQI$Promedio,Datos_meteorologicos_mod[,2:dim(Datos_meteorologicos_mod)[2]],Horas_auto,Horas_bus,Horas_Cam,Horas_moto,Horas_taxi))
for (i in 2:dim(Datos_para_reg)[2]){
  Datos_para_reg[,i]<-as.numeric(Datos_para_reg[,i])
}
pairs(Datos_para_reg)
reg_1<-glm(New_AQI.Promedio~., data = Datos_para_reg)
stepAIC(reg_1,direction = "backward")
#aca intentaremos transformaciones
load("Datos_reg.RData")
Datos_para_reg$New_AQI.Promedio<-New_AQI$Promedio
pairs(Datos_para_reg)
cor(Datos_para_reg)
#modelos desechados
reg_2m1<-glm(New_AQI.Promedio ~ Temperature  + Sunshine.Duration +Shortwave.Radiation + Wind.Speed...8  + poly(Horas_auto,3) +Horas_bus + poly(Horas_moto,2) + Horas_taxi-1,data = Datos_para_reg)
reg_2m2<-glm(New_AQI.Promedio ~ Temperature  + Sunshine.Duration +Shortwave.Radiation + Wind.Speed...8  + Horas_auto +Horas_bus + Horas_moto + Horas_taxi-1,data = Datos_para_reg)
reg_2m3<- nls(New_AQI.Promedio ~ a*Temperature  + b*Sunshine.Duration +c*Shortwave.Radiation + d*Wind.Speed...8  + Horas_auto^e +g*Horas_bus + Horas_moto^h + i*Horas_taxi,data = Datos_para_reg,start = list(a=10,b=10,c=10,d=10,e=0.3,g=0,h=0.4,i=0))
#############################################
###########este fue el modelo################
############################################

reg_2<- nls(New_AQI.Promedio ~ a*Temperature  + b*Sunshine.Duration +c*Shortwave.Radiation+ d/Relative.Humidity+f*Cloud.Cover.Total  + Horas_auto^e +Horas_bus^g + Horas_moto^h + Horas_taxi^i,data = Datos_para_reg,start = list(a=10,b=10,c=10,d=40,e=0.3,g=0.5,h=0.4,i=0.5,f=-40))
summary(reg_2)
# example of how to predict
Y_hatreg2<-predict(reg_2,newdata =  data.frame(Temperature=Datos_para_reg$Temperature,Sunshine.Duration=Datos_para_reg$Sunshine.Duration,Shortwave.Radiation=Datos_para_reg$Shortwave.Radiation,Relative.Humidity=Datos_para_reg$Relative.Humidity,Cloud.Cover.Total=Datos_para_reg$Cloud.Cover.Total,Horas_auto=Datos_para_reg$Horas_auto,Horas_bus=Datos_para_reg$Horas_bus,Horas_moto=Datos_para_reg$Horas_moto,Horas_taxi=Datos_para_reg$Horas_taxi))
AIC(reg_2)
AIC(reg_1)
AIC(reg_2m1)
AIC(reg_2m2)
AIC(reg_2m3)
RMSE(Y_hatreg2,Datos_para_reg$New_AQI.Promedio)
#############################################
#############################################
#############################################
#intentemos sin outliers
Datos_para_reg_OD<-Datos_para_reg
mcd_data <- CovOgk(Datos_para_reg_OD)
break_mahal_data_fmcd <- qchisq(0.95, ncol(Datos_para_reg_OD))
Outs <- which(mcd_data$raw.mah > break_mahal_data_fmcd)
Datos_para_reg_OD <- Datos_para_reg_OD[-Outs,]
reg_3<-glm(New_AQI.Promedio ~ Temperature  + Sunshine.Duration+ Relative.Humidity + Shortwave.Radiation  + poly(Wind.Speed...8,2)  + poly(Horas_auto,2) + poly(Horas_bus,2) + poly(Horas_Cam,2) + poly(Horas_moto,2) + poly(Horas_taxi,2)-1,data = Datos_para_reg_OD)

summary(reg_3)
rmse.glm(reg_3)

#no funcionó muy bien, asi que ......
#Haganos elastic net
set.seed(123)
training.samples <- Datos_para_reg$New_AQI.Promedio %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- Datos_para_reg[training.samples, ]
test.data <- Datos_para_reg[-training.samples, ]
x <- model.matrix(New_AQI.Promedio~., train.data)[,-1]
y <- train.data$New_AQI.Promedio
set.seed(123)
model <- train(
  New_AQI.Promedio ~., data = train.data, method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
)
# Best tuning parameter
model$bestTune
coef(model$finalModel, model$bestTune$lambda)
x.test <- model.matrix(New_AQI.Promedio ~., test.data)[,-1]
predictions <- model %>% predict(x.test)
# Model performance metrics
data.frame(
  RMSE = RMSE(predictions, test.data$New_AQI.Promedio),
  Rsquare = R2(predictions, test.data$New_AQI.Promedio))
# terminó como Ridge eliminandome muchos predictores
