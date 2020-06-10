#Limpiar ambiente
rm(list= ls())

#Instalar y cargar paquetes relevantes
load.lib <- c('PerformanceAnalytics','xtable','stargazer', 'xts',"readxl","data.table","lubridate",'tidyr',
              'dplyr','ggplot2','forecast','RColorBrewer','quadprog','NMOF','fBonds','cvar','mFilter',"x13binary","seasonal","lmtest","RCurl") 
install.lib <- load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib)
sapply(load.lib,require,character=TRUE)

url_robust <- "https://raw.githubusercontent.com/IsidoreBeautrelet/economictheoryblog/master/robust_summary.R"
eval(parse(text = getURL(url_robust, ssl.verifypeer = FALSE)),
     envir=.GlobalEnv)
#Convencion de notacion cientifica
options(scipen=10000)
col<-brewer.pal(6,"Blues")

#Seleccion de directorio
dir <- 'C:/Users/sebit/Documents/GitHub/Causal-inference-course/3rd assignment/Data'
setwd(dir)

#Importacion de datos.
Datos=as.data.table(read_excel("covid example dataset.xlsx",col_names = T))

#Primer modelo Y = a+ BD
modelo1 <- lm(Y ~ D, data = Datos) 

#Segundo modelo Y = a+ B1D+ B2Age

modelo2 <- lm(Y ~ D + Age, data = Datos) 

stargazer(modelo1, modelo2, title="Table 1", align=TRUE, covariate.labels=c("Treatment (binary)","Age"),dep.var.labels=c("Outcome"),type='text')

#tercer modelo, regresion auxiliar
modelo3.1 <- lm(Y ~ D + Age, data = Datos) 
modelo3.2 <- lm(D ~ Age, data = Datos)
D_swivel = modelo3.2[["residuals"]]
modelo3.3 <- lm(Y ~ D_swivel, data = Datos) 

stargazer(modelo3.1, modelo3.2, modelo3.3, title="Table 2", align=TRUE, covariate.labels=c("Treatment (binary)","Age","Residual treatment variance"),dep.var.labels=c("Outcome","Treatment","Outcome"),type='text')


