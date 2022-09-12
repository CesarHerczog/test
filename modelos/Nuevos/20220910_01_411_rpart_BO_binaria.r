# Este script esta pensado para corren en Google Cloud
# si se lo desea correr en Windows debera
#  * cambiar el setwd()  y las rutas
#  * cuando llame a la funcion mcmapply  poner  mc.cores=1
#  * armarse de mucha paciencia porque va a demorar muchas horas en Windows

#Optimizacion Bayesiana de hiperparametros de  rpart
# Hace  1-Repeated  5-Fold Cross Validation


# NO utiliza Feature Engineering  ( el Fiscal General se enoja ... )


#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("rlist")

require("rpart")
require("parallel")

#paquetes necesarios para la Bayesian Optimization
require("DiceKriging")
require("mlrMBO")

#aqui deben ir SUS semillas, se usan para  1-Repeated  (5-Fold Cross Validation)
ksemilla_azar  <- c(113111)


#Defino la  Optimizacion Bayesiana

kBO_iter  <- 400  #cantidad de iteraciones de la Optimizacion Bayesiana

hs  <- makeParamSet(
          makeNumericParam("cp"       , lower=  -1, upper=    -0.50),
          makeNumericParam("minsplit" , lower=   600L,   upper= 2000L ),
          makeNumericParam("minbucket", lower=   160L,   upper= 500L ),
          makeIntegerParam("maxdepth" , lower=   6L,  upper=   9L),  #la letra L al final significa ENTERO
          forbidden = quote( minbucket > 0.3*minsplit ) )             # minbuket NO PUEDE ser mayor que la tercera


#------------------------------------------------------------------------------
#graba a un archivo los componentes de lista
#para el primer registro, escribe antes los titulos

loguear  <- function( reg, arch=NA, folder="./work/", ext=".txt", verbose=TRUE )
{
  archivo  <- arch
  if( is.na(arch) )  archivo  <- paste0( folder, substitute( reg), ext )

  if( !file.exists( archivo ) )  #Escribo los titulos
  {
    linea  <- paste0( "fecha\t", 
                      paste( list.names(reg), collapse="\t" ), "\n" )

    cat( linea, file=archivo )
  }

  linea  <- paste0( format(Sys.time(), "%Y%m%d %H%M%S"),  "\t",     #la fecha y hora
                    gsub( ", ", "\t", toString( reg ) ),  "\n" )

  cat( linea, file=archivo, append=TRUE )  #grabo al archivo

  if( verbose )  cat( linea )   #imprimo por pantalla
}
#------------------------------------------------------------------------------
#particionar agrega una columna llamada fold a un dataset que consiste en una particion estratificada segun agrupa
# particionar( data=dataset, division=c(70,30), agrupa=clase_ternaria, seed=semilla)   crea una particion 70, 30 
# particionar( data=dataset, division=c(1,1,1,1,1), agrupa=clase_ternaria, seed=semilla)   divide el dataset en 5 particiones

particionar  <- function( data, division, agrupa="", campo="fold", start=1, seed=NA )
{
  if( !is.na( seed)  )   set.seed( seed )

  bloque  <- unlist( mapply(  function(x,y) { rep( y, x ) }, division, seq( from=start, length.out=length(division) )  ) )

  data[ , (campo) :=  sample( rep( bloque, ceiling(.N/length(bloque))) )[1:.N],
           by= agrupa ]
}
#------------------------------------------------------------------------------
#fold_test  tiene el numero de fold que voy a usar para testear, entreno en el resto de los folds
#param tiene los hiperparametros del arbol

ArbolSimple  <- function( fold_test, data, param )
{
  param2 <- param
  #param2$minsplit   <- as.integer( round( 2^param$minsplit ) )
  #param2$minbucket  <- as.integer( round( 2^param$minbucket ) )
  
  #genero el modelo
  modelo  <- rpart("clase_binaria ~ .  -clase_ternaria", #-Visa_mpagado -mcomisiones_mantenimiento 
                   data= data[ fold != fold_test, ],  #entreno en todo MENOS el fold_test que uso para testing
                   xval= 0,
                   control= param2 )

  #aplico el modelo a los datos de testing
  prediccion  <- predict( modelo, 
                          data[ fold==fold_test, ],  #aplico el modelo sobre los datos de testing
                          type= "prob")   #quiero que me devuelva probabilidades

  #En el 1er cuatrimestre del Tercer Año de la Maestria se explicaran las siguientes 12 lineas
  dtest <- copy( data[ fold==fold_test , list( clase_ternaria )] )
  dtest[ , pred := prediccion[ ,"SI"] ]
  dtest[ , azar := runif( nrow( dtest ) ) ]
  setorder(  dtest, -pred, azar )

  dtest[ , gan :=  ifelse( clase_ternaria=="BAJA+2", 78000, -2000 ) ]
  dtest[ , gan_acum := cumsum( gan ) ]

  #calculo la ganancia
  dtest2   <- dtest[ (1:100)*100,  ]
  idx_max  <- which.max( dtest2$gan_acum ) 
  ganancia_testing  <- dtest2[ (idx_max-1):(idx_max+1),  mean(gan_acum) ]


  rm( dtest )
  rm( dtest2 )

  return( ganancia_testing )  #esta es la ganancia sobre el fold de testing, NO esta normalizada
}
#------------------------------------------------------------------------------

ArbolesCrossValidation  <- function( semilla, data, param, qfolds, pagrupa )
{
  divi  <- rep( 1, qfolds )  # generalmente  c(1, 1, 1, 1, 1 )  cinco unos

  particionar( data, divi, seed=semilla, agrupa=pagrupa )  #particiono en dataset en folds

  ganancias  <- mcmapply( ArbolSimple, 
                          seq(qfolds), # 1 2 3 4 5
                          MoreArgs= list( data, param), 
                          SIMPLIFY= FALSE,
                          mc.cores= 1 )   #debe ir 1 si es Windows mc.cores= 5 ) 

  data[ , fold := NULL ]

  #devuelvo la primer ganancia y el promedio
  ganancia_promedio  <- mean( unlist( ganancias ) )   #promedio las ganancias
  ganancia_promedio_normalizada  <- ganancia_promedio * qfolds  #aqui normalizo la ganancia

  gc()

  return( ganancia_promedio_normalizada )
}
#------------------------------------------------------------------------------
#esta funcion solo puede recibir los parametros que se estan optimizando
#el resto de los parametros, lamentablemente se pasan como variables globales

EstimarGanancia  <- function( x )
{
   GLOBAL_iteracion  <<-  GLOBAL_iteracion + 1

   xval_folds  <- 5
   vganancias <- mcmapply( ArbolesCrossValidation,
                           ksemilla_azar,
                           MoreArgs= list ( dtrain, param=x, qfolds= xval_folds, pagrupa= "clase_ternaria" ),
                           SIMPLIFY= FALSE,
                           mc.cores = 1 )  #debe ir 1 si es Windows


   ganancia_promedio  <- mean( unlist( vganancias ) )
   #logueo 
   xx  <- x
   xx$xval_folds  <-  xval_folds
   xx$ganancia  <- ganancia_promedio
   xx$iteracion <- GLOBAL_iteracion
   loguear( xx,  arch= archivo_log )

   return( xx$ganancia )
}
#------------------------------------------------------------------------------
#Aqui empieza el programa

#setwd( "~/buckets/b1/" )
setwd("C:\\gdrive\\UBA2022\\")

#cargo el dataset, aqui debe poner  SU RUTA
dataset  <- fread("./datasets/competencia1_2022.csv")   #donde entreno

#creo la clase_binaria  SI= {BAJA+1, BAJA+2}  NO={CONTINUA}
dataset[ foto_mes==202101, clase_binaria :=  ifelse( clase_ternaria=="CONTINUA", "NO", "SI" ) ]
#dataset[ ,principal_1 := as.integer( ctrx_quarter >= 49 & mpayroll >= 7043 & (ccaja_ahorro < 4 | is.na(ccaja_ahorro) ) & mpayroll < 1000000 & ( mcheques_depositados < 45000 | is.na(mcheques_depositados) ) & ( Visa_fechaalta < 8986 | is.na(Visa_fechaalta) ) & ( mtarjeta_master_consumo < 77000 | is.na(mtarjeta_master_consumo) )) ]
#dataset[ ,principal_3 := as.integer( ctrx_quarter >= 49 & (mpayroll < 7043 | is.na(mpayroll) ) & mtarjeta_visa_consumo >= 2130 & (mpasivos_margen < 699 | is.na(mpasivos_margen) ) & Visa_msaldototal >= 5997 & mrentabilidad_annual >= 11000 & ( Visa_msaldodolares < 3026 | is.na(Visa_msaldodolares) ))]
#dataset[ ,principal_2 := as.integer( ctrx_quarter >= 49 & (mpayroll < 7043 | is.na(mpayroll) ) & mtarjeta_visa_consumo >= 2130 & mpasivos_margen >= 699 & mcaja_ahorro >= 325 & ( cliente_edad < 78 | is.na(cliente_edad) ) & ( ccallcenter_transacciones < 10 | is.na(ccallcenter_transacciones) )) ]
#dataset[ ,principal_4 := as.integer( ctrx_quarter >= 49 & (mpayroll < 7043 | is.na(mpayroll) ) & mtarjeta_visa_consumo >= 2130 & (mpasivos_margen < 699 | is.na(mpasivos_margen) ) & Visa_msaldototal >= 5997 & ( mrentabilidad_annual < 11000 | is.na(mrentabilidad_annual) ) & ( ctrx_quarter < 148 | is.na(ctrx_quarter) ))]

#Creo ranking de variables #frankv(DT, cols="x", na.last="keep")

frankv(dataset, cols="ctrx_quarter", na.last="keep")
frankv(dataset, cols="mcuentas_saldo", na.last="keep")
frankv(dataset, cols="cdescubierto_preacordado", na.last="keep")
frankv(dataset, cols="mprestamos_personales", na.last="keep")
frankv(dataset, cols="active_quarter", na.last="keep")
frankv(dataset, cols="cprestamos_personales", na.last="keep")
frankv(dataset, cols="mcaja_ahorro", na.last="keep")
frankv(dataset, cols="mcuenta_corriente", na.last="keep")
frankv(dataset, cols="mactivos_margen", na.last="keep")
frankv(dataset, cols="ccomisiones_otras", na.last="keep")
frankv(dataset, cols="mpasivos_margen", na.last="keep")
frankv(dataset, cols="mtarjeta_visa_consumo", na.last="keep")
frankv(dataset, cols="Visa_msaldototal", na.last="keep")
frankv(dataset, cols="Visa_msaldopesos", na.last="keep")
frankv(dataset, cols="ctarjeta_visa_transacciones", na.last="keep")
frankv(dataset, cols="mcomisiones", na.last="keep")
frankv(dataset, cols="cliente_antiguedad", na.last="keep")
frankv(dataset, cols="mcomisiones_otras", na.last="keep")
frankv(dataset, cols="mpayroll", na.last="keep")


#Creo nuevas features
dataset[ ,feature1:= as.integer( ctrx_quarter+ctrx_quarter )]
dataset[ ,feature2:= as.integer( ctrx_quarter+mpayroll )]
dataset[ ,feature3:= as.integer( ctrx_quarter+ccaja_ahorro )]
dataset[ ,feature4:= as.integer( ctrx_quarter+mcheques_depositados )]
dataset[ ,feature5:= as.integer( ctrx_quarter+Visa_fechaalta )]
dataset[ ,feature6:= as.integer( ctrx_quarter+mtarjeta_master_consumo )]
dataset[ ,feature7:= as.integer( ctrx_quarter+ctarjeta_visa_transacciones )]
dataset[ ,feature8:= as.integer( ctrx_quarter+Visa_mpagospesos )]
dataset[ ,feature9:= as.integer( ctrx_quarter+mtarjeta_visa_consumo )]
dataset[ ,feature10:= as.integer( ctrx_quarter+mpasivos_margen )]
dataset[ ,feature11:= as.integer( ctrx_quarter+ccallcenter_transacciones )]
dataset[ ,feature12:= as.integer( ctrx_quarter+cliente_edad )]
dataset[ ,feature13:= as.integer( ctrx_quarter+mcaja_ahorro )]
dataset[ ,feature14:= as.integer( ctrx_quarter+Visa_msaldototal )]
dataset[ ,feature15:= as.integer( mpayroll+ctrx_quarter )]
dataset[ ,feature16:= as.integer( mpayroll+mpayroll )]
dataset[ ,feature17:= as.integer( mpayroll+ccaja_ahorro )]
dataset[ ,feature18:= as.integer( mpayroll+mcheques_depositados )]
dataset[ ,feature19:= as.integer( mpayroll+Visa_fechaalta )]
dataset[ ,feature20:= as.integer( mpayroll+mtarjeta_master_consumo )]
dataset[ ,feature21:= as.integer( mpayroll+ctarjeta_visa_transacciones )]
dataset[ ,feature22:= as.integer( mpayroll+Visa_mpagospesos )]
dataset[ ,feature23:= as.integer( mpayroll+mtarjeta_visa_consumo )]
dataset[ ,feature24:= as.integer( mpayroll+mpasivos_margen )]
dataset[ ,feature25:= as.integer( mpayroll+ccallcenter_transacciones )]
dataset[ ,feature26:= as.integer( mpayroll+cliente_edad )]
dataset[ ,feature27:= as.integer( mpayroll+mcaja_ahorro )]
dataset[ ,feature28:= as.integer( mpayroll+Visa_msaldototal )]
dataset[ ,feature29:= as.integer( ccaja_ahorro+ctrx_quarter )]
dataset[ ,feature30:= as.integer( ccaja_ahorro+mpayroll )]
dataset[ ,feature31:= as.integer( ccaja_ahorro+ccaja_ahorro )]
dataset[ ,feature32:= as.integer( ccaja_ahorro+mcheques_depositados )]
dataset[ ,feature33:= as.integer( ccaja_ahorro+Visa_fechaalta )]
dataset[ ,feature34:= as.integer( ccaja_ahorro+mtarjeta_master_consumo )]
dataset[ ,feature35:= as.integer( ccaja_ahorro+ctarjeta_visa_transacciones )]
dataset[ ,feature36:= as.integer( ccaja_ahorro+Visa_mpagospesos )]
dataset[ ,feature37:= as.integer( ccaja_ahorro+mtarjeta_visa_consumo )]
dataset[ ,feature38:= as.integer( ccaja_ahorro+mpasivos_margen )]
dataset[ ,feature39:= as.integer( ccaja_ahorro+ccallcenter_transacciones )]
dataset[ ,feature40:= as.integer( ccaja_ahorro+cliente_edad )]
dataset[ ,feature41:= as.integer( ccaja_ahorro+mcaja_ahorro )]
dataset[ ,feature42:= as.integer( ccaja_ahorro+Visa_msaldototal )]
dataset[ ,feature43:= as.integer( ccaja_ahorro+ctrx_quarter )]
dataset[ ,feature44:= as.integer( mcheques_depositados+mpayroll )]
dataset[ ,feature45:= as.integer( mcheques_depositados+ccaja_ahorro )]
dataset[ ,feature46:= as.integer( mcheques_depositados+mcheques_depositados )]
dataset[ ,feature47:= as.integer( mcheques_depositados+Visa_fechaalta )]
dataset[ ,feature48:= as.integer( mcheques_depositados+mtarjeta_master_consumo )]
dataset[ ,feature49:= as.integer( mcheques_depositados+ctarjeta_visa_transacciones )]
dataset[ ,feature50:= as.integer( mcheques_depositados+Visa_mpagospesos )]
dataset[ ,feature51:= as.integer( mcheques_depositados+mtarjeta_visa_consumo )]
dataset[ ,feature52:= as.integer( mcheques_depositados+mpasivos_margen )]
dataset[ ,feature53:= as.integer( mcheques_depositados+ccallcenter_transacciones )]
dataset[ ,feature54:= as.integer( mcheques_depositados+cliente_edad )]
dataset[ ,feature55:= as.integer( mcheques_depositados+mcaja_ahorro )]
dataset[ ,feature56:= as.integer( mcheques_depositados+Visa_msaldototal )]
dataset[ ,feature57:= as.integer( mcheques_depositados+ctrx_quarter )]
dataset[ ,feature58:= as.integer( Visa_fechaalta+mpayroll )]
dataset[ ,feature59:= as.integer( Visa_fechaalta+ccaja_ahorro )]
dataset[ ,feature60:= as.integer( Visa_fechaalta+mcheques_depositados )]
dataset[ ,feature61:= as.integer( Visa_fechaalta+Visa_fechaalta )]
dataset[ ,feature62:= as.integer( Visa_fechaalta+mtarjeta_master_consumo )]
dataset[ ,feature63:= as.integer( Visa_fechaalta+ctarjeta_visa_transacciones )]
dataset[ ,feature64:= as.integer( Visa_fechaalta+Visa_mpagospesos )]
dataset[ ,feature65:= as.integer( Visa_fechaalta+mtarjeta_visa_consumo )]
dataset[ ,feature66:= as.integer( Visa_fechaalta+mpasivos_margen )]
dataset[ ,feature67:= as.integer( Visa_fechaalta+ccallcenter_transacciones )]
dataset[ ,feature68:= as.integer( Visa_fechaalta+cliente_edad )]
dataset[ ,feature69:= as.integer( Visa_fechaalta+mcaja_ahorro )]
dataset[ ,feature70:= as.integer( Visa_fechaalta+Visa_msaldototal )]
dataset[ ,feature71:= as.integer( Visa_fechaalta+ctrx_quarter )]
dataset[ ,feature72:= as.integer( mtarjeta_master_consumo+mpayroll )]
dataset[ ,feature73:= as.integer( mtarjeta_master_consumo+ccaja_ahorro )]
dataset[ ,feature74:= as.integer( mtarjeta_master_consumo+mcheques_depositados )]
dataset[ ,feature75:= as.integer( mtarjeta_master_consumo+Visa_fechaalta )]
dataset[ ,feature76:= as.integer( mtarjeta_master_consumo+mtarjeta_master_consumo )]
dataset[ ,feature77:= as.integer( mtarjeta_master_consumo+ctarjeta_visa_transacciones )]
dataset[ ,feature78:= as.integer( mtarjeta_master_consumo+Visa_mpagospesos )]
dataset[ ,feature79:= as.integer( mtarjeta_master_consumo+mtarjeta_visa_consumo )]
dataset[ ,feature80:= as.integer( mtarjeta_master_consumo+mpasivos_margen )]
dataset[ ,feature81:= as.integer( mtarjeta_master_consumo+ccallcenter_transacciones )]
dataset[ ,feature82:= as.integer( mtarjeta_master_consumo+cliente_edad )]
dataset[ ,feature83:= as.integer( mtarjeta_master_consumo+mcaja_ahorro )]
dataset[ ,feature84:= as.integer( mtarjeta_master_consumo+Visa_msaldototal )]
dataset[ ,feature85:= as.integer( mtarjeta_master_consumo+ctrx_quarter )]
dataset[ ,feature86:= as.integer( ctarjeta_visa_transacciones+mpayroll )]
dataset[ ,feature87:= as.integer( ctarjeta_visa_transacciones+ccaja_ahorro )]
dataset[ ,feature88:= as.integer( ctarjeta_visa_transacciones+mcheques_depositados )]
dataset[ ,feature89:= as.integer( ctarjeta_visa_transacciones+Visa_fechaalta )]
dataset[ ,feature90:= as.integer( ctarjeta_visa_transacciones+mtarjeta_master_consumo )]
dataset[ ,feature91:= as.integer( ctarjeta_visa_transacciones+ctarjeta_visa_transacciones )]
dataset[ ,feature92:= as.integer( ctarjeta_visa_transacciones+Visa_mpagospesos )]
dataset[ ,feature93:= as.integer( ctarjeta_visa_transacciones+mtarjeta_visa_consumo )]
dataset[ ,feature94:= as.integer( ctarjeta_visa_transacciones+mpasivos_margen )]
dataset[ ,feature95:= as.integer( ctarjeta_visa_transacciones+ccallcenter_transacciones )]
dataset[ ,feature96:= as.integer( ctarjeta_visa_transacciones+cliente_edad )]
dataset[ ,feature97:= as.integer( ctarjeta_visa_transacciones+mcaja_ahorro )]
dataset[ ,feature98:= as.integer( ctarjeta_visa_transacciones+Visa_msaldototal )]
dataset[ ,feature99:= as.integer( ctarjeta_visa_transacciones+ctrx_quarter )]
dataset[ ,feature100:= as.integer( Visa_mpagospesos+mpayroll )]
dataset[ ,feature101:= as.integer( Visa_mpagospesos+ccaja_ahorro )]
dataset[ ,feature102:= as.integer( Visa_mpagospesos+mcheques_depositados )]
dataset[ ,feature103:= as.integer( Visa_mpagospesos+Visa_fechaalta )]
dataset[ ,feature104:= as.integer( Visa_mpagospesos+mtarjeta_master_consumo )]
dataset[ ,feature105:= as.integer( Visa_mpagospesos+ctarjeta_visa_transacciones )]
dataset[ ,feature106:= as.integer( Visa_mpagospesos+Visa_mpagospesos )]
dataset[ ,feature107:= as.integer( Visa_mpagospesos+mtarjeta_visa_consumo )]
dataset[ ,feature108:= as.integer( Visa_mpagospesos+mpasivos_margen )]
dataset[ ,feature109:= as.integer( Visa_mpagospesos+ccallcenter_transacciones )]
dataset[ ,feature110:= as.integer( Visa_mpagospesos+cliente_edad )]
dataset[ ,feature111:= as.integer( Visa_mpagospesos+mcaja_ahorro )]
dataset[ ,feature112:= as.integer( Visa_mpagospesos+Visa_msaldototal )]
dataset[ ,feature113:= as.integer( Visa_mpagospesos+ctrx_quarter )]
dataset[ ,feature114:= as.integer( mtarjeta_visa_consumo+mpayroll )]
dataset[ ,feature115:= as.integer( mtarjeta_visa_consumo+ccaja_ahorro )]
dataset[ ,feature116:= as.integer( mtarjeta_visa_consumo+mcheques_depositados )]
dataset[ ,feature117:= as.integer( mtarjeta_visa_consumo+Visa_fechaalta )]
dataset[ ,feature118:= as.integer( mtarjeta_visa_consumo+mtarjeta_master_consumo )]
dataset[ ,feature119:= as.integer( mtarjeta_visa_consumo+ctarjeta_visa_transacciones )]
dataset[ ,feature120:= as.integer( mtarjeta_visa_consumo+Visa_mpagospesos )]
dataset[ ,feature121:= as.integer( mtarjeta_visa_consumo+mtarjeta_visa_consumo )]
dataset[ ,feature122:= as.integer( mtarjeta_visa_consumo+mpasivos_margen )]
dataset[ ,feature123:= as.integer( mtarjeta_visa_consumo+ccallcenter_transacciones )]
dataset[ ,feature124:= as.integer( mtarjeta_visa_consumo+cliente_edad )]
dataset[ ,feature125:= as.integer( mtarjeta_visa_consumo+mcaja_ahorro )]
dataset[ ,feature126:= as.integer( mtarjeta_visa_consumo+Visa_msaldototal )]
dataset[ ,feature127:= as.integer( mtarjeta_visa_consumo+ctrx_quarter )]
dataset[ ,feature128:= as.integer( mpasivos_margen+mpayroll )]
dataset[ ,feature129:= as.integer( mpasivos_margen+ccaja_ahorro )]
dataset[ ,feature130:= as.integer( mpasivos_margen+mcheques_depositados )]
dataset[ ,feature131:= as.integer( mpasivos_margen+Visa_fechaalta )]
dataset[ ,feature132:= as.integer( mpasivos_margen+mtarjeta_master_consumo )]
dataset[ ,feature133:= as.integer( mpasivos_margen+ctarjeta_visa_transacciones )]
dataset[ ,feature134:= as.integer( mpasivos_margen+Visa_mpagospesos )]
dataset[ ,feature135:= as.integer( mpasivos_margen+mtarjeta_visa_consumo )]
dataset[ ,feature136:= as.integer( mpasivos_margen+mpasivos_margen )]
dataset[ ,feature137:= as.integer( mpasivos_margen+ccallcenter_transacciones )]
dataset[ ,feature138:= as.integer( mpasivos_margen+cliente_edad )]
dataset[ ,feature139:= as.integer( mpasivos_margen+mcaja_ahorro )]
dataset[ ,feature140:= as.integer( mpasivos_margen+Visa_msaldototal )]
dataset[ ,feature141:= as.integer( mpasivos_margen+ctrx_quarter )]
dataset[ ,feature142:= as.integer( ccallcenter_transacciones+mpayroll )]
dataset[ ,feature143:= as.integer( ccallcenter_transacciones+ccaja_ahorro )]
dataset[ ,feature144:= as.integer( ccallcenter_transacciones+mcheques_depositados )]
dataset[ ,feature145:= as.integer( ccallcenter_transacciones+Visa_fechaalta )]
dataset[ ,feature146:= as.integer( ccallcenter_transacciones+mtarjeta_master_consumo )]
dataset[ ,feature147:= as.integer( ccallcenter_transacciones+ctarjeta_visa_transacciones )]
dataset[ ,feature148:= as.integer( ccallcenter_transacciones+Visa_mpagospesos )]
dataset[ ,feature149:= as.integer( ccallcenter_transacciones+mtarjeta_visa_consumo )]
dataset[ ,feature150:= as.integer( ccallcenter_transacciones+mpasivos_margen )]
dataset[ ,feature151:= as.integer( ccallcenter_transacciones+ccallcenter_transacciones )]
dataset[ ,feature152:= as.integer( ccallcenter_transacciones+cliente_edad )]
dataset[ ,feature153:= as.integer( ccallcenter_transacciones+mcaja_ahorro )]
dataset[ ,feature154:= as.integer( ccallcenter_transacciones+Visa_msaldototal )]
dataset[ ,feature155:= as.integer( ccallcenter_transacciones+ctrx_quarter )]
dataset[ ,feature156:= as.integer( cliente_edad+mpayroll )]
dataset[ ,feature157:= as.integer( cliente_edad+ccaja_ahorro )]
dataset[ ,feature158:= as.integer( cliente_edad+mcheques_depositados )]
dataset[ ,feature159:= as.integer( cliente_edad+Visa_fechaalta )]
dataset[ ,feature160:= as.integer( cliente_edad+mtarjeta_master_consumo )]
dataset[ ,feature161:= as.integer( cliente_edad+ctarjeta_visa_transacciones )]
dataset[ ,feature162:= as.integer( cliente_edad+Visa_mpagospesos )]
dataset[ ,feature163:= as.integer( cliente_edad+mtarjeta_visa_consumo )]
dataset[ ,feature164:= as.integer( cliente_edad+mpasivos_margen )]
dataset[ ,feature165:= as.integer( cliente_edad+ccallcenter_transacciones )]
dataset[ ,feature166:= as.integer( cliente_edad+cliente_edad )]
dataset[ ,feature167:= as.integer( cliente_edad+mcaja_ahorro )]
dataset[ ,feature168:= as.integer( cliente_edad+Visa_msaldototal )]
dataset[ ,feature169:= as.integer( cliente_edad+ctrx_quarter )]
dataset[ ,feature170:= as.integer( mcaja_ahorro+mpayroll )]
dataset[ ,feature171:= as.integer( mcaja_ahorro+ccaja_ahorro )]
dataset[ ,feature172:= as.integer( mcaja_ahorro+mcheques_depositados )]
dataset[ ,feature173:= as.integer( mcaja_ahorro+Visa_fechaalta )]
dataset[ ,feature174:= as.integer( mcaja_ahorro+mtarjeta_master_consumo )]
dataset[ ,feature175:= as.integer( mcaja_ahorro+ctarjeta_visa_transacciones )]
dataset[ ,feature176:= as.integer( mcaja_ahorro+Visa_mpagospesos )]
dataset[ ,feature177:= as.integer( mcaja_ahorro+mtarjeta_visa_consumo )]
dataset[ ,feature178:= as.integer( mcaja_ahorro+mpasivos_margen )]
dataset[ ,feature179:= as.integer( mcaja_ahorro+ccallcenter_transacciones )]
dataset[ ,feature180:= as.integer( mcaja_ahorro+cliente_edad )]
dataset[ ,feature181:= as.integer( mcaja_ahorro+mcaja_ahorro )]
dataset[ ,feature182:= as.integer( mcaja_ahorro+Visa_msaldototal )]
dataset[ ,feature183:= as.integer( mcaja_ahorro+ctrx_quarter )]
dataset[ ,feature184:= as.integer( Visa_msaldototal+mpayroll )]
dataset[ ,feature185:= as.integer( Visa_msaldototal+ccaja_ahorro )]
dataset[ ,feature186:= as.integer( Visa_msaldototal+mcheques_depositados )]
dataset[ ,feature187:= as.integer( Visa_msaldototal+Visa_fechaalta )]
dataset[ ,feature188:= as.integer( Visa_msaldototal+mtarjeta_master_consumo )]
dataset[ ,feature189:= as.integer( Visa_msaldototal+ctarjeta_visa_transacciones )]
dataset[ ,feature190:= as.integer( Visa_msaldototal+Visa_mpagospesos )]
dataset[ ,feature191:= as.integer( Visa_msaldototal+mtarjeta_visa_consumo )]
dataset[ ,feature192:= as.integer( Visa_msaldototal+mpasivos_margen )]
dataset[ ,feature193:= as.integer( Visa_msaldototal+ccallcenter_transacciones )]
dataset[ ,feature194:= as.integer( Visa_msaldototal+cliente_edad )]
dataset[ ,feature195:= as.integer( Visa_msaldototal+mcaja_ahorro )]
dataset[ ,feature196:= as.integer( Visa_msaldototal+Visa_msaldototal )]
dataset[ ,feature197:= as.integer( ctrx_quarter*ctrx_quarter )]
dataset[ ,feature198:= as.integer( ctrx_quarter*mpayroll )]
dataset[ ,feature199:= as.integer( ctrx_quarter*ccaja_ahorro )]
dataset[ ,feature200:= as.integer( ctrx_quarter*mcheques_depositados )]
dataset[ ,feature201:= as.integer( ctrx_quarter*Visa_fechaalta )]
dataset[ ,feature202:= as.integer( ctrx_quarter*mtarjeta_master_consumo )]
dataset[ ,feature203:= as.integer( ctrx_quarter*ctarjeta_visa_transacciones )]
dataset[ ,feature204:= as.integer( ctrx_quarter*Visa_mpagospesos )]
dataset[ ,feature205:= as.integer( ctrx_quarter*mtarjeta_visa_consumo )]
dataset[ ,feature206:= as.integer( ctrx_quarter*mpasivos_margen )]
dataset[ ,feature207:= as.integer( ctrx_quarter*ccallcenter_transacciones )]
dataset[ ,feature208:= as.integer( ctrx_quarter*cliente_edad )]
dataset[ ,feature209:= as.integer( ctrx_quarter*mcaja_ahorro )]
dataset[ ,feature210:= as.integer( ctrx_quarter*Visa_msaldototal )]
dataset[ ,feature211:= as.integer( mpayroll*ctrx_quarter )]
dataset[ ,feature212:= as.integer( mpayroll*mpayroll )]
dataset[ ,feature213:= as.integer( mpayroll*ccaja_ahorro )]
dataset[ ,feature214:= as.integer( mpayroll*mcheques_depositados )]
dataset[ ,feature215:= as.integer( mpayroll*Visa_fechaalta )]
dataset[ ,feature216:= as.integer( mpayroll*mtarjeta_master_consumo )]
dataset[ ,feature217:= as.integer( mpayroll*ctarjeta_visa_transacciones )]
dataset[ ,feature218:= as.integer( mpayroll*Visa_mpagospesos )]
dataset[ ,feature219:= as.integer( mpayroll*mtarjeta_visa_consumo )]
dataset[ ,feature220:= as.integer( mpayroll*mpasivos_margen )]
dataset[ ,feature221:= as.integer( mpayroll*ccallcenter_transacciones )]
dataset[ ,feature222:= as.integer( mpayroll*cliente_edad )]
dataset[ ,feature223:= as.integer( mpayroll*mcaja_ahorro )]
dataset[ ,feature224:= as.integer( mpayroll*Visa_msaldototal )]
dataset[ ,feature225:= as.integer( ccaja_ahorro*ctrx_quarter )]
dataset[ ,feature226:= as.integer( ccaja_ahorro*mpayroll )]
dataset[ ,feature227:= as.integer( ccaja_ahorro*ccaja_ahorro )]
dataset[ ,feature228:= as.integer( ccaja_ahorro*mcheques_depositados )]
dataset[ ,feature229:= as.integer( ccaja_ahorro*Visa_fechaalta )]
dataset[ ,feature230:= as.integer( ccaja_ahorro*mtarjeta_master_consumo )]
dataset[ ,feature231:= as.integer( ccaja_ahorro*ctarjeta_visa_transacciones )]
dataset[ ,feature232:= as.integer( ccaja_ahorro*Visa_mpagospesos )]
dataset[ ,feature233:= as.integer( ccaja_ahorro*mtarjeta_visa_consumo )]
dataset[ ,feature234:= as.integer( ccaja_ahorro*mpasivos_margen )]
dataset[ ,feature235:= as.integer( ccaja_ahorro*ccallcenter_transacciones )]
dataset[ ,feature236:= as.integer( ccaja_ahorro*cliente_edad )]
dataset[ ,feature237:= as.integer( ccaja_ahorro*mcaja_ahorro )]
dataset[ ,feature238:= as.integer( ccaja_ahorro*Visa_msaldototal )]
dataset[ ,feature239:= as.integer( ccaja_ahorro*ctrx_quarter )]
dataset[ ,feature240:= as.integer( mcheques_depositados*mpayroll )]
dataset[ ,feature241:= as.integer( mcheques_depositados*ccaja_ahorro )]
dataset[ ,feature242:= as.integer( mcheques_depositados*mcheques_depositados )]
dataset[ ,feature243:= as.integer( mcheques_depositados*Visa_fechaalta )]
dataset[ ,feature244:= as.integer( mcheques_depositados*mtarjeta_master_consumo )]
dataset[ ,feature245:= as.integer( mcheques_depositados*ctarjeta_visa_transacciones )]
dataset[ ,feature246:= as.integer( mcheques_depositados*Visa_mpagospesos )]
dataset[ ,feature247:= as.integer( mcheques_depositados*mtarjeta_visa_consumo )]
dataset[ ,feature248:= as.integer( mcheques_depositados*mpasivos_margen )]
dataset[ ,feature249:= as.integer( mcheques_depositados*ccallcenter_transacciones )]
dataset[ ,feature250:= as.integer( mcheques_depositados*cliente_edad )]
dataset[ ,feature251:= as.integer( mcheques_depositados*mcaja_ahorro )]
dataset[ ,feature252:= as.integer( mcheques_depositados*Visa_msaldototal )]
dataset[ ,feature253:= as.integer( mcheques_depositados*ctrx_quarter )]
dataset[ ,feature254:= as.integer( Visa_fechaalta*mpayroll )]
dataset[ ,feature255:= as.integer( Visa_fechaalta*ccaja_ahorro )]
dataset[ ,feature256:= as.integer( Visa_fechaalta*mcheques_depositados )]
dataset[ ,feature257:= as.integer( Visa_fechaalta*Visa_fechaalta )]
dataset[ ,feature258:= as.integer( Visa_fechaalta*mtarjeta_master_consumo )]
dataset[ ,feature259:= as.integer( Visa_fechaalta*ctarjeta_visa_transacciones )]
dataset[ ,feature260:= as.integer( Visa_fechaalta*Visa_mpagospesos )]
dataset[ ,feature261:= as.integer( Visa_fechaalta*mtarjeta_visa_consumo )]
dataset[ ,feature262:= as.integer( Visa_fechaalta*mpasivos_margen )]
dataset[ ,feature263:= as.integer( Visa_fechaalta*ccallcenter_transacciones )]
dataset[ ,feature264:= as.integer( Visa_fechaalta*cliente_edad )]
dataset[ ,feature265:= as.integer( Visa_fechaalta*mcaja_ahorro )]
dataset[ ,feature266:= as.integer( Visa_fechaalta*Visa_msaldototal )]
dataset[ ,feature267:= as.integer( Visa_fechaalta*ctrx_quarter )]
dataset[ ,feature268:= as.integer( mtarjeta_master_consumo*mpayroll )]
dataset[ ,feature269:= as.integer( mtarjeta_master_consumo*ccaja_ahorro )]
dataset[ ,feature270:= as.integer( mtarjeta_master_consumo*mcheques_depositados )]
dataset[ ,feature271:= as.integer( mtarjeta_master_consumo*Visa_fechaalta )]
dataset[ ,feature272:= as.integer( mtarjeta_master_consumo*mtarjeta_master_consumo )]
dataset[ ,feature273:= as.integer( mtarjeta_master_consumo*ctarjeta_visa_transacciones )]
dataset[ ,feature274:= as.integer( mtarjeta_master_consumo*Visa_mpagospesos )]
dataset[ ,feature275:= as.integer( mtarjeta_master_consumo*mtarjeta_visa_consumo )]
dataset[ ,feature276:= as.integer( mtarjeta_master_consumo*mpasivos_margen )]
dataset[ ,feature277:= as.integer( mtarjeta_master_consumo*ccallcenter_transacciones )]
dataset[ ,feature278:= as.integer( mtarjeta_master_consumo*cliente_edad )]
dataset[ ,feature279:= as.integer( mtarjeta_master_consumo*mcaja_ahorro )]
dataset[ ,feature280:= as.integer( mtarjeta_master_consumo*Visa_msaldototal )]
dataset[ ,feature281:= as.integer( mtarjeta_master_consumo*ctrx_quarter )]
dataset[ ,feature282:= as.integer( ctarjeta_visa_transacciones*mpayroll )]
dataset[ ,feature283:= as.integer( ctarjeta_visa_transacciones*ccaja_ahorro )]
dataset[ ,feature284:= as.integer( ctarjeta_visa_transacciones*mcheques_depositados )]
dataset[ ,feature285:= as.integer( ctarjeta_visa_transacciones*Visa_fechaalta )]
dataset[ ,feature286:= as.integer( ctarjeta_visa_transacciones*mtarjeta_master_consumo )]
dataset[ ,feature287:= as.integer( ctarjeta_visa_transacciones*ctarjeta_visa_transacciones )]
dataset[ ,feature288:= as.integer( ctarjeta_visa_transacciones*Visa_mpagospesos )]
dataset[ ,feature289:= as.integer( ctarjeta_visa_transacciones*mtarjeta_visa_consumo )]
dataset[ ,feature290:= as.integer( ctarjeta_visa_transacciones*mpasivos_margen )]
dataset[ ,feature291:= as.integer( ctarjeta_visa_transacciones*ccallcenter_transacciones )]
dataset[ ,feature292:= as.integer( ctarjeta_visa_transacciones*cliente_edad )]
dataset[ ,feature293:= as.integer( ctarjeta_visa_transacciones*mcaja_ahorro )]
dataset[ ,feature294:= as.integer( ctarjeta_visa_transacciones*Visa_msaldototal )]
dataset[ ,feature295:= as.integer( ctarjeta_visa_transacciones*ctrx_quarter )]
dataset[ ,feature296:= as.integer( Visa_mpagospesos*mpayroll )]
dataset[ ,feature297:= as.integer( Visa_mpagospesos*ccaja_ahorro )]
dataset[ ,feature298:= as.integer( Visa_mpagospesos*mcheques_depositados )]
dataset[ ,feature299:= as.integer( Visa_mpagospesos*Visa_fechaalta )]
dataset[ ,feature300:= as.integer( Visa_mpagospesos*mtarjeta_master_consumo )]
dataset[ ,feature301:= as.integer( Visa_mpagospesos*ctarjeta_visa_transacciones )]
dataset[ ,feature302:= as.integer( Visa_mpagospesos*Visa_mpagospesos )]
dataset[ ,feature303:= as.integer( Visa_mpagospesos*mtarjeta_visa_consumo )]
dataset[ ,feature304:= as.integer( Visa_mpagospesos*mpasivos_margen )]
dataset[ ,feature305:= as.integer( Visa_mpagospesos*ccallcenter_transacciones )]
dataset[ ,feature306:= as.integer( Visa_mpagospesos*cliente_edad )]
dataset[ ,feature307:= as.integer( Visa_mpagospesos*mcaja_ahorro )]
dataset[ ,feature308:= as.integer( Visa_mpagospesos*Visa_msaldototal )]
dataset[ ,feature309:= as.integer( Visa_mpagospesos*ctrx_quarter )]
dataset[ ,feature310:= as.integer( mtarjeta_visa_consumo*mpayroll )]
dataset[ ,feature311:= as.integer( mtarjeta_visa_consumo*ccaja_ahorro )]
dataset[ ,feature312:= as.integer( mtarjeta_visa_consumo*mcheques_depositados )]
dataset[ ,feature313:= as.integer( mtarjeta_visa_consumo*Visa_fechaalta )]
dataset[ ,feature314:= as.integer( mtarjeta_visa_consumo*mtarjeta_master_consumo )]
dataset[ ,feature315:= as.integer( mtarjeta_visa_consumo*ctarjeta_visa_transacciones )]
dataset[ ,feature316:= as.integer( mtarjeta_visa_consumo*Visa_mpagospesos )]
dataset[ ,feature317:= as.integer( mtarjeta_visa_consumo*mtarjeta_visa_consumo )]
dataset[ ,feature318:= as.integer( mtarjeta_visa_consumo*mpasivos_margen )]
dataset[ ,feature319:= as.integer( mtarjeta_visa_consumo*ccallcenter_transacciones )]
dataset[ ,feature320:= as.integer( mtarjeta_visa_consumo*cliente_edad )]
dataset[ ,feature321:= as.integer( mtarjeta_visa_consumo*mcaja_ahorro )]
dataset[ ,feature322:= as.integer( mtarjeta_visa_consumo*Visa_msaldototal )]
dataset[ ,feature323:= as.integer( mtarjeta_visa_consumo*ctrx_quarter )]
dataset[ ,feature324:= as.integer( mpasivos_margen*mpayroll )]
dataset[ ,feature325:= as.integer( mpasivos_margen*ccaja_ahorro )]
dataset[ ,feature326:= as.integer( mpasivos_margen*mcheques_depositados )]
dataset[ ,feature327:= as.integer( mpasivos_margen*Visa_fechaalta )]
dataset[ ,feature328:= as.integer( mpasivos_margen*mtarjeta_master_consumo )]
dataset[ ,feature329:= as.integer( mpasivos_margen*ctarjeta_visa_transacciones )]
dataset[ ,feature330:= as.integer( mpasivos_margen*Visa_mpagospesos )]
dataset[ ,feature331:= as.integer( mpasivos_margen*mtarjeta_visa_consumo )]
dataset[ ,feature332:= as.integer( mpasivos_margen*mpasivos_margen )]
dataset[ ,feature333:= as.integer( mpasivos_margen*ccallcenter_transacciones )]
dataset[ ,feature334:= as.integer( mpasivos_margen*cliente_edad )]
dataset[ ,feature335:= as.integer( mpasivos_margen*mcaja_ahorro )]
dataset[ ,feature336:= as.integer( mpasivos_margen*Visa_msaldototal )]
dataset[ ,feature337:= as.integer( mpasivos_margen*ctrx_quarter )]
dataset[ ,feature338:= as.integer( ccallcenter_transacciones*mpayroll )]
dataset[ ,feature339:= as.integer( ccallcenter_transacciones*ccaja_ahorro )]
dataset[ ,feature340:= as.integer( ccallcenter_transacciones*mcheques_depositados )]
dataset[ ,feature341:= as.integer( ccallcenter_transacciones*Visa_fechaalta )]
dataset[ ,feature342:= as.integer( ccallcenter_transacciones*mtarjeta_master_consumo )]
dataset[ ,feature343:= as.integer( ccallcenter_transacciones*ctarjeta_visa_transacciones )]
dataset[ ,feature344:= as.integer( ccallcenter_transacciones*Visa_mpagospesos )]
dataset[ ,feature345:= as.integer( ccallcenter_transacciones*mtarjeta_visa_consumo )]
dataset[ ,feature346:= as.integer( ccallcenter_transacciones*mpasivos_margen )]
dataset[ ,feature347:= as.integer( ccallcenter_transacciones*ccallcenter_transacciones )]
dataset[ ,feature348:= as.integer( ccallcenter_transacciones*cliente_edad )]
dataset[ ,feature349:= as.integer( ccallcenter_transacciones*mcaja_ahorro )]
dataset[ ,feature350:= as.integer( ccallcenter_transacciones*Visa_msaldototal )]
dataset[ ,feature351:= as.integer( ccallcenter_transacciones*ctrx_quarter )]
dataset[ ,feature352:= as.integer( cliente_edad*mpayroll )]
dataset[ ,feature353:= as.integer( cliente_edad*ccaja_ahorro )]
dataset[ ,feature354:= as.integer( cliente_edad*mcheques_depositados )]
dataset[ ,feature355:= as.integer( cliente_edad*Visa_fechaalta )]
dataset[ ,feature356:= as.integer( cliente_edad*mtarjeta_master_consumo )]
dataset[ ,feature357:= as.integer( cliente_edad*ctarjeta_visa_transacciones )]
dataset[ ,feature358:= as.integer( cliente_edad*Visa_mpagospesos )]
dataset[ ,feature359:= as.integer( cliente_edad*mtarjeta_visa_consumo )]
dataset[ ,feature360:= as.integer( cliente_edad*mpasivos_margen )]
dataset[ ,feature361:= as.integer( cliente_edad*ccallcenter_transacciones )]
dataset[ ,feature362:= as.integer( cliente_edad*cliente_edad )]
dataset[ ,feature363:= as.integer( cliente_edad*mcaja_ahorro )]
dataset[ ,feature364:= as.integer( cliente_edad*Visa_msaldototal )]
dataset[ ,feature365:= as.integer( cliente_edad*ctrx_quarter )]
dataset[ ,feature366:= as.integer( mcaja_ahorro*mpayroll )]
dataset[ ,feature367:= as.integer( mcaja_ahorro*ccaja_ahorro )]
dataset[ ,feature368:= as.integer( mcaja_ahorro*mcheques_depositados )]
dataset[ ,feature369:= as.integer( mcaja_ahorro*Visa_fechaalta )]
dataset[ ,feature370:= as.integer( mcaja_ahorro*mtarjeta_master_consumo )]
dataset[ ,feature371:= as.integer( mcaja_ahorro*ctarjeta_visa_transacciones )]
dataset[ ,feature372:= as.integer( mcaja_ahorro*Visa_mpagospesos )]
dataset[ ,feature373:= as.integer( mcaja_ahorro*mtarjeta_visa_consumo )]
dataset[ ,feature374:= as.integer( mcaja_ahorro*mpasivos_margen )]
dataset[ ,feature375:= as.integer( mcaja_ahorro*ccallcenter_transacciones )]
dataset[ ,feature376:= as.integer( mcaja_ahorro*cliente_edad )]
dataset[ ,feature377:= as.integer( mcaja_ahorro*mcaja_ahorro )]
dataset[ ,feature378:= as.integer( mcaja_ahorro*Visa_msaldototal )]
dataset[ ,feature379:= as.integer( mcaja_ahorro*ctrx_quarter )]
dataset[ ,feature380:= as.integer( Visa_msaldototal*mpayroll )]
dataset[ ,feature381:= as.integer( Visa_msaldototal*ccaja_ahorro )]
dataset[ ,feature382:= as.integer( Visa_msaldototal*mcheques_depositados )]
dataset[ ,feature383:= as.integer( Visa_msaldototal*Visa_fechaalta )]
dataset[ ,feature384:= as.integer( Visa_msaldototal*mtarjeta_master_consumo )]
dataset[ ,feature385:= as.integer( Visa_msaldototal*ctarjeta_visa_transacciones )]
dataset[ ,feature386:= as.integer( Visa_msaldototal*Visa_mpagospesos )]
dataset[ ,feature387:= as.integer( Visa_msaldototal*mtarjeta_visa_consumo )]
dataset[ ,feature388:= as.integer( Visa_msaldototal*mpasivos_margen )]
dataset[ ,feature389:= as.integer( Visa_msaldototal*ccallcenter_transacciones )]
dataset[ ,feature390:= as.integer( Visa_msaldototal*cliente_edad )]
dataset[ ,feature391:= as.integer( Visa_msaldototal*mcaja_ahorro )]
dataset[ ,feature392:= as.integer( Visa_msaldototal*Visa_msaldototal )]


#defino los datos donde entreno
dtrain  <- dataset[ foto_mes==202101, ]


#creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
dir.create( "./exp/",  showWarnings = FALSE ) 
dir.create( "./exp/HT113111/", showWarnings = FALSE )
setwd("./exp/HT113111/")   #Establezco el Working Directory DEL EXPERIMENTO

#defino los archivos donde guardo los resultados de la Bayesian Optimization
archivo_log  <- "HT20220910_1.txt"
archivo_BO   <- "HT20220910_1.RDATA"

#leo si ya existe el log, para retomar en caso que se se corte el programa
GLOBAL_iteracion  <- 0

if( file.exists(archivo_log) )
{
 tabla_log  <- fread( archivo_log )
 GLOBAL_iteracion  <- nrow( tabla_log )
}



#Aqui comienza la configuracion de la Bayesian Optimization

funcion_optimizar  <- EstimarGanancia

configureMlr( show.learner.output= FALSE)

#configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar
#por favor, no desesperarse por lo complejo
obj.fun  <- makeSingleObjectiveFunction(
              fn=       funcion_optimizar,
              minimize= FALSE,   #estoy Maximizando la ganancia
              noisy=    TRUE,
              par.set=  hs,
              has.simple.signature = FALSE   #espia Tomas Delvechio, dejar este parametro asi
             )

ctrl  <- makeMBOControl( save.on.disk.at.time= 600,  save.file.path= archivo_BO)
ctrl  <- setMBOControlTermination(ctrl, iters= kBO_iter )
ctrl  <- setMBOControlInfill(ctrl, crit= makeMBOInfillCritEI())

surr.km  <- makeLearner("regr.km", predict.type= "se", covtype= "matern3_2", control= list(trace= TRUE))

#inicio la optimizacion bayesiana
if( !file.exists( archivo_BO ) ) {

  run  <- mbo( fun=     obj.fun, 
               learner= surr.km,
               control= ctrl)

} else  run  <- mboContinue( archivo_BO )   #retomo en caso que ya exista

