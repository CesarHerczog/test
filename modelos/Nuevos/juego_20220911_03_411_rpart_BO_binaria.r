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
ksemilla_azar  <- c(113111, 724111, 725111, 742111, 644131, 734131)


#Defino la  Optimizacion Bayesiana

kBO_iter  <- 50  #cantidad de iteraciones de la Optimizacion Bayesiana

hs  <- makeParamSet(
          makeNumericParam("cp"       , lower=  -0.6, upper=    -0.56),
          makeNumericParam("minsplit" , lower=   1180L,   upper= 1300L ),
          makeNumericParam("minbucket", lower=   240L,   upper= 310L ),
          makeIntegerParam("maxdepth" , lower=   8L,  upper=   9L),  #la letra L al final significa ENTERO
          forbidden = quote( minbucket > 0.5*minsplit ) )             # minbuket NO PUEDE ser mayor que la tercera


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


#Nuevas Features útiles

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


#defino los datos donde entreno
dtrain  <- dataset[ foto_mes==202101, ]


#creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
dir.create( "./exp/",  showWarnings = FALSE ) 
dir.create( "./exp/HT113111/", showWarnings = FALSE )
setwd("./exp/HT113111/")   #Establezco el Working Directory DEL EXPERIMENTO

#defino los archivos donde guardo los resultados de la Bayesian Optimization
archivo_log  <- "HT20220911_01.txt"
archivo_BO   <- "HT20220911_01.RDATA"

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

