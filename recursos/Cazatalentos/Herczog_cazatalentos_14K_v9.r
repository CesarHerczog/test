#Solucion del desafio  14k
#Se logró un resultado de un XX% (con la semilla 102191) sin superar los en ningun intento los 13K


#limpio la memoria
rm( list=ls() )
gc()


require("data.table")

ftirar  <- function( prob, qty )
{
  return(  sum( runif(qty) < prob ) )
}


GLOBAL_jugadores  <- c()

#Crea el juego
#a cada jugador se le pone un numero de 1 a 100 en la espalda
#debajo de ese numero esta el indice_de_enceste  que NO puede ser visto por el cazatalentos
gimnasio_init  <- function() 
{
  GLOBAL_jugadores  <<-  sample( c( (501:599 ) / 1000 , 0.7 ) )
  GLOBAL_tiros_total  <<- 0
}


#se le pasa un vector con los IDs de los jugadores y la cantidad de tiros a realizar
#devuelve en un vector cuantos aciertos tuvo cada jugador
gimnasio_tirar  <- function(  pids,  pcantidad )
{
  GLOBAL_tiros_total  <<-  GLOBAL_tiros_total + length( pids )*pcantidad
  res  <- mapply(  ftirar, GLOBAL_jugadores[pids], pcantidad )
  
  return( res )
}


#El cazatalentos decide a que jugador llevarse
#devuelve la cantidad de tiros libres y si le acerto al verdadero_mejor o no
gimnasio_veredicto  <- function( pid )
{
  return( list("tiros_total"= GLOBAL_tiros_total, 
               "acierto"=     as.integer( GLOBAL_jugadores[pid]==0.7) ))
}
#------------------------------------------------------------------------------

set.seed( 113111 )

#inicializo el juego
gimnasio_init()

#Esta el la planilla del cazatalentos
planilla_cazatalentos  <- data.table( "id" = 1:100 )


#variables globales que usan las funciones gimnasio_xxxx
tasa_de_aciertos  <- 0
#GLOBAL_tiros_total  <- 0
MAX_GLOBAL_tiros_total  <- 0


exito <- function()




  for( minimo in c(10) ){
    for( maximo in c(10) ){
      for( valor in c(0) ){
        for( suma in c(0) ){
          for( potencia in c(0) ){
            for( minimo_r in c(2) ){
              for( maximo_r in c(50) ){
                for( valor_r in c(1,2)){
                  for( suma_r in c(2,3,4,5)){
                    for( potencia_r in c(0.6,0.9,1.2,1.5)){
                      tasa_de_aciertos  <- 0 # % de veces que salió elegido el mejor
                      MAX_GLOBAL_tiros_total  <- 0
                      
                      for( i in 1:1000 ) { #diez mil experimentos
                        n = 1
                        acumulado_ratio_de_acierto <-0
                        acumulado_aciertos <- 0
                        acumulado_tiros <- 0
                        acumulado_potencia <- 1
                        GLOBAL_tiros_total  <- 0
                        cant_tiros = minimo #Cantidad de tiros con la que empieza la primera ronda
                        ratio = minimo_r
                        
                        planilla_cazatalentos  <- data.table( "id" = 1:100 ) #Creación de la planilla del cazatalentos
                        idjuegan <- 1:100   #los jugadores que participan la primera ronda
                        
                        for( rondas in 1:11){
                         
                          rondas_pasadas <- n-1
                          
                          #Juegan
                          planilla_cazatalentos[ idjuegan,  paste("tiros",n, sep="") := cant_tiros ]  #registro en la planilla la cantidad de tiros
                          resultado <- gimnasio_tirar( idjuegan, cant_tiros)
                          planilla_cazatalentos[ idjuegan,  paste("aciertos",n, sep="") := resultado ]  #registro en la planilla lo resultados de la ronda
                          ratio_de_acierto <- resultado/(cant_tiros+0.01) #defino un ratio de aciertos
                          
                          #Valores acumulados
                          planilla_cazatalentos[ idjuegan,  acumulado_tiros := (acumulado_tiros+cant_tiros) ]
                          planilla_cazatalentos[ idjuegan,  acumulado_aciertos := (acumulado_aciertos+resultado)]
                          planilla_cazatalentos[ idjuegan,  acumulado_ratio_de_acierto := (((acumulado_ratio_de_acierto*rondas_pasadas)+ratio_de_acierto)/n) ]
                          
                          #Valores de la ronda
                          planilla_cazatalentos[ idjuegan,  paste("acumulado_aciertos",n, sep="") := (acumulado_aciertos+resultado)]
                          planilla_cazatalentos[ idjuegan,  paste("acumulado_tiros",n, sep="") := (acumulado_tiros+cant_tiros) ]
                          planilla_cazatalentos[ idjuegan,  paste("ratio_acierto",n, sep="") := (((acumulado_ratio_de_acierto*rondas_pasadas)+ratio_de_acierto)/n) ]
                          
                          #Selección de quienes no pasan de ronda
                          
                          idjuegan  <- planilla_cazatalentos[ idjuegan ][acumulado_ratio_de_acierto > ratio/100 , id ]
                          
                          #Aumento de ratio para la proxima ronda
                          
                          acumulado_potencia <- acumulado_potencia+potencia_r
                          suma_de_ratio = ((valor_r+suma_r)^acumulado_potencia)+ratio
                          if (suma_de_ratio > maximo_r) ratio <- maximo_r
                          if (suma_de_ratio <= maximo_r) ratio <- suma_de_ratio 
                          if ((max(planilla_cazatalentos$acumulado_ratio_de_acierto)*100)+2 <= ratio) ratio <- (max(planilla_cazatalentos$acumulado_ratio_de_acierto)*100)-2.01 
                          
                          
                          #Cantidad de tiros para la siguiente ronda
                          
                          suma_de_tiros = ((valor+suma)^potencia)+cant_tiros
                          if (suma_de_tiros > maximo) cant_tiros <- maximo
                          if (suma_de_tiros <= maximo) cant_tiros <- suma_de_tiros 
                          
                          #Para quienes pasan de ronda
                          n = n + 1
                          #idjuegan <- paste("ids_juegan",n, sep = "")
                          tiros <- paste("tiros",n, sep = "")
                          resultado <- paste("resultado",n, sep = "")
                        }  
                        
                        #Ultima ronda
                        
                        #Juegan
                        planilla_cazatalentos[ idjuegan,  paste("tiros",n, sep="") := cant_tiros ]  #registro en la planilla la cantidad de tiros
                        resultado <- gimnasio_tirar( idjuegan, (450-acumulado_tiros))
                        planilla_cazatalentos[ idjuegan,  paste("aciertos",n, sep="") := resultado ]  #registro en la planilla lo resultados de la ronda
                        ratio_de_acierto <- resultado/(450-acumulado_tiros) #defino un ratio de aciertos
                        
                        #Valores acumulados
                        planilla_cazatalentos[ idjuegan,  acumulado_tiros := (acumulado_tiros+cant_tiros) ]
                        planilla_cazatalentos[ idjuegan,  acumulado_aciertos := (acumulado_aciertos+resultado)]
                        planilla_cazatalentos[ idjuegan,  acumulado_ratio_de_acierto := (((acumulado_ratio_de_acierto*rondas_pasadas)+ratio_de_acierto)/n) ]
                        
                        
                        #El cazatalentos toma una decision, elige al que mas aciertos tuvo en la ronda2
                        pos_mejor <-  planilla_cazatalentos[ , which.max(acumulado_ratio_de_acierto*acumulado_aciertos^2) ]#aca está la clave de todo
                        id_mejor  <-  planilla_cazatalentos[ pos_mejor, id ]
                        #Finalmente, la hora de la verdadero_mejor
                        #Termino el juego
                        veredicto  <- gimnasio_veredicto( id_mejor )
                        veredicto[2]
                        #El veredicto da que la estrategia seguida por el cazatalentos fue exitosa para este caso
                        #Le acerto al verdadero_mejor
                        #En el siguiente script veremos de hacer una Estimacion Montecarlo
                        #De 10000 veces que el entrenador sigue esta estrategia, cuantas realmente le acierta
                        if(GLOBAL_tiros_total >= MAX_GLOBAL_tiros_total) MAX_GLOBAL_tiros_total <- GLOBAL_tiros_total
                        if( veredicto[2] == 1 )  tasa_de_aciertos  <- tasa_de_aciertos + 1
                      }
                      #Tiros por ronda.
                      cat(rondas,minimo, maximo, valor, suma, potencia, MAX_GLOBAL_tiros_total, tasa_de_aciertos/10, "\n" )
                      #Tiros por ronda.
                      cat(rondas,minimo_r, maximo_r, valor_r, suma_r, potencia_r, MAX_GLOBAL_tiros_total, tasa_de_aciertos/10, "\n" )
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }


