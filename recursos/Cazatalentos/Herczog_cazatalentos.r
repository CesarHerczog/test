#Solucion posible del desafio  15k


#Se logró un resultado de un XX% sin superar 


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

set.seed( 102191 )

#inicializo el juego
gimnasio_init()

#Esta el la planilla del cazatalentos
planilla_cazatalentos  <- data.table( "id" = 1:100 )


#variables globales que usan las funciones gimnasio_xxxx
tasa_de_aciertos  <- 0
#GLOBAL_tiros_total  <- 0
MAX_GLOBAL_tiros_total  <- 0


for( i in 1:10000 ) { #diez mil experimentos
  
  GLOBAL_tiros_total  <- 0
  tiros_r1 <- 66 #70 #40
  tiros_r2 <- 33 
  tiros_r3 <- 31
  tiros_r4 <- 23 #30
  tiros_r5 <- 22 #30
  tiros_r6 <- 25 
  tiros_r7 <- 200 
  
  poda_r1 <- 0    #Solo la declaro para imprimir al final
  poda_r2 <- 31#(tiros_r1) * 99 / 411 #Elimino al los que estan por debajo de percentil  
  poda_r3 <- 31#(tiros_r1 + tiros_r2) * 99 / 411  #Elimino al los que estan por debajo de percentil  
  poda_r4 <- 31#30(tiros_r1 + tiros_r2 + tiros_r3) * 99 / 411  #Elimino al los que estan por debajo de percentil  
  poda_r5 <- 31#(tiros_r1 + tiros_r2 + tiros_r3 + tiros_r4) * 99 / 411  #Elimino al los que estan por debajo de percentil  
  poda_r6 <- 31#(tiros_r1 + tiros_r2 + tiros_r3 + tiros_r4 + tiros_r5) * 99 / 411  #Elimino al los que estan por debajo de percentil  
  poda_r7 <- 5#(tiros_r1 + tiros_r2 + tiros_r3 + tiros_r4 + tiros_r5 + tiros_r6) * 99 / 411 #Elimino al los que estan por debajo de percentil  

  planilla_cazatalentos  <- data.table( "id" = 1:100 )
  
  #Ronda 1  ------------------------------------------------------
  #tiran los 100 jugadores es decir 1:100 50 tiros libres cada uno
  ids_juegan1  <- 1:100   #los jugadores que participan en la ronda,
  
  planilla_cazatalentos[ ids_juegan1,  tiros1 := tiros_r1 ]  #registro en la planilla que tiran 90 tiros
  
  #Hago que tiren
  resultado1  <- gimnasio_tirar( ids_juegan1, tiros_r1)
  planilla_cazatalentos[ ids_juegan1,  aciertos1 := resultado1 ]  #registro en la planilla
  
  
  #Ronda 2 -------------------------------------------------------
  #los mejores 40 jugadores tiran x tiros cada uno
  minmed  <- planilla_cazatalentos[ ids_juegan1, quantile(aciertos1, prob=seq(0, 1, length = 101))[poda_r2]]
  ids_juegan2  <- planilla_cazatalentos[ ids_juegan1 ][aciertos1 > minmed, id ]
  
  planilla_cazatalentos[ ids_juegan2,  tiros2 := tiros_r2 ]  #registro en la planilla que tiran x tiros
  resultado2  <- gimnasio_tirar( ids_juegan2, tiros_r2)
  planilla_cazatalentos[ ids_juegan2,  aciertos2 := resultado2 ]  #registro en la planilla lo resultados de la ronda 2
  planilla_cazatalentos[ ids_juegan2,  acumulado2 := (aciertos1+aciertos2) ] #acumulo los resultados
  
  #Ronda 3 -------------------------------------------------------
  #los mejores 40 jugadores tiran x tiros cada uno
  
  minmed  <- planilla_cazatalentos[ ids_juegan2, quantile(acumulado2, prob=seq(0, 1, length = 101))[poda_r3]]
  ids_juegan3  <- planilla_cazatalentos[ ids_juegan2 ][acumulado2 > minmed, id ]
  
  
  planilla_cazatalentos[ ids_juegan3,  tiros3 := tiros_r3 ]  #registro en la planilla que tiran x tiros
  resultado3  <- gimnasio_tirar( ids_juegan3, tiros_r3)
  planilla_cazatalentos[ ids_juegan3,  aciertos3 := resultado3 ]  #registro en la planilla la suma de la ronda uno y la dos
  planilla_cazatalentos[ ids_juegan3,  acumulado3 := (acumulado2+aciertos3) ]
  
  #Ronda 4 -------------------------------------------------------
  #los mejores 40 jugadores tiran x tiros cada uno
  
  
  minmed  <- planilla_cazatalentos[ ids_juegan3, quantile(acumulado3, prob=seq(0, 1, length = 101))[poda_r4] ]
  ids_juegan4  <- planilla_cazatalentos[ ids_juegan3 ][acumulado3 > minmed, id ]
  
  planilla_cazatalentos[ ids_juegan4,  tiros4 := tiros_r4 ]  #registro en la planilla que tiran x tiros
  resultado4  <- gimnasio_tirar( ids_juegan4, tiros_r4)
  planilla_cazatalentos[ ids_juegan4,  aciertos4 := resultado4 ]  #registro en la planilla la suma de la ronda uno y la dos
  planilla_cazatalentos[ ids_juegan4,  acumulado4 := (acumulado3+aciertos4) ]
  
  #Ronda 5 -------------------------------------------------------
  #los mejores 40 jugadores tiran x tiros cada uno
  
  
  minmed  <- planilla_cazatalentos[ ids_juegan4, quantile(acumulado4, prob=seq(0, 1, length = 101))[poda_r5] ]
  ids_juegan5  <- planilla_cazatalentos[ ids_juegan4 ][acumulado4 > minmed, id ]
  
  planilla_cazatalentos[ ids_juegan5,  tiros5 := tiros_r5 ]  #registro en la planilla que tiran x tiros
  resultado5  <- gimnasio_tirar( ids_juegan5, tiros_r5)
  planilla_cazatalentos[ ids_juegan5,  aciertos5 := resultado5 ]  #registro en la planilla la suma de la ronda uno y la dos
  planilla_cazatalentos[ ids_juegan5,  acumulado5 := (acumulado4+aciertos5) ]  
  
  #Ronda 6 -------------------------------------------------------
  #los mejores 40 jugadores tiran x tiros cada uno
  
  
  minmed  <- planilla_cazatalentos[ ids_juegan5, quantile(acumulado5, prob=seq(0, 1, length = 101))[poda_r6] ]
  ids_juegan6  <- planilla_cazatalentos[ ids_juegan5 ][acumulado5 > minmed, id ]
  
  planilla_cazatalentos[ ids_juegan6,  tiros6 := tiros_r6 ]  #registro en la planilla que tiran x tiros
  resultado6  <- gimnasio_tirar( ids_juegan6, tiros_r6)
  planilla_cazatalentos[ ids_juegan6,  aciertos6 := resultado6 ]  #registro en la planilla la suma de la ronda uno y la dos
  planilla_cazatalentos[ ids_juegan6,  acumulado6 := (acumulado5+aciertos6) ]
  
  #Ronda 7 -------------------------------------------------------
  #los mejores 40 jugadores tiran x tiros cada uno
  
  
  minmed  <- planilla_cazatalentos[ ids_juegan6, quantile(acumulado6, prob=seq(0, 1, length = 101))[poda_r7] ]
  ids_juegan7  <- planilla_cazatalentos[ ids_juegan6 ][acumulado6 > minmed, id ]
  
  planilla_cazatalentos[ ids_juegan6,  tiros7 := tiros_r7 ]  #registro en la planilla que tiran x tiros
  resultado7  <- gimnasio_tirar( ids_juegan7, tiros_r7)
  planilla_cazatalentos[ ids_juegan7,  aciertos7 := resultado7 ]  #registro en la planilla la suma de la ronda uno y la dos
  planilla_cazatalentos[ ids_juegan7,  acumulado7 := (acumulado6+aciertos7) ]
  
  #El cazatalentos toma una decision, elige al que mas aciertos tuvo en la ronda2
  pos_mejor <-  planilla_cazatalentos[ , which.max(acumulado7) ]
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
cat(tiros_r1 , tiros_r2, tiros_r3, tiros_r4, tiros_r5, tiros_r6, tiros_r7, MAX_GLOBAL_tiros_total, tasa_de_aciertos/100, "\n" )
#Eliminación a quienes estan debajo de X percentil antes de cada ronda.
cat(poda_r1 , poda_r2, poda_r3, poda_r4, poda_r5, poda_r6, poda_r7, MAX_GLOBAL_tiros_total, tasa_de_aciertos/100, "\n" )

