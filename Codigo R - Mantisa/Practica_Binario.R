#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
#Convertir a binario
#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
options(scipen = 999)
Convertir_a_binario <- function(n, dec) {


# --------------
# Validaciones
# --------------


if (!is.numeric(n)) {
  stop("El argumento 'numero' debe ser numérico")
}
if (length(n) != 1) {
  stop("El argumento 'numero' debe ser un único valor")
}
if (dec < 0 || dec != as.integer(dec)) {
  stop("'decimales' debe ser un entero no negativo")
}


# ------------------
# Manejo del signo
# ------------------


signo <- if (n < 0) "-" else ""
n <- abs(n)


# ----------------------------------------------------------
# Redondeo en base 10 para evitar errores de punto flotante
# ----------------------------------------------------------

n <- round(n, dec)


# ------------------------------------
# Separar parte entera y fraccionaria
# ------------------------------------

part_ent <- floor(n)
Part_dec <- n - part_ent

bits <- integer(0)
bits_dec <- integer(0)

#-------------
#Parte entera
#-------------


if(part_ent == 0) {
  
  bin_entero <- 0
  } else {
  
  while (part_ent > 0) {
  
  resto <- part_ent %% 2
  bits <- c(resto, bits)
  part_ent <- part_ent %/% 2
  
  }
    bin_entero <- paste(bits, collapse = "")
    }

#-------------
#Parte decimal
#-------------

bits_dec <- integer(dec)
f <- Part_dec

for (i in seq_len(dec)) {
  
  f <- f * 2
  bits_dec[i] <- floor(f)
  f <- f - floor(f)
  
}

bin_dec <- paste(bits_dec, collapse = "")

#----------
#Resultado
#----------

gsub(" ", "", paste(signo, bin_entero,".", bin_dec))
}




#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
#Estandar IEEE 754
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------

Binario_estandar <- function(numero) {
  
  
binario <- Convertir_a_binario(numero, 40)

#----------------
#Determinar signo
#----------------

if (numero >= 0) {
  s=0
} else {
  s=1
}


#-----------
#Rodar punto
#-----------


posicion <- regexpr("1", binario) ##[1]

primer <- substr(binario, posicion, posicion )
segundo <- substr(binario, posicion+1, nchar(binario))
segundo <- gsub(".", "", segundo,fixed = TRUE)

binario_n <-gsub( " ", "", paste(primer,".",segundo))


#----------------------
#Determinar exponente
#----------------------

if(numero == 0) {
  
  exponente = -127
  
} else if ( abs(numero) > 0 & abs(numero) < 1 ) {
    
    exponente <- regexpr(".", binario, fixed = TRUE)[1] - posicion 
    
  } else {
  
    exponente <- regexpr(".", binario, fixed = TRUE)[1] - posicion -1 
}


E <- exponente + 127

binario_ex  <- Convertir_a_binario(E, 6)

punto_ex  <- regexpr(".", binario_ex, fixed = TRUE)

Bits_8 <- substr(binario_ex, 1, punto_ex-1)


if(nchar(Bits_8) == 8) {
  
  exponente_alm <- Bits_8
  
} else if(nchar(Bits_8)> 8) {
  
  exponente_alm <- substr(Bits_8, 1, 8)
} else {
  exponente_alm <- gsub(" ","", paste(strrep("0",(8-nchar(Bits_8))), Bits_8))
}


#-----------------------------------
# 23 bits de mantisa (con redondeo)
#-----------------------------------


if(as.numeric(substr(segundo, 24,24)) == 1){

  i=25
  
for (i in seq_len(nchar(segundo))) {
  if(substr(segundo,i,i)==1){
    redondeo = TRUE
  }
if(redondeo == TRUE) {
  mantisa <- substr(segundo,1,23)
  emp <-gregexpr("0",mantisa)[[1]][length(gregexpr("0",mantisa)[[1]])]
  part_1 <- substr(mantisa, 1,emp-1)
  part_2 <- gsub( "1", "0", substr(mantisa,emp+1,nchar(mantisa)))
  mantisa_completa <- gsub( " ", "", paste(part_1,"1",part_2))
  
} else {
  mantisa_completa <- substr(segundo, 1,23)
}}
  } else {
  mantisa_completa <- substr(segundo, 1,23)
}


#-----------------
#Binario completo
#-----------------


return(list(Estandar = paste(s, exponente_alm, mantisa_completa), Mantisa = mantisa_completa, Exponente = E))

}

#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
#Suma y resta de binarios
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------


Sumar_binario <- function(n_1,n_2){
  
a <- Binario_estandar(n_1)
b <- Binario_estandar(n_2)

#-----------------------------------
#Obtener la mantisa de cada numero
#-----------------------------------

man_1 <- gsub(" ", "", paste("1", ".", a$Mantisa))
man_2 <- gsub(" ", "", paste("1", ".", b$Mantisa))

#------------------
#Restar exponentes
#------------------

diferencia <- a$Exponente - b$Exponente

#-------------------------------------
#Desplazar bist del valor mas pequeño
#-------------------------------------

if (diferencia > 0){
  man_2 <- substr(gsub(" ","",paste("0",".",strrep("0",abs(diferencia)-1),gsub(".","",man_2, fixed = TRUE))),1,25)
}else {
  man_1 <- substr(gsub(" ","",paste("0",".",strrep("0",abs(diferencia)-1),gsub(".","",man_1, fixed = TRUE))),1,25)
  
}

if (abs(n_1) >abs(n_2)){
  
x<- as.numeric(rev(strsplit(gsub(".", "", man_1, fixed = TRUE), "")[[1]]))
z<-as.numeric(rev(strsplit(gsub(".", "", man_2, fixed = TRUE), "")[[1]]))

} else {
  x<- as.numeric(rev(strsplit(gsub(".", "", man_2, fixed = TRUE), "")[[1]]))
  z<-as.numeric(rev(strsplit(gsub(".", "", man_1, fixed = TRUE), "")[[1]]))
}



#--------------------
#Sumar las mantisas
#--------------------

acarreo <-0
operacion <- integer(0)


if (n_1>0 & n_2>0 || n_1<0 & n_2<0 ) {

for (i in seq_len(24)) {

  if (x[i]== 1 & z[i]== 1) {
  
    if(acarreo==1) {
      operacion <- c(operacion,1)
      acarreo <- 1
    } else {
      operacion <- c(operacion,0)
      acarreo <- 1
    }

} else if (x[i]== 0 & z[i]== 1 || x[i]== 1 & z[i]== 0 ){
  
  
  if(acarreo==1) {
    operacion <- c(operacion,0)
    acarreo <- 1
  } else {
    operacion <- c(operacion,1)
    acarreo <- 0
  }
  
} else if (x[i]== 0 & z[i]== 0){
  
  if(acarreo==1) {
    operacion <- c(operacion,1)
    acarreo <- 0
  } else {
    operacion <- c(operacion,0)
    acarreo <- 0
  }
 
}}} else { 
  
  for (i in seq_len(24)) {
    
    if (x[i]== 0 & z[i]== 1) {
      
      if(acarreo==1) {
        operacion <- c(operacion,0)
        acarreo <- 1
      } else {
        operacion <- c(operacion,1)
        acarreo <- 1
      }
      
    } else if (x[i]== 1 & z[i]== 1 || x[i]== 0 & z[i]== 0 ){
      
      
      if(acarreo==1) {
        operacion <- c(operacion,1)
        acarreo <- 1
      } else {
        operacion <- c(operacion,0)
        acarreo <- 0
      }
      
    } else if (x[i]== 1 & z[i]== 0){
      
      if(acarreo==1) {
        operacion <- c(operacion,0)
        acarreo <- 0
      } else {
        operacion <- c(operacion,1)
        acarreo <- 0
      }
      
    }
  }


#---------------------------
#Resultado de la operacion 
#---------------------------

if (abs(n_1)>abs(n_2)){
 sig <- substr(a$Estandar, 1, 1)
 exp <- substr(a$Estandar, 3, 10)
  
} else{
  sig <- substr(b$Estandar, 1, 1)
  exp <- substr(b$Estandar, 3, 10)
}


operacion <- append(as.character(rev(operacion)), values = ".", after = 1)
operacion <- gsub(" ","",paste( operacion ,collapse = ""))

binario_32 <- paste(sig, exp, substr(operacion,3,nchar(operacion)))

return(list(operacion = operacion, Binario = binario_32))


##Append es una funcion que sirve para agregar un valor dentro de un caracter
}}


z <- Binario_estandar(33554432)
h <- Sumar_binario(0.375, -12.625)
c <- Sumar_binario(1, 2^-24)
