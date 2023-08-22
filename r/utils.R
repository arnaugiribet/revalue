f_hipoteca <- function(cost,entrada){
  
  if(cost<=entrada){
    hipoteca<-0
  } else{
    hipoteca<-cost-entrada
  }
  return(round(hipoteca,2))
}

f_quotaMensual <- function(hipoteca,interes,anys){
  
  if(hipoteca==0){
    quotaMensual<-0
  } else{
    quotaMensual<-hipoteca/((1-(1+interes/12)^(-anys*12))/interes)/12
  }
  return(round(quotaMensual,2))
}