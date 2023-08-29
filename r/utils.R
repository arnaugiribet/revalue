roundup2 <- function(x, digits=0) {
  posneg = sign(x)
  z = abs(x)*10^digits
  z = z + 0.5 + sqrt(.Machine$double.eps)
  z = trunc(z)
  z = z/10^digits
  z*posneg
}

f_hipoteca <- function(cost,entrada){
  
  if(cost<=entrada){
    hipoteca<-0
  } else{
    hipoteca<-cost-entrada
  }
  return(round(hipoteca,2))
}

f_quotaMensual <- function(hipoteca,interes,anys){
  
  if(interes==0){
    quotaMensual<-hipoteca/30/12
  } else{
    if(hipoteca==0){
      quotaMensual<-0
      } else{
        quotaMensual<-hipoteca/((1-(1+interes/12)^(-anys*12))/interes)/12
      }
    }
  return(quotaMensual)
}

f_hipotecaRestant <- function(hipoteca,anys,interes,quota_mensual){
  
  #mensual (es necessita per l'anual)
  interes_mensual<-c()
  amortitzacio_mensual<-c()
  for(mes in 1:(anys*12)){
    interes_mensual[mes]<-(hipoteca-sum(amortitzacio_mensual))*interes/12
    amortitzacio_mensual[mes]<-quota_mensual-interes_mensual[mes]
  }
  hipoteca_restant_mensual<-hipoteca-cumsum(amortitzacio_mensual)
  
  #anual
  interes_anual<-c()
  amortitzacio_anual<-c()
  for(any in 1:anys){
    interes_anual[any]<-sum(interes_mensual[(((any-1)*12+1):(any*12))])
    amortitzacio_anual[any]<-sum(amortitzacio_mensual[(((any-1)*12+1):(any*12))])
  }
  
  amortitzacioAnual<-roundup2(c(0,amortitzacio_anual),2)
  amortitzacioAcumulada<-roundup2(cumsum(amortitzacioAnual))
  hipotecaRestantAnual<-roundup2(hipoteca-amortitzacioAcumulada,2)
  
  return(list(hipotecaRestantAnual=hipotecaRestantAnual,
              amortitzacioAnual=amortitzacioAnual,
              amortitzacioAcumulada=amortitzacioAcumulada))
}
  
  
f <- function(x) {
  format(roundup2(x,2),big.mark = '.',decimal.mark = ',', nsmall=2)
}


f_dadesEvolucioHipoteca <- function(anys,quotaMensual,hipotecaRestantAnual){

  #sèries anuals
  quota_anual_acumulada=roundup2(12*quotaMensual*(0:anys),2)

  dadesEvolucioHipoteca<-data.frame(
    `Hipoteca Restant` = hipotecaRestantAnual$hipotecaRestantAnual,
    `Valor Amortitzat Hipoteca` = hipotecaRestantAnual$amortitzacioAcumulada,
    `Quota Pagada` = quota_anual_acumulada,
    `Any` = (0:anys), check.names = F)
  
  return(dadesEvolucioHipoteca)
}


f_dadesEstalviHipoteca <- function(ingressos,lloguer,costVida,
                      anys,entrada,quotaMensual,hipotecaRestantAnual,
                      upfrontImprovements,valueIncreaseImprovements,manteniment,
                      cancelacioHipoteca,honoraris,incrementValor,
                      preu,cost){
  
  #valors inicial
  habitatge_amortitzat_inicial<-entrada-(cost-preu)
  
  #valors constants al temps
  estalvi_previ<-ingressos-12*lloguer-costVida
  nou_estalvi<-ingressos-12*quotaMensual-manteniment-costVida
  
  #sèries anuals
  estalvi_previ_anual<-roundup2(entrada+upfrontImprovements+estalvi_previ*(0:anys),2)
  nou_estalvi_anual<-roundup2(nou_estalvi * (0:anys), 2)
  habitatge_amortitzat_anual<-habitatge_amortitzat_inicial+hipotecaRestantAnual$amortitzacioAcumulada
  quota_anual_acumulada=roundup2(12*quotaMensual*(0:anys),2)
  preu_anual<-roundup2((preu+valueIncreaseImprovements)*(incrementValor+1)**(0:anys))
  despeses_venda_anual<-preu_anual*(honoraris)+(hipotecaRestantAnual$hipotecaRestantAnual*cancelacioHipoteca)
  valor_habitatge_post_venda<-roundup2(preu_anual-hipotecaRestantAnual$hipotecaRestantAnual-despeses_venda_anual,2)

  dadesEstalviHipoteca<-data.frame(`Estalvi Previ`= estalvi_previ_anual,
                    `Nou Estalvi` = nou_estalvi_anual,
                    `Hipoteca Restant` = hipotecaRestantAnual$hipotecaRestantAnual,
                    `Valor Amortitzat Hipoteca` = hipotecaRestantAnual$amortitzacioAcumulada,
                    `Quota Pagada` = quota_anual_acumulada,
                    `Valor Liquidat Habitatge (menys despeses de venda)`=valor_habitatge_post_venda,
                    `Any` = (0:anys), check.names = F) %>%
    mutate(`Nou Patrimoni Total` = `Valor Liquidat Habitatge (menys despeses de venda)` + `Nou Estalvi`, .before=Any)
  
  return(dadesEstalviHipoteca)
}










