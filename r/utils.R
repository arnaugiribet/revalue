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

f_estalviPrevi <- function(ingressos,lloguer,costVida){
  estalviPrevi<-ingressos-12*lloguer-costVida
  return(estalviPrevi)
}

f_estalviNou <- function(ingressos,quotaMensual,costVida,manteniment){
  estalviNou<-ingressos-12*quotaMensual-manteniment-costVida
  return(estalviNou)
}

f_anysGuanyEconomic <- function(estalviPreviAnual,nouPatrimoniAnual){
  
  dif<-estalviPreviAnual-nouPatrimoniAnual
  negs<-dif<0
  if(sum(negs)>0){
    anysGuanyEconomic<-which(dif<0)[1]-1 #recordar que les series comencen a l'any 0
    anysGuanyEconomic<-f(anysGuanyEconomic,dec=0)
  } else {
    anysGuanyEconomic<- "No hi haurà benefici econòmic"
  }

  return(anysGuanyEconomic)
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
  
  
f <- function(x,dec=2) {
  format(roundup2(x,dec),big.mark = '.',decimal.mark = ',', nsmall=dec)
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


f_dadesEstalviHipoteca <- function(estalviPrevi,estalviNou,
                      anys,entrada,quotaMensual,hipotecaRestantAnual,
                      upfrontImprovements,valueIncreaseImprovements,
                      cancelacioHipoteca,honoraris,incrementValor,
                      preu,cost,impostosVenda){
  
  #valors inicial
  habitatge_amortitzat_inicial<-entrada-(cost-preu)

  #booleans d'impostos
  irpfBoolean<-as.numeric('irpf' %in% impostosVenda)
  plusvaluaBoolean<-as.numeric('plusvaluaMunicipal' %in% impostosVenda)
  ibiBoolean<-as.numeric('ibi' %in% impostosVenda)
  
  #sèries anuals
  estalvi_previ_anual<-roundup2(entrada+upfrontImprovements+estalviPrevi*(0:anys),2)
  nou_estalvi_anual<-roundup2(estalviNou * (0:anys), 2)
  habitatge_amortitzat_anual<-habitatge_amortitzat_inicial+hipotecaRestantAnual$amortitzacioAcumulada
  quota_anual_acumulada=roundup2(12*quotaMensual*(0:anys),2)
  preu_anual<-roundup2((preu+valueIncreaseImprovements)*(incrementValor+1)**(0:anys))
  impostIrpf<-irpfBoolean*f_calculaQuota(x=(preu_anual-preu),limSup=c(6,50,200,300,Inf)*1000,valor=c(0.19,0.21,0.23,0.27,0.28))
  despeses_venda_anual<-preu_anual*(honoraris)+(hipotecaRestantAnual$hipotecaRestantAnual*cancelacioHipoteca)+impostIrpf
  valor_habitatge_post_venda<-roundup2(preu_anual-hipotecaRestantAnual$hipotecaRestantAnual-despeses_venda_anual,2)

  dadesEstalviHipoteca<-data.frame(
    `Estalvi Previ`= estalvi_previ_anual,
    `Nou Estalvi` = nou_estalvi_anual,
    `Hipoteca Restant` = hipotecaRestantAnual$hipotecaRestantAnual,
    `Valor Amortitzat Hipoteca` = hipotecaRestantAnual$amortitzacioAcumulada,
    `Quota Pagada` = quota_anual_acumulada,
    `Valor Liquidat Habitatge (menys despeses de venda)` = valor_habitatge_post_venda,
    `Quantitat a pagar d'IRPF` = impostIrpf,
    `Any` =  (0:anys), check.names = F) %>%
    mutate(
      `Nou Patrimoni Total` = roundup2(`Valor Liquidat Habitatge (menys despeses de venda)` + `Nou Estalvi`,2), .before=Any
      )
  
  return(dadesEstalviHipoteca)
}

f_calculaQuota<-function(x,limSup,valor){
  #x: valor sobre el qual calcular la quota
  #limSup: límits superior dels trams - normalment acaba amb Infinit
  #valor: percentatge que s'aplica a cada tram
  
  qi<-rep(0,length(x))
  for(i in 1:length(valor)){
    
    limSup_ant<-0
    if(i>1){
      limSup_ant<-limSup[i-1]
    }
    
    qi_i<-fifelse(x>=limSup[i], 
                  (limSup[i]-limSup_ant)*valor[i], #si queda per sobre del tram actual
                  fifelse(x>limSup_ant, 
                          (x-limSup_ant)*valor[i], #si està al tram actual
                          0)) #trams inferiors als que evaluem
    
    qi<-qi+qi_i
  }
  
  return(qi)
}







