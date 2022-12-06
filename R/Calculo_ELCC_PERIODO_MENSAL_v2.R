#require(gtools)

calculo_ELCC_LOLE_mes <- function(dados_no_tempo, Usinas_ONS, Usinas_Termicas, capacidade_hidreletrica, AREA, BISSEXTO, COPT){

  cat("inicio ")
  ELCC_mes = list()

  if(BISSEXTO == "NOT"){
    meses = c(31,28,31,30,31,30,31,31,30,31,30,31)
  }

  if(BISSEXTO == "YES"){
    meses = c(31,29,31,30,31,30,31,31,30,31,30,31)
  }

  cat(meses, "\n")

  for(i in 1:12){

    if(i==1){
      a=1
      b=1
      c=0
    }else{
      c=1
      a = 24
      b = sum(meses[1:i-1])
    }

    dados_no_tempo_mod = dados_no_tempo[(b*a+c):(sum(meses[1:i])*24), ]
    capacidade_hidreletrica_mod = capacidade_hidreletrica[ ,(b+c):(sum(meses[1:i]))]

    #dados_no_tempo_mod$Data =  format(as.Date(dados_no_tempo_mod$Data-2, origin = "1900-01-01"), "%d/%m/%Y %H:%M:%S")

    if(missing(COPT)){
      ELCC_mes[[i]] <- calculo_ELCC_LOLE(dados_no_tempo_mod, Usinas_ONS, Usinas_Termicas, capacidade_hidreletrica_mod, AREA)
    }else{

      COPTx = list()
      COPTx[[1]] = COPT[[b+c]]
      for(j in (b+c):(sum(meses[1:i])-1)){
        COPTx = append(COPTx,COPT[j+1])
      }

      ELCC_mes[[i]] <- calculo_ELCC_LOLE(dados_no_tempo_mod, Usinas_ONS, Usinas_Termicas, capacidade_hidreletrica_mod, AREA, COPTx)
    }
  }

  return(ELCC_mes)

}
