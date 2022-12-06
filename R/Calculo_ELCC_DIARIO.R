#Programa para calcular ELCC (Effective Load carrying Capability) e LOLE (Loss of Load Expectation)
#nas condicoes do Sistema Eletrico Brasileiro (SEB), isto e, levando-se em conta a capacidade variavel das
#usinas despachaveis hidreletricas no periodo estudado.

calculo_ELCC_LOLE_dia <- function(dados_no_tempo, Usinas_ONS, Usinas_Termicas, capacidade_hidreletrica, AREA, COPT){

#repra eh o pacote pacote desenvolvido pelo NREL que calcula o credito de capacidade (ELCC) e LOLE de fontes
#intermitentes. Sua operacao requer a utilizacao dos seguintes pacotes: data.table, dplyr, Rcpp, reshape2,
#ggplot2, assertthat, testthat, knitr, markdown, devtools, lubridate, httr, stringr, XML, rJava, XLConnect. A
#chamada de todos os pacotes necessarios é feita utilizando-se as linhas de comando abaixo:

  require(dplyr)
  require(reshape2)
  require(ggplot2)
  require(assertthat)
  require(testthat)
  require(knitr)
  require(magrittr)
  require(markdown)
  require(devtools)
  require(lubridate)
  require(httr)
  require(stringr)
  require(XML)
  require(data.table)
  require(repra)

  #source("~/Nilton/Projetos_Computacao/R/Capacity_Credit_Brazil/R/funcao_elcc_DIARIO.R", encoding = 'UTF-8')

  tempo_inicial = Sys.time()

  tamanho <- length(dados_no_tempo$Time)/24
  elcc_lole <- data.frame("ELCC" = c(1:tamanho), "LOLE" = c(1:tamanho))
  geradores_despachaveis_termicos <- data.frame("Area" = c(AREA), "Capacity" = c(Usinas_Termicas$Capacidade),
                                       "EFOR" = c((1-(1-Usinas_Termicas$TEIF)*(1-Usinas_Termicas$IP))))

  if (missing(COPT)){
   COPTx <- vector(mode = "list", length = tamanho)
  }

  #enlace para calculo das figuras de mérito ELCC (Effcetive Load Carrying Capability) e LOLE
  #(Loss of Load Expectation) levando em conta a capacidade despachavel variavel das hidreletricas

  for (i in 1:tamanho){
    if (missing(COPT)){
      for (j in 1:length(Usinas_ONS$Usina)){
          Usinas_ONS$`Capacidade.(MW)`[j] <- capacidade_hidreletrica[j,i]
      }

      geradores_hidreletricos = as.data.frame(matrix(1:(3*nrow(Usinas_ONS)), nrow(Usinas_ONS), 3))
      colnames(geradores_hidreletricos) = c("Area", "Capacity", "EFOR")
      geradores_hidreletricos$Area = AREA
      geradores_hidreletricos$Capacity = Usinas_ONS$`Capacidade.(MW)`
      geradores_hidreletricos$EFOR = 1-(1-Usinas_ONS$`TEIF`)*(1-Usinas_ONS$`TEIP`)

      geradores_despachaveis <- rbind( geradores_hidreletricos,geradores_despachaveis_termicos)
      geradores_despachaveis <- na.omit(geradores_despachaveis)
      geradores_despachaveis <- filter(geradores_despachaveis, geradores_despachaveis$Capacity > 0)

      COPTx[[i]] <- outage_table(geradores_despachaveis)

    } else {
      COPTx = COPT
    }

    k <- i-1
    cat("   processei a copt  k=  ", k, " ")

    #formatacao dos dados temporais
    time_data <- format_timedata(dados_no_tempo[(24*k+1):(24*(k+1)),])

    #determinacao lo LOLH existente durante a hora i antes da insercao da fonte intermitente
    time_data2 <-  time_data %>% mutate(NetLoad = Load)

    #formatacao dos dados temporais para levar em conta a insercao da fonte intermitente, isto e,
    #ajuste do NetLoad
    time_data2 <- time_data2 %>% mutate(NetLoad = Load - Wind - PV)

    #calculo de ELCC e LOLH. Função necessaria porque no REPRA o calculo do ELCC esta
    #incorreto: para o calculo do ELCC deve ser tomado como LOLH-objetivo o LOLH
    #antes da insercao da fonte, ao inves de travar num valor pre-especificado (0.1 hora por ano),
    #como e feito no REPRA. Ao tentar parametrizar este valor nos argumentos da funcao do REPRA diretamente
    #(calculate_elcc), a função exibe erro ou retorna resultado inconsistente

    elcc_lole[i,1:2] <- funcao_elcc(time_data2, COPTx[[i]])
    rm(geradores_despachaveis)
  }

  tempo_final = Sys.time()
  #RESULTADO <- data.frame("ELCC" = min(elcc_lole[,1]), "LOLH" = sum(elcc_lole[,2]))

  DeltaT = tempo_final - tempo_inicial
  print (DeltaT)

  if(missing(COPT)){
    return(list("ELCC" = elcc_lole,"COPT" = COPTx))
  } else {
    return(list("ELCC" = elcc_lole,"COPT" = COPT))
    }
}

