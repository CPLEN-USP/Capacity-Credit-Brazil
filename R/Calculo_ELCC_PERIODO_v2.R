#Programa para calcular ELCC (Effective Load carrying Capability) e LOLE (Loss of Load Expectation)
#nas condições do Sistema Eletrico Brasileiro (SEB), isto e, levando-se em conta a capacidade variavel das
#usinas despachaveis hidreletricas no periodo estudado.

calculo_ELCC_LOLE <- function(dados_no_tempo, Usinas_ONS, Usinas_Termicas, capacidade_hidreletrica, AREA, COPT){

#repra eh o pacote pacote desenvolvido pelo NREL que calcula o credito de capacidade (ELCC) e LOLE de fontes
#intermitentes. Sua operação requer a utilizacao dos seguintes pacotes: data.table, dplyr, Rcpp, reshape2,
#ggplot2, assertthat, testthat, knitr, markdown, devtools, lubridate, httr, stringr, XML, rJava, XLConnect. A
#chamada de todos os pacotes necessarios eh feita utilizando-se as linhas de comando abaixo:

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
  #require(rJava)
  require(repra)
  require(data.table)

  #source("~/Nilton/Projetos_Computacao/R/Capacity_Credit_Brazil/R/funcao_elcc_DIARIO.R", encoding = 'UTF-8')


  tempo_inicial = Sys.time()

  tamanho <- ceiling(length(dados_no_tempo$Time)/24)

  LOLH <- data.frame("LOLH" = c(1:tamanho))
  LOLH_sem_Intermitente <- vector(mode = "numeric", length = length(dados_no_tempo$Time))
  LOLH_com_Intermitente <- vector(mode = "numeric", length = length(dados_no_tempo$Time))

  if(missing(COPT)){
    COPTx <- vector(mode = "list", length = tamanho)
  }

  #enlace para cálculo do COPT diário, levando-se em conta a capacidade disponível variável
  #nas hidrelétricas e a capacidade das térmicas

  if(missing(COPT)){
    for (i in 1:tamanho){
      for (j in 1:length(Usinas_ONS$Usina)){
        #Usinas_ONS$`Capacidade.(MW)`[j] <- capacidade_hidreletrica[[j]][[1]][i,1]
        Usinas_ONS$`Capacidade.(MW)`[j] <- capacidade_hidreletrica[j,i]
      }

      geradores_despachaveis <- data.frame("Area" = c(AREA),
                                           "Capacity" = c(Usinas_ONS$`Capacidade.(MW)`,
                                                          Usinas_Termicas$Capacidade),
                                           "EFOR" = c((1-(1-Usinas_ONS$TEIF)*(1-Usinas_ONS$TEIP)),
                                                      (1-(1-Usinas_Termicas$TEIF)*(1-Usinas_Termicas$IP))))
      geradores_despachaveis <- na.omit(geradores_despachaveis)
      geradores_despachaveis <- filter(geradores_despachaveis, geradores_despachaveis$Capacity > 0)
      COPTx[[i]] <- outage_table(geradores_despachaveis)

      cat(i, "    ")
    }
  }else{
    COPTx = COPT
  }

  #if(missing(COPT)){
  #saveRDS(COPTx, "D:\\Erick\\Credito Capacidade Solar Brasil\\Credito Capacidade Solar Brasil\\Erick\\COPT2018")
  #}

  #enlace para cálculo do LOLH horário sem e com a fonte intermitente adicionada, levando-se em conta a
  #capacidade disponível variável nas hidrelétricas, a capacidade das térmicas e a curva de carga
  #horária
  provisorio <- dados_no_tempo[1,]

  for (i in 1:(length(dados_no_tempo$Time))){
    provisorio$Load[1] <- dados_no_tempo$Load[i]
    provisorio$Intermitente[1] <- dados_no_tempo$PV[i]+dados_no_tempo$Wind[i]
    provisorio2 <- provisorio
    provisorio3 <- provisorio

    provisorio2 <- format_timedata(provisorio2, day.steps = 1)
    provisorio3 <- format_timedata(provisorio3, day.steps = 1)

    provisorio2 <- provisorio2 %>% mutate(NetLoad = Load)
    provisorio3 <- provisorio2 %>% mutate(NetLoad = Load - Intermitente)

    metrics1 <- calculate_metrics(provisorio2, COPTx[[ceiling(i/24)]])
    metrics2 <- calculate_metrics(provisorio3, COPTx[[ceiling(i/24)]])

    LOLH_sem_Intermitente[i] <- metrics1$LOLH
    LOLH_com_Intermitente[i] <- metrics2$LOLH
    print(i)
    cat (" ")
  }

  #LOLH_sem_Intermitente[i] <- metrics1$LOLH

  LOLH_antes <- sum(LOLH_sem_Intermitente)
  LOLH_depois <- sum(LOLH_com_Intermitente)

  #Inicialização de variaveis a serem utilizadas no enlace do calculo do
  #Effective Load Carrying Capability (ELCC)
  ELCC <- 0
  Pmenor <- 0
  Pmaior <- max(dados_no_tempo$PV+dados_no_tempo$Wind)
  #P <- max(dados_no_tempo$Intermitente)/1000
  #P <- max(dados_no_tempo$Intermitente)
  LOLH_periodo_analise <- 0
  LOLH_final <- 0
  TESTE <- Pmaior - Pmenor

  #enlace para calculo do ELCC, levando-se em conta a capacidade disponivel (diaria)
  #variavel nas hidreletricas, a capacidade das termicas, a curva de carga e a producao
  #horaria da fonte intermitente adicionada
  #ELCC <- (Pmenor + Pmaior)/2

  h=1
  while(TESTE > 1){
    #while(LOLH_periodo_analise < LOLH_antes){
    #print(ELCC)
    ELCC_antes <- ELCC
    ELCC <- (Pmenor + Pmaior)/2

    provisorio <- dados_no_tempo[1,]
    for (i in 1:(length(dados_no_tempo$Time))){
      provisorio$Load[1] <- dados_no_tempo$Load[i]+ ELCC
      provisorio$PV[1] <- dados_no_tempo$PV[i]
      provisorio$Wind[1] <- dados_no_tempo$Wind[i]
      provisorio3 <- provisorio
      #formata o dia j
      provisorio3 <- format_timedata(provisorio3, day.steps = 1)
      #prepara os dados temporais para usar a função calculate_metrics do REPRA
      #(a ser utilizada no cálculo do LOLH). A preparação consiste na criação da coluna
      #NetLoad. Para o cálculo do LOLH antes da inseção da fonte intermitente NetLoad
      #recebe Load (NetLoad= Load)
      provisorio3 <- provisorio3 %>% mutate(NetLoad = Load - PV - Wind)
      metrics2 <- calculate_metrics(provisorio3, COPTx[[ceiling(i/24)]])
      LOLH_com_Intermitente[i] <- metrics2$LOLH
    }

    LOLH_periodo_analise <- sum(LOLH_com_Intermitente)
    #print(LOLH_periodo_analise)
    if((LOLH_periodo_analise - LOLH_antes) < 0){
      Pmenor <- ELCC
    }else{
      Pmaior <- ELCC
    }

    #TESTE <- abs(Pmenor - Pmaior)
    #TESTE <- abs(LOLH_periodo_analise - LOLH_antes)
    TESTE <- abs(ELCC - ELCC_antes)
    #print(TESTE)
    #print(ELCC)
    #print(LOLH_periodo_analise)
    h=h+1
    cat(h, " ")
    cat("teste = ", TESTE, "  " )
  }

  cat("sai do while")
  tempo_final = Sys.time()
  tempo_de_execucao = tempo_inicial - tempo_final

  if(!missing(COPT)){
   ELCC_LOLE <- list("ELCC" = round(ELCC, digits = 0), "LOLH_antes" = LOLH_antes, "LOLH_depois" = LOLH_depois, "COPT" = COPT, "Tempo de execucao" = tempo_de_execucao)
    #ELCC_LOLE <- list("LOLH_antes" = LOLH_antes, "LOLH_depois" = LOLH_depois)
  }else{
    ELCC_LOLE <- list("ELCC" = round(ELCC, digits = 0), "LOLH_antes" = LOLH_antes, "LOLH_depois" = LOLH_depois, "COPT" = COPTx, "Tempo de execucao" = tempo_de_execucao)
  }

  return(ELCC_LOLE)

}


