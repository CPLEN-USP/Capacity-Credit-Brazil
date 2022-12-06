#A funcao que calcula ELCC (Effective Load Carrying Capability) no REPRA
#apresenta problemas. Esta funcao esta sendo implementada como uma alternativa 
#a função calculate_elcc do REPRA. Ha 3 formatos temporais possiveis ao trabalhar com dados
#temporais no REPRA. No formato mais basico (dados brutos) (Formato 1) os dados precisam ser processados 
#usando a funcao format_timedata. Feita esta transformacao (Formato 2), para algumas funcoes 
#(calculate_metrics) eh necessario criar uma coluna adicional (NetLoad) (Formato 3). 
#Para entrada neste programa, devemos utlizar o formato 2

#commit

funcao_elcc <- function(dados_temporais, tabela_COPT){
  require(repra)
  require(dplyr)
  require(reshape2)
  require(ggplot2)
  require(magrittr)
  require (data.table)
  
  LOLH <- 0
  ELCC <- 0
  #estipula passo dos incrementos a partir da fonte inserida
  P2 <- max(dados_temporais$PV + dados_temporais$Wind)
  if(P2<0.1){
    P2 = 1
  }
  #preparação do dataframe para calcular LOLE antes de adicionar a fonte
  LOLH_final <- mutate(dados_temporais, NetLoad = dados_temporais$Load - dados_temporais$PV - dados_temporais$Wind) 
  LOLH_final <- calculate_metrics(LOLH_final, tabela_COPT)
  
  dados_temporais <- mutate(dados_temporais, NetLoad = dados_temporais$Load)
  #cálculo do LOLE antes de adicionar dataframe
  metrics_antes <- calculate_metrics(dados_temporais, tabela_COPT) 
  #preparação do dataframe para calculos após adicionar fonte
  dados_temporais <- mutate(dados_temporais, NetLoad = dados_temporais$Load - dados_temporais$PV - dados_temporais$Wind)
  #incrementa a carga enquanto LOLE anterior à adição da fonte não for atingido
  
  P1 = 0
  #P2 = 10000
  
  Original <- dados_temporais$NetLoad
  l = 0
  
  while(abs(P1-P2) >0.1){
    P = (P1 + P2)/2
    dados_temporais$NetLoad <- Original + P
    metrics <- calculate_metrics(dados_temporais, tabela_COPT)
    LOLH <- metrics$LOLH
    
    if((LOLH - metrics_antes$LOLH) < 0){
      P1 = P 
    } else {
      P2 = P
    }
    
    DELTALOLH = LOLH - metrics_antes$LOLH
    #cat("\n\n","DELTALOLH", DELTALOLH)
   l = l+1
  }
  
  cat(l )
  ELCC = P
  ELCC_LOLE <- data.frame("ELCC" = round(ELCC, digits = 3), "LOLH" = LOLH)
  
  return(ELCC_LOLE)
  
}
