#Comentario Teste
#Programa utilizado para baixar os dados do site da ANA e pre-processa-los para posterior utilizacao. 
#O programa retorna uma lista em que cada elemento da lista eh um dataframe cujas colunas de 1 a 11 
#correspondem, respectivamente, as variaveis codigo do reservatorio (codigo ANA), nome do reservatorio, cota (em m),
#afluencia (em m3/s), defluencia (em m3/s), vazao vertida (em m3/s), vazao turbinada (em m3/s), vazao natural (em m3/s),
#volume util (%), vazao incremental (em m3/s), data da medicao. A titulo de exemplo, a cota corresponde a coluna 3;a
#a cota do reservatório i pode ser acessada usando a seguinte sintaxe: reservatorio_i <- RESERVATORIOS[[i]][[1]][3]

#Os dados poderiam ter sido baixados tanto do site da ANA quanto do site do ONS. Optou-se pelo site da ANA 
#por mera conveniencia: no site ANA os dados são acessados via protocolo GET, estando explicito os pares
#variavel-nome a serem utilizados.

#Como entrada para esta funcao usam-se as seguintes informacoes: (i) dataframe identificando o codigo ANA 
# do reservatório e seu respectivo nome ONS; (ii) data inicial dos dados; (iii) data final dos dados;  

#OBSERVACAO: Quando se iniciou o processo de baixar dados do site da ANA, a quantidade de dias disponibilizada 
#correspondia exatamente ao especificado pelas variáveis dataInicial e dataFinal. Entretanto, por razão desconhecida, 
#em determinado momento (26/03/2019) a data final dos dias disponibilizados passou a corresponder sempre a 1 dia a mais 
#em relação à dataFinal. Por exemplo, caso especifique-se dataFinal= "31/12/2018", os dados baixados vão até "01/01/2019".
#Simplesmente não sei a razão desta mudança.


BAIXAR <- function(nomes_reservatorios_ANA, dataInicial, dataFinal){
  
#pacote utilizado para acessar o banco de dados on line da ANA
  require(httr)
  require(RCurl)
  require(XML)
  
#pacote utilizado para usar comandos em estilo 'pipeline'
  require(magrittr) 
  
#pacote utilizado para inserir linha, por meio da função 'InsertRow'
  require(DataCombine) 
  
#pacote utilizado para trabalhar com datas, por meio da função 'dmy'
  require(lubridate) 
  
#pacote para trabalhar com listas, do qual sera utilizada a função list.append
  require(rlist)
  
#pacote para selecionar valores usando a função filter_if
  require(dplyr)
  
#calculo do numero de posicoes a serem guardadas a partir das datas inicial e final
#usa a função 'dmy' do pacote lubridate ajustar o formato do dado de entrada
#intervalo <- as.integer(as.Date(dmy(pedido$dataFinal))) - as.integer(as.Date(dmy(pedido$dataInicial))) + 1 
#monta a lista correspondente a requisicao ao site da ANA. Esta lista sera utilizada na funcao GET do pacote httr
#para requerer os dados usando a opção query dentro da função GET (query= pedido). A lista pedido contem os seguintes 
#elementos: (i) pedido$`dropDownListReservatorios= nomes_reservatorios_ANA$Codigo_ANA[i]; (ii) pedido$dataInicial= dataInicial; (iii) pedido$dataFinal= dataFinal; (iv)
#pedido$button= "Buscar"
    
  pedido <- list(dropDownListReservatorios= nomes_reservatorios_ANA$Codigo_ANA[1], dataInicial= dataInicial, dataFinal= dataFinal, button= "Buscar")
#utiliza os dados de um dos reservatorios como referencia de forma: as demais formas
#são criadas como variacoes desta referencia (reservatorio com dados de volume util para todos os dias)

  REFERENCIA <- data.frame()

#Para cada um dos reservatorios ANA, serão baixados os dados. Serao utilizadas uma lista provisoria (PROVISORIO), que contera
#os dados de um reservatorio especifico a cada nova aquisicao de dados e uma lista permanente (RESERVATORIOS), que
#contera os dados de todos os reservatorios
  RESERVATORIOS <- list()
  PROVISORIO <- list()
  Sem_Dados_Nulo <- data.frame()
  nulo = 0
  for (i in 1:nrow(nomes_reservatorios_ANA)){
    pedido$dropDownListReservatorios <- nomes_reservatorios_ANA$Codigo_ANA[i]
    PROVISORIO <- GET("https://www.ana.gov.br/sar0/MedicaoSin?", query= pedido) %>% htmlParse %>% readHTMLTable

#Para lidar com a eventual existencia de dados nulos, a rotina abaixo identifica tais casos e ignora o correspondente reservatorio,
#registando a ocorrencia no dataframe Sem_Dados_Nulo. A eventual existencia de dias faltantes eh simplesmente ignorada,
#assumindo-se assim a hipotese de que se faltarem dias isso ocorrerá para alguns poucos dias, sem impactos significativos
#para o calculo das figuras de merito ELCC (Effective Load Carryng Capability) e LOLE(Loss of LoadExpectation)
    
      if(is.null(PROVISORIO[[1]][[1]])){
        Sem_Dados_Nulo <- rbind(Sem_Dados_Nulo, nomes_reservatorios_ANA[i,])
        nulo = nulo+1
      } else {
      RESERVATORIOS[i-nulo]= PROVISORIO
      }
  }
   
  RESERVATORIOS = list("RESERVATORIOS" = RESERVATORIOS, "Dados Ausentes"= Sem_Dados_Nulo)   
  return(RESERVATORIOS)
}
