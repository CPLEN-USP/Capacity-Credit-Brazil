##Programa Auxiliar
# O objetivo deste programa e facilitar a leitura e estabelecimento das condicoes de analise, como periodo, grupo de usinas avaliados,
# escopo locacional da analise (Brasil ou NE). Alem disso, o programa faz o ajuste e complementacoes de dados quando necessario.


############################################################## NOTAS ##############################################################

#1

# A Usina Cachoeira Caldeirao tem dados apenas a partir de 01/04/2018 na ANA, apesar que sua operacao se inicia em
#07/05/2016. Sendo assim. Caso simule-se anos anteriores a 2019, há que se complementar os dados desta usina.

#2

# A Usina COLIDER tem dados na ANA disponiveis desde 01/04/2018, porem sua operacao comercial inicia-se em 11/03/2019.
# Deve-se levar em conta a entrada em operacao, pois toda a potencia disponivel entre 01/04/2018 e 11/03/2019 calculada pelo
# programa nao esta efetivamente disponivel. O SubPrograma POTENCIAS ja leva isto em conta. No ambito deste codigo não e necessario
# que, para o ano do 2019, se complete os dados da variavel RESERVATORIOS pois estes estao disponiveis nos dados da ANA e o que
# ocorre eh que o programa de potencia vai considerar apenas a potencia a partir da operacao da usinas. Sendo assim nao e preciso
# fazer nada para esta Usina.

#3

#Teles Pires (07/11/2015), Sao Manoel (28/12/2017),  Itiquira II (06/11/2002) tem dados apenas a partir de 01/04/2018.

#EM SUMA:
#CACHOEIRA CALDEIRAO
#A usina de Cachoeira Caldeirao deve ser incluida para simulações dos anos 2017 e 2018

#Itiquira II
#A usina de Itiquira II deve ser incluida para simulaçoes dos anos  2017 e 2018

#Teles Pires
#A usina de Teles Pires deve ser incluida para simulações dos anos de 2017 e 2018

#São Manoel
#A usina de São Manoel deve ser incluida para o ano de 2018. A rigor o ano de 2017 tambem deveria sen incluido porem são apenas 3 dias de
#operação para este ano, de maneira que eles serão ignorados para simplificação

#Colider
#Nada precisa ser feito

#A Usina de Candonga nao esta operando por conta do desastre de Mariana e por isso deve ser retirada da analise

Programa_Aux = function(Regiao, dataInicial, dataFinal,i,j,h,k, retirar){

  require(stringr)
  require(openxlsx)
  require(dplyr)

#### Definindo os parametros da simulacao a ser feita

# i, j, h e k - servem para definir um intervalo das usinas a serem consideradas. Facilita a criação de subgrupos contínuos de análise.
# Ex: Caso deseje-se simular dois subgrupos no subgrupo das 144 UHEs, de 1 a 10 e de 70 a 144, faz-se i=1, j=10, h=70 e k=144. Para simular
# todas as Usinas faz-se i=1,j=2,h=3,k=144 ou outras combinações com i=1 e k=144, e com j e h sendo quaiquer numeros inteiros imediadamente
# sucessivos com j<h e 1<j,h<144.

# A variavel retirar retira as usinas especificadas dentro delas da lista de usinas que serao simuladas e terao seus dados baixados.
# O nome das usinas deve estar exatamente igual ao em nomes_reservatorios_ANA. Caso nao se deseje retirar nenhuma Usinas colocar: retirar = c()
# Este artificio eh relevante para se excluir usinas da analise, sem a necessidade de alterar os dados de entrada em nomes_reservatorios_ANA

# Ex:
#retirar = c("FONTES", "NILO PECANHA")



#Transformacao das datas iniciais e finais em numero para a correcao das Usinas de Tres Marias e Henry Borden referente a inconsistencia de dados e dias faltantes.
  DI = as.numeric(as.Date(dataInicial, "%d/%m/%Y"))
  DF = as.numeric(as.Date(dataFinal, "%d/%m/%Y"))

#DP se refere a Dia Problema (dias nos quais Tres Marias e Henry Borden apresentam inconsistencias e dias faltantes)
  DP1 = as.numeric(as.Date("24/11/2019", "%d/%m/%Y"))
  DP2 = as.numeric(as.Date("24/11/2017", "%d/%m/%Y"))


  if(DI<DF){

    if(43466 <= as.numeric(as.Date(dataInicial, "%d/%m/%Y"))+25569 & as.numeric(as.Date(dataFinal, "%d/%m/%Y"))+25569 <= 43830){
      Ano = "2019"
    }

    if(43101 <= as.numeric(as.Date(dataInicial, "%d/%m/%Y"))+25569 & as.numeric(as.Date(dataFinal, "%d/%m/%Y"))+25569 <= 43465){
      Ano = "2018"
    }

    if(42736 <= as.numeric(as.Date(dataInicial, "%d/%m/%Y"))+25569 & as.numeric(as.Date(dataFinal, "%d/%m/%Y"))+25569 <= 43100){
    Ano = "2017"
    }

#############BRASIL##################

    if(Regiao == "BRASIL"){

      #NR se refere a Named Region
      #str_sub eh funcao do pacote do pacote stringr
      #No caso, da string Ano preserva apenas os caracteres 3 e 4
      NR1 = str_sub(Ano,3,4)
      #cola a string "Dados" na string contida no vetor NR1,
      #incluindo a separacao sep="" entre ambas as strings.
      NR1 = paste("Dados_", sep = "", NR1) #equivale a NR1 = paste0("Dados_", NR1)

      #Usa a funcao file.path do pacote base para montar consistentemente o
      #caminho das planilhas de dados a serem utilziadas, independentemente
      #do sistema operacional e da estrutura de diretorios do usuario
      #O uso abaixo da funcao file.path assume que ha um diretorio no qual o
      #projeto Capacity_Credit_Brazil eh executado.
      local_Usinas_UHE <- file.path("Data", "Usinas_UHE.xlsx")
      local_Termicas <- file.path("Data", "Termicas.xlsx")
      local_Dados_Finais_BR_1999_2019 <- file.path("Data", "Dados_Finais_BR_1999_2019.xlsx")
      local_Dados_Finais_NE_2017_2020 <- file.path("Data", "Dados_Finais_NE_2017_2020.xlsx")
      local_Dados_faltantes <- file.path("Data", "Dados_faltantes_ANA.xlsx")

      #guarda dados das planilhas de dados utilizadas
      #em variaveis
      dados_maquinas = read.xlsx(local_Usinas_UHE, namedRegion= "Dados_Maquinas")
      tipo_turbina = read.xlsx(local_Usinas_UHE, namedRegion= "Tipo_Turbina")
      Usinas_ONS2 = read.xlsx(local_Usinas_UHE, namedRegion= "Us_ONS2_BR")
      nomes_reservatorios_ANA = read.xlsx(local_Usinas_UHE, namedRegion= "ANA_Data")
      dados_no_tempo = read.xlsx(local_Dados_Finais_BR_1999_2019, namedRegion= NR1)
      Usinas_ONS = read.xlsx(local_Usinas_UHE, namedRegion= "Us_ONS_BR")
      Usinas_Termicas = read.xlsx(local_Termicas, namedRegion= "Termtotal")
    }

#############NORDESTE##################

    if(Regiao == "NORDESTE"){

      NR2 = str_sub(Ano,3,4)
      NR2 = paste("NE_", sep = "", NR2)



      #guarda dados das planilhas de dados utilizadas
      #em variaveis
      dados_maquinas = read.xlsx( local_Usinas_UHE, namedRegion= "Potencias2_NE")
      tipo_turbina = read.xlsx( local_Usinas_UHE, namedRegion= "Tipo_Turbina_NE")
      Usinas_ONS2 = read.xlsx( local_Usinas_UHE, namedRegion = "Usinas_ONS2_NE")
      nomes_reservatorios_ANA = read.xlsx(local_Usinas_UHE, namedRegion= "ANA_Data_NE")
      Usinas_ONS = read.xlsx(local_Usinas_UHE, namedRegion = "Usinas_ONS_NE")
      Usinas_Termicas = read.xlsx(local_Termicas, namedRegion= "TERMO_NE")
      dados_no_tempo = read.xlsx(local_Dados_Finais_NE_2017_2020, namedRegion= NR2)
    }

# Originalmente os objetos dados_maquinas e tipo_turbina nao tem o nome das usinas. Este sera necessario para aplicacao da funcao filter neste programa
# e por isso os nomes estão sendo adicionados

    dados_maquinas = cbind(dados_maquinas, nomes_reservatorios_ANA$Nome)
    tipo_turbina = cbind(tipo_turbina, nomes_reservatorios_ANA$Nome)

#Selecao do intervalo [i,j] e [h,k] para o objeto nomes_reservatorios_ANA

    nomes_reservatorios_ANA1 = nomes_reservatorios_ANA[i:j, ]
    nomes_reservatorios_ANA2 = nomes_reservatorios_ANA[h:k, ]
    nomes_reservatorios_ANA = rbind(nomes_reservatorios_ANA1, nomes_reservatorios_ANA2)

#retirada das Usinas presentes em "retirar" caso este vetor nao tenha comprimento 0.
    if(length(retirar)!=0){
      for (u in 1:length(retirar)){
        nomes_reservatorios_ANA = filter(nomes_reservatorios_ANA, nomes_reservatorios_ANA$Nome != retirar[u])
      }
    }

#Secao para baixar o dados dos reservatorios

    RESERVATORIOS = BAIXAR(nomes_reservatorios_ANA,dataInicial, dataFinal)
    Sem_Dados_Nulo = RESERVATORIOS$`Dados Ausentes`
    RESERVATORIOS = RESERVATORIOS$RESERVATORIOS
    RESERVATORIOS = RESERVATORIOS_PROGRAMA(RESERVATORIOS, nomes_reservatorios_ANA)

#Secao para arrumar os titulos dos dados vindos da ANA. Como eles veem com acento a visualizacao fica desconfigurada aqui no RStudio
    Titulo_Col_RES = c("Codigo do Reservatorio", "Reservatorio", "Cota [m]", "Afluencia [m3/s]", "Defluencia [m3/s]", "Vazao Vertida [m3/s]", "Vazao Turbinada [m3/s]", "Vazao Natural [m3/s]", "Volume Util (%)", "Vazao Incremental [m3/s]", "Data da Medicao")

    for(cont in 1:length(RESERVATORIOS)){
      colnames(RESERVATORIOS[[cont]]) = Titulo_Col_RES
    }

    #if(nrow(Sem_Dados_Nulo)!=0){
      #for(u in 1:nrow(Sem_Dados_Nulo)){
        #nomes_reservatorios_ANA = filter(nomes_reservatorios_ANA, nomes_reservatorios_ANA$Nome != Sem_Dados_Nulo$Nome[u])
      #}
    #}


################################################################
# As Usinas "CACH.CALDEIR", "COLIDER" , "TELES PIRES", "SAO MANOEL", "ITIQUIRA II" precisam ser corrigidas
# Bloco de codigo para consertar isso
#Codigos_ANA_UsFaltantes = c(19153, 19155, 19160, 19158, 19163)

    NR3 = paste("CACH_CALD_", sep = "", Ano)
    NR4 = paste("ITIQUIRA_II_", sep = "", Ano)
    NR5 = paste("Teles_Pires_", sep = "", Ano)
    NR6 = paste("SAO_MANOEL_", sep = "", Ano)
    NR7 = paste("BAIXO_IGUACU_", sep = "", Ano)
    NR8 = paste("COLIDER_", sep = "", Ano)

    if(Ano == "2017"|Ano == "2018"){
      #guarda dados das planilhas de dados utilizadas
      #em variaveis
      Dados_faltantes_CACHOEIRA = read.xlsx(local_Dados_faltantes, namedRegion= NR3)
      Dados_faltantes_ITIQUIRA_II = read.xlsx(local_Dados_faltantes, namedRegion= NR4)
      Dados_faltantes_TELES_PIRES = read.xlsx(local_Dados_faltantes, namedRegion= NR5)
    }

    if(Ano == "2018"){
      #guarda dados das planilhas de dados utilizadas
      #em variaveis
      Dados_faltantes_SAO_MANOEL = read.xlsx(local_Dados_faltantes, namedRegion= NR6)
      Dados_faltantes_COLIDER = read.xlsx(local_Dados_faltantes, namedRegion= NR8)
    }

    if(Ano =="2019"){
      #guarda dados das planilhas de dados utilizadas
      #em variaveis
      Dados_faltantes_BAIXO_IGUACU = read.xlsx(local_Dados_faltantes, namedRegion= NR7)
    }

    Usinas_Incluidas = vector()
    if(Ano == "2017"){

      #CACHOEIRA CALDEIRAO
      #2017

      if(nrow(filter(nomes_reservatorios_ANA, nomes_reservatorios_ANA$Codigo_ANA == 19153))!=0){
        adendo = matrix(NA, nrow(RESERVATORIOS[[1]]), 11)
        colnames(adendo) = colnames(RESERVATORIOS[[1]])
        adendo = as.data.frame(adendo)

        adendo$`Codigo do Reservatorio` = "19153"
        adendo$Reservatorio = "CACHOEIRA CALDEIRAO"
        adendo$`Cota [m]` = Dados_faltantes_CACHOEIRA$Hmon
        adendo$`Defluencia [m3/s]` = Dados_faltantes_CACHOEIRA$Defluencia
        adendo$`Data da Medicao` = format(as.Date.numeric(Dados_faltantes_CACHOEIRA$Data-2, "1900-01-01"), "%d/%m/%Y")

        #Achar CACH CALD na lista

        for (v in 1:length(RESERVATORIOS)){
          if (RESERVATORIOS[[v]][1,2]== "CORUMBA-3"){
            posicao3 = v
          }
        }

        RESERVATORIOS = append(RESERVATORIOS, list(adendo),posicao3)
        Usinas_Incluidas = c(Usinas_Incluidas, "CACH.CALDEIR")

      }

      #ITIQUIRA II

      if(nrow(filter(nomes_reservatorios_ANA, nomes_reservatorios_ANA$Codigo_ANA == 19155))!=0){
        adendo = matrix(NA, nrow(RESERVATORIOS[[1]]), 11)
        colnames(adendo) = colnames(RESERVATORIOS[[1]])
        adendo = as.data.frame(adendo)

        adendo$`Codigo do Reservatorio` = "19155"
        adendo$Reservatorio = "ITIQUIRA II"
        adendo$`Cota [m]` = Dados_faltantes_ITIQUIRA_II$Hmon
        adendo$`Defluencia [m3/s]` = Dados_faltantes_ITIQUIRA_II$Defluencia
        adendo$`Data da Medicao` = format(as.Date.numeric(Dados_faltantes_ITIQUIRA_II$Data-2, "1900-01-01"), "%d/%m/%Y")

      #### Achar Itiquira na lista

        for (v in 1:length(RESERVATORIOS)){
          if (RESERVATORIOS[[v]][1,2]== "ITIQUIRA I"){
              posicao5 = v
          }
        }

          RESERVATORIOS = append(RESERVATORIOS, list(adendo),posicao5)
          Usinas_Incluidas = c(Usinas_Incluidas, "ITIQUIRA II")
      }

          #########2017#######################

      if(nrow(filter(nomes_reservatorios_ANA, nomes_reservatorios_ANA$Codigo_ANA == 19160))!=0){
        for (v in 1:length(RESERVATORIOS)){
          if (RESERVATORIOS[[v]][1,2]== "ROSAL"){
            posicao6 = v
          }
        }

        adendo = matrix(NA, nrow(RESERVATORIOS[[1]]), 11)
        colnames(adendo) = colnames(RESERVATORIOS[[1]])
        adendo = as.data.frame(adendo)

        adendo$`Codigo do Reservatorio` = "19160"
        adendo$Reservatorio = "TELES PIRES"
        adendo$`Cota [m]` = Dados_faltantes_TELES_PIRES$Hmon
        adendo$`Defluencia [m3/s]` = Dados_faltantes_TELES_PIRES$Defluencia
        adendo$`Data da Medicao` = format(as.Date.numeric(Dados_faltantes_TELES_PIRES$Data-2, "1900-01-01"), "%d/%m/%Y")

        RESERVATORIOS = append(RESERVATORIOS, list(adendo),posicao6)
        Usinas_Incluidas = c(Usinas_Incluidas, "TELES PIRES")

      }
    }

    if(Ano == "2018"){
    #COLIDER

      if(nrow(filter(nomes_reservatorios_ANA, nomes_reservatorios_ANA$Codigo_ANA == 19154))!=0){
        for (v in 1:length(RESERVATORIOS)){
          if (RESERVATORIOS[[v]][1,2]== "COLIDER"){
            posicao14 = v
          }
        }

        linhas = nrow(RESERVATORIOS[[posicao14]])

        adendo = matrix(NA, nrow(RESERVATORIOS[[1]]) - linhas, 11)
        colnames(adendo) = colnames(RESERVATORIOS[[posicao14]])
        adendo = as.data.frame(adendo)
        adendo$`Codigo do Reservatorio` = "19154"
        adendo$Reservatorio = "COLIDER"

#O dia 01/01/1900 é o dia 0 no R e não o dia 1 como no excel. Por isso tenho que tirar um dia. Alem disso temos que
# tirar outro dia pois o ano de 1900 é bissexto no R e não no excel.

        adendo$`Data da Medicao` = format(as.Date.numeric(Dados_faltantes_COLIDER$Data-2, "1900-01-01"), "%d/%m/%Y")
        adendo$`Cota [m]` = Dados_faltantes_COLIDER$Hmon
        adendo$`Defluencia [m3/s]` = Dados_faltantes_COLIDER$Defluencia

        RESERVATORIOS[[posicao14]] = rbind(adendo, RESERVATORIOS[[posicao14]])

      }

    #2018
    # LEMBRETE: SERA PRECISO ACHAR ELES NA LISTA ATUALIZADA

      if(nrow(filter(nomes_reservatorios_ANA, nomes_reservatorios_ANA$Codigo_ANA == 19153))!=0){
        for (v in 1:length(RESERVATORIOS)){
          if (RESERVATORIOS[[v]][1,2]== "CACHOEIRA CALDEIRAO"){
            posicao4 = v
          }
        }

        linhas = nrow(RESERVATORIOS[[posicao4]])

        adendo = matrix(NA, nrow(RESERVATORIOS[[1]]) - linhas, 11)
        colnames(adendo) = colnames(RESERVATORIOS[[posicao4]])
        adendo = as.data.frame(adendo)

        adendo$`Codigo do Reservatorio` = "19153"
        adendo$Reservatorio = "CACHOEIRA CALDEIRAO"

# O dia 01/01/1900 é o dia 0 no R e não o dia 1 como no excel. Por isso tenho que tirar um dia. Alem disso temos que
# tirar outro dia pois o ano de 1900 é bissexto no R e não no excel.

        adendo$`Data da Medicao` = format(as.Date.numeric(Dados_faltantes_CACHOEIRA$Data-2, "1900-01-01"), "%d/%m/%Y")
        adendo$`Cota [m]` = Dados_faltantes_CACHOEIRA$Hmon
        adendo$`Defluencia [m3/s]` = Dados_faltantes_CACHOEIRA$Defluencia

        RESERVATORIOS[[posicao4]] = rbind(adendo, RESERVATORIOS[[posicao4]])

      }

      #ITIQUIRA II
      # Achar Itiquira na lista
      if(nrow(filter(nomes_reservatorios_ANA, nomes_reservatorios_ANA$Codigo_ANA == 19155))!=0){
        for (v in 1:length(RESERVATORIOS)){
          if (RESERVATORIOS[[v]][1,2]== "ITIQUIRA II"){
            posicao5 = v
          }
        }

        linhas = nrow(RESERVATORIOS[[posicao5]])

        adendo = matrix(NA, nrow(RESERVATORIOS[[1]]) - linhas, 11)
        colnames(adendo) = colnames(RESERVATORIOS[[1]])
        adendo = as.data.frame(adendo)

        adendo$`Codigo do Reservatorio` = "19155"
        adendo$Reservatorio = "ITIQUIRA II"
        adendo$`Cota [m]` = Dados_faltantes_ITIQUIRA_II$Hmon
        adendo$`Defluencia [m3/s]` = Dados_faltantes_ITIQUIRA_II$Defluencia
        adendo$`Data da Medicao` = format(as.Date.numeric(Dados_faltantes_ITIQUIRA_II$Data-2, "1900-01-01"), "%d/%m/%Y")

        RESERVATORIOS[[posicao5]] = rbind(adendo, RESERVATORIOS[[posicao5]])
      }

      #TELES PIRES
      #########2018###########

      if(nrow(filter(nomes_reservatorios_ANA, nomes_reservatorios_ANA$Codigo_ANA == 19160))!=0){
        for (v in 1:length(RESERVATORIOS)){
          if (RESERVATORIOS[[v]][1,2]== "TELES PIRES"){
            posicao6 = v
          }
        }

        linhas = nrow(RESERVATORIOS[[posicao6]])

        adendo = matrix(NA, nrow(RESERVATORIOS[[1]])-linhas, 11)
        colnames(adendo) = colnames(RESERVATORIOS[[1]])
        adendo = as.data.frame(adendo)

        adendo$`Codigo do Reservatorio` = "19160"
        adendo$Reservatorio = "TELES PIRES"
        adendo$`Cota [m]` = Dados_faltantes_TELES_PIRES$Hmon
        adendo$`Defluencia [m3/s]` = Dados_faltantes_TELES_PIRES$Defluencia
        adendo$`Data da Medicao` = format(as.Date.numeric(Dados_faltantes_TELES_PIRES$Data-2, "1900-01-01"), "%d/%m/%Y")

        RESERVATORIOS[[posicao6]] = rbind(adendo, RESERVATORIOS[[posicao6]])

      }

      #SAO MANOEL
      #2018

      if(nrow(filter(nomes_reservatorios_ANA, nomes_reservatorios_ANA$Codigo_ANA == 19158))!=0){
        for (v in 1:length(RESERVATORIOS)){
          if (RESERVATORIOS[[v]][1,2]== "SAO MANOEL"){
            posicao7 = v
          }
        }

        linhas = nrow(RESERVATORIOS[[posicao7]])

        adendo = matrix(NA, nrow(RESERVATORIOS[[1]])-linhas, 11)
        colnames(adendo) = colnames(RESERVATORIOS[[1]])
        adendo = as.data.frame(adendo)

        adendo$`Codigo do Reservatorio` = "19158"
        adendo$Reservatorio = "SAO MANOEL"
        adendo$`Cota [m]` = Dados_faltantes_SAO_MANOEL$Hmon
        adendo$`Defluencia [m3/s]` = Dados_faltantes_SAO_MANOEL$Defluencia
        adendo$`Data da Medicao` = format(as.Date.numeric(Dados_faltantes_SAO_MANOEL$Data-2, "1900-01-01"), "%d/%m/%Y")
        RESERVATORIOS[[posicao7]] = rbind(adendo, RESERVATORIOS[[posicao7]])
      }
    }


    if(Ano == "2019"){
      # BAIXO IGUACU
      # 2019
      if(nrow(filter(nomes_reservatorios_ANA, nomes_reservatorios_ANA$Codigo_ANA == 19163))!=0){
        for (v in 1:length(RESERVATORIOS)){
          if (RESERVATORIOS[[v]][1,2]== "BAIXO IGUAÇU"){
            posicao10 = v
          }
        }

        linhas = nrow(RESERVATORIOS[[posicao10]])

        adendo = matrix(NA, nrow(RESERVATORIOS[[1]]) - linhas, 11)
        colnames(adendo) = colnames(RESERVATORIOS[[posicao10]])
        adendo = as.data.frame(adendo)

        adendo$`Codigo do Reservatorio` = "19163"
        adendo$Reservatorio = "BAIXO IGUAÇU"
        adendo$`Cota [m]` = Dados_faltantes_BAIXO_IGUACU$Hmon
        adendo$`Defluencia [m3/s]` = Dados_faltantes_BAIXO_IGUACU$Defluencia
        adendo$`Data da Medicao` = format(as.Date.numeric(Dados_faltantes_BAIXO_IGUACU$Data-2, "1900-01-01"), "%d/%m/%Y")

        RESERVATORIOS[[posicao10]] = rbind(adendo, RESERVATORIOS[[posicao10]])
      }
    }

###########################################################################################################################################
#Sem_Dados_Nulo2 contera as usinas que apos a adicao feita das usinas faltantes no bloco anterior de codigo, ainda assim ficaram de fora da
#simulacao

    Sem_Dados_Nulo2 = Sem_Dados_Nulo
    if(length(Usinas_Incluidas)!=0){
      for(l in 1:length(Usinas_Incluidas)){
        Sem_Dados_Nulo2 =filter(Sem_Dados_Nulo2, Sem_Dados_Nulo2$Nome != Usinas_Incluidas[l])
      }
    }

    if(nrow(filter(nomes_reservatorios_ANA, nomes_reservatorios_ANA$Codigo_ANA == 19035))!=0){
      for(p in 1:length(RESERVATORIOS)){
        if(RESERVATORIOS[[p]][[1]][1]=="19035"){
          posicao1 = p
        }
      }

      if(DI<=DP1&DP1<=DF&RESERVATORIOS[[posicao1]][[1]][1]=="19035"){
        RESERVATORIOS[[posicao1]][3][DP1-DI+1,1] = (RESERVATORIOS[[posicao1]][3][DP1-DI,1] + RESERVATORIOS[[posicao1]][3][DP1-DI+2,1])/2
      }
    }

    if(nrow(filter(nomes_reservatorios_ANA, nomes_reservatorios_ANA$Codigo_ANA == 19119))!=0){
      for(n in 1:length(RESERVATORIOS)){
        if(RESERVATORIOS[[n]][[1]][1]=="19119"){
          posicao2 = n
        }
      }

      if(DI<=DP2&DP2<=DF&RESERVATORIOS[[posicao2]][[1]][1]=="19119"){
        add = c(rep(0,11))

        RESERVATORIOS_parte1= RESERVATORIOS[[posicao2]][1:(DP2-DI), ]
        RESERVATORIOS_parte2= RESERVATORIOS[[posicao2]][(DP2-DI+1):(DF-DI+1), ]

        aux1 = rbind(RESERVATORIOS_parte1, add)
        RESERVATORIOS[[posicao2]] = rbind(aux1, RESERVATORIOS_parte2)

        for(t in 3:10){
          RESERVATORIOS[[posicao2]][t][(DP2-DI)+1,1] = (RESERVATORIOS[[posicao2]][t][(DP2-DI),1] + RESERVATORIOS[[posicao2]][t][(DP2-DI+2),1])/2
        }

        RESERVATORIOS[[posicao2]][1][(DP2 - DI+1), 1] = 19119
        RESERVATORIOS[[posicao2]][2][(DP2 - DI+1), 1] = "TRÊS MARIAS"
        RESERVATORIOS[[posicao2]][11][(DP2 - DI+1), 1] = "24/11/2017"
      }
    }

    if(nrow(Sem_Dados_Nulo2)!=0){
      for(u in 1:nrow(Sem_Dados_Nulo2)){
        nomes_reservatorios_ANA = filter(nomes_reservatorios_ANA, nomes_reservatorios_ANA$Nome != Sem_Dados_Nulo2$Nome[u])
      }
    }


#O nomes da Usinas de Barra Bonita precisar ser igual nas arquivos Usinas_ONS, Usinas_ONS2 e nomes_reservatorios_ANA

#Bloco de codigo para retirar os espacos dos nomes das usinas nos arquivos Usinas_ONS e Usinas_ONS2

    for (y in 1:nrow(Usinas_ONS2)){
      a = -1

      if(str_sub(Usinas_ONS2$Usina[y], a,a) == " "){
        while(str_sub(Usinas_ONS2$Usina[y], a,a) == " "){
        a = a - 1
        }
        Usinas_ONS2$Usina[y]=str_sub(Usinas_ONS2$Usina[y], end=a)
      }
    }

    for (y in 1:nrow(Usinas_ONS)){
      a = -1
      if(str_sub(Usinas_ONS$Usina[y], a,a) == " "){
        while(str_sub(Usinas_ONS$Usina[y], a,a) == " "){
          a = a - 1
        }
        Usinas_ONS$Usina[y]=str_sub(Usinas_ONS$Usina[y], end=a)
      }
    }

###### fim da retirada dos espacos

    Usinas_ONS2_1 = Usinas_ONS2[i:j,]
    Usinas_ONS2_2 = Usinas_ONS2[h:k,]
    Usinas_ONS2 = rbind(Usinas_ONS2_1, Usinas_ONS2_2)

#Atentar para a coluna PH4(5) na planilha Usinas_ONS2 esta errada e repetida como PH4(4)
    if(length(retirar)!=0){
      for (u in 1:length(retirar)){
        Usinas_ONS2 = filter(Usinas_ONS2, Usinas_ONS2$Usina != retirar[u])
      }
    }

    if(nrow(Sem_Dados_Nulo2)!=0){
      for(u in 1:nrow( Sem_Dados_Nulo2)){
        Usinas_ONS2 = filter(Usinas_ONS2, Usinas_ONS2$Usina != Sem_Dados_Nulo2$Nome[u])
      }
    }

    Usinas_ONS_1 = Usinas_ONS[i:j,]
    Usinas_ONS_2 = Usinas_ONS[h:k,]
    Usinas_ONS = rbind(Usinas_ONS_1, Usinas_ONS_2)

    if(length(retirar)!=0){
      for (u in 1:length(retirar)){
        Usinas_ONS = filter(Usinas_ONS, Usinas_ONS$Usina != retirar[u])
      }
    }

    if(nrow(Sem_Dados_Nulo2)!=0){
      for(u in 1:nrow( Sem_Dados_Nulo2)){
        Usinas_ONS = filter(Usinas_ONS, Usinas_ONS$Usina != Sem_Dados_Nulo2$Nome[u])
      }
    }

#dados_maquinas = cbind(dados_maquinas, nomes_reservatorios_ANA$Nome)
    dados_maquinas1 = dados_maquinas[i:j,]
    dados_maquinas2 = dados_maquinas[h:k,]
    dados_maquinas = rbind(dados_maquinas1, dados_maquinas2)

    if(length(retirar)!=0){
      for (u in 1:length(retirar)){
        dados_maquinas = filter(dados_maquinas, dados_maquinas$`nomes_reservatorios_ANA$Nome` !=retirar[u])
      }
    }

    if(nrow(Sem_Dados_Nulo2)!=0){
      for(u in 1:nrow( Sem_Dados_Nulo2)){
        dados_maquinas= filter(dados_maquinas, dados_maquinas$`nomes_reservatorios_ANA$Nome` != Sem_Dados_Nulo2$Nome[u])
      }
    }

    dados_maquinas = dados_maquinas[,1:(length(dados_maquinas)-1) ]

    tipo_turbina_1 = tipo_turbina[i:j, ]
    tipo_turbina_2 = tipo_turbina[h:k, ]
    tipo_turbina = rbind(tipo_turbina_1, tipo_turbina_2)

    if(length(retirar)!=0){
      for (u in 1:length(retirar)){
        tipo_turbina = filter(tipo_turbina, tipo_turbina$`nomes_reservatorios_ANA$Nome` !=retirar[u])
      }
    }


    if(nrow(Sem_Dados_Nulo2)!=0){
      for(u in 1:nrow( Sem_Dados_Nulo2)){
        tipo_turbina= filter(tipo_turbina, tipo_turbina$`nomes_reservatorios_ANA$Nome`!= Sem_Dados_Nulo2$Nome[u])
      }
    }

#para compatibilizar com o programa Potencias

    tipo_turbina = tipo_turbina$Tipo.Turbina
    num = c(rep(0,length(Usinas_ONS$Usina)))

    for (m in 1:length(Usinas_ONS$Usina)){
      num[m] = nrow(RESERVATORIOS[[m]][11])
    }

    return(list("nomes_reservatorios_ANA" = nomes_reservatorios_ANA,  "RESERVATORIOS" = RESERVATORIOS, "Dados_Ausentes" = Sem_Dados_Nulo, "Dados_Ausentes2" = Sem_Dados_Nulo2,"dados_maquinas" =  dados_maquinas,"Usinas_ONS" = Usinas_ONS,"Usinas_ONS2" =  Usinas_ONS2,"Usinas_Termicas" =  Usinas_Termicas,"tipo_turbina" =  tipo_turbina, "dados_no_tempo" = dados_no_tempo, "num"=num))}

  else {
    return(cat("Valores de datas incorretos"))
  }
}
