#Programa Potencia para o calculo da potencia disponivel em todos os dias para todas as hidreletricas.
#Por potencia disponivel entende-se a potencia que numa dada altura de queda liquida e com a entrada de agua 100%
#aberta as Usinas produziriam. O algoritmo implementado foi o proposto em : SILVA FILHO, Donato da. Dimensionamento
#de usinas hidroeletricas atraves de tecnicas de otimizacao evolutiva. 2003. Tese de Doutorado. Universidade de Sao
#Paulo. O algoritmo encontra-se na Pagina 195.


#################### POSSIVEIS PROBLEMAS###############################
# o polinomio cota jusante de Coaracy Nunes, por exemplo, pode vezes operar fora de seu intervalo de validade,
#de maneira que em situacoes de vertimento, a altura de queda liquida fica negativa
# Neste codigo ha que se tomar cuidado com os dados de entrada RESERVATORIOS. A ausencia de dados, sendo que onde
#eles faltam esta preenchido NA, impede a execucao deste programa. Alem disso, algumas usinas, Tres Marias, por exemplo,
#tem dias faltantes na base de dados da ANA. Isto deve ser observado pois tambem impede a execucao deste programa.
#source("~/Nilton/Projetos_Computacao/R/Capacity_Credit_Brazil/R/POTENCIAS.R", encoding = 'UTF-8')

POTENCIA <- function(RESERVATORIOS, Usinas_ONS2, dados_maquinas, tipo_turbina, nomes_reservatorios_ANA){
#Inicializacao de variaveis a serem utilizadas.
#hjus = altura da jusante. De início, recebe o valor de vazao defluente proveniente de
#hmon - Altura montante ou cota montante
#hbruta - Altura de queda bruta
#hliquida - Altura de queda liquida
#def - Defluenciada Entrada RESERVATORIOS (dados ANA-
#de dataframes da lista da variavel RESERVATORIOS)
#A altura de montante (Cota) e a Defluencia saem direto

  hjus <- RESERVATORIOS
  hbruta = RESERVATORIOS
  hliquida = RESERVATORIOS
  hmon = RESERVATORIOS
  def = RESERVATORIOS
  for (i in 1:nrow(Usinas_ONS2[2])){
    hjus[[i]] <- hjus[[i]][5]
    hmon[[i]] = RESERVATORIOS[[i]][[3]]
    def[[i]] = RESERVATORIOS[[i]][[5]]
  }

#Calculo de hjus com base no polinomio de jusante

  for (i in 1:nrow(Usinas_ONS2)){
    hjus[[i]] <- Usinas_ONS2[i, 147] + Usinas_ONS2[i, 148]*(hjus[[i]])+
    Usinas_ONS2[i, 149]*(hjus[[i]]^2)+
    Usinas_ONS2[i, 150]*(hjus[[i]]^3)+
    Usinas_ONS2[i, 151]*(hjus[[i]]^4)
  }

#Calculo da altura de queda liquida

  for (i in 1:nrow(Usinas_ONS2)){
    if(Usinas_ONS2[i,46]==1){

    hliquida[[i]] = hmon[[i]] - hjus[[i]] - Usinas_ONS2[i,47]/100*(hmon[[i]] - hjus[[i]])
    } else {
      hliquida[[i]] = hmon[[i]] - hjus[[i]] - Usinas_ONS2[i,47]
    }
  }

#Criacao de dataframes a serem utilizados no processo iterativo
  Qmax = as.data.frame(matrix(0,nrow(Usinas_ONS2),nrow(RESERVATORIOS[[1]])))
  row.names(Qmax) <- Usinas_ONS2$Usina
  colnames(Qmax) <- c(1:nrow(RESERVATORIOS[[1]]))
  q <- Qmax
  Qconj <- as.data.frame(matrix(0,nrow(Usinas_ONS2),5))
  row.names(Qconj) <- Usinas_ONS2$Usina
  colnames(Qconj) <- c("Conj1","Conj2","Conj3","Conj4","Conj5")
  maquinas <- Qconj
  RELATORIO = data.frame()

  cont.Erro = 1

  for(i in 1:5){
    k = 1 + 4*(i-1)
    maquinas[,i] <- dados_maquinas[,k]
  }

#inicio do procedimento iterativo
#i - numero de usinas
#j - numero de dias
#k - conjuntos

  for (i in 1:nrow(Usinas_ONS2)){
    cat("\n\n", i,"\n")
    for(j in 1:nrow(RESERVATORIOS[[i]])){
      cat("   ",j)

      if(hliquida[[i]][[1]][[j]]>0){

    #chutes iniciais de a e b para iniciar-se o while

        a=0
        b=100
        l=0

        while(abs(b)>10^-5){

          b = hliquida[[i]][[1]][[j]] - a
          a = hliquida[[i]][[1]][[j]]

      #Calculo da Vazão máxima para cada conjunto de máquina, conforme o tipo de turbina

          for(k in 0:4){
            if(maquinas[i,k+1] != 0){
              if(tipo_turbina[i] == "2 - Kaplan/Propeller"){alpha = 0.2}
              else{ alpha = 0.5}
              if(a<dados_maquinas[i,4*k+4]){
                Qconj[i,k+1] = (hliquida[[i]][[1]][[j]]/dados_maquinas[i,4*k+4])^alpha*dados_maquinas[i,4*k+3]
              } else {
                Qconj[i,k+1] = (hliquida[[i]][[1]][[j]]/dados_maquinas[i,4*k+4])^(-1)*dados_maquinas[i,4*k+3]
              }
            }
          }

      #Calculo da Vazão máxima para cada dia
            Qmax[i,j] = sum(maquinas[i,]*Qconj[i,])

            if(Qmax[i,j] > def[[i]][[j]]){
              def[[i]][[j]] = Qmax[i,j]
              q[i,j] = Qmax[i,j]
            }else{
              q[i,j] = Qmax[i,j]
            }

      #recálculo da altura de jusante com o novo valor de vazao

            hjus[[i]][[1]][[j]] <- Usinas_ONS2[i, 147] + Usinas_ONS2[i, 148]*(def[[i]][[j]])+
            Usinas_ONS2[i, 149]*(def[[i]][[j]]^2)+
            Usinas_ONS2[i, 150]*(def[[i]][[j]]^3)+
            Usinas_ONS2[i, 151]*(def[[i]][[j]]^4)

      #recalculo da altura de queda liquida com o novo valor de vazao

            if( Usinas_ONS2[i,46]==1){
              hliquida[[i]][[1]][[j]] = hmon[[i]][[j]] - hjus[[i]][[1]][[j]] - Usinas_ONS2[i,47]/100*(hmon[[i]][[j]] - hjus[[i]][[1]][[j]])
            }else{
            hliquida[[i]][[1]][[j]] = hmon[[i]][[j]] - hjus[[i]][[1]][[j]] - Usinas_ONS2[i,47]
            }
            l = l+1
          }

        } else {

          # Caso a hliquida seja negativa. Adota-se como altura jusante a cota média do canal de fuga obtida em Usinas_ONS2

          Notificao1 = "A usina numero "
          Notificacao2 = "Teve hliq negativa no dia "
          Notificacao3 = "Foi adotado hjus como igual a cota media do canal de fuga"

          if(Usinas_ONS2[i,46]==1){
            hliquida[[i]][[1]][[j]] = hmon[[i]][[j]] - Usinas_ONS2[i,38] - Usinas_ONS2[i,47]/100*(hmon[[i]][[j]] - Usinas_ONS2[i,38])
          }else{
            hliquida[[i]][[1]][[j]] = hmon[[i]][[j]] - Usinas_ONS2[i,38] - Usinas_ONS2[i,47]
          }

          RELATORIO[cont.Erro, 1] = paste("A usina numero", i, "Teve hliq negativa no dia", j , ". Foi adotado hjus como igual a cota media do canal de fuga")
          cont.Erro = cont.Erro +1
        }
    }
  }

#criação de variáveis para calculo da potência
  Pmax = as.data.frame(matrix(0,nrow(Usinas_ONS2),nrow(RESERVATORIOS[[1]][11])))
  row.names(Pmax) <- Usinas_ONS2$Usina
  colnames(Pmax) <- c(1:nrow(RESERVATORIOS[[1]][11]))
  Pconj <- as.data.frame(matrix(0,nrow(Usinas_ONS2),5))
  row.names(Pconj) <- Usinas_ONS2$Usina
  colnames(Pconj) <- c("Conj1","Conj2","Conj3","Conj4","Conj5")
  maquinas2 <- as.data.frame(matrix(0,nrow(Usinas_ONS2),10))
  row.names(maquinas2) <- Usinas_ONS2$Usina
  colnames(maquinas2) <- c("Hef1","Pef1","Hef2","Pef2","Hef3", "Pef3","Hef4", "Pef4","Hef5", "Pef5")
  conjunto <- 1
  cont.Erro2 = 1
  RELATORIO2 = data.frame()

  for(k in seq(from=1, to=9, by=2)){
    maquinas2[,k] <- dados_maquinas[,4*conjunto]
    maquinas2[,k+1] <- dados_maquinas[,4*conjunto-2]
    conjunto <- conjunto + 1
  }

  #calculo da potencia

  for (i in 1:nrow(Usinas_ONS2)){
    cat(" \n\n",i, " \n")
    for(j in 1:nrow(RESERVATORIOS[[1]])){
      cat(j, " ")
      #calculo da potencia para cada usina, em cada dia, levando-se em conta as
      #características individuais das máquinas que perfazem cada conjunto de máquinas

      if(hliquida[[i]][[1]][[j]]>0){

      if(tipo_turbina[i] == "2 - Kaplan/Propeller"){beta = 1.2} else {beta = 1.5}
      conjunto <- 1
      for(k in seq(from=1, to=9, by=2)){
        if(hliquida[[i]][[1]][[j]] < maquinas2[i,k]&maquinas2[i,k]!=0){
        Pconj[i,conjunto] = ((hliquida[[i]][[1]][[j]]/maquinas2[i,k])^beta)*(maquinas2[i,k+1])
        conjunto <- conjunto + 1
        } else {Pconj[i, conjunto] = maquinas2[i,k+1]
        conjunto <- conjunto + 1}
      }

      Pconj <- maquinas*Pconj

      #inserir do if para que apenas os dias apos o inicio da entrada em operacao da usina sejam efetivamente contabilizados
      #para o calculo da potencia disponivel
      #25569 ? o numero de dias entre o padrao utilizado pelo excel com inicio em 01/01/1900 e o utilizado pelo R que inicia-se em 01/01/1970
      #O R contabiliza o ano de 1900 como bisexto, sendo que h? o dia 29/02/1900 e o Excel no. Isto pode gerar erro ao converter-se um padrao para o outro.
      if(as.numeric(as.Date(RESERVATORIOS[[i]][11][j,], "%d/%m/%Y")) < (nomes_reservatorios_ANA[i,3] - 25569)){
        Pmax[i,j]=0
      }else{
        Pmax[i,j] <- sum(Pconj[i,])
      }
      }else{
        Pmax[i,j] = Usinas_ONS2[i,181]
        RELATORIO2[cont.Erro2,1] = paste("A Usina", i, "teve hliq negativa mesmo com a primeira intervencao no dia", j, " Foi adotada a potencia nominal como potencia disponivel.")
        cont.Erro2 = cont.Erro2 + 1
      }

    }
  }

  return(list("Pmax" = Pmax, "RELATORIO" = RELATORIO, "RELATORIO2" = RELATORIO2))
}


#####
# Foi corrigido um erro na linha 93, onde havia uma condicionante que nao havia sido implementada. hl< hef,maq e hl>= hef,maq.
# Outro ponto que foi corrigido foi que o termo hliquida[[i]][[1]][[1]][[k]] foi trocado por hliquida[[i]][[1]][[1]][[j]], pois o termo
#deve variar com j, que representa os dias, e nao com k, que eh apenas um contador auxiliar.


######NOTA 1#######

#Ha casos onde ha o vertimento mas a usina nao esta na sua cota maxima, esse vertimento pode
#ter ocorrido por outras raz?es al?m do reservatorio estar cheio. Ainda assim, a potencia e calculada
#normalmente pelo programa, pois queremos saber qual potencia aquela usina poderia estar produzindo,
#independentemente de razoes operacionais especificas.




######NOTA 2########

# A usina Henry Borden, no dia 24/11/2019 - Est? com o dado de Cota errado. Isto impede a execucao deste programa pois a altura de queda liquida
# fica com valor negativo.



########### NOTA 3######

# A usinas Fontes est? com os dados de Cota montante errados no site da ANA.
# A altura que esta la produz valores de altura ade queda liquida negativa e isso
# inviabiliza a execucao do programa. Esse erro decorre de um erro nos dados
# originais da ANA.



################ NOTA 4#################

# A Usina Cachoeira Caldeirao tem dados disponiveis na ANA apenas a partir de 01/04/2018, apesar da UHE estar operando desde 07/05/2016




######## NOTA 5#####################

# As Usinas COLIDER tem dados na ANA dispon?veis desde 01/04/2018, por?m sua operacao comercial inicia-se em 11/03/2019. Deve-se levar em conta a entrada em opera??o, pois toda a
# potencia disponivel entre 01/04/2018 e 11/03/2019 calculada pelo programa nao esta efetivamente disponivel.


######## NOTA 6#####################

#Usinas Nilo Pecanha e Fontes do complexto da Light no Rio n?o possuem dados de Cota montante para o intervado de 01/07/2017 a 01/07/2019

###### NOTA 7#######

#Alturas de queda liquida negativas em Coaracy Nunes, Itiquira II (24/05/2019 e 25/07/2019)
