#Programa para fazer a adequacao dos dados de entrada no programa POTENCIAS. 
#Versao 29/04/2020

ADEQ_DADOS = function(RESERVATORIOS, Usinas_ONS2){

# i se refere a qual usina sendo processada
for (i in 1:nrow(Usinas_ONS2[2])){
  
  cat(" " , i , " ")
  
  #caso n?o haja nenhum NA na sequencia o programa nao e executado.
  if(nrow(filter(RESERVATORIOS[[i]],is.na(RESERVATORIOS[[i]][3])))!=0){
    
    cat("NA_1  ")
    
  # j se refere ao dia
    j=1
    n=0
    o = 1
    #aux1a e aux2a sao os vetores que conterao a posicao do inicio e fim de cada grupo de NA's
    aux1 = c()
    aux2 = aux1
    
   #esta secao faz a contagem de quantos grupos de NA foram encontrados nos dados de cada usina
     while(j<nrow(RESERVATORIOS[[i]])){
      
      while(!is.na(RESERVATORIOS[[i]][3][j,1])&j!=(nrow(RESERVATORIOS[[i]]))){
        j=j+1
        #cat(j)
      }

      aux1[o] = j
      while(is.na(RESERVATORIOS[[i]][3][j,1])&j!=(nrow(RESERVATORIOS[[i]]))){
        j=j+1
      }
      
      #if(j>1&j!=nrow(RESERVATORIOS[[i]][[1]])){aux2a[o] = j-1}
     # else{aux2a[o] = j}
      
      if(j==1|(j==nrow(RESERVATORIOS[[i]])&is.na(RESERVATORIOS[[i]][3][j,1]))){
        aux2[o] = j
      }  else{
        aux2[o] = j-1
      }
      
      o = o+1
    }
    
    
  o = length(aux1)
    
    #Esta secao faz a substituicao dos NA's encontrados ou pelas medias quando e o caso ex: {2,3,4, NA , NA, NA, 7,5} onde os NA's serao a media entre 4 e 7
    #ou fara a substituicao pelo valor adjacente ex: {NA, 1,2,3, NA}, onde o primeiro NA e susbstituido por 1 e o ultimo por 3. 
    
  
    for(h in 1:o){
      for(aux in aux1[h]:aux2[h]){
        
        if(aux1[h]==1|aux2[h]==nrow(RESERVATORIOS[[i]])){
          if(aux1[h]==1){
            RESERVATORIOS[[i]][3][aux,1] = RESERVATORIOS[[i]][3][aux2[h]+1,1]
          }
          if(aux2[h]==nrow(RESERVATORIOS[[i]])){
            
            RESERVATORIOS[[i]][3][aux,1] = RESERVATORIOS[[i]][3][aux1[h]-1,1]
          } 
        } else {
          
          RESERVATORIOS[[i]][3][aux,1] = (RESERVATORIOS[[i]][3][aux1[h]-1,1] + RESERVATORIOS[[i]][3][aux2[h]+1,1])/2
          
        }
        
      }
    }
    
  }
  
  #rm(aux1, aux2)
  #mesmo programa porem agora para os dados de defluencia, o anterior era para os dados de cota.
  
  if(nrow(filter(RESERVATORIOS[[i]],is.na(RESERVATORIOS[[i]][5])))!=0){
    
    cat("NA_2   ")
    
    j=1
    n=0
    o = 1
    aux1 = c()
    aux2 = aux1
    
    while(j<nrow(RESERVATORIOS[[i]])){
      
      while(!is.na(RESERVATORIOS[[i]][5][j,1])&j!=(nrow(RESERVATORIOS[[i]]))){
        j=j+1
        #cat(j)
      }
      
      aux1[o] = j
      while(is.na(RESERVATORIOS[[i]][5][j,1])&j!=(nrow(RESERVATORIOS[[i]]))){
        j=j+1
      }
      
      
      if(j==1|(j==nrow(RESERVATORIOS[[i]])&is.na(RESERVATORIOS[[i]][5][j,1]))){
        aux2[o] = j
      }  else{
        aux2[o] = j-1
      }
      
      
      o = o+1
      
    }
    
    o = length(aux1)
    
    
    for(h in 1:o){
      for(aux in aux1[h]:aux2[h]){
        
        if(aux1[h]==1|aux2[h]==nrow(RESERVATORIOS[[i]])){
          if(aux1[h]==1){
            RESERVATORIOS[[i]][5][aux,1] = RESERVATORIOS[[i]][5][aux2[h]+1,1]
          }
          if(aux2[h]==nrow(RESERVATORIOS[[i]])){
            
            RESERVATORIOS[[i]][5][aux,1] = RESERVATORIOS[[i]][5][aux1[h]-1,1]
          } 
        } else {
          
          RESERVATORIOS[[i]][5][aux,1] = (RESERVATORIOS[[i]][5][aux1[h]-1,1] + RESERVATORIOS[[i]][5][aux2[h]+1,1])/2
          
        }
        
      }
    }
    
  }
 
  
}
  return(RESERVATORIOS)

}
