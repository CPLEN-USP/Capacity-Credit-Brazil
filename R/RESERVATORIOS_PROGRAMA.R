RESERVATORIOS_PROGRAMA <- function(RESERVATORIOS, nomes_reservatorios_ANA){
  for (i in 1:length(RESERVATORIOS)){
    RESERVATORIOS[[i]][1][,1] <- unlist(RESERVATORIOS[[i]][1][,1], recursive = FALSE, use.names = FALSE) 
    RESERVATORIOS[[i]][1][,1] <- as.character(gsub(",", ".", RESERVATORIOS[[i]][1][,1]))
    RESERVATORIOS[[i]][2][,1] <- unlist(RESERVATORIOS[[i]][2][,1], recursive = FALSE, use.names = FALSE) 
    RESERVATORIOS[[i]][2][,1] <- as.character(gsub(",", ".", RESERVATORIOS[[i]][2][,1]))
    RESERVATORIOS[[i]][3][,1] <- unlist(RESERVATORIOS[[i]][3][,1], recursive = FALSE, use.names = FALSE) 
    RESERVATORIOS[[i]][3][,1] <- as.numeric(gsub(",", ".", RESERVATORIOS[[i]][3][,1]))
    RESERVATORIOS[[i]][4][,1] <- unlist(RESERVATORIOS[[i]][4][,1], recursive = FALSE, use.names = FALSE) 
    RESERVATORIOS[[i]][4][,1] <- as.numeric(gsub(",", ".", RESERVATORIOS[[i]][4][,1]))
    RESERVATORIOS[[i]][5][,1] <- unlist(RESERVATORIOS[[i]][5][,1], recursive = FALSE, use.names = FALSE) 
    RESERVATORIOS[[i]][5][,1] <- as.numeric(gsub(",", ".", RESERVATORIOS[[i]][5][,1]))
    RESERVATORIOS[[i]][6][,1] <- unlist(RESERVATORIOS[[i]][6][,1], recursive = FALSE, use.names = FALSE) 
    RESERVATORIOS[[i]][6][,1] <- as.numeric(gsub(",", ".", RESERVATORIOS[[i]][6][,1]))
    RESERVATORIOS[[i]][7][,1] <- unlist(RESERVATORIOS[[i]][7][,1], recursive = FALSE, use.names = FALSE) 
    RESERVATORIOS[[i]][7][,1] <- as.numeric(gsub(",", ".", RESERVATORIOS[[i]][7][,1]))
    RESERVATORIOS[[i]][8][,1] <- unlist(RESERVATORIOS[[i]][8][,1], recursive = FALSE, use.names = FALSE) 
    RESERVATORIOS[[i]][8][,1] <- as.numeric(gsub(",", ".", RESERVATORIOS[[i]][8][,1]))
    RESERVATORIOS[[i]][9][,1] <- unlist(RESERVATORIOS[[i]][9][,1], recursive = FALSE, use.names = FALSE) 
    RESERVATORIOS[[i]][9][,1] <- as.numeric(gsub(",", ".", RESERVATORIOS[[i]][9][,1]))
    RESERVATORIOS[[i]][10][,1] <- unlist(RESERVATORIOS[[i]][10][,1], recursive = FALSE, use.names = FALSE) 
    RESERVATORIOS[[i]][10][,1] <- as.numeric(gsub(",", ".", RESERVATORIOS[[i]][10][,1]))
    RESERVATORIOS[[i]][11][,1] <- unlist(RESERVATORIOS[[i]][11][,1], recursive = FALSE, use.names = FALSE) 
    RESERVATORIOS[[i]][11][,1] <- as.character(gsub(",", ".", RESERVATORIOS[[i]][11][,1]))
    #cat("\n\n" , i, "\n\n")
      }

return(RESERVATORIOS)
}

