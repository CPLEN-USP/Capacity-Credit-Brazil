#This file exemplifies the steps to calculate
#ELCC (Effective Load Carrying Capability) of
#solar and wind resources for Brazil's SIN
#(National Interligated System).
#The specificity of such an ELCC calculation
#resides in that Brazil has a large share of
#hydroelectric resources, a dispatchable source
#with variable capacity. Typically, the ELCC calculation
#takes thermoelectric plants as a reference,
#and it considers that the dispatchable
#capacity is constant throughout the analysis period.
#For systems with a large share of limited-energy resources
#(e.g., hydropower), the assumption of constant dispatchable
#capacity is not acceptable.
#Therefore, it is necessary to adjust the available
#dispatchable capacity throughout the period of analysis.
#We have developed a method that considers the capacity variation
#of hydro sources, and we have also applied
#such a method to calculate
#ELCC from wind and solar in Brazil


#Path to used packages:
local_pacotes <- file.path("Variaveis", "pacotes.rds")
pacotes <- readRDS(local_pacotes)

#REPRA installation from source code:
install.packages("repra_0.4.4.tar.gz", repos = NULL, type="source")

#Installation of all other packages
ativar <- lapply(pacotes[-length(pacotes)], install.packages, character.only=TRUE)

#Load all used packages:
ativar <- lapply(pacotes, library, character.only=TRUE)




#Path to all used programs:
#OBSERVATIONS: First, the names of all the files used are loaded,the files
#are in the R Folder of the project
#In the list.files function, the path argument indicates the folder where to look for
#the files; the full.names argument is used to store the full name from the
#path to the file; the pattern argument indicates to store only
#paths of .R files: '\\' indicates the beginning of the string of interest and
#'$' indicates the end of the string of interest.
files_sources = list.files(path="./R", full.names = TRUE, pattern = "\\.R$")
#Load all used programs
#OBSERVATIONS: the invisible function prevents display of the (possibly confusing) output message
#of the sapply command when executing the source function for each of the files
#whose paths are stored in the files_sources array
invisible(sapply(files_sources, source))


#Paths to all data used:
local_Usinas_UHE <- file.path("Data", "Usinas_UHE.xlsx")
local_Termicas <- file.path("Data", "Termicas.xlsx")
local_Dados_Finais_BR_1999_2019 <- file.path("Data", "Dados_Finais_BR_1999_2019.xlsx")
local_Dados_Finais_NE_2017_2020 <- file.path("Data", "Dados_Finais_NE_2017_2020.xlsx")
local_Dados_faltantes <- file.path("Data", "Dados_faltantes_ANA.xlsx")
#Save names of named regions in the worksheets Dados_Finais_NE_2017_2020.xlsx
#and Date/Dados_Finais_BR_1999_2019.xlsx in vectors.
#These named regions store load levels and intermittent source generation
#(solar and wind) in selected years
NE <- getNamedRegions("Data/Dados_Finais_NE_2017_2020.xlsx")
BR <- getNamedRegions("Data/Dados_Finais_BR_1999_2019.xlsx")
#Transfer the used data to variables used by the programs
dados_maquinas = read.xlsx(local_Usinas_UHE, namedRegion= "Dados_Maquinas")
tipo_turbina = read.xlsx(local_Usinas_UHE, namedRegion= "Tipo_Turbina")
Usinas_ONS = read.xlsx(local_Usinas_UHE, namedRegion= "Us_ONS_BR")
Usinas_Termicas = read.xlsx(local_Termicas, namedRegion= "Termtotal")
Usinas_ONS2 = read.xlsx(local_Usinas_UHE, namedRegion= "Us_ONS2_BR")
nomes_reservatorios_ANA = read.xlsx(local_Usinas_UHE, namedRegion= "ANA_Data")






#EXAMPLE (year 2017, Northwest region)

#Define Region and analysis period using Programa_auxiliar.R:
dataInicial <- c('01/01/2017')
dataFinal <- c('31/12/2017')
#In Brazil, each plant is identified by a number. There are 144 plants.
# i, j, h and k - serve to define a range of plants to be considered. It facilitates
#the creation of continuous analysis subgroups.
#Example: If you want to simulate two subgroups in the subgroup of 144 plants,
#from 1 to 10 and from 70 to 144, do i=1, j=10, h=70 and k=144 to simulate.
#The below combination implies using all plants in the simulation
i <- c(1)
j <- c(2)
h <- c(3)
k <- c(144)
Regiao <- c('NORDESTE')
#Load year (Ano=2017) and data used for Nortweast:
Ano <- '2017'
NR1 <- NE[1]
#Load curve data and intermittent source generation for Nortweast year 2017:
dados_no_tempo = read.xlsx(local_Dados_Finais_NE_2017_2020, namedRegion= NR1)
#Change format dataframe:
dados_PV_NE_2017 <- dados_no_tempo
dados_PV_NE_2017[,2] <- 'NORDESTE'


#Calculate capacity for each hydropower in each day of the studied year
#(year 2017, NORDESTE region):
reservatorios_ne <- file.path("Variaveis", "RESERVATORIOS_NE_2017.rds")
RESERVATORIOS_NE_2017 <- readRDS(reservatorios_ne)
capacidade_hidreletrica_NE_2017 <- POTENCIA(RESERVATORIOS_NE_2017$RESERVATORIOS, RESERVATORIOS_NE_2017$Usinas_ONS2, RESERVATORIOS_NE_2017$dados_maquinas, RESERVATORIOS_NE_2017$tipo_turbina,  RESERVATORIOS_NE_2017$nomes_reservatorios_ANA)


#Calculate ELCC and LOLE for each day (year 2017, NORDESTE):
ELCC_NE_2017_dia_PV <- calculo_ELCC_LOLE_dia(dados_PV_NE_2017, RESERVATORIOS_NE_2017$Usinas_ONS, RESERVATORIOS_NE_2017$Usinas_Termicas, capacidade_hidreletrica_NE_2017$Pmax, 'NORDESTE')
#The output of the above command

#Before executing the month calculation, it is necessary to indicate if the year is bissextile or not
#For bissextile year, BISSEXTO = "YES"; otherwise, BISSEXTO = "NOT"
#As 2017 is not bissextile, BISSEXTO = "NOT":
#Calculate ELCC and LOLE for each month (year 2017, NORDESTE):
ELCC_NE_2017_mes_PV <- calculo_ELCC_LOLE_mes(dados_PV_NE_2017, RESERVATORIOS_NE_2017$Usinas_ONS, RESERVATORIOS_NE_2017$Usinas_Termicas, capacidade_hidreletrica_NE_2017$Pmax, BISSEXTO= 'NOT', AREA = 'NORDESTE')


#Calculate ELCC and LOLE for the year 2017 (NORDESTE):
ELCC_NE_2017_period_PV <- calculo_ELCC_LOLE(dados_PV_NE_2017, RESERVATORIOS_NE_2017$Usinas_ONS, RESERVATORIOS_NE_2017$Usinas_Termicas, capacidade_hidreletrica_NE_2017$Pmax, AREA= 'NORDESTE')

