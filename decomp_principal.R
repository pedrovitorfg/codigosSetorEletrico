library(lubridate)

############## LEITURA DE DADOS DO DECOMP
# rm(list = ls())

today <- Sys.Date()
format(today, format="%d de %B de %Y")

k3 = "C:\\Estágio\\RESULTADOS_DEC_ONS_052020_RV4_VE"

setwd(k3)

library(stringr)
library(readr)


####FUNCAO QUE LE O ARQUIVO PATAMAR
leitura_patamar <- function(linha_comeco, pat)
{
  
  pat <- pat[linha_comeco:(linha_comeco+14),]               #seleciona as linhas 2 a 16
  
  pat <- as.numeric(as.matrix(pat))                       #transforma em vetor
  
  pat_saida <- matrix(data = NA, nrow = 15, ncol = 13)    #cria um auxiliar
  
  pat_saida <- as.numeric(as.matrix(pat_saida))         #transforma em vetor
  
  aux = 16                                                #numero de colunas mais 01
  i = 1
  
  for (i in 1:195){                                       # colunas x linhas 
    
    if(i == aux){
      
      pat_saida[i-15] = pat[i]                            #coloca o elemento em sua posicao correta
      aux = aux + 3
      
    } else {
      pat_saida[i] = pat[i]
    }
    
  }
  
  pat_aux <- matrix(data = NA, nrow = 15, ncol = 13)
  
  for(j in 1:12){
    for(i in 1:15){
      pat_aux[i,j] = pat_saida[i + (j-1)*15]              #cont(linha) + (cont(col)-1)xnum_linha
    }
  }
  
  pat_saida <- pat_aux[,1:12]                             #excluir a ultima linha que é NA
  
  return(pat_saida)
}



########## FUNCAO IMPORTANTE

bloco_de_variaveis <- function(posicao)
{
  
  for(i in (dado_ini[posicao]):(dado_fim[posicao])){
    
    j2 = 1
    
    for(j in 2:ncol(sumario)){
      
      if(!(is.na(sumario[i,j]))){
        
        sumario[i,j2] <- sumario[i,j]
        
        if(j2 == 1){
          
          if(j > 2){
            
            for(k in 2:(j-1)){
              
              sumario_orig[i,1] <- str_c(sumario_orig[i,1],sumario_orig[i,k])
            }
            
          }
          else{
            
            k = i - linha_XXX[posicao]
            
            sumario_orig[i,1] <- str_c(sumario_orig[i,1]," ",'(',k,')')
            
          }
          
        }
        
        j2 = j2 + 1
      }
      
    }
    
  }
  
  
  saida = sumario[(dado_ini[posicao]):(dado_fim[posicao]),1:(semanas+1)]
  
  row.names(saida) = sumario_orig[(dado_ini[posicao]):(dado_fim[posicao]),1]
  
  return(saida)
}


ger_hidr <- function(ger){
  
  se = ger[c(1:109,150,155),]
  s = ger[c(110:136,152:154,156:157),]
  ne = ger[c(137:144,151),]
  n = ger[c(145:149,158:162),]
  
  se = apply(se, 2, sum)
  s = apply(s, 2, sum)
  ne = apply(ne, 2, sum)
  n = apply(n, 2, sum)
  
  sin = rbind(se,s,ne,n)
  
  sin = sin[,-ncol(sin)]
  
  return(sin)
  
}
# setwd("Y:/Decks Modelos Computacionais/CVAR_2019/Decomp_modif/DEC_ONS_012018_RV0_VE")


if(file.exists("sumario.rv0")){
  
  sumario_orig <- read.table("sumario.rv0", header=FALSE, sep="",skip=0,fill=TRUE,dec=".",stringsAsFactors = FALSE)
  
}
if(file.exists("sumario.rv1")){
  
  sumario_orig <- read.table("sumario.rv1", header=FALSE, sep="",skip=0,fill=TRUE,dec=".",stringsAsFactors = FALSE)
  
}
if(file.exists("sumario.rv2")){
  
  sumario_orig <- read.table("sumario.rv2", header=FALSE, sep="",skip=0,fill=TRUE,dec=".",stringsAsFactors = FALSE)
  
}
if(file.exists("sumario.rv3")){
  
  sumario_orig <- read.table("sumario.rv3", header=FALSE, sep="",skip=0,fill=TRUE,dec=".",stringsAsFactors = FALSE)
  
}
if(file.exists("sumario.rv4")){
  
  sumario_orig <- read.table("sumario.rv4", header=FALSE, sep="",skip=0,fill=TRUE,dec=".",stringsAsFactors = FALSE)
  
}


sumario = sumario_orig

linha_XXX = NULL

for(i in 1:nrow(sumario)){
  
  k = unlist(strsplit(sumario[i,1], ""))
  
  k1 = which(k == "-")
  
  if(length(k1) > 5){    ######### Vai começar uma variável
    
    linha_XXX = c(linha_XXX,i)
    
  }
  
}


#### QUANTAS SEMANAS TEM O MES?

dect = sumario[linha_XXX[2]-1,]

semanas = -1

for(j in 1:ncol(dect)){
  
  if(dect[j] != ""){
    
    semanas = semanas + 1
  }
  
}


linha_XXX = c(linha_XXX, (nrow(sumario)+1))

for(i in 1:nrow(sumario)){
  
  sumario[i,] <- as.numeric(sumario[i,])
  
}

######## Analisa todos os marcadores e vê os que tem o indicador de semana na linha de cima #########

dado_ini=NULL
dado_fim=NULL

k=1

for (i in 1:length(linha_XXX)){
  j=1
  while(!(xor((sumario_orig[linha_XXX[i]-1,j]!="Sem_01"),(sumario_orig[linha_XXX[i]-2,j]!="Sem_01")))&(j<20)&!(is.na(sumario_orig[linha_XXX[i]-1,j]))){
    j=j+1
  }
  if((j<20)&!(is.na(sumario_orig[linha_XXX[i]-1,j]))){
    sumario[linha_XXX[i]+1,j-1]
    dado_ini[k] = linha_XXX[i]+1
    dado_fim[k] = linha_XXX[i+1]-1
    k=k+1
  }
}

dado_fim[length(dado_fim)] = linha_XXX[length(linha_XXX)]-1


######## Obtem a linha de títulos dos blocos ###########

dado_title=NULL

k=1

for (i in 1:length(linha_XXX)){
  
  comp_ini = sum((linha_XXX[i])!=(dado_ini-1))
  comp_fim = sum((linha_XXX[i])!=(dado_fim+1))

  if((comp_ini == length(dado_ini))&(comp_fim == length(dado_fim))){
    dado_title[k] = linha_XXX[i]-1
    k=k+1
  }
}

for(i in (length(dado_title)+1):(length(dado_fim)+1) ){
  dado_title[i]=dado_fim[i-2]
}

x=which(sumario_orig == "50_Hz")

for(i in 1:length(x)){
  for(j in 1:length(dado_title)){
    if(dado_title[j]==x[i]){
      y=i
    }
  }
}

i=which(dado_title == x[y])

dado_title <-dado_title[-i]

######## Coloca os dados de fim da parte do EARM na posição certa, pq eles não acabam no marcador padrão #######

x = which(sumario_orig == "REE:")

for(i in 2:length(x)){
  y = which(dado_fim == x[i])
  dado_fim[y] = dado_fim[y] - 3
}

########  ########

variaveis <- list()

for(i in 1:length(dado_ini)){
  
  variaveis[[i]] = bloco_de_variaveis(i)
  
}


########### CMO MEDIO DE CADA SUBSISTEMA

if(file.exists("patamar.dat")){
  
  patamar <- read_table2("patamar.dat", skip = 4)
  
  #anoini <- read.table("cmarg001.out", header=FALSE, sep="",skip=2,fill=TRUE,dec=".", nrows = 1)
  
  #anoini <- unlist(anoini[2])
  
  anoini = 2020
  
  linha <- which(patamar == anoini)
  
  pat_duracao <- leitura_patamar(linha[1],patamar)
  
  pat <- array(NA, c(15,12,4))
  
  for(SM in 1:4){
    pat[,,SM] <- leitura_patamar(linha[SM+1],patamar)
  }
  
}else if(file.exists("patamar.eas")){
  
  patamar <- read_table2("patamar.eas", skip = 4)
  
}else{ 
  print("Coloque o arquivo patamar na sua pasta!")
  break
}

mes = strsplit(sumario_orig[3,3], split = "")

if( mes[[1]][1] == 'J' )
{
  
  if( (mes[[1]][2] == 'A') && (mes[[1]][3] == 'N') )
  {
    
    mes = 1
    
  }else if ( (mes[[1]][2] == 'U') && (mes[[1]][3] == 'N') )
  {
    
    mes = 6
    
  }else if ( (mes[[1]][2] == 'U') && (mes[[1]][3] == 'L') )
  {
    
    mes = 7
    
  }
  
  
  
  
}else if( mes[[1]][1] == 'M' )
{
  
  if( (mes[[1]][2] == 'A') && (mes[[1]][3] == 'R') )
  {
    
    mes = 3
    
  }else if ( (mes[[1]][2] == 'A') && (mes[[1]][3] == 'I') )
  {
    
    mes = 5
    
  }
  
  
}else if( mes[[1]][1] == 'A' )
{
  
  if( (mes[[1]][2] == 'B') && (mes[[1]][3] == 'R') )
  {
    
    mes = 4
    
  }else if ( (mes[[1]][2] == 'G') && (mes[[1]][3] == 'O') )
  {
    
    mes = 8
    
  }
  
}else if((mes[[1]][1] == 'F') && (mes[[1]][2] == 'E') && (mes[[1]][3] == 'V') )
{
  mes = 2
  
}else if((mes[[1]][1] == 'S') && (mes[[1]][2] == 'E') && (mes[[1]][3] == 'T') )
{
  mes = 9
  
}else if((mes[[1]][1] == 'O') && (mes[[1]][2] == 'U') && (mes[[1]][3] == 'T') )
{
  
  mes = 10
  
}else if((mes[[1]][1] == 'N') && (mes[[1]][2] == 'O') && (mes[[1]][3] == 'V') )
{
  
  mes = 11
  
}else if((mes[[1]][1] == 'D') && (mes[[1]][2] == 'E') && (mes[[1]][3] == 'Z') )
{
  
  mes = 12
  
}

if(nrow(variaveis[[7]]) > 10){
  
  write.csv2(variaveis[[7]], file = "cmo.csv",row.names = FALSE,na="")
  
}else{
  
  write.csv2(variaveis[[6]], file = "cmo.csv",row.names = FALSE,na="")
  
}

cmo = read.csv("cmo.csv", sep = ";")

pesos = pat[1:3,mes,]

cmo_valendo = array(data = 0, dim = c(4,semanas))

for(j in 1:semanas){
  
  for(sm in 1:4){
    
    a = 0
    
    for(pat2 in 1:3){
      
      a = a + pesos[pat2,sm]
      
      cmo_valendo[sm,j] = cmo_valendo[sm,j] + cmo[(pat2 + 4*(sm-1)),j]*pesos[pat2,sm]
      
    }
    
    cmo_valendo[sm,j] = cmo_valendo[sm,j]/a
    
  }
  
}

variaveis[[7]] = cmo_valendo

file.remove("cmo.csv")

SM = c('SE', 'S', 'NE', 'N')
row.names(variaveis[[7]]) = SM

#################################################################################

################ GER HIDRICA POR SUBSISTEMA FICARÁ NA ÚLTIMA POSIÇÃO DO VARIAVEIS

geracao_sm = array(data = NA, dim = c(4,3,semanas) )


geracao_T = NULL

for(i in 1:3){
  
  write.csv2(variaveis[[9+i]], file = "ger.csv",row.names = FALSE,na="")
  
  ger = read.csv("ger.csv", sep = ";")
  
  geracao_sm[,i,] = ger_hidr(ger)
  
  file.remove("ger.csv")
  
}

ger_H = array(data = NA, dim = c(semanas,4))

for(i in 1:semanas)
{
  
  ger_M_SEM = geracao_sm[,,i]%*%pesos
  soma = apply(pesos, 2, sum)
  
  for(j in 1:length(soma))
  {
    
    ger_H[i,j] = ger_M_SEM[j,j]/soma[j]
  }
  
}

variaveis[[length(variaveis)+1]] = t(ger_H)

SM = c('SE', 'S', 'NE', 'N')

row.names(variaveis[[length(variaveis)]]) = SM

variaveis[[3]] = sumario_orig[dado_ini[3]:dado_fim[3],2:9]
row.names(variaveis[[3]]) = SM

SM = sumario_orig[dado_ini[2]:dado_fim[2],1]
variaveis[[2]] = sumario_orig[dado_ini[2]:dado_fim[2],2:10]
row.names(variaveis[[2]]) = SM

#################################################################################

tt = NULL
ss = NULL
for(i in 1:length(dado_title)){

    tt[i] = str_trim( paste( sumario_orig[dado_title[i],],collapse = " ") )
    ss[i] = strsplit(tt[i],split = "")
    j=1
    while ((ss[[i]][j] != "/")&(j<length(ss[[i]]))) {
      j=j+1
    }
    if (j != length(ss[[i]])){
      ss[[i]][j] = " por "
    }
    tt[[i]] = paste(ss[[i]],collapse = "")
}

ss = NULL
for(l in 1:3){
  for(i in 1:length(dado_title)){
    
    ss[i] = strsplit(tt[i],split = "")
    j=1
    while ((ss[[i]][j] != ":")&(j<length(ss[[i]]))) {
      j=j+1
    }
    if (j != length(ss[[i]])){
      ss[[i]][j] = " - "
    }
    tt[[i]] = paste(ss[[i]],collapse = "")
  }
}

tt[length(tt)+1] = "GERAÇÃO HÍDRICA POR SUBSISTEMA"

tt2 = array(data = NA, dim = length(tt))

for(i in 1:length(tt)){
  
  ttemp = strsplit(tt[i],split = "")
  if ((ttemp[[1]][1]=="R")&(ttemp[[1]][2]=="E")&(ttemp[[1]][3]=="E")){
    tt[i] = paste("EARM em MWmes",tt[i],sep = " ")
  }
  tt2[i] = paste(i, tt[i], sep = " - ")
  
}
tt2[length(tt2)+1] = "99 - Tudo"

erros = c(4, 5, 6, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22) 
i=1
while (i<=length(erros)) {
  variaveis[[erros[i]]] = variaveis[[erros[i]]][,-ncol(variaveis[[erros[i]]])]
i=i+1
  }



algo_mais = 1

num_arq = NULL
num_lista = NULL

nome_orig = NULL


while (algo_mais == 1) {
  
  print(tt2)
  
  num_arq2 <- as.numeric(readline("O que você quer? "))
  
  num_arq = c(num_arq,num_arq2)
  
  if(num_arq2 != 99){
    
    print("Deseja realizar a leitura de mais algum arquivo? ")
    
    algo_mais <- as.numeric(readline("Digite: 1 - SIM        2 - NÃO : "))
    
  } else{
    
    algo_mais = 2
    num_arq = num_arq2
  }
  
}


if(file.exists("rodada.R")){

  load("rodada.R")

}else{

  rodada = 0

}

rodada = rodada + 1

nome1 = paste("saidas",rodada)

save(rodada,file = "rodada.R")

dir.create(file.path(k3,nome1))

k2 = file.path(k3,nome1)

setwd(k2)

if(num_arq != 99){
  for(i in 1:length(num_arq)){
    
    write.csv2(variaveis[[num_arq[i]]], file = paste(tt[num_arq[i]],".csv",collapse = ""),row.names = TRUE,na="")
  }
}else{
  for(i in 1:(length(variaveis))){
    
    write.csv2(variaveis[[i]], file = paste(tt[i],".csv",collapse = ""),row.names = TRUE,na="")
    
  }
}
setwd(k3)

print("Foram gerados, na pasta saidasX, arquivos .csv para estudo de caso.")
print("Caso você pretenda utilizar os dados gerados em R, crie um novo script, ou utilize o console.")
print("Chame a variável variaveis[[n]].")
print("Em que n é o número do arquivo apresentado no sumário. Ex: CMO - variaveis[[7]]")

