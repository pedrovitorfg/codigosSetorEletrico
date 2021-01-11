library(readr)
library(lubridate)

dirload = "C:\\Estágio"
setwd(dirload)

source("carregarFuncoes.R")

dir1 = "C:\\Estágio\\dessem_semana\\DS_ONS_012020_RV3D19"

setwd(dir1)


rampas = read_lines("rampas.dat")

rampas[121]

marc = grep("&XX XXX XXX  X   X  XXXXXXXXXX XXXXX X",rampas)

usinas = rampas[marc-2]

usinas = strsplit(usinas,split = "")

rampasBD = list()
nomes = NULL
codigos = NULL
guardar = NULL

for(imarc in 1:length(marc))
{
  nome = rampas[marc[imarc]-2]
  a = max(which(usinas[[imarc]]=="&"))+2
  b = min(which(usinas[[imarc]]=="-"))-1
  codigo = paste(usinas[[imarc]][a:b],sep = "",collapse = "")
  
  nomes[imarc] = nome
  codigos[imarc] = codigo
  
  if(imarc!=length(marc))
  {
    dados = rampas[marc[imarc]:(marc[imarc+1]-5)]
    
    #dados
    
  }
  else
  {
    dados = rampas[marc[imarc]:(grep("FIM",rampas)-1)]
  }
  
  aux = strsplit(dados,split = "")
  coef = NULL
  
  # coeficiente de cada linha, para saber se ela esta vazia ou se ha dados
  for(i in 1:length(aux))
  {
    coef[i] = sum(aux[[i]]==" ") - sum(aux[[i]]!=" ")
  }
  
  coef2 = coef[which(coef==max(coef))]
  
  if(sum(coef<0)!=0)
  {
    guardar = rbind(guardar,
                    cbind(dados[which(coef<0)],rep(codigo,length(dados[which(coef<0)]))))
    
    if(which(coef<0)!=1)
    {
      dados = dados[-which(coef<0)]
    }
    
    
  }
  
  if((sum(grepl("&",dados[-1]))!=0)&&(sum(nchar(dados)<3)!=0))
  {
    a = which((grepl("&",dados))&(nchar(dados)<3))
    dados = dados[-a]
  }
  
  if(coef2>25)
  {
    dados = dados[-which(coef==max(coef))]
  }
  
  
  a = strsplit(dados[1],split = "")[[1]]
  a = which(a!=" ")
  #a = setdiff(1:length(a),aa)
  
  b = diff(a)
  a = cbind(c(1,(a[which(b!=1)]+1)),c(a[which(b!=1)],nchar(dados[2])))
  
  a
  
  matriz = matrix(nrow = (length(dados)-1),ncol = nrow(a))
  b = strsplit(rampas[marc[imarc]-1],split = "")[[1]]
  nomeColuna = array(dim = nrow(a))
  
  for(i in 2:length(dados))
  {
    aux = strsplit(dados[i],split = "")[[1]]
    
    for(j in 1:nrow(a))
    {
      matriz[(i-1),j] = tirarEspaco(paste(aux[a[j,1]:a[j,2]],sep = "",collapse = ""))
      nomeColuna[j] = paste(b[a[j,1]:a[j,2]],sep = "",collapse = "")
    }
    
    
  }
  
  
  colnames(matriz) = nomeColuna
  
  
  rampasBD[[imarc]] = matriz
  
}

rampasBD[[which(codigos=="110")]]
usinas = rampas[marc-2]
usinas[90]
