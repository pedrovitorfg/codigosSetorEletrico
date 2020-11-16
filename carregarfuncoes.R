lerClast = function()
{
  # clast
  clast = read_lines("clast.dat")
  
  campos = strsplit(clast[2],split=" ")[[1]]
  campos = nchar(campos)
  campos = campos+1
  colunas = clast[1]
  
  cc = matrix(nrow = length(3:length(clast)),ncol = length(campos[-1]))
  
  for(i in 3:length(clast))
  {
    a = strsplit(clast[i],split = "")[[1]]
    marc=campos[-1]
    marc[1]=5
    marc = cumsum(marc)
    
    for(im in 1:length(marc))
    {
      if(im==1)
      {
        cc[i-2,im]=tirarEspaco(paste(a[1:marc[im]],sep = "",collapse = ""))
      }
      else
      {
        cc[i-2,im]=tirarEspaco(paste(a[(marc[im-1]+1):(marc[im])],sep = "",collapse = ""))
      }
    }
    
  }
  
  clast = cc
  
  return(clast)
}

lerTerm = function()
{
  term = read_lines("term.dat")
  
  cod = NULL
  nome = NULL
  pot = NULL
  fcmax = NULL
  teif = NULL
  ip = NULL
  inflex = matrix(data = 0,nrow = (length(term)-2),ncol = 13)
  
  for(i in 3:length(term))
  {
    aux = strsplit(term[i],split = "")[[1]]
    cod[i] = as.numeric(paste(aux[1:5][-which(aux[1:5]==" ")],sep = "",collapse = ""))
    nome[i] = paste(aux[6:18],sep = "",collapse = "")
    pot[i] = as.numeric(paste(aux[19:25][-which(aux[19:25]==" ")],sep = "",collapse = ""))
    fcmax[i] = as.numeric(paste(aux[26:30][-which(aux[26:30]==" ")],sep = "",collapse = ""))
    teif[i] = as.numeric(paste(aux[31:38][-which(aux[31:38]==" ")],sep = "",collapse = ""))
    ip[i] = as.numeric(paste(aux[39:45][-which(aux[39:45]==" ")],sep = "",collapse = ""))
    
    for(j in 1:13)
    {
      inflex[(i-2),j] = as.numeric(paste(aux[(46+(j-1)*7):(52+(j-1)*7)][-which(aux[(46+(j-1)*7):(52+(j-1)*7)]==" ")],sep = "",collapse = ""))
    }
  }
  
  cod = cod[-(1:2)]
  nome = nome[-(1:2)]
  pot = pot[-(1:2)]
  fcmax = fcmax[-(1:2)]
  teif = teif[-(1:2)]
  ip = ip[-(1:2)]
  
  term = cbind(cod,nome,pot,fcmax,teif,ip)
  rownames(inflex) = cod
  
  xx = list()
  
  xx[[1]] = term
  xx[[2]] = inflex
  
  return(xx)
  
}

lerConft = function()
{
  conft = read_lines("conft.dat")
  
  codConft = NULL
  sit = NULL
  subs = NULL
  for(i in 3:length(conft))
  {
    codConft[i-2]=as.numeric(paste(strsplit(conft[i],split = "")[[1]][1:5][-which(strsplit(conft[i],split = "")[[1]][1:5]==" ")],
                                   sep = "",collapse = ""))
    sit[i-2] = paste(strsplit(conft[i],split = "")[[1]][31:32],sep = "",collapse = "")
    
    subs[i-2] = paste(strsplit(conft[i],split = "")[[1]][25],sep = "",collapse = "")
  }
  
  conft = cbind(codConft,sit,subs)
  
  return(conft)
  
}
