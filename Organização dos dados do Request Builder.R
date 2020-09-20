#Rotina para organizar saída do request builder
#Feito por: Felipe Simplício Ferreira
#última atualização: 20/09/2020


#Definindo diretórios a serem utilizados
getwd()
setwd("C:/Users/User/Documents")

#Carregando pacotes que serão utilizados
library(readxl)
library(reshape)

#Importação e tratamento de arquivo
arquivo_entrada = read.csv("cds2.csv", dec=".",header=F)
arquivo_entrada = colsplit(arquivo_entrada$V1, split = "\\|", names = c("1")) #Dividindo a única coluna em várias colunas, usando o separador "|"
arquivo_entrada = arquivo_entrada[,-4] #Colunas a serem tiradas podem ser modificadas de acordo com a formatação do arquivo (para deixar a coluna que tem o comando "start of data" que separa o começo de cada série, uma coluna de data e outra com o dado, nessa ordem)
arquivo_entrada = t(arquivo_entrada) #Transpondo os arquivos, para facilitar a separação
marcadores_start = which(arquivo_entrada[1,] %in% c("START SECURITY"))
marcadores_end = which(arquivo_entrada[1,] %in% c("END SECURITY"))
marcadores = marcadores_start #Definindo onde cada bloco de dados (série) será coletada
marcadores[length(marcadores_start)+1] = marcadores_end[length(marcadores_end)] + 1

#Separação de cada série
for (j in 1:length(marcadores)){''
  for (i in 1:dim(arquivo_entrada)[2]){
    dados = arquivo_entrada[,i]
    if(i < marcadores[j])
      rm(dados)
    if(i == marcadores[j])
      base = dados
    if(i > marcadores[j])
      base = rbind(base, dados)
    if(i > (marcadores[j+1]-3))
      break
  }
  if(is.null(ncol(base)) == TRUE)
    next
  base = base[-1,]
  base = base[,-1]
  nome_arquivo = as.character(arquivo_entrada[2,marcadores[j]])
  nome_coluna = paste(nome_arquivo, as.character(arquivo_entrada[3,marcadores[j]]), sep = " - ")
  colnames(base) = c('data', nome_coluna)
  if(j==1)
    juntos = base
  else
    juntos = merge(juntos, base, by = "data", all = T)
  assign(nome_arquivo, base)
}

#Exportação de dados
write.csv2(juntos, "Dados_RB.csv", row.names = F)