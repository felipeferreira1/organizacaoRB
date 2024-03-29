#Rotina para organizar sa�da do request builder
#Feito por: Felipe Simpl�cio Ferreira
#�ltima atualiza��o: 21/11/2020


#Definindo diret�rios a serem utilizados
getwd()
setwd("D:\\Documentos")

#Carregando pacotes que ser�o utilizados
library(readxl)
library(reshape)
library(data.table)
library(stringr)
library(dplyr)

arquivo_entrada = read.csv("entrada.csv", dec=".",header=F)
arquivo_entrada = colsplit(arquivo_entrada[,1], split = "\\|", names = c("1")) #Dividindo a �nica coluna em v�rias colunas, usando o separador "|"
decisor <- str_detect(arquivo_entrada[,1], "START SECURITY", negate = FALSE)
decisor <- data.frame(decisor = decisor)
decisor <- colSums(decisor)

### M�todo 1 ###
#Onde as securities est�o separadas por "start security" e "end security"
#Importa��o e tratamento de arquivo
#arquivo_entrada = read.csv("entrada.csv", dec=".",header=F)
#arquivo_entrada = colsplit(arquivo_entrada[,1], split = "\\|", names = c("1")) #Dividindo a �nica coluna em v�rias colunas, usando o separador "|"
if (decisor != 0){
  arquivo_entrada = arquivo_entrada[,-4] #Colunas a serem tiradas podem ser modificadas de acordo com a formata��o do arquivo (para deixar a coluna que tem o comando "start of data" que separa o come�o de cada s�rie, uma coluna de data e outra com o dado, nessa ordem)
  arquivo_entrada = t(arquivo_entrada) #Transpondo os arquivos, para facilitar a separa��o
  marcadores_start = which(arquivo_entrada[1,] %in% c("START SECURITY"))
  marcadores_end = which(arquivo_entrada[1,] %in% c("END SECURITY"))
  marcadores = marcadores_start #Definindo onde cada bloco de dados (s�rie) ser� coletada
  marcadores[length(marcadores_start)+1] = marcadores_end[length(marcadores_end)] + 1
  
  #Separa��o de cada s�rie
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
}

if (decisor != 0){
  for(j in 2:dim(juntos)[2]){
    juntos[,j] <- as.numeric(unlist(juntos[,j]))
    print(j)
    }
  juntos[,1] <- as.Date(juntos[,1], "%d/%m/%y")
  juntos <- arrange(juntos, data)
}

### M�todo 2 ###
#Onde as securities n�o est�o separadas por "start security" e "end security", mas temos que usar "start of data" e "end of data" para separar a parte com os dados
#Importa��o e tratamento de arquivo
#arquivo_entrada = read.csv("entrada.csv", dec=".",header=F)
#arquivo_entrada = colsplit(arquivo_entrada[,1], split = "\\|", names = c("1")) #Dividindo a �nica coluna em v�rias colunas, usando o separador "|"
if (decisor == 0){
  marcadores_start = which(arquivo_entrada[,1] %in% c("START-OF-DATA")) + 1 
  marcadores_end = which(arquivo_entrada[,1] %in% c("END-OF-DATA")) - 1
  dados <- data.table(arquivo_entrada)
  dados <- dados[c(marcadores_start:marcadores_end),]
  dados <- dados[,c(-2,-3)]
  
  dados1_melt = melt(dados, id.vars = "X1", measure.vars = "NA..2")
  dados1_melt= dados1_melt[,-2]
  dados2_melt = melt(dados, id.vars = "X1", measure.vars = "NA..3")
  dados2_melt = dados2_melt[,-2]
  dados_melt = cbind(dados1_melt, dados2_melt)
  dados_melt = dados_melt[,-3]
  colnames(dados_melt) <- c("Nomes", "Datas", "Valores")
  juntos = dcast(dados_melt, Nomes~Datas)
  juntos = t(juntos)
  colnames(juntos) <- juntos[1,]
  juntos <- juntos[-c(1:2),]
  juntos <- data.frame(data = row.names(juntos), juntos)
  for(j in 2:dim(juntos)[2]){
    juntos[,j] <- as.numeric(unlist(juntos[,j]))
  }
  juntos[,1] <- as.Date(juntos[,1], "%d/%m/%Y")
  juntos <- arrange(juntos, data)
}

#Exporta��o de dados
write.csv2(juntos, "Dados_RB.csv", row.names = F)

