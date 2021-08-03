# Projeto com feedback 1 - Detecção de Fraudes no Tráfego de Cliques 
# em Propagandas de Aplicações Mobile

# Autor: João Augusto de Oliveira Jorge

# Breve descrição sobre a empresa: A TalkingData, a maior plataforma de Big 
# Data independente da China, cobre mais de 70% dos dispositivos móveis ativos em 
# todo o país. Eles lidam com 3 bilhões de cliques por dia, dos quais 90% são 
# potencialmente fraudulentos.

# Objetivo geral: medir a jornada do clique de um  usuário em todo o portfólio
# e sinalizar endereços IP que produzem muitos cliques, 
# mas nunca acabam instalando aplicativos. Com essas informações, eles criaram 
# uma lista negra de IPs e uma lista negra de dispositivos.

# Objetivo especifio: criar um algoritmo que possa prever se um usuário fará o 
# download de um aplicativo depois de clicar em um anúncio de aplicativo para 
# dispositivos móveis. 

# Objetivo do Script: Preparar e realizar a limpeza inicial dos dados


########################################################################

# Definindo diretorio de trabalho
setwd("C:/Users/JoaoA/OneDrive - SENAC - SP/CoisasdoJoao/Curso/Data Science/Cursos/01_Big_Data_Analytics_Com_R/Mini-Projetos/Projeto1")
getwd()

# Carregando dataset de amostra de treino
# Devido a problemas com espaço, não consigo utilizar os dados de treino ofical
library(data.table)
df = fread('dataset/train_sample.csv')

# Verificando informações básicas
class(df) # Classe do dataframe
dim(df) # Numero de linhas x Colunas
View(df) # Vizualizando Dataset
str(df) # Verificando classes

# Realizando a transformação dos dados
df$ip = as.character(df$ip)
df$device = as.factor(df$device)
df$os = as.factor(df$os)
df$is_attributed = as.factor(df$is_attributed)

# Adicionando uma nova coluna de quanto tempo demorou para o cliente
# Realizar a instalação do arquivo
require(dplyr)
df = as.data.frame(df) # Transformando os dados para formato Dataset

# Criando uma nova coluna de tempo de instalação do usuario
df = df %>%
    mutate(tempo_instalacao = attributed_time - click_time )

df$tempo_instalacao = as.integer(df$tempo_instalacao)

# Criando uma nova coluna com o dia da semana de acesso
df = df%>%
  mutate(dia_semana = weekdays(click_time))
df$dia_semana = as.factor(df$dia_semana)

# Vizualizando Dataframe
str(df)
df$app = as.character(df$app)
summary(df)

# Vizualizando resultado final:
View(df)

###################################################################
# Objetivo especifico do script: Analise Exploratoria dos Dados
library(ggplot2)

# Verificando se existe valores ausentes
sapply(df, function(x){
  sum(is.na(x))
})
# Somente as variaveis attributed_time e tempo_instalacao possuem valores ausentes

# Analisando variavel target:
ggplot(df, aes(x = is_attributed, fill = is_attributed)) +
  geom_bar(stat = 'count') +
  ggtitle('Gráfico de Distribuição entre a variavel target')

prop.table(table(df$is_attributed)) * 100

# Foi detectado uma grande desproporção entre os dados, sendo necessario o ajuste.

# Verificando em quais dias da semana acontecem mais a instalação
instalado = filter(df, is_attributed == 1)
colnames = c('app', 'device', 'os', 'channel', 'dia_semana')

lapply(colnames, function(x){
  if(is.factor(instalado[,x])) {
    ggplot(instalado, aes_string(x)) +
      geom_bar(stat = 'count') + 
      facet_grid(. ~ is_attributed) + 
      ggtitle(paste("Total de links instalados por",x))}})

# Verificando distribuição para pessoas instalar
hist(instalado$tempo_instalacao)
boxplot(instalado$tempo_instalacao)

# Para tentar remover um pouco dos Outliers estarei removendo os dados
# Acima do terceiro quartil
summary(instalado$tempo_instalacao)
x = instalado%>%
  filter(tempo_instalacao <= 500) %>%
  select(tempo_instalacao)

# Ou seja, a Maior parte das pessoas realiza a rapidamente a instalação
hist(x$tempo_instalacao)
boxplot(x$tempo_instalacao)

# Criando uma lista com a quantidade de cliques de cada ip
# Que não instalou
apoio = df %>%
  filter(is_attributed == 0) %>%
  group_by(ip) %>%
  summarise(cliques = length(ip)) %>%
  arrange(desc(cliques))

# Realizando uma analise
summary(apoio$cliques)
hist(apoio$cliques)
boxplot(apoio$cliques)
table(apoio$cliques)

# Criando uma lista com classificação de risco

apoio$risco = lapply(apoio$cliques, function(x){
  ifelse(x < 21, 'Risco Baixo', ifelse(x < 51, 'Risco Medio', 'Risco Alto'))
})
apoio$risco = unlist(apoio$risco)

# Vizualizando resultado final 
View(apoio)
apoio$risco = as.factor(apoio$risco)

# Construindo gráfico
ggplot(filter(apoio, risco != "Risco Baixo"), aes(x = risco, fill = (risco))) +
  geom_bar(stat = 'count') +
  ggtitle('Classificação de Risco Média/ Alta') +
  xlab('Nivel de Risco') +
  ylab('Quantidade')

# Vizualizando o comportamento de cada grupo
apoio %>%
  group_by(risco) %>%
  summarise(media = mean(cliques),
            soma = sum(cliques,
                       desvPadrao = sd(cliques))) %>%
  View


# Coletando a lista negra de ip
blacklist = apoio %>%
  filter(risco == 'Risco Alto') %>%
  select(ip, risco)

write.csv(blacklist, 'blacklist.csv')
#################################################################
# Verificando as melhores variaveis
library(randomForest)

# Preparando dados para o modelo de teste
datarascunho = df
datarascunho$attributed_time = NULL
datarascunho$tempo_instalacao = NULL
datarascunho$device = as.character(datarascunho$device)
datarascunho$os = as.character(datarascunho$os)
datarascunho$channel = as.character(datarascunho$channel)

# Criando o modelo teste
teste1 = randomForest(is_attributed~., importance = TRUE, data = datarascunho)
teste2 = randomForest(is_attributed~. -ip, importance = TRUE, data = datarascunho)

# Verificando as melhores variaveis nos dois cenarios
varImpPlot(teste1)
varImpPlot(teste2)

# Classificação Variaveis:
# Boas: App, Channel
# Médias: device, os, click_time
# Dúvidosas: dia_semana
# Ruim: ip 


#####################################################
# Preparando os Dados para o primeiro modelo

# Criando o Datset Final
dadosfinal = datarascunho
dadosfinal$ip = NULL
dadosfinal$click_time = NULL

# Criando dataset de treino e teste
library(caTools)
separador = sample.split(dadosfinal$app, 0.7)
treino = dadosfinal[separador, ]
teste = dadosfinal[separador== FALSE, ]

# Balanceando as classes no Dataset
library(ROSE)

# Estou transfomando as variaveis em categoricas para conseguir realizar o balanceamento de classe
treino$app = as.factor(treino$app)
treino$device = as.factor(treino$device)
treino$os =  as.factor(treino$device)
treino$channel =  as.factor(treino$channel)

table(treino$is_attributed) # Antes
treino2 = ROSE(is_attributed~ .,data = as.data.frame(treino))
treino2 = treino2$data
table(treino2$is_attributed) # Depois

# Mesmo procedimento para os dados de teste
teste$app = as.factor(teste$app)
teste$device = as.factor(teste$device)
teste$os =  as.factor(teste$device)
teste$channel =  as.factor(teste$channel)

table(teste$is_attributed) # Antes
teste2 = ROSE(is_attributed~ .,data = as.data.frame(teste))
teste2 = teste2$data
table(teste2$is_attributed) # Depois

# Coletando respostas:
respostas = teste2$is_attributed
teste2$is_attributed = NULL

####################################################
# Construindo o primeiro modelo
library(caret)

# Arrumando variaveis
treino2$device = as.character(treino2$device)
treino2$os = as.character(treino2$os)
treino2$channel = as.character(treino2$channel)
treino2$app = as.character(treino2$app)

teste2$device = as.character(teste2$device)
teste2$os = as.character(teste2$os)
teste2$channel = as.character(teste2$channel)
teste2$app = as.character(teste2$app)

#######################################
# Construindo modelo1
model1 = randomForest(is_attributed~app+ channel+device+ 
                        os + dia_semana, data = treino2)

# Realizando previsão 1
previsao1 = predict(model1, teste2)
confusionMatrix(respostas, previsao1)

# Primeiro modelo obteve precisão de 87%



#################################
# Construindo modelo2
model2 = randomForest(is_attributed~app+ channel+device+ 
                        os, data = treino2)

# Realizando previsão 2
previsao2 = predict(model2, teste2)
confusionMatrix(respostas, previsao2)

# Segundo modelo obteve precisão de 89%


###################################
# Verificando Curva ROC
roc.curve(respostas, previsao2)


# Modelo ok e com ótimo desempenho.