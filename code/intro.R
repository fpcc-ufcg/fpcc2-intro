library(dplyr)
library(readr)
library(here)

# lendo arquivo de dados
salarios <- read_csv(here("data/salarios-ti-formatted.csv"))

# visão inicial
glimpse(salarios)
head(salarios)

# vendo cargos
salarios$Cargo
unique(salarios$Cargo)

# média e desvio padrão
mean(salarios$Salario.Bruto)
sd(salarios$Salario.Bruto)

# sumários de salários
summary(salarios$Salario.Bruto)

#manipular o frame, criar variaveis
salario  <- salarios$Salario.Bruto #cria variaveis vetor de colunas
salario

# cargo salário máximo
salarios %>% filter(Salario.Bruto == max(Salario.Bruto)) %>% select(Cargo)

library(ggplot2)
ggplot(salarios, aes(UF, Salario.Bruto)) + geom_histogram()

# frequência de pós ou certificação

# histogram - para variaveis continuas
ggplot(salarios, aes(Horas.Diarias)) + geom_histogram()
hist(salarios$Horas.Diarias)

# bin menores
ggplot(salarios, aes(Horas.Diarias)) + geom_histogram(binwidth = 0.5)

# gŕafico de barras

# lendo segundo arquivo de dados
lifeExp <- read.table(here("data/LifeExpTable.txt"))

# ler dados dos alunos
# renomear colunas
# explorar dados


# Qual o número de homens e mulheres
# Quais são as áreas de interesse?
# Qual a idade média da turma
# Qual o percentual da turma que faz doutorado
# Qual o percentual da turma que são da ufcg (entre homens e mulheres)
# Quem programa em R?