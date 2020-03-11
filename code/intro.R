library(dplyr)
library(readr)
library(here)


# Dados de salários em TI -------------------------------------------------

# lendo arquivo de dados
salarios <- read_csv(here("data/salarios-ti-formatted.csv"))

# visão inicial
glimpse(salarios)
head(salarios)

# dez primeiros cargos do vetor de cargos
salarios$Cargo[1:10]

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
salarios %>% filter(Salario.Bruto == max(Salario.Bruto)) %>% select(Cargo, Cidade, Iniciativa.Privada.ou.Concursado)

# gráfico de barras da média salarial bruta
library(ggplot2)
salarios %>% group_by(UF) %>% summarise(val = mean(Salario.Bruto)) %>% 
ggplot(aes(UF, val)) + geom_bar(stat = "identity")

# histogram - para variaveis continuas
ggplot(salarios, aes(Horas.Diarias)) + geom_histogram()
hist(salarios$Horas.Diarias)

# bin menores
ggplot(salarios, aes(Horas.Diarias)) + geom_histogram(binwidth = 0.5)

# dados da turma de fpcc2 2020
fpcc2 <- read_csv("data/dados-fpcc2.csv")

# renomeando colunas
colnames(fpcc2) <- c("data", "idade", "sexo", "curso", "area", "interesse", 
                     "progr", "origem", "uf", "irmaos", "altura")

# biblioteca para escala percentual em gráfico
library(scales)

# percentual de programadores R na turma
fpcc2 %>% mutate(total = n()) %>% group_by(progr) %>% 
  summarise(perc = n() / first(total)) %>% 
  ggplot(aes(progr, perc)) + 
  geom_bar(stat = "identity") + 
  theme_bw(base_size = 15) + 
  xlab("Prigrama em R?") + 
  ylab(NULL) + 
  scale_y_continuous(labels = percent)

# contagem de instituições de origem em barras
ggplot(fpcc2, aes(origem)) + geom_bar() + 
  theme_bw(base_size = 15)

# percentual de homens e mulheres por turma
fpcc2 %>% mutate(total = n()) %>% group_by(sexo) %>% 
  summarise(perc = n() / first(total)) %>% 
  ggplot(aes(sexo, perc)) + 
  geom_bar(stat = "identity") + 
  theme_bw(base_size = 15) + 
  xlab(NULL) + 
  ylab(NULL) + 
  scale_y_continuous(labels = percent)

# média do número de irmãos
fpcc2 %>% filter(!is.na(irmaos)) %>% 
  summarise(media_irmaos = mean(as.numeric(irmaos)))

# média do número de irmãos por grupo de homem e mulher
fpcc2 %>% filter(!is.na(irmaos)) %>% group_by(sexo) %>% 
  summarise(media_irmaos = mean(as.numeric(irmaos)))

# Outras perguntas --------------------------------------------------------

# Quais são as áreas de interesse?
# Qual a idade média da turma?
# Qual o percentual da turma que faz doutorado?
# Qual o percentual da turma que são da ufcg (entre homens e mulheres)?
