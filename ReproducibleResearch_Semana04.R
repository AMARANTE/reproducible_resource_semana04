##DATA SCIENCE
##Módulo : Reproducible Resource
##Semana 4 - Tarefa avaliada por colega : Projeto-01
##Questão-01
##Geraldo Barbosa do Amarante
##-----------------------------------
##Carga das bibliotecas  ------------
##-----------------------------------
install.packages("dplyr")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("sqldf")
install.packages("knitr")
##----------------------------------
library(dplyr)
library(lubridate)
library(ggplot2)
library(sqldf)
library(knitr)
##-------------------------------------
## Definição da pasta de trabalho -----
##-------------------------------------
setwd('C:/Amarante/ReproducibleResearch/ArquivoDeDados')
## Leitura do arquivo
arquivo <- read.csv("./StormData.csv", sep=",", header=T)
head(arquivo)
str(arquivo)
-----------------------------------------------------------------
##gráfico teste
hist(year(as.Date(arquivo$BGN_DATE, '%m/%d/%Y')), col = "blue", main="Major storms and weather events in the United States",xlab = "Year",ylab="Events",border="green")
----------------------------------------------------------------
##gráfico teste
fatalidades <- aggregate(FATALITIES ~ EVTYPE, data=arquivo, sum)
fatalidades <- fatalidades[order(-fatalidades$FATALITIES), ][1:10, ]
fatalidades$EVTYPE <- factor(fatalidades$EVTYPE, levels = fatalidades$EVTYPE)

ggplot(fatalidades, aes(x = EVTYPE, y = FATALITIES)) +geom_bar(stat = "identity", fill = "green", las = 3) + 
theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
xlab("Event Type") + ylab("Fatalities") + ggtitle("Number of fatalities by top 10 Weather Events")
##--------------------------------------------------------------------------------------------------------------------------------------------
## 1-Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?
##--------------------------------------------------------------------------------------------------------------------------------------------
## Montagem do Comando ##
##---------------------##
comando_sql <- "select EVTYPE as Event,sum(FATALITIES) as Fatalites,sum(INJURIES) as lesoes from arquivo
        group by EVTYPE order by sum(FATALITIES) desc,sum(INJURIES) desc limit 10"
# Execução do comando
resultado_sql01 <-sqldf::sqldf(comando_sql)
#Gráfico
ggplot(resultado_sql01, aes(x = Event, y = Fatalites)) + geom_bar(stat = "identity", fill = "blue") + 
       theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
       xlab("Event") + ylab("Fatalities") + ggtitle("The greater 10 Ocorrency of fatalities by Events")

##--------------------------------------------------------------------------------------------------------------------------------------------
## 2 - Across the United States, which types of events have the greatest economic consequences?
##--------------------------------------------------------------------------------------------------------------------------------------------
## Montagem do Comando ##
##---------------------##
comando_sql <- "select EVTYPE as Event,sum(PROPDMG)/1000 as Property_Damage from arquivo  group by EVTYPE order by sum(PROPDMG) desc limit 10"
# Execução do comando
resultado_sql02 <-sqldf(comando_sql)
#Gráfico
ggplot(resultado_sql02, aes(x = Event, y = Property_Damage)) + geom_bar(stat = "identity", fill = "green") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  xlab("Event") + ylab("Property Damage X 1000") + ggtitle("The greater 10 Ocorrency of Property Damage by Events")




