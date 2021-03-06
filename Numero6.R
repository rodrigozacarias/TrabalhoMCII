library(tidyverse)

rm(list = ls())

# Vargha & Delaney's A12
vargha.delaney <- function(r1, r2) {
  m <- length(r1);
  n <- length(r2);
  return ((sum(rank(c(r1, r2))[seq_along(r1)]) / m - (m + 1) / 2) / n);
}

data_t5t6 <- read.delim("C:\\Users\\rodri\\OneDrive\\Documentos\\Material de Estudo\\MC II\\Trabalho Final\\Trabalho Final\\data_t5-t6.txt")

glimpse(data_t5t6)

codigo_instancia <- c("I0", "I1", "I2", "I3", "I4", "I5");
nome_instancia <- c("ACAD", "OMET", "PARM", "PSOA", "WAMS", "WMET");
instancias <- tibble(codigo_instancia, nome_instancia)

resultado <- tibble(Instancia_= character(), Abordagem= character(), IC= character(), IHV= character(), IGD= character(), ISP= character())

for (inst_ in instancias$codigo_instancia){
  instancia <- data_t5t6 %>% filter(inst ==  inst_ & config	== 'nsga150k2x')
  NSGASE <- data_t5t6 %>% filter(inst ==  inst_ & config	== 'nsga150k2xse')
  
  #aplicação do vargha.delayne para cada abordagem - BEST (IC)
  es_nsgase_best <- vargha.delaney (instancia$best,NSGASE$best)  
  
  #aplicação do vargha.delayne para cada abordagem - IHV
  es_nsgase_hv <- vargha.delaney (instancia$hv,NSGASE$hv)  
  
  #aplicação do vargha.delayne para cada abordagem - IGD
  es_nsgase_gd <- vargha.delaney (instancia$gd,NSGASE$gd)  
  
  #aplicação do vargha.delayne para cada abordagem - ISP
  es_nsgase_sp <- vargha.delaney (instancia$spr,NSGASE$spr)  
  
  
  ic <- c(es_nsgase_best)
  ihv <- c(es_nsgase_hv)
  igd <- c(es_nsgase_gd)
  isp <- c(es_nsgase_sp)
  
  abordagens <- c("NSGASE")
  
  instancia_atual <- instancias %>% filter(instancias$codigo_instancia == inst_) %>%
    select(nome_instancia)
  #Adicionando valores no resultado
  resultado %<>% summarize(Instancia_ = paste(instancia_atual),
                           Abordagem = paste(abordagens), 
                           IC = toString(paste(format(ic*100),"%")), 
                           IHV = toString(paste(format(ihv*100),"%")), 
                           IGD = toString(paste(format((1-igd)*100),"%")),
                           ISP = toString(paste(format((1-isp)*10000),"%"))) %>%
    bind_rows(resultado , .)
  
}

print (resultado)
