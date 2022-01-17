library(tidyverse)

rm(list = ls())

# Vargha & Delaney's A12
vargha.delaney <- function(r1, r2) {
  m <- length(r1);
  n <- length(r2);
  return ((sum(rank(c(r1, r2))[seq_along(r1)]) / m - (m + 1) / 2) / n);
}

data_t3t4 <- read.delim("C:\\Users\\rodri\\OneDrive\\Documentos\\Material de Estudo\\MC II\\Trabalho Final\\Trabalho Final\\data_t3-t4.txt")

glimpse(data_t3t4)

codigo_instancia <- c("I0", "I1", "I2", "I3", "I4", "I5");
nome_instancia <- c("ACAD", "OMET", "PARM", "PSOA", "WAMS", "WMET");
instancias <- tibble(codigo_instancia, nome_instancia)


for (inst_ in instancias$codigo_instancia){
  instancia <- data_t3t4 %>% filter(inst ==  inst_ & config	== 'nsga150k2x')
  MAR <- data_t3t4 %>% filter(inst ==  inst_ & config	== 'MAR')
  CPM <- data_t3t4 %>% filter(inst ==  inst_ & config	== 'CPM')
  SH <- data_t3t4 %>% filter(inst ==  inst_ & config	== 'SH')
  
  #aplicação do vargha.delayne para cada abordagem - BEST (IC)
  es_mar_best <- vargha.delaney (instancia$best,MAR$best)  
  es_sh_best <- vargha.delaney (instancia$best,SH$best)  
  es_cpm_best <- vargha.delaney (instancia$best,CPM$best)    
  
  #aplicação do vargha.delayne para cada abordagem - IHV
  es_mar_hv <- vargha.delaney (instancia$hv,MAR$hv)  
  es_sh_hv <- vargha.delaney (instancia$hv,SH$hv)  
  es_cpm_hv <- vargha.delaney (instancia$hv,CPM$hv)  
  
  #aplicação do vargha.delayne para cada abordagem - IGD
  es_mar_gd <- vargha.delaney (instancia$gd,MAR$gd)  
  es_sh_gd <- vargha.delaney (instancia$gd,SH$gd)  
  es_cpm_gd <- vargha.delaney (instancia$gd,CPM$gd)  
  
  abordagens <- c("MAR", "SH", "CPM");
  ic <- c(es_mar_best, es_sh_best, es_cpm_best)
  ihv <- c(es_mar_hv, es_sh_hv, es_cpm_hv)
  igd <- c(es_mar_gd, es_sh_gd, es_cpm_gd)
  
  instancia_atual <- instancias %>% filter(instancias$codigo_instancia == inst_) %>%
    select(nome_instancia)
  
  resultado <- tibble(instancia_atual, abordagens,IC = ic*100, IHV = ihv*100, IGD = (1-igd)*100)
  
  print (resultado)
}
