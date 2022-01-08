library(tidyverse)
rm(list = ls())

data_t1 <- read.delim("C:\\Users\\rodri\\OneDrive\\Documentos\\Material de Estudo\\MC II\\Trabalho Final\\Trabalho Final\\data_t1.txt")

glimpse(data_t1)

codigo_instancia <- c("I0", "I1", "I2", "I3", "I4", "I5");
nome_instancia <- c("ACAD", "OMET", "PARM", "PSOA", "WAMS", "WMET");
instancias <- tibble(codigo_instancia, nome_instancia)


#Testes de inferência com base no Igd

for (inst_ in instancias$codigo_instancia){
  instancia <- data_t1 %>% filter(inst ==  inst_)
  
  pv <- kruskal.test(gd~config, data=instancia)$p.value
  
  wt <- pairwise.wilcox.test(instancia$gd, instancia$config, p.adjust.method ="bonferroni")$p.value
  
  print(paste("INSTÂNCIA ->", instancias %>% filter(instancias$codigo_instancia == inst_) %>%
                select(nome_instancia)))
  

  print(paste("P-Value: ",pv))
  
  print("Teste de Wilcox usando a correção de Bonferroni")
  
  print(wt)
  print("------------------------------------------------------------------------------------------------")
}
