library(tidyverse)
rm(list = ls())

data_t1 <- read.delim("C:\\Users\\rodri\\OneDrive\\Documentos\\Material de Estudo\\MC II\\Trabalho Final\\Trabalho Final\\data_t1.txt")

#glimpse(data_t1)

codigo_instancia <- c("I0", "I1", "I2", "I3", "I4", "I5");
nome_instancia <- c("ACAD", "OMET", "PARM", "PSOA", "WAMS", "WMET");
instancias <- tibble(codigo_instancia, nome_instancia)


#Testes de inferência com base no Igd

for (inst_ in instancias$codigo_instancia){
  instancia <- data_t1 %>% filter(inst ==  inst_)
  
  pv <- kruskal.test(gd~config, data=instancia)$p.value
  
  wt <- pairwise.wilcox.test(instancia$gd, instancia$config, p.adjust.method ="bonferroni", exact = F)$p.value
  
  print("Análise do critério de parada para o algoritmo genético")
  cat("\n")
  print(paste("INSTÂNCIA ->", instancias %>% filter(instancias$codigo_instancia == inst_) %>%
                select(nome_instancia)))
  

  print(paste("P-Value: ",pv))
  cat("\n")
  print("Teste de Wilcox usando a correção de Bonferroni")
  cat("\n")
  
  rownames <- names(wt[,1]);
  colnames <- names(wt[1,]);
  
  
  for (i in 1:5) 
  {
    for (j in 1:i)
    {
      if (!is.nan(wt) && wt[i,j] < 0.05)
      {
        print(paste("Há diferenças signicativas entre", rownames[i], "e", colnames[j], sep=" "));
      }
    }
  }
  
  cat("\n")
  print(wt)
  cat("\n")
  print("------------------------------------------------------------------------------------------------")
}
