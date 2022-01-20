library(tidyverse)
library(dplyr)
rm(list = ls())

data_t5_t6 <- read.delim("C:\\Users\\rodri\\OneDrive\\Documentos\\Material de Estudo\\MC II\\Trabalho Final\\Trabalho Final\\data_t5-t6.txt")

#glimpse(data_t5_t6)

codigo_instancia <- c("I0", "I5", "I3", "I4", "I2", "I1");
nome_instancia <- c("ACAD", "WMET", "PSOA", "WAMS", "PARM", "OMET");
instancias <- tibble(codigo_instancia, nome_instancia)

quadro_qualidade <- tibble(Config= character(), NSGAII= character(), NSGAIIse = character())

for (inst_ in instancias$codigo_instancia){
  instancia <- data_t5_t6 %>% filter(inst ==  inst_)
  
  print(paste("INSTÂNCIA ->", instancias %>% filter(instancias$codigo_instancia == inst_) %>%
                select(nome_instancia)))
  
  c("\n")
  print("Teste de Wilcoxon-Mann-Whitney")
  c("\n")
  #Teste de Mann-Whitney
  
  print("Indicador Ic")
  c("\n")
  #Indicador Ic
  
  wt <- wilcox.test(instancia$best ~ instancia$config,
                    data = instancia,
                    exact = FALSE)$p.value
  
  if (wt < 0.05){
        print("Há diferenças signicativas!")
  }

  print(wt)
  cat("\n")
  
  print("Indicador Ihv")
  c("\n")
  #Indicador Ihv
  wt <- wilcox.test(instancia$hv ~ instancia$config,
                    data = instancia,
                    exact = FALSE)$p.value
  
  if (wt < 0.05){
    print("Há diferenças signicativas!")
  }
  
  print(wt)
  cat("\n")
  
  print("Indicador Igd")
  c("\n")
  #Indicador Igd
  wt <- wilcox.test(instancia$gd ~ instancia$config,
                    data = instancia,
                    exact = FALSE)$p.value
  
  if (wt < 0.05){
    print("Há diferenças signicativas!")
  }
  
  print(wt)
  cat("\n")
  
  print("------------------------------------------------------------------------------------------------")
  
  #Criando Tabela
  
  #NSGAII
  Ic <- instancia %>% 
    filter(instancia$config ==  "nsga150k2x") %>% 
    select(best) * 100
  Ihv <- instancia %>% filter(instancia$config ==  "nsga150k2x") %>% 
    select(hv)
  Igd <- instancia %>% filter(instancia$config ==  "nsga150k2x") %>% 
    select(gd) * 100
  
  NSGAII_meanIc <- Ic %>% summarize(round(mean(Ic$best), digits = 4))
  NSGAII_meanIhv <- Ihv %>% summarize(round(mean(Ihv$hv), digits = 4))
  NSGAII_meanIgd <- Igd %>% summarize(round(mean(Igd$gd), digits = 4))
  
  NSGAII_sdIc <- Ic %>% summarize(round(sd(Ic$best), digits = 4))
  NSGAII_sdIhv <- Ihv %>% summarize(round(sd(Ihv$hv), digits = 4))
  NSGAII_sdIgd <- Igd %>% summarize(round(sd(Igd$gd), digits = 4))
  
  #NSGAIIse
  Ic <- instancia %>% 
    filter(instancia$config ==  "nsga150k2xse") %>% 
    select(best) * 100
  Ihv <- instancia %>% filter(instancia$config ==  "nsga150k2xse") %>% 
    select(hv)
  Igd <- instancia %>% filter(instancia$config ==  "nsga150k2xse") %>% 
    select(gd) * 100
  
  NSGAIIse_meanIc <- Ic %>% summarize(round(mean(Ic$best), digits = 4))
  NSGAIIse_meanIhv <- Ihv %>% summarize(round(mean(Ihv$hv), digits = 4))
  NSGAIIse_meanIgd <- Igd %>% summarize(round(mean(Igd$gd), digits = 4))
  
  NSGAIIse_sdIc <- Ic %>% summarize(round(sd(Ic$best), digits = 4))
  NSGAIIse_sdIhv <- Ihv %>% summarize(round(sd(Ihv$hv), digits = 4))
  NSGAIIse_sdIgd <- Igd %>% summarize(round(sd(Igd$gd), digits = 4))
  
  
  #Adionando valores de Ic
  quadro_qualidade %<>% summarize(Config = paste(instancias %>% filter(instancias$codigo_instancia == inst_) %>%
                                                   select(nome_instancia), "- Ic"),
                                  NSGAII = paste(NSGAII_meanIc,"+-",NSGAII_sdIc), 
                                  NSGAIIse = paste(NSGAIIse_meanIc,"+-",NSGAIIse_sdIc)) %>%
    bind_rows(quadro_qualidade , .)
  
  #Adcionando valores de Ihv
  quadro_qualidade %<>% summarize(Config = paste(instancias %>% filter(instancias$codigo_instancia == inst_) %>%
                                                   select(nome_instancia), "- Ihv"),
                                  NSGAII = paste(NSGAII_meanIhv,"+-",NSGAII_sdIhv), 
                                  NSGAIIse = paste(NSGAIIse_meanIhv,"+-",NSGAIIse_sdIhv)) %>%
    bind_rows(quadro_qualidade , .)
  
  #Adcionando valores de Igd
  quadro_qualidade %<>% summarize(Config = paste(instancias %>% filter(instancias$codigo_instancia == inst_) %>%
                                                   select(nome_instancia), "- Igd"),
                                  NSGAII = paste(NSGAII_meanIgd,"+-",NSGAII_sdIgd), 
                                  NSGAIIse = paste(NSGAIIse_meanIgd,"+-",NSGAIIse_sdIgd)) %>%
    bind_rows(quadro_qualidade , .)
  
  
  
}

print(quadro_qualidade)


