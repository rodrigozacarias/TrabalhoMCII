library(tidyverse)
library(dplyr)
rm(list = ls())

data_t3_t4 <- read.delim("C:\\Users\\rodri\\OneDrive\\Documentos\\Material de Estudo\\MC II\\Trabalho Final\\Trabalho Final\\data_t3-t4.txt")

#glimpse(data_t3_t4)

codigo_instancia <- c("I0", "I5", "I3", "I4", "I2", "I1");
nome_instancia <- c("ACAD", "WMET", "PSOA", "WAMS", "PARM", "OMET");
instancias <- tibble(codigo_instancia, nome_instancia)

quadro_qualidade <- tibble(Config= character(), NSGAII= character(), MAR= character(), SH= character(), CPM= character())

for (inst_ in instancias$codigo_instancia){
  instancia <- data_t3_t4 %>% filter(inst ==  inst_)
  
  print(paste("INSTÂNCIA ->", instancias %>% filter(instancias$codigo_instancia == inst_) %>%
                select(nome_instancia)))
  
  c("\n")
  print("Teste de Wilcoxon-Mann-Whitney")
  c("\n")
  #Teste de Mann-Whitney
  
  print("Indicador Ic")
  c("\n")
  #Indicador Ic
  wt <- pairwise.wilcox.test(instancia$best, instancia$config, p.adjust.method ="bonferroni", exact=F)$p.value
  
  rownames <- names(wt[,1]);
  colnames <- names(wt[1,]);

  
  for (i in 1:3) 
  {
    for (j in 1:i)
    {
      if (!is.nan(wt) && wt[i,j] < 0.05)
      {
        print(paste("Há diferenças signicativas entre", rownames[i], "e", colnames[j], sep=" "));
      }
    }
  }
  
  print(wt)
  cat("\n")
  
  print("Indicador Ihv")
  c("\n")
  #Indicador Ic
  wt <- pairwise.wilcox.test(instancia$hv, instancia$config, p.adjust.method ="bonferroni", exact=F)$p.value
  
  rownames <- names(wt[,1]);
  colnames <- names(wt[1,]);
  
  
  for (i in 1:3) 
  {
    for (j in 1:i)
    {
      if (!is.nan(wt) && wt[i,j] < 0.05)
      {
        print(paste("Há diferenças signicativas entre", rownames[i], "e", colnames[j], sep=" "));
      }
    }
  }
  
  print(wt)
  cat("\n")
  
  print("Indicador Igd")
  c("\n")
  #Indicador Ic
  wt <- pairwise.wilcox.test(instancia$gd, instancia$config, p.adjust.method ="bonferroni", exact=F)$p.value
  
  rownames <- names(wt[,1]);
  colnames <- names(wt[1,]);
  
  
  for (i in 1:3) 
  {
    for (j in 1:i)
    {
      if (!is.nan(wt) && wt[i,j] < 0.05)
      {
        print(paste("Há diferenças signicativas entre", rownames[i], "e", colnames[j], sep=" "));
      }
    }
  }
  
  print(wt)
  cat("\n")
  
  print("------------------------------------------------------------------------------------------------")
  
  #Criando Tabela
  
  Ic <- instancia %>% 
    filter(instancia$config ==  "nsga150k2x") %>% 
    select(best)
  Ihv <- instancia %>% filter(instancia$config ==  "nsga150k2x") %>% 
    select(hv)
  Igd <- instancia %>% filter(instancia$config ==  "nsga150k2x") %>% 
    select(gd)

   NSGAII_meanIc <- Ic %>% summarize(round(mean(Ic$best), digits = 4))
   NSGAII_meanIhv <- Ihv %>% summarize(round(mean(Ihv$hv), digits = 4))
   NSGAII_meanIgd <- Igd %>% summarize(round(mean(Igd$gd), digits = 4))
   
   NSGAII_sdIc <- Ic %>% summarize(round(sd(Ic$best), digits = 4))
   NSGAII_sdIhv <- Ihv %>% summarize(round(sd(Ihv$hv), digits = 4))
   NSGAII_sdIgd <- Igd %>% summarize(round(sd(Igd$gd), digits = 4))
   
   MAR_Ic <- instancia %>% 
     filter(instancia$config ==  "MAR") %>% 
     select(best)
   MAR_Ihv <- instancia %>% filter(instancia$config ==  "MAR") %>% 
     select(hv)
   MAR_Igd <- instancia %>% filter(instancia$config ==  "MAR") %>% 
     select(gd)
   
   SH_Ic <- instancia %>% 
     filter(instancia$config ==  "SH") %>% 
     select(best)
   SH_Ihv <- instancia %>% filter(instancia$config ==  "SH") %>% 
     select(hv)
   SH_Igd <- instancia %>% filter(instancia$config ==  "SH") %>% 
     select(gd)
   
  CPM_Ic <- instancia %>% 
     filter(instancia$config ==  "CPM") %>% 
     select(best)
  CPM_Ihv <- instancia %>% filter(instancia$config ==  "CPM") %>% 
     select(hv)
  CPM_Igd <- instancia %>% filter(instancia$config ==  "CPM") %>% 
     select(gd)
  


  #Adionando valores de Ic
  quadro_qualidade %<>% summarize(Config = paste(instancias %>% filter(instancias$codigo_instancia == inst_) %>%
                                                select(nome_instancia), "- Ic"),
                               NSGAII = paste(NSGAII_meanIc,"+-",NSGAII_sdIc), 
                               MAR = toString(format(round(MAR_Ic, 4))), 
                               SH = toString(format(round(SH_Ic, 4))), 
                               CPM = toString(format(round(CPM_Ic, 4)))) %>%
    bind_rows(quadro_qualidade , .)
  
  #Adcionando valores de Ihv
  quadro_qualidade %<>% summarize(Config = paste(instancias %>% filter(instancias$codigo_instancia == inst_) %>%
                                                   select(nome_instancia), "- Ihv"),
                                  NSGAII = paste(NSGAII_meanIhv,"+-",NSGAII_sdIhv), 
                                  MAR = toString(format(round(MAR_Ihv, 4))), 
                                  SH = toString(format(round(SH_Ihv, 4))), 
                                  CPM = toString(format(round(CPM_Ihv, 4)))) %>%
    bind_rows(quadro_qualidade , .)
  
  #Adcionando valores de Igd
  quadro_qualidade %<>% summarize(Config = paste(instancias %>% filter(instancias$codigo_instancia == inst_) %>%
                                                   select(nome_instancia), "- Igd"),
                                  NSGAII = paste(NSGAII_meanIgd,"+-",NSGAII_sdIgd), 
                                  MAR = toString(format(round(MAR_Igd, 4))), 
                                  SH = toString(format(round(SH_Igd, 4))), 
                                  CPM = toString(format(round(CPM_Igd, 5)))) %>%
    bind_rows(quadro_qualidade , .)

  
  
}

print(quadro_qualidade)


