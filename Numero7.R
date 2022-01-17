library(tidyverse)
library(ggplot2)
library(dplyr)
library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(ggpubr)
library("gridExtra")

theme_set(theme_bw())

dividemile <- function(l) {
  # turn in to character string in scientific notation
  l <-l /1000
  parse(text=l)
} 

multiplyten <- function(l) {
  # turn in to character string in scientific notation
  l <-l *10
  parse(text=l)
} 

multiplyhundred <- function(l) {
  # turn in to character string in scientific notation
  l <-l *100
  parse(text=l)
} 

data_t7_CPM_error1_frontier_obj <- read.delim("C:\\Dados\\data_t7_CPM_error1_frontier_obj.txt");
data_t7_Margarine_error1_frontier_obj <- read.delim("C:\\Dados\\data_t7_Margarine_error1_frontier_obj.txt");
data_t7_nsga_150k_c50_2x_error1_frontier_obj <- read.delim("C:\\Dados\\data_t7_nsga_150k_c50_2x_error1_frontier_obj.txt");
data_t7_nsga_150k_c50_2x_noerror_frontier_obj <- read.delim("C:\\Dados\\data_t7_nsga_150k_c50_2x_noerror_frontier_obj.txt");
data_t7_SecondHalf_error1_frontier_obj <- read.delim("C:\\Dados\\data_t7_SecondHalf_error1_frontier_obj.txt");

data  <- bind_rows(data_t7_CPM_error1_frontier_obj,
                   data_t7_Margarine_error1_frontier_obj,
            data_t7_nsga_150k_c50_2x_error1_frontier_obj,
            data_t7_nsga_150k_c50_2x_noerror_frontier_obj,
            data_t7_SecondHalf_error1_frontier_obj);



data_ACAD <- filter(data, inst == "ACAD")

data_PARM <- filter(data, inst == "PARM")

ggplot_acad_1 <- ggplot(data_ACAD, aes(x=mks, y=cst)) +
  geom_point() +
  scale_y_continuous(labels=dividemile) +
  geom_smooth() + 
labs(title="ACAD", y="Cost(1000$)", x="Makespan(days)")

ggplot_acad_2 <- ggplot(data_ACAD, aes(x=noh, y=mks)) +
  geom_point() +
  scale_x_continuous(labels=multiplyten, limits = c(0, 40)) +
  geom_smooth() + 
  labs(title="ACAD", y="Makespan(days)", x="Overtime(hours)")

ggplot_acad_3 <- ggplot(data_ACAD, aes(x=noh, y=cst)) +
  geom_point() +
  scale_y_continuous(labels=dividemile) +
  scale_x_continuous(labels=multiplyten, limits = c(0, 40)) +
  geom_smooth() + 
  labs(title="ACAD", y="Cost(1000$)", x="Overtime(hours)")

ggplot_parm_1 <- ggplot(data_PARM, aes(x=mks, y=cst)) +
  geom_point() +
  scale_y_continuous(labels=dividemile) +
  scale_x_continuous(limits = c(300, 420)) +
  geom_smooth() + 
  labs(title="PARM", y="Cost(1000$)", x="Makespan(days)")

ggplot_parm_2 <- ggplot(data_PARM, aes(x=noh, y=mks)) +
  geom_point() +
  scale_x_continuous(labels=multiplyten,limits = c(0, 100), breaks = seq(0, 100, by=20)) +
  scale_y_continuous(limits = c(300, 420), breaks = seq(300, 420, by=40)) +
  geom_smooth() + 
  labs(title="PARM", y="Makespan(days)", x="Overtime(hours)")

ggplot_parm_3 <- ggplot(data_PARM, aes(x=noh, y=cst)) +
  geom_point() +
  scale_y_continuous(labels=dividemile, breaks = seq(210000, 250000, by=20000)) +  
  scale_x_continuous(labels=multiplyhundred, breaks = seq(0, 100, by=20)) +
  geom_smooth() + 
  labs(title="PARM", y="Cost(1000$)", x="Overtime(hours)")

figure <- grid.arrange(ggplot_acad_1, ggplot_acad_2, ggplot_acad_3, 
                       ggplot_parm_1, ggplot_parm_2, ggplot_parm_3,
                    ncol = 3, nrow = 2)
figure