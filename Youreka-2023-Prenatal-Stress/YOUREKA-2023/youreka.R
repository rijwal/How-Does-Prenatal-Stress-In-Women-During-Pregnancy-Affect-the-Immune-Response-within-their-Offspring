library(tidyverse)
library(ggplot2)
library(car)
library(ggsignif)
library(dplyr)

#null: The concentrations in cytokines are equal between stressed and non-stressed pregnancies
#alternative: The concentrations in cytokines are not equal between stressed and non-stressed pregnancies

setwd('/Users/kartikbudihal/YOUREKA-2023')

myData <- read.csv(file = 'source.csv')

#separating each cytokine into respective lists
IL.1b_notStressed = dplyr::filter(myData, Condition == "Nonstressed", Type != "Maternal Serum", IL.1b_a != 0, Cohort == 1)$IL.1b_a
IL.1b_prenatallyStressed = dplyr::filter(myData, Condition == "Prenatally stressed", Type != "Maternal Serum", IL.1b_a != 0, Cohort == 1)$IL.1b_a
IL.1b_total = dplyr::filter(myData, Type != "Maternal Serum", IL.1b_a != 0, Cohort == 1)$IL.1b_a
#for Graph
IL.1b_graph = dplyr::filter(myData, Type != "Maternal Serum", IL.1b_a != 0, Cohort == 1)

IL.2_notStressed = dplyr::filter(myData, Condition == "Nonstressed", Type == "Placenta", IL.2_a != 0)$IL.2_a
IL.2_prenatallyStressed = dplyr::filter(myData, Condition == "Prenatally stressed", Type == "Placenta", IL.2_a != 0)$IL.2_a
IL.2_total = dplyr::filter(myData, Type == "Placenta", IL.2_a != 0)$IL.2_a
#for graph
IL.2_graph = dplyr::filter(myData, Type == "Placenta", IL.2_a != 0)

IL.4_notStressed = dplyr::filter(myData, Condition == "Nonstressed", Type == "Maternal Serum", IL.4_a != 0, Cohort == 2)$IL.4_a
IL.4_prenatallyStressed = dplyr::filter(myData, Condition == "Prenatally stressed", Type == "Maternal Serum", IL.4_a != 0, Cohort == 2)$IL.4_a
IL.4_total = dplyr::filter(myData, Type == "Maternal Serum", IL.4_a != 0, Cohort == 2)$IL.4_a
#for graph
IL.4_graph = dplyr::filter(myData, Type == "Maternal Serum", IL.4_a != 0, Cohort == 2)

IL.6_notStressed = dplyr::filter(myData, Condition == "Nonstressed", Type == "Placenta", IL.6_b != 0, Cohort == 1, IL.6_b != "6.21E-05", IL.6_b != "6.03E-05")$IL.6_b
IL.6_prenatallyStressed = dplyr::filter(myData, Condition == "Prenatally stressed", Type == "Placenta", IL.6_b != 0, Cohort == 1, IL.6_b != "6.21E-05", IL.6_b != "6.03E-05")$IL.6_b
IL.6_total = dplyr::filter(myData, Type == "Placenta", IL.6_b != 0, Cohort == 1, IL.6_b != "6.21E-05", IL.6_b != "6.03E-05")$IL.6_b
#for graph
IL.6_graph = dplyr::filter(myData, Type == "Placenta", IL.6_b != 0, Cohort == 1, IL.6_b != "6.21E-05", IL.6_b != "6.03E-05", IL.6_b != "N/A")

IL.10_notStressed = dplyr::filter(myData, Condition == "Nonstressed", Type == "Placenta", IL.10_a != 0, Cohort == 1)$IL.10_a
IL.10_prenatallyStressed = dplyr::filter(myData, Condition == "Prenatally stressed", Type == "Placenta", IL.10_a != 0, Cohort == 1)$IL.10_a
IL.10_total = dplyr::filter(myData, Type == "Placenta", IL.10_a != 0, Cohort == 1)$IL.10_a
#for graph
IL.10_graph = dplyr::filter(myData, Type == "Placenta", IL.10_a != 0, Cohort == 1)

IL.17_notStressed = dplyr::filter(myData, Condition == "Nonstressed", Type == "Maternal Serum", IL.17_a != 0, IL.17_a != 142.62, Cohort == 2)$IL.17_a
IL.17_prenatallyStressed = dplyr::filter(myData, Condition == "Prenatally stressed", Type == "Maternal Serum", IL.17_a != 0, IL.17_a != 142.62, Cohort == 2)$IL.17_a
IL.17_total = dplyr::filter(myData, Type == "Maternal Serum", IL.17_a != 0, IL.17_a != 142.62, Cohort == 2)$IL.17_a
#for graph
IL.17_graph = dplyr::filter(myData, Type == "Maternal Serum", IL.17_a != 0, IL.17_a != 142.62, Cohort == 2)


#TESTING:

#variance testing

#IL.1b placenta a col cohort 1
wilcox.test(as.numeric(as.character(IL.1b_notStressed)), as.numeric(as.character(IL.1b_prenatallyStressed))) #0.02297
shapiro.test(IL.1b_total) #0.02982

#IL.2 placenta a col
wilcox.test(IL.2_notStressed, IL.2_prenatallyStressed) #0.009066
shapiro.test(IL.2_total) #0.000167

#IL.4 maternal serum a col cohort 2
wilcox.test(IL.4_notStressed, IL.4_prenatallyStressed) #0.4127
shapiro.test(IL.4_total) #0.3202

#IL.6 placenta b col cohort 1
wilcox.test(as.numeric(as.character(IL.6_notStressed)), as.numeric(as.character(IL.6_prenatallyStressed))) #0.03805
shapiro.test(as.numeric(as.character(IL.6_total))) #0.5309

#IL.10 placenta a col cohort 1
wilcox.test(IL.10_notStressed, IL.10_prenatallyStressed) #0.5333
shapiro.test(IL.10_total) #0.642

#IL.17 maternal serum a col cohort 2
wilcox.test(IL.17_notStressed, IL.17_prenatallyStressed) #0.02857
shapiro.test(IL.17_total) #0.01003

#MEAN TESTING

#IL-1b
mean(IL.1b_prenatallyStressed) #2.08625
mean(IL.1b_notStressed) #1.216
sd(IL.1b_prenatallyStressed) #0.998083
sd(IL.1b_notStressed) #0.7212366

#IL-2
mean(IL.2_prenatallyStressed) #4.3775
mean(IL.2_notStressed) #7.937692
sd(IL.2_prenatallyStressed) #1.707441
sd(IL.2_notStressed) #4.319433

#IL-4
mean(IL.4_prenatallyStressed) #73.3325
mean(IL.4_notStressed) #79.698
sd(IL.4_prenatallyStressed) #6.165362
sd(IL.4_notStressed) #12.59076

#IL-6
mean(as.numeric(as.character(IL.6_prenatallyStressed))) # 5.22e-05
mean(as.numeric(as.character(IL.6_notStressed))) #5.98e-05
sd(as.numeric(as.character(IL.6_prenatallyStressed))) #5.402592e-06
sd(as.numeric(as.character(IL.6_notStressed))) #1.556063e-05

#IL-10
mean(IL.10_prenatallyStressed) #0.925
mean(IL.10_notStressed) #2.4325
sd(IL.10_prenatallyStressed) #1.166726
sd(IL.10_notStressed) #1.702907

#IL-17
mean(IL.17_prenatallyStressed) #154.5825
mean(IL.17_notStressed) #287.2375
sd(IL.17_prenatallyStressed) #9.65879
sd(IL.17_notStressed) #102.0531


#GRAPHING

#IL.1b
ggplot(IL.1b_graph, aes(x = Condition, y = IL.1b_a)) + 
  geom_boxplot() +
  labs(x = "Stress Level", y = "Concentration (pg/mL)") +
  ggtitle("Interleukin 1b") +
  theme_bw() +
  theme(plot.title = element_text(size=18, face="bold", hjust = 0.5),
        axis.title = element_text(size=14, face="bold"),
        axis.text = element_text(size=12, face="bold"), 
        panel.grid = element_line(color = '#33333333')) +
  geom_signif(comparisons = list(c("Nonstressed", "Prenatally stressed")), map_signif_level=TRUE)


#IL.2
ggplot(IL.2_graph, aes(x = Condition, y = IL.2_a)) + 
  geom_boxplot() +
  labs(x = "Stress Level", y = "Concentration (pg/mL)") +
  ggtitle("Interleukin 2") +
  theme_bw() +
  theme(plot.title = element_text(size=18, face="bold", hjust = 0.5),
        axis.title = element_text(size=14, face="bold"),
        axis.text = element_text(size=12, face="bold"), 
        panel.grid = element_line(color = '#33333333')) +
  geom_signif(comparisons = list(c("Nonstressed", "Prenatally stressed")), map_signif_level=TRUE)


#IL.4
ggplot(IL.4_graph, aes(x = Condition, y = IL.4_a)) + 
  geom_boxplot() +
  labs(x = "Stress Level", y = "Concentration (pg/mL)") +
  ggtitle("Interleukin 4") +
  theme_bw() +
  theme(plot.title = element_text(size=18, face="bold", hjust = 0.5),
        axis.title = element_text(size=14, face="bold"),
        axis.text = element_text(size=12, face="bold"), 
        panel.grid = element_line(color = '#33333333')) +
  geom_signif(comparisons = list(c("Nonstressed", "Prenatally stressed")), map_signif_level=TRUE)


#IL.6
ggplot(IL.6_graph, aes(x = Condition, y = as.numeric(as.character(IL.6_b)))) + 
  geom_boxplot() +
  labs(x = "Stress Level", y = "Concentration (pg/ug total protein)") +
  ggtitle("Interleukin 6") +
  theme_bw() +
  theme(plot.title = element_text(size=18, face="bold", hjust = 0.5),
        axis.title = element_text(size=14, face="bold"),
        axis.text = element_text(size=12, face="bold"), 
        panel.grid = element_line(color = '#33333333')) +
  geom_signif(comparisons = list(c("Nonstressed", "Prenatally stressed")), map_signif_level=TRUE)


#IL.10
ggplot(IL.10_graph, aes(x = Condition, y = IL.10_a)) + 
  geom_boxplot() +
  labs(x = "Stress Level", y = "Concentration (pg/mL)") +
  ggtitle("Interleukin 10") +
  theme_bw() +
  theme(plot.title = element_text(size=18, face="bold", hjust = 0.5),
        axis.title = element_text(size=14, face="bold"),
        axis.text = element_text(size=12, face="bold"), 
        panel.grid = element_line(color = '#33333333')) +
  geom_signif(comparisons = list(c("Nonstressed", "Prenatally stressed")), map_signif_level=TRUE)


#IL.17
ggplot(IL.17_graph, aes(x = Condition, y = IL.17_a)) + 
  geom_boxplot() +
  labs(x = "Stress Level", y = "Concentration (pg/mL)") +
  ggtitle("Interleukin 17") +
  theme_bw() +
  theme(plot.title = element_text(size=18, face="bold", hjust = 0.5),
        axis.title = element_text(size=14, face="bold"),
        axis.text = element_text(size=12, face="bold"), 
        panel.grid = element_line(color = '#33333333')) +
  geom_signif(comparisons = list(c("Nonstressed", "Prenatally stressed")), map_signif_level=TRUE)










