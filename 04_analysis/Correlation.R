library(corrplot)
library(lme4)
library(lmerTest)
library(data.table)
library(ggplot2)
library(patchwork)
library(plotly)
library(dplyr)
library(hms)
library(ggpubr)
library(boot)
library(corrplot)
library(gridExtra)
library(grid)
library(png)
library("PerformanceAnalytics")

CORR = fread("data.csv")

CORR = CORR[ID != 103]

is_outlier = function(x, coef = 1) {
  Q1 = quantile(x, 0.25, na.rm = TRUE)
  Q3 = quantile(x, 0.75, na.rm = TRUE)
  IQR = Q3 - Q1
  (x < (Q1 - coef * IQR)) | (x > (Q3 + coef * IQR))
}

CORR = CORR[!is_outlier(CORR$KSS, coef = 1.5) & !is.na(CORR$KSS) &
            !is_outlier(CORR$Mental.demand, coef = 1.5) & !is.na(CORR$Mental.demand) &
            !is_outlier(CORR$Temporal.demand, coef = 1.5) & !is.na(CORR$Temporal.demand) &
            !is_outlier(CORR$Success, coef = 1.5) & !is.na(CORR$Success) &
            !is_outlier(CORR$MAS.comfort.original, coef = 1.5) & !is.na(CORR$MAS.comfort.original) &
            !is_outlier(CORR$MAS.alertness.original, coef = 1.5) & !is.na(CORR$MAS.alertness.original) &
            !is_outlier(CORR$Means.correct.responses, coef = 1.5) & !is.na(CORR$Means.correct.responses) &
            !is_outlier(CORR$Medians.correct.responses, coef = 1.5) & !is.na(CORR$Medians.correct.responses) &
            !is_outlier(CORR$Accuracy, coef = 1.5) & !is.na(CORR$Accuracy) &
            !is_outlier(CORR$Mean.RT, coef = 1.5) & !is.na(CORR$Mean.RT) &
            !is_outlier(CORR$Median.RT, coef = 1.5) & !is.na(CORR$Median.RT)  
            ,]

CORR[, `:=` (
  ID = as.numeric(ID),
  Mental.demand = as.numeric(Mental.demand),
  Temporal.demand = as.numeric(Temporal.demand),
  Success = as.numeric(Success),
  MAS.comfort.original = as.numeric(MAS.comfort.original),
  MAS.alertness.original = as.numeric(MAS.alertness.original),
  Means.correct.responses = as.numeric(Means.correct.responses),
  Medians.correct.responses = as.numeric(Medians.correct.responses),
  Accuracy = as.numeric(Accuracy),
  Mean.RT = as.numeric(Mean.RT),
  Median.RT = as.numeric(Median.RT),
  KSS = as.numeric(KSS),
  Scenario = as.factor(Scenario)
)]

##### Correlation matrix

AllCORR = CORR[, .(
  Mental_demand = mean(Mental.demand, na.rm = TRUE),
  Temporal_demand = mean(Temporal.demand, na.rm = TRUE),
  Performance = mean(Success, na.rm = TRUE),
  Tense_Arousal = mean(MAS.comfort.original, na.rm = TRUE),
  Energetic_Arousal = mean(MAS.alertness.original, na.rm = TRUE),
  Accuracy = mean(Accuracy, na.rm = TRUE),
  RT = mean(Mean.RT, na.rm = TRUE),
  KSS = mean(KSS, na.rm = TRUE)), by = .(ID)]

matrix = cor(AllCORR)
matrix_rounded = round(matrix, 2)

jpeg(filename = "Supplementary_corr-matrix.jpg", width = 10.5 * 300, height = 8.5 * 300, res = 300)

chart.Correlation(matrix_rounded, histogram=TRUE, pch=19)
dev.off()


####################Correlation between two parameters

TwoCorr = CORR[, .(
  Mental_demand = mean(Mental.demand, na.rm = TRUE),
  Temporal_demand = mean(Temporal.demand, na.rm = TRUE),
  Performance = mean(Success, na.rm = TRUE),
  Tense_Arousal = mean(MAS.comfort.original, na.rm = TRUE),
  Energetic_Arousal = mean(MAS.alertness.original, na.rm = TRUE),
  Accuracy = mean(Accuracy, na.rm = TRUE),
  RT = mean(Mean.RT, na.rm = TRUE),
  KSS = mean(KSS, na.rm = TRUE)), by = .(ID,Scenario)]


Acc.RT=ggplot(TwoCorr, mapping=aes(x=Accuracy, y=RT, colour = Scenario))+
  geom_point(size = 2.5,na.rm = TRUE, alpha = 0.8)+
  labs(
    x = "N-back - Accuracy",
    y = "PVT - Reaction time [ms]")+
  scale_color_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43"))+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),  
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        legend.text=element_text(size=10),
        legend.title=element_text(size=10))+ 
  theme(legend.position = "bottom")
print(Acc.RT)
ggsave(filename = "Supplementary_Corr-Acc.RT.jpg",width = 10.5, height = 8.5, unit = "cm", dpi =350)

Acc.KSS=ggplot(TwoCorr, mapping=aes(x=Accuracy, y=KSS, colour = Scenario))+
  geom_point(size = 2.5,na.rm = TRUE, alpha = 0.8)+
  labs(
    x = "N-back - Accuracy",
    y = "KSS - Sleepiness")+
  scale_color_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43"))+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),  
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        legend.text=element_text(size=10),
        legend.title=element_text(size=10))+ 
  theme(legend.position = "bottom")
print(Acc.KSS)
ggsave(filename = "Supplementary_Corr-Acc.KSS.jpg",width = 10.5, height = 8.5, unit = "cm", dpi =350)

RT.KSS=ggplot(TwoCorr, mapping=aes(x=RT, y=KSS, colour = Scenario))+
  geom_point(size = 2.5,na.rm = TRUE, alpha = 0.8)+
  labs(
    x = "PVT - Reaction time [ms]",
    y = "KSS - Sleepiness")+
  scale_color_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43"))+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),  
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        legend.text=element_text(size=10),
        legend.title=element_text(size=10))+ 
  theme(legend.position = "bottom")
print(RT.KSS)
ggsave(filename = "Supplementary_Corr-RT.KSS.jpg",width = 10.5, height = 8.5, unit = "cm", dpi =350)

RT.MD=ggplot(TwoCorr, mapping=aes(x=RT, y=Mental_demand, colour = Scenario))+
  geom_point(size = 2.5,na.rm = TRUE, alpha = 0.8)+
  labs(
    x = "PVT - Reaction time [ms]",
    y = "NASA-TLX - Mental demand")+
  scale_color_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43"))+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),  
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        legend.text=element_text(size=10),
        legend.title=element_text(size=10))+ 
  theme(legend.position = "bottom")
print(RT.MD)
ggsave(filename = "Supplementary_Corr-RT.MD.jpg",width = 10.5, height = 8.5, unit = "cm", dpi =350)

RT.TD=ggplot(TwoCorr, mapping=aes(x=RT, y=Temporal_demand, colour = Scenario))+
  geom_point(size = 2.5,na.rm = TRUE, alpha = 0.8)+
  labs(
    x = "PVT - Reaction time [ms]",
    y = "NASA-TLX - Temporal demand")+
  scale_color_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43"))+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),  
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        legend.text=element_text(size=10),
        legend.title=element_text(size=10))+ 
  theme(legend.position = "bottom")
print(RT.TD)
ggsave(filename = "Supplementary_Corr-RT.TD.jpg",width = 10.5, height = 8.5, unit = "cm", dpi =350)

ACC.MD=ggplot(TwoCorr, mapping=aes(x=Accuracy, y=Mental_demand, colour = Scenario))+
  geom_point(size = 2.5,na.rm = TRUE, alpha = 0.8)+
  labs(
    x = "N-back - Accuracy",
    y = "NASA-TLX - Mental demand")+
  scale_color_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43"))+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),  
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        legend.text=element_text(size=10),
        legend.title=element_text(size=10))+ 
  theme(legend.position = "bottom")
print(ACC.MD)
ggsave(filename = "Supplementary_Corr-ACC.MD.jpg",width = 10.5, height = 8.5, unit = "cm", dpi =350)

ACC.TD=ggplot(TwoCorr, mapping=aes(x=Accuracy, y=Temporal_demand, colour = Scenario))+
  geom_point(size = 2.5,na.rm = TRUE, alpha = 0.8)+
  labs(
    x = "N-back - Accuracy",
    y = "NASA-TLX - Temporal demand")+
  scale_color_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43"))+
  theme_minimal()+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),  
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        legend.text=element_text(size=10),
        legend.title=element_text(size=10))+ 
  theme(legend.position = "bottom")
print(ACC.TD)
ggsave(filename = "Supplementary_Corr-ACC.TD.jpg",width = 10.5, height = 8.5, unit = "cm", dpi =350)







#########Matrix plot for each individual participant
#Page 1 P101-103

for(participant in unique(CORR$ID)){
  data_ID = CORR[ID == participant]
  
  set.seed(555)
  KSS=ggplot(data_ID, aes(x = Scenario, y = KSS, group = Scenario, colour = Scenario, fill = Scenario)) +
    geom_jitter(alpha = 0.6, size = 1) +
    geom_boxplot(alpha = 0.4, linewidth = 0.25) +
    labs(x = "Photopic illuminance [lx]",
         y = "KSS\nSleepiness")+ 
    scale_x_discrete(labels = c("1", "10","100", "1000")) +
    scale_y_continuous(breaks = c(1, 5, 9), limits = c(1,9), labels = c("  Extr.  \nalert 1","5","  Extr. 9\nsleepy"))+
    scale_color_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
    scale_fill_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
    theme_minimal()+
    ggtitle(paste("ID", participant))+
    theme(axis.text.x = element_text(size = 10),
          axis.text.y = element_blank(),  
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.text=element_text(size=10),
          legend.title=element_text(size=10),
          #plot.background = element_rect(fill="white"),
          panel.background = element_rect(fill = "#FAF3F2"),
          plot.title = element_text(hjust = 0.5, size =10))+ 
    theme(legend.position = "none")
  
  name = paste0("KSS", participant)
  assign(name, KSS, envir =.GlobalEnv)
}
print(KSS115)

set.seed(555)
KSS101=ggplot(CORR[ID==101], aes(x = Scenario, y = KSS, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Photopic illuminance [lx]",
       y = "KSS\nSleepiness")+ 
  scale_x_discrete(labels = c("1", "10","100", "1000")) +
  scale_y_continuous(breaks = c(1, 5, 9), limits = c(1,9), labels = c("  Extr.  \nalert 1","5","  Extr. 9\nsleepy"))+
  scale_color_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) + 
  scale_fill_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  theme_minimal()+
  ggtitle("ID 101")+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),  
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10),
        legend.text=element_text(size=10),
        legend.title=element_text(size=10),
        #plot.background = element_rect(fill="white"),
        panel.background = element_rect(fill = "#FAF3F2"),
        plot.title = element_text(hjust = 0.5, size =10))+ 
  theme(legend.position = "none")
KSS101







for(participant in unique(CORR$ID)){
  data_ID = CORR[ID == participant]
  
  set.seed(555)
  PVT=ggplot(data_ID, aes(x = Scenario, y = Median.RT, group = Scenario, colour = Scenario, fill = Scenario)) +
    geom_jitter(alpha = 0.6, size = 1) +
    geom_boxplot(alpha = 0.4, linewidth = 0.25) +
    labs(x = "Photopic illuminance [lx]",
         y = "PVT\nReaction time\n[ms]")+ 
    scale_x_discrete(labels = c("1", "10","100", "1000")) +
    #scale_y_continuous(limits = c(150,450), breaks = seq(150,450,100), labels = seq(150,450,100))+ 
    scale_y_continuous(limits = c(150,450), breaks = seq(150,450,100), labels = c("  150","  250","  350","  450"))+
    scale_color_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
    scale_fill_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
    theme_minimal()+
    theme(axis.text.x = element_text(size = 10),
          axis.text.y = element_blank(),  
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.text=element_text(size=10),
          #plot.background = element_rect(fill="lightgrey"),
          panel.background = element_rect(fill = "#F5EDED"),
          legend.title=element_text(size=10))+ 
    theme(legend.position = "none")
  
  name = paste0("PVT", participant)
  assign(name, PVT, envir =.GlobalEnv)
}
print(PVT115)

set.seed(555)
PVT101=ggplot(CORR[ID==101], aes(x = Scenario, y = Median.RT, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Photopic illuminance [lx]",
       y = "PVT\nReaction time\n[ms]")+ 
  scale_x_discrete(labels = c("1", "10","100", "1000")) +
  #scale_y_continuous(limits = c(150,450), breaks = seq(150,450,100), labels = seq(150,450,100))+ 
  scale_y_continuous(limits = c(150,450), breaks = seq(150,450,100), labels = c("  150","  250","  350","  450"))+
  scale_color_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  scale_fill_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  theme_minimal()+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),  
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10),
        legend.text=element_text(size=10),
        #plot.background = element_rect(fill="lightgrey"),
        panel.background = element_rect(fill = "#F5EDED"),
        legend.title=element_text(size=10))+ 
  theme(legend.position = "none")
PVT101



for(participant in unique(CORR$ID)){
  data_ID = CORR[ID == participant]
  
  set.seed(555)
  ACC=ggplot(data_ID, aes(x = Scenario, y = Accuracy, group = Scenario, colour = Scenario, fill = Scenario)) +
    geom_jitter(alpha = 0.6, size = 1) +
    geom_boxplot(alpha = 0.4, linewidth = 0.25) +
    labs(x = "Photopic illuminance [lx]",
         y = "N-back\nAccuracy")+ 
    scale_x_discrete(labels = c("1", "10","100", "1000")) +
    #scale_y_continuous(label = scales::percent, breaks = seq(0.70, 1.00, 0.10), limits = c(0.70, 1.00))+
    scale_y_continuous(breaks = seq(0.70, 1.00, 0.10), limits = c(0.70, 1.00), labels = c("  70%","   80%","   90%","   100%"))+  
    scale_color_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
    scale_fill_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
    theme_minimal()+
    theme(axis.text.x = element_text(size = 10),
          axis.text.y = element_blank(),  
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.text=element_text(size=10),
          #plot.background = element_rect(fill="white"),
          panel.background = element_rect(fill = "#FBF5F3"),
          legend.title=element_text(size=10))+ 
    theme(legend.position = "none")
  
  name = paste0("ACC", participant)
  assign(name, ACC, envir =.GlobalEnv)
}
print(ACC115)

set.seed(555)
ACC101=ggplot(CORR[ID==101], aes(x = Scenario, y = Accuracy, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Photopic illuminance [lx]",
       y = "N-back\nAccuracy")+ 
  scale_x_discrete(labels = c("1", "10","100", "1000")) +
  #scale_y_continuous(label = scales::percent, breaks = seq(0.70, 1.00, 0.10), limits = c(0.70, 1.00))+
  scale_y_continuous(breaks = seq(0.70, 1.00, 0.10), limits = c(0.70, 1.00), labels = c("   70%","   80%","    90%","   100%"))+  
  scale_color_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  scale_fill_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  theme_minimal()+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),  
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10),
        legend.text=element_text(size=10),
        #plot.background = element_rect(fill="white"),
        panel.background = element_rect(fill = "#FBF5F3"),
        legend.title=element_text(size=10))+ 
  theme(legend.position = "none")
ACC101




for(participant in unique(CORR$ID)){
  data_ID = CORR[ID == participant]
  
  set.seed(555)
  MD=ggplot(data_ID, aes(x = Scenario, y = Mental.demand, group = Scenario, colour = Scenario, fill = Scenario)) +
    geom_jitter(alpha = 0.6, size = 1) +
    geom_boxplot(alpha = 0.4, linewidth = 0.25) +
    labs(x = "Photopic illuminance [lx]",
         y = "NASA-TLX\nMental demand")+ 
    scale_x_discrete(labels = c("1", "10","100", "1000")) +
    scale_y_continuous(breaks = seq(0,20,5), limits = c(0,20), labels = c("Very    \nlow    1", "", "10", "", "Very 20\nhigh     "))+
    scale_color_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
    scale_fill_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
    theme_minimal()+
    theme(axis.text.x = element_text(size = 10),
          axis.text.y = element_blank(),  
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.text=element_text(size=10),
          #plot.background = element_rect(fill="lightgrey"),
          panel.background = element_rect(fill = "#F5F3FA"),
          legend.title=element_text(size=10))+ 
    theme(legend.position = "none")
  
  name = paste0("MD", participant)
  assign(name, MD, envir =.GlobalEnv)
}
print(MD115)

set.seed(555)
MD101=ggplot(CORR[ID==101], aes(x = Scenario, y = Mental.demand, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Photopic illuminance [lx]",
       y = "NASA-TLX\nMental demand")+ 
  scale_x_discrete(labels = c("1", "10","100", "1000")) +
  scale_y_continuous(breaks = seq(0,20,5), limits = c(0,20), labels = c("Very    \nlow    1", "", "10", "", "Very 20\nhigh     "))+
  scale_color_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  scale_fill_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  theme_minimal()+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),  
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10),#, hjust = -0.5
        legend.text=element_text(size=10),
        #plot.background = element_rect(fill="lightgrey"),
        panel.background = element_rect(fill = "#F5F3FA"),
        legend.title=element_text(size=10))+ 
  theme(legend.position = "none")
MD101




for(participant in unique(CORR$ID)){
  data_ID = CORR[ID == participant]
  
  set.seed(555)
  TD=ggplot(data_ID, aes(x = Scenario, y = Temporal.demand, group = Scenario, colour = Scenario, fill = Scenario)) +
    geom_jitter(alpha = 0.6, size = 1) +
    geom_boxplot(alpha = 0.4, linewidth = 0.25) +
    labs(x = "Photopic illuminance [lx]",
         y = "NASA-TLX\nTemporal demand")+ 
    scale_x_discrete(labels = c("1", "10","100", "1000")) +
    scale_y_continuous(breaks = seq(0,20,5), limits = c(0,20), labels = c("Very    \nlow    1", "", "10", "", "Very 20\nhigh     "))+
    scale_color_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
    scale_fill_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
    theme_minimal()+
    theme(axis.text.x = element_text(size = 10),
          axis.text.y = element_blank(),  
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.text=element_text(size=10),
          #plot.background = element_rect(fill="white"),
          panel.background = element_rect(fill = "#ECEAF2"),
          legend.title=element_text(size=10))+ 
    theme(legend.position = "none")
  
  name = paste0("TD", participant)
  assign(name, TD, envir =.GlobalEnv)
}
print(TD115)

set.seed(555)
TD101=ggplot(CORR[ID==101], aes(x = Scenario, y = Temporal.demand, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Photopic illuminance [lx]",
       y = "NASA-TLX\nTemporal demand")+ 
  scale_x_discrete(labels = c("1", "10","100", "1000")) +
  scale_y_continuous(breaks = seq(0,20,5), limits = c(0,20), labels = c("Very    \nlow    1", "", "10", "", "Very 20\nhigh     "))+
  scale_color_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  scale_fill_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  theme_minimal()+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),  
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10),#, hjust = -0.25
        legend.text=element_text(size=10),
        #plot.background = element_rect(fill="white"),
        panel.background = element_rect(fill = "#ECEAF2"),
        legend.title=element_text(size=10))+ 
  theme(legend.position = "none")





for(participant in unique(CORR$ID)){
  data_ID = CORR[ID == participant]
  
  set.seed(555)
  PFC=ggplot(data_ID, aes(x = Scenario, y = Success, group = Scenario, colour = Scenario, fill = Scenario)) +
    geom_jitter(alpha = 0.6, size = 1) +
    geom_boxplot(alpha = 0.4, linewidth = 0.25) +
    labs(x = "Photopic illuminance [lx]",
         y = "NASA-TLX\nPerformance")+ 
    scale_x_discrete(labels = c("1", "10","100", "1000")) +
    scale_y_continuous(breaks = seq(0,20,5), limits = c(0,20), labels = c("Per-    \nfect   1", "","10","", "Fail- 20\nure      ")) +
    scale_color_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
    scale_fill_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
    theme_minimal()+
    theme(axis.text.x = element_text(size = 10),
          axis.text.y = element_blank(),  
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.text=element_text(size=10),
          #plot.background = element_rect(fill="lightgrey"),
          panel.background = element_rect(fill = "#F0EEF4"),
          legend.title=element_text(size=10))+ 
    theme(legend.position = "none")
  
  name = paste0("PFC", participant)
  assign(name, PFC, envir =.GlobalEnv)
}
print(PFC115)

set.seed(555)
PFC101=ggplot(CORR[ID==101], aes(x = Scenario, y = Success, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Photopic illuminance [lx]",
       y = "NASA-TLX\nPerformance")+ 
  scale_x_discrete(labels = c("1", "10","100", "1000")) +
  scale_y_continuous(breaks = seq(0,20,5), limits = c(0,20), labels = c("Per-    \nfect   1", "","10","", "Fail- 20\nure      ")) +
  scale_color_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  scale_fill_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  theme_minimal()+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),  
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10),#, hjust = -0.75
        legend.text=element_text(size=10),
        #plot.background = element_rect(fill="lightgrey"),
        panel.background = element_rect(fill = "#F0EEF4"),
        legend.title=element_text(size=10))+ 
  theme(legend.position = "none")
PFC101




for(participant in unique(CORR$ID)){
  data_ID = CORR[ID == participant]
  
  set.seed(555)
  TA=ggplot(data_ID, aes(x = Scenario, y = MAS.comfort, group = Scenario, colour = Scenario, fill = Scenario)) +
    geom_jitter(alpha = 0.6, size = 1) +
    geom_boxplot(alpha = 0.4, linewidth = 0.25) +
    labs(x = "Photopic illuminance [lx]",
         y = "MAS\nTense arousal")+ 
    scale_x_discrete(labels = c("1", "10","100", "1000")) +
    scale_y_continuous(n.breaks = 3, limits = c(-5,+5), labels = c("   Calm\n-5", "0", "+5\n   Tense")) +
    scale_color_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
    scale_fill_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
    theme_minimal()+
    theme(axis.text.x = element_text(size = 10),
          axis.text.y = element_blank(),  
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.text=element_text(size=10),
          #plot.background = element_rect(fill="white"),
          panel.background = element_rect(fill = "#F3FAF5"),
          legend.title=element_text(size=10))+ 
    theme(legend.position = "none")
  
  name = paste0("TA", participant)
  assign(name, TA, envir =.GlobalEnv)
}
print(TA115)

set.seed(555)
TA101=ggplot(CORR[ID==101], aes(x = Scenario, y = MAS.comfort, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Photopic illuminance [lx]",
       y = "MAS\nTense arousal")+ 
  scale_x_discrete(labels = c("1", "10","100", "1000")) +
  scale_y_continuous(n.breaks = 3, limits = c(-5,+5), labels = c("   Calm\n-5", "0", "+5\n   Tense")) +
  scale_color_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  scale_fill_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  theme_minimal()+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),  
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10),
        legend.text=element_text(size=10),
        #plot.background = element_rect(fill="white"),
        panel.background = element_rect(fill = "#F3FAF5"),
        legend.title=element_text(size=10))+ 
  theme(legend.position = "none")
TA101




for(participant in unique(CORR$ID)){
  data_ID = CORR[ID == participant]
  
  set.seed(555)
  EA=ggplot(data_ID, aes(x = Scenario, y = MAS.alertness, group = Scenario, colour = Scenario, fill = Scenario)) +
    geom_jitter(alpha = 0.6, size = 1) +
    geom_boxplot(alpha = 0.4, linewidth = 0.25) +
    labs(x = "Photopic illuminance [lx]",
         y = "MAS\nEnergetic arousal")+ 
    scale_x_discrete(labels = c("1", "10","100", "1000")) +
    scale_y_continuous(n.breaks = 3, limits = c(-5,+5), labels = c("Tired\n-5", "0", "En-  +5\nergetic ")) +
    scale_color_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
    scale_fill_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
    theme_minimal()+
    theme(axis.text.x = element_text(size = 10),
          axis.text.y = element_blank(),  
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.text=element_text(size=10),
          #plot.background = element_rect(fill="lightgrey"),
          panel.background = element_rect(fill = "#EBF4EF"),
          legend.title=element_text(size=10))+ 
    theme(legend.position = "none")
  
  name = paste0("EA", participant)
  assign(name, EA, envir =.GlobalEnv)
}
print(EA115)

set.seed(555)
EA101=ggplot(CORR[ID==101], aes(x = Scenario, y = MAS.alertness, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Photopic illuminance [lx]",
       y = "MAS\nEnergetic arousal")+ 
  scale_x_discrete(labels = c("1", "10","100", "1000")) +
  scale_y_continuous(n.breaks = 3, limits = c(-5,+5), labels = c("Tired\n-5", "0", "En-  +5\nergetic ")) +
  scale_color_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  scale_fill_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  theme_minimal()+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),  
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10),
        legend.text=element_text(size=10),
        #plot.background = element_rect(fill="lightgrey"),
        panel.background = element_rect(fill = "#EBF4EF"),
        legend.title=element_text(size=10))+ 
  theme(legend.position = "none")
EA101


ggarrange(KSS101,KSS102,KSS103, PVT101,PVT102,PVT103, ACC101,ACC102,ACC103, MD101,MD102,MD103, TD101,TD102,TD103, PFC101,PFC102,PFC103, TA101,TA102,TA103, EA101,EA102,EA103, hjust = -0.1, vjust = 1.2, nrow =8, ncol =3, widths = c(1.33, 1,1), heights = c(1.15,1,1,1,1,1,1,1), common.legend = TRUE, font.label = list(size=10), legend ="none") 
ggsave(filename = "Supplementary_matrixP1.jpg",width = 21, height = 29, unit = "cm", dpi =350)



#########Matrix plot with all results
#Page 2 P104-106####################################################################################################################################################################################################################################################



set.seed(555)
KSS104=ggplot(CORR[ID==104], aes(x = Scenario, y = KSS, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Photopic illuminance [lx]",
       y = "KSS\nSleepiness")+ 
  scale_x_discrete(labels = c("1", "10","100", "1000")) +
  scale_y_continuous(breaks = c(1, 5, 9), limits = c(1,9), labels = c("  Extr.  \nalert 1","5","  Extr. 9\nsleepy"))+
  scale_color_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) + 
  scale_fill_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  theme_minimal()+
  ggtitle("ID 104")+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),  
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10),
        legend.text=element_text(size=10),
        legend.title=element_text(size=10),
        #plot.background = element_rect(fill="white"),
        panel.background = element_rect(fill = "#FAF3F2"),
        plot.title = element_text(hjust = 0.5, size =10))+ 
  theme(legend.position = "none")
KSS104



set.seed(555)
PVT104=ggplot(CORR[ID==104], aes(x = Scenario, y = Median.RT, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Photopic illuminance [lx]",
       y = "PVT\nReaction time\n[ms]")+ 
  scale_x_discrete(labels = c("1", "10","100", "1000")) +
  #scale_y_continuous(limits = c(150,450), breaks = seq(150,450,100), labels = seq(150,450,100))+ 
  scale_y_continuous(limits = c(150,450), breaks = seq(150,450,100), labels = c("  150","  250","  350","  450"))+
  scale_color_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  scale_fill_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  theme_minimal()+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),  
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10),
        legend.text=element_text(size=10),
        #plot.background = element_rect(fill="lightgrey"),
        panel.background = element_rect(fill = "#F5EDED"),
        legend.title=element_text(size=10))+ 
  theme(legend.position = "none")
PVT104



set.seed(555)
ACC104=ggplot(CORR[ID==104], aes(x = Scenario, y = Accuracy, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Photopic illuminance [lx]",
       y = "N-back\nAccuracy")+ 
  scale_x_discrete(labels = c("1", "10","100", "1000")) +
  #scale_y_continuous(label = scales::percent, breaks = seq(0.70, 1.00, 0.10), limits = c(0.70, 1.00))+
  scale_y_continuous(breaks = seq(0.70, 1.00, 0.10), limits = c(0.70, 1.00), labels = c("   70%","   80%","    90%","   100%"))+  
  scale_color_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  scale_fill_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  theme_minimal()+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),  
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10),
        legend.text=element_text(size=10),
        #plot.background = element_rect(fill="white"),
        panel.background = element_rect(fill = "#FBF5F3"),
        legend.title=element_text(size=10))+ 
  theme(legend.position = "none")
ACC104



set.seed(555)
MD104=ggplot(CORR[ID==104], aes(x = Scenario, y = Mental.demand, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Photopic illuminance [lx]",
       y = "NASA-TLX\nMental demand")+ 
  scale_x_discrete(labels = c("1", "10","100", "1000")) +
  scale_y_continuous(breaks = seq(0,20,5), limits = c(0,20), labels = c("Very    \nlow    1", "", "10", "", "Very 20\nhigh     "))+
  scale_color_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  scale_fill_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  theme_minimal()+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),  
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10),#, hjust = -0.5
        legend.text=element_text(size=10),
        #plot.background = element_rect(fill="lightgrey"),
        panel.background = element_rect(fill = "#F5F3FA"),
        legend.title=element_text(size=10))+ 
  theme(legend.position = "none")
MD104




set.seed(555)
TD104=ggplot(CORR[ID==104], aes(x = Scenario, y = Temporal.demand, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Photopic illuminance [lx]",
       y = "NASA-TLX\nTemporal demand")+ 
  scale_x_discrete(labels = c("1", "10","100", "1000")) +
  scale_y_continuous(breaks = seq(0,20,5), limits = c(0,20), labels = c("Very    \nlow    1", "", "10", "", "Very 20\nhigh     "))+
  scale_color_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  scale_fill_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  theme_minimal()+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),  
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10),#, hjust = -0.25
        legend.text=element_text(size=10),
        #plot.background = element_rect(fill="white"),
        panel.background = element_rect(fill = "#ECEAF2"),
        legend.title=element_text(size=10))+ 
  theme(legend.position = "none")



set.seed(555)
PFC104=ggplot(CORR[ID==104], aes(x = Scenario, y = Success, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Photopic illuminance [lx]",
       y = "NASA-TLX\nPerformance")+ 
  scale_x_discrete(labels = c("1", "10","100", "1000")) +
  scale_y_continuous(breaks = seq(0,20,5), limits = c(0,20), labels = c("Per-    \nfect   1", "","10","", "Fail- 20\nure      ")) +
  scale_color_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  scale_fill_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  theme_minimal()+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),  
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10),#, hjust = -0.75
        legend.text=element_text(size=10),
        #plot.background = element_rect(fill="lightgrey"),
        panel.background = element_rect(fill = "#F0EEF4"),
        legend.title=element_text(size=10))+ 
  theme(legend.position = "none")
PFC104




set.seed(555)
TA104=ggplot(CORR[ID==104], aes(x = Scenario, y = MAS.comfort, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Photopic illuminance [lx]",
       y = "MAS\nTense arousal")+ 
  scale_x_discrete(labels = c("1", "10","100", "1000")) +
  scale_y_continuous(n.breaks = 3, limits = c(-5,+5), labels = c("   Calm\n-5", "0", "+5\n   Tense")) +
  scale_color_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  scale_fill_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  theme_minimal()+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),  
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10),
        legend.text=element_text(size=10),
        #plot.background = element_rect(fill="white"),
        panel.background = element_rect(fill = "#F3FAF5"),
        legend.title=element_text(size=10))+ 
  theme(legend.position = "none")
TA104



set.seed(555)
EA104=ggplot(CORR[ID==104], aes(x = Scenario, y = MAS.alertness, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Photopic illuminance [lx]",
       y = "MAS\nEnergetic arousal")+ 
  scale_x_discrete(labels = c("1", "10","100", "1000")) +
  scale_y_continuous(n.breaks = 3, limits = c(-5,+5), labels = c("Tired\n-5", "0", "En-  +5\nergetic ")) +
  scale_color_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  scale_fill_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  theme_minimal()+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),  
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10),
        legend.text=element_text(size=10),
        #plot.background = element_rect(fill="lightgrey"),
        panel.background = element_rect(fill = "#EBF4EF"),
        legend.title=element_text(size=10))+ 
  theme(legend.position = "none")
EA104


ggarrange(KSS104,KSS105,KSS106, PVT104,PVT105,PVT106, ACC104,ACC105,ACC106, MD104,MD105,MD106, TD104,TD105,TD106, PFC104,PFC105,PFC106, TA104,TA105,TA106, EA104,EA105,EA106, hjust = -0.1, vjust = 1.2, nrow =8, ncol =3, widths = c(1.33, 1,1), heights = c(1.15,1,1,1,1,1,1,1), common.legend = TRUE, font.label = list(size=10), legend ="none") 
ggsave(filename = "Supplementary_matrixP2.jpg",width = 21, height = 29, unit = "cm", dpi =350)



#########Matrix plot with all results
#Page 3 P107-109####################################################################################################################################################################################################################################################


set.seed(555)
KSS107=ggplot(CORR[ID==107], aes(x = Scenario, y = KSS, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Photopic illuminance [lx]",
       y = "KSS\nSleepiness")+ 
  scale_x_discrete(labels = c("1", "10","100", "1000")) +
  scale_y_continuous(breaks = c(1, 5, 9), limits = c(1,9), labels = c("  Extr.  \nalert 1","5","  Extr. 9\nsleepy"))+
  scale_color_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) + 
  scale_fill_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  theme_minimal()+
  ggtitle("ID 107")+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),  
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10),
        legend.text=element_text(size=10),
        legend.title=element_text(size=10),
        #plot.background = element_rect(fill="white"),
        panel.background = element_rect(fill = "#FAF3F2"),
        plot.title = element_text(hjust = 0.5, size =10))+ 
  theme(legend.position = "none")
KSS107


set.seed(555)
PVT107=ggplot(CORR[ID==107], aes(x = Scenario, y = Median.RT, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Photopic illuminance [lx]",
       y = "PVT\nReaction time\n[ms]")+ 
  scale_x_discrete(labels = c("1", "10","100", "1000")) +
  #scale_y_continuous(limits = c(150,450), breaks = seq(150,450,100), labels = seq(150,450,100))+ 
  scale_y_continuous(limits = c(150,450), breaks = seq(150,450,100), labels = c("  150","  250","  350","  450"))+
  scale_color_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  scale_fill_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  theme_minimal()+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),  
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10),
        legend.text=element_text(size=10),
        #plot.background = element_rect(fill="lightgrey"),
        panel.background = element_rect(fill = "#F5EDED"),
        legend.title=element_text(size=10))+ 
  theme(legend.position = "none")
PVT107


set.seed(555)
ACC107=ggplot(CORR[ID==107], aes(x = Scenario, y = Accuracy, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Photopic illuminance [lx]",
       y = "N-back\nAccuracy")+ 
  scale_x_discrete(labels = c("1", "10","100", "1000")) +
  #scale_y_continuous(label = scales::percent, breaks = seq(0.70, 1.00, 0.10), limits = c(0.70, 1.00))+
  scale_y_continuous(breaks = seq(0.70, 1.00, 0.10), limits = c(0.70, 1.00), labels = c("   70%","   80%","    90%","   100%"))+  
  scale_color_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  scale_fill_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  theme_minimal()+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),  
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10),
        legend.text=element_text(size=10),
        #plot.background = element_rect(fill="white"),
        panel.background = element_rect(fill = "#FBF5F3"),
        legend.title=element_text(size=10))+ 
  theme(legend.position = "none")
ACC107



set.seed(555)
MD107=ggplot(CORR[ID==107], aes(x = Scenario, y = Mental.demand, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Photopic illuminance [lx]",
       y = "NASA-TLX\nMental demand")+ 
  scale_x_discrete(labels = c("1", "10","100", "1000")) +
  scale_y_continuous(breaks = seq(0,20,5), limits = c(0,20), labels = c("Very    \nlow    1", "", "10", "", "Very 20\nhigh     "))+
  scale_color_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  scale_fill_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  theme_minimal()+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),  
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10),#, hjust = -0.5
        legend.text=element_text(size=10),
        #plot.background = element_rect(fill="lightgrey"),
        panel.background = element_rect(fill = "#F5F3FA"),
        legend.title=element_text(size=10))+ 
  theme(legend.position = "none")
MD107



set.seed(555)
TD107=ggplot(CORR[ID==107], aes(x = Scenario, y = Temporal.demand, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Photopic illuminance [lx]",
       y = "NASA-TLX\nTemporal demand")+ 
  scale_x_discrete(labels = c("1", "10","100", "1000")) +
  scale_y_continuous(breaks = seq(0,20,5), limits = c(0,20), labels = c("Very    \nlow    1", "", "10", "", "Very 20\nhigh     "))+
  scale_color_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  scale_fill_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  theme_minimal()+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),  
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10),#, hjust = -0.25
        legend.text=element_text(size=10),
        #plot.background = element_rect(fill="white"),
        panel.background = element_rect(fill = "#ECEAF2"),
        legend.title=element_text(size=10))+ 
  theme(legend.position = "none")


set.seed(555)
PFC107=ggplot(CORR[ID==107], aes(x = Scenario, y = Success, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Photopic illuminance [lx]",
       y = "NASA-TLX\nPerformance")+ 
  scale_x_discrete(labels = c("1", "10","100", "1000")) +
  scale_y_continuous(breaks = seq(0,20,5), limits = c(0,20), labels = c("Per-    \nfect   1", "","10","", "Fail- 20\nure      ")) +
  scale_color_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  scale_fill_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  theme_minimal()+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),  
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10),#, hjust = -0.75
        legend.text=element_text(size=10),
        #plot.background = element_rect(fill="lightgrey"),
        panel.background = element_rect(fill = "#F0EEF4"),
        legend.title=element_text(size=10))+ 
  theme(legend.position = "none")
PFC107



set.seed(555)
TA107=ggplot(CORR[ID==107], aes(x = Scenario, y = MAS.comfort, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Photopic illuminance [lx]",
       y = "MAS\nTense arousal")+ 
  scale_x_discrete(labels = c("1", "10","100", "1000")) +
  scale_y_continuous(n.breaks = 3, limits = c(-5,+5), labels = c("   Calm\n-5", "0", "+5\n   Tense")) +
  scale_color_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  scale_fill_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  theme_minimal()+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),  
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10),
        legend.text=element_text(size=10),
        #plot.background = element_rect(fill="white"),
        panel.background = element_rect(fill = "#F3FAF5"),
        legend.title=element_text(size=10))+ 
  theme(legend.position = "none")
TA107



set.seed(555)
EA107=ggplot(CORR[ID==107], aes(x = Scenario, y = MAS.alertness, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Photopic illuminance [lx]",
       y = "MAS\nEnergetic arousal")+ 
  scale_x_discrete(labels = c("1", "10","100", "1000")) +
  scale_y_continuous(n.breaks = 3, limits = c(-5,+5), labels = c("Tired\n-5", "0", "En-  +5\nergetic ")) +
  scale_color_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  scale_fill_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  theme_minimal()+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),  
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10),
        legend.text=element_text(size=10),
        #plot.background = element_rect(fill="lightgrey"),
        panel.background = element_rect(fill = "#EBF4EF"),
        legend.title=element_text(size=10))+ 
  theme(legend.position = "none")
EA107


ggarrange(KSS107,KSS108,KSS109, PVT107,PVT108,PVT109, ACC107,ACC108,ACC109, MD107,MD108,MD109, TD107,TD108,TD109, PFC107,PFC108,PFC109, TA107,TA108,TA109, EA107,EA108,EA109, hjust = -0.1, vjust = 1.2, nrow =8, ncol =3, widths = c(1.33, 1,1), heights = c(1.15,1,1,1,1,1,1,1), common.legend = TRUE, font.label = list(size=10), legend ="none") 
ggsave(filename = "Supplementary_matrixP3.jpg",width = 21, height = 29, unit = "cm", dpi =350)



#########Matrix plot with all results
#Page 4 P110-112####################################################################################################################################################################################################################################################


set.seed(555)
KSS110=ggplot(CORR[ID==110], aes(x = Scenario, y = KSS, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Photopic illuminance [lx]",
       y = "KSS\nSleepiness")+ 
  scale_x_discrete(labels = c("1", "10","100", "1000")) +
  scale_y_continuous(breaks = c(1, 5, 9), limits = c(1,9), labels = c("  Extr.  \nalert 1","5","  Extr. 9\nsleepy"))+
  scale_color_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) + 
  scale_fill_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  theme_minimal()+
  ggtitle("ID 110")+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),  
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10),
        legend.text=element_text(size=10),
        legend.title=element_text(size=10),
        #plot.background = element_rect(fill="white"),
        panel.background = element_rect(fill = "#FAF3F2"),
        plot.title = element_text(hjust = 0.5, size =10))+ 
  theme(legend.position = "none")
KSS110


set.seed(555)
PVT110=ggplot(CORR[ID==110], aes(x = Scenario, y = Median.RT, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Photopic illuminance [lx]",
       y = "PVT\nReaction time\n[ms]")+ 
  scale_x_discrete(labels = c("1", "10","100", "1000")) +
  #scale_y_continuous(limits = c(150,450), breaks = seq(150,450,100), labels = seq(150,450,100))+ 
  scale_y_continuous(limits = c(150,450), breaks = seq(150,450,100), labels = c("  150","  250","  350","  450"))+
  scale_color_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  scale_fill_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  theme_minimal()+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),  
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10),
        legend.text=element_text(size=10),
        #plot.background = element_rect(fill="lightgrey"),
        panel.background = element_rect(fill = "#F5EDED"),
        legend.title=element_text(size=10))+ 
  theme(legend.position = "none")
PVT110


set.seed(555)
ACC110=ggplot(CORR[ID==110], aes(x = Scenario, y = Accuracy, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Photopic illuminance [lx]",
       y = "N-back\nAccuracy")+ 
  scale_x_discrete(labels = c("1", "10","100", "1000")) +
  #scale_y_continuous(label = scales::percent, breaks = seq(0.70, 1.00, 0.10), limits = c(0.70, 1.00))+
  scale_y_continuous(breaks = seq(0.70, 1.00, 0.10), limits = c(0.70, 1.00), labels = c("   70%","   80%","    90%","   100%"))+  
  scale_color_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  scale_fill_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  theme_minimal()+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),  
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10),
        legend.text=element_text(size=10),
        #plot.background = element_rect(fill="white"),
        panel.background = element_rect(fill = "#FBF5F3"),
        legend.title=element_text(size=10))+ 
  theme(legend.position = "none")
ACC110



set.seed(555)
MD110=ggplot(CORR[ID==110], aes(x = Scenario, y = Mental.demand, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Photopic illuminance [lx]",
       y = "NASA-TLX\nMental demand")+ 
  scale_x_discrete(labels = c("1", "10","100", "1000")) +
  scale_y_continuous(breaks = seq(0,20,5), limits = c(0,20), labels = c("Very    \nlow    1", "", "10", "", "Very 20\nhigh     "))+
  scale_color_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  scale_fill_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  theme_minimal()+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),  
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10),#, hjust = -0.5
        legend.text=element_text(size=10),
        #plot.background = element_rect(fill="lightgrey"),
        panel.background = element_rect(fill = "#F5F3FA"),
        legend.title=element_text(size=10))+ 
  theme(legend.position = "none")
MD110



set.seed(555)
TD110=ggplot(CORR[ID==110], aes(x = Scenario, y = Temporal.demand, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Photopic illuminance [lx]",
       y = "NASA-TLX\nTemporal demand")+ 
  scale_x_discrete(labels = c("1", "10","100", "1000")) +
  scale_y_continuous(breaks = seq(0,20,5), limits = c(0,20), labels = c("Very    \nlow    1", "", "10", "", "Very 20\nhigh     "))+
  scale_color_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  scale_fill_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  theme_minimal()+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),  
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10),#, hjust = -0.25
        legend.text=element_text(size=10),
        #plot.background = element_rect(fill="white"),
        panel.background = element_rect(fill = "#ECEAF2"),
        legend.title=element_text(size=10))+ 
  theme(legend.position = "none")


set.seed(555)
PFC110=ggplot(CORR[ID==110], aes(x = Scenario, y = Success, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Photopic illuminance [lx]",
       y = "NASA-TLX\nPerformance")+ 
  scale_x_discrete(labels = c("1", "10","100", "1000")) +
  scale_y_continuous(breaks = seq(0,20,5), limits = c(0,20), labels = c("Per-    \nfect   1", "","10","", "Fail- 20\nure      ")) +
  scale_color_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  scale_fill_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  theme_minimal()+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),  
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10),#, hjust = -0.75
        legend.text=element_text(size=10),
        #plot.background = element_rect(fill="lightgrey"),
        panel.background = element_rect(fill = "#F0EEF4"),
        legend.title=element_text(size=10))+ 
  theme(legend.position = "none")
PFC110



set.seed(555)
TA110=ggplot(CORR[ID==110], aes(x = Scenario, y = MAS.comfort, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Photopic illuminance [lx]",
       y = "MAS\nTense arousal")+ 
  scale_x_discrete(labels = c("1", "10","100", "1000")) +
  scale_y_continuous(n.breaks = 3, limits = c(-5,+5), labels = c("   Calm\n-5", "0", "+5\n   Tense")) +
  scale_color_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  scale_fill_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  theme_minimal()+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),  
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10),
        legend.text=element_text(size=10),
        #plot.background = element_rect(fill="white"),
        panel.background = element_rect(fill = "#F3FAF5"),
        legend.title=element_text(size=10))+ 
  theme(legend.position = "none")
TA110



set.seed(555)
EA110=ggplot(CORR[ID==110], aes(x = Scenario, y = MAS.alertness, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Photopic illuminance [lx]",
       y = "MAS\nEnergetic arousal")+ 
  scale_x_discrete(labels = c("1", "10","100", "1000")) +
  scale_y_continuous(n.breaks = 3, limits = c(-5,+5), labels = c("Tired\n-5", "0", "En-  +5\nergetic ")) +
  scale_color_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  scale_fill_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  theme_minimal()+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),  
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10),
        legend.text=element_text(size=10),
        #plot.background = element_rect(fill="lightgrey"),
        panel.background = element_rect(fill = "#EBF4EF"),
        legend.title=element_text(size=10))+ 
  theme(legend.position = "none")
EA110


ggarrange(KSS110,KSS111,KSS112, PVT110,PVT111,PVT112, ACC110,ACC111,ACC112, MD110,MD111,MD112, TD110,TD111,TD112, PFC110,PFC111,PFC112, TA110,TA111,TA112, EA110,EA111,EA112, hjust = -0.1, vjust = 1.2, nrow =8, ncol =3, widths = c(1.33, 1,1), heights = c(1.15,1,1,1,1,1,1,1), common.legend = TRUE, font.label = list(size=10), legend ="none") 
ggsave(filename = "Supplementary_matrixP4.jpg",width = 21, height = 29, unit = "cm", dpi =350)



#########Matrix plot with all results
#Page 5 P113-115####################################################################################################################################################################################################################################################


set.seed(555)
KSS113=ggplot(CORR[ID==113], aes(x = Scenario, y = KSS, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Photopic illuminance [lx]",
       y = "KSS\nSleepiness")+ 
  scale_x_discrete(labels = c("1", "10","100", "1000")) +
  scale_y_continuous(breaks = c(1, 5, 9), limits = c(1,9), labels = c("  Extr.  \nalert 1","5","  Extr. 9\nsleepy"))+
  scale_color_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) + 
  scale_fill_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  theme_minimal()+
  ggtitle("ID 113")+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),  
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10),
        legend.text=element_text(size=10),
        legend.title=element_text(size=10),
        #plot.background = element_rect(fill="white"),
        panel.background = element_rect(fill = "#FAF3F2"),
        plot.title = element_text(hjust = 0.5, size =10))+ 
  theme(legend.position = "none")
KSS113


set.seed(555)
PVT113=ggplot(CORR[ID==113], aes(x = Scenario, y = Median.RT, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Photopic illuminance [lx]",
       y = "PVT\nReaction time\n[ms]")+ 
  scale_x_discrete(labels = c("1", "10","100", "1000")) +
  #scale_y_continuous(limits = c(150,450), breaks = seq(150,450,100), labels = seq(150,450,100))+ 
  scale_y_continuous(limits = c(150,450), breaks = seq(150,450,100), labels = c("  150","  250","  350","  450"))+
  scale_color_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  scale_fill_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  theme_minimal()+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),  
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10),
        legend.text=element_text(size=10),
        #plot.background = element_rect(fill="lightgrey"),
        panel.background = element_rect(fill = "#F5EDED"),
        legend.title=element_text(size=10))+ 
  theme(legend.position = "none")
PVT113


set.seed(555)
ACC113=ggplot(CORR[ID==113], aes(x = Scenario, y = Accuracy, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Photopic illuminance [lx]",
       y = "N-back\nAccuracy")+ 
  scale_x_discrete(labels = c("1", "10","100", "1000")) +
  #scale_y_continuous(label = scales::percent, breaks = seq(0.70, 1.00, 0.10), limits = c(0.70, 1.00))+
  scale_y_continuous(breaks = seq(0.70, 1.00, 0.10), limits = c(0.70, 1.00), labels = c("   70%","   80%","    90%","   100%"))+  
  scale_color_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  scale_fill_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  theme_minimal()+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),  
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10),
        legend.text=element_text(size=10),
        #plot.background = element_rect(fill="white"),
        panel.background = element_rect(fill = "#FBF5F3"),
        legend.title=element_text(size=10))+ 
  theme(legend.position = "none")
ACC113



set.seed(555)
MD113=ggplot(CORR[ID==113], aes(x = Scenario, y = Mental.demand, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Photopic illuminance [lx]",
       y = "NASA-TLX\nMental demand")+ 
  scale_x_discrete(labels = c("1", "10","100", "1000")) +
  scale_y_continuous(breaks = seq(0,20,5), limits = c(0,20), labels = c("Very    \nlow    1", "", "10", "", "Very 20\nhigh     "))+
  scale_color_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  scale_fill_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  theme_minimal()+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),  
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10),#, hjust = -0.5
        legend.text=element_text(size=10),
        #plot.background = element_rect(fill="lightgrey"),
        panel.background = element_rect(fill = "#F5F3FA"),
        legend.title=element_text(size=10))+ 
  theme(legend.position = "none")
MD113



set.seed(555)
TD113=ggplot(CORR[ID==113], aes(x = Scenario, y = Temporal.demand, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Photopic illuminance [lx]",
       y = "NASA-TLX\nTemporal demand")+ 
  scale_x_discrete(labels = c("1", "10","100", "1000")) +
  scale_y_continuous(breaks = seq(0,20,5), limits = c(0,20), labels = c("Very    \nlow    1", "", "10", "", "Very 20\nhigh     "))+
  scale_color_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  scale_fill_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  theme_minimal()+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),  
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10),#, hjust = -0.25
        legend.text=element_text(size=10),
        #plot.background = element_rect(fill="white"),
        panel.background = element_rect(fill = "#ECEAF2"),
        legend.title=element_text(size=10))+ 
  theme(legend.position = "none")


set.seed(555)
PFC113=ggplot(CORR[ID==113], aes(x = Scenario, y = Success, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Photopic illuminance [lx]",
       y = "NASA-TLX\nPerformance")+ 
  scale_x_discrete(labels = c("1", "10","100", "1000")) +
  scale_y_continuous(breaks = seq(0,20,5), limits = c(0,20), labels = c("Per-    \nfect   1", "","10","", "Fail- 20\nure      ")) +
  scale_color_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  scale_fill_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  theme_minimal()+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),  
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10),#, hjust = -0.75
        legend.text=element_text(size=10),
        #plot.background = element_rect(fill="lightgrey"),
        panel.background = element_rect(fill = "#F0EEF4"),
        legend.title=element_text(size=10))+ 
  theme(legend.position = "none")
PFC113



set.seed(555)
TA113=ggplot(CORR[ID==113], aes(x = Scenario, y = MAS.comfort, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Photopic illuminance [lx]",
       y = "MAS\nTense arousal")+ 
  scale_x_discrete(labels = c("1", "10","100", "1000")) +
  scale_y_continuous(n.breaks = 3, limits = c(-5,+5), labels = c("   Calm\n-5", "0", "+5\n   Tense")) +
  scale_color_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  scale_fill_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  theme_minimal()+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),  
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10),
        legend.text=element_text(size=10),
        #plot.background = element_rect(fill="white"),
        panel.background = element_rect(fill = "#F3FAF5"),
        legend.title=element_text(size=10))+ 
  theme(legend.position = "none")
TA113



set.seed(555)
EA113=ggplot(CORR[ID==113], aes(x = Scenario, y = MAS.alertness, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Photopic illuminance [lx]",
       y = "MAS\nEnergetic arousal")+ 
  scale_x_discrete(labels = c("1", "10","100", "1000")) +
  scale_y_continuous(n.breaks = 3, limits = c(-5,+5), labels = c("Tired\n-5", "0", "En-  +5\nergetic ")) +
  scale_color_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  scale_fill_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  theme_minimal()+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),  
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10),
        legend.text=element_text(size=10),
        #plot.background = element_rect(fill="lightgrey"),
        panel.background = element_rect(fill = "#EBF4EF"),
        legend.title=element_text(size=10))+ 
  theme(legend.position = "none")
EA113


ggarrange(KSS113,KSS114,KSS115, PVT113,PVT114,PVT115, ACC113,ACC114,ACC115, MD113,MD114,MD115, TD113,TD114,TD115, PFC113,PFC114,PFC115, TA113,TA114,TA115, EA113,EA114,EA115, hjust = -0.1, vjust = 1.2, nrow =8, ncol =3, widths = c(1.33, 1,1), heights = c(1.15,1,1,1,1,1,1,1), common.legend = TRUE, font.label = list(size=10), legend ="none") 
ggsave(filename = "Supplementary_matrixP5.jpg",width = 21, height = 29, unit = "cm", dpi =350)









#########Matrix plot with all results
#Page 6 P116####################################################################################################################################################################################################################################################


set.seed(555)
KSS116=ggplot(CORR[ID==116], aes(x = Scenario, y = KSS, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Photopic illuminance [lx]",
       y = "KSS\nSleepiness")+ 
  scale_x_discrete(labels = c("1", "10","100", "1000")) +
  scale_y_continuous(breaks = c(1, 5, 9), limits = c(1,9), labels = c("  Extr.  \nalert 1","5","  Extr. 9\nsleepy"))+
  scale_color_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) + 
  scale_fill_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  theme_minimal()+
  ggtitle("ID 116")+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),  
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10),
        legend.text=element_text(size=10),
        legend.title=element_text(size=10),
        #plot.background = element_rect(fill="white"),
        panel.background = element_rect(fill = "#FAF3F2"),
        plot.title = element_text(hjust = 0.5, size =10))+ 
  theme(legend.position = "none")
KSS116


set.seed(555)
PVT116=ggplot(CORR[ID==116], aes(x = Scenario, y = Median.RT, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Photopic illuminance [lx]",
       y = "PVT\nReaction time\n[ms]")+ 
  scale_x_discrete(labels = c("1", "10","100", "1000")) +
  #scale_y_continuous(limits = c(150,450), breaks = seq(150,450,100), labels = seq(150,450,100))+ 
  scale_y_continuous(limits = c(150,450), breaks = seq(150,450,100), labels = c("  150","  250","  350","  450"))+
  scale_color_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  scale_fill_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  theme_minimal()+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),  
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10),
        legend.text=element_text(size=10),
        #plot.background = element_rect(fill="lightgrey"),
        panel.background = element_rect(fill = "#F5EDED"),
        legend.title=element_text(size=10))+ 
  theme(legend.position = "none")
PVT116


set.seed(555)
ACC116=ggplot(CORR[ID==116], aes(x = Scenario, y = Accuracy, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Photopic illuminance [lx]",
       y = "N-back\nAccuracy")+ 
  scale_x_discrete(labels = c("1", "10","100", "1000")) +
  #scale_y_continuous(label = scales::percent, breaks = seq(0.70, 1.00, 0.10), limits = c(0.70, 1.00))+
  scale_y_continuous(breaks = seq(0.70, 1.00, 0.10), limits = c(0.70, 1.00), labels = c("   70%","   80%","    90%","   100%"))+  
  scale_color_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  scale_fill_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  theme_minimal()+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),  
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10),
        legend.text=element_text(size=10),
        #plot.background = element_rect(fill="white"),
        panel.background = element_rect(fill = "#FBF5F3"),
        legend.title=element_text(size=10))+ 
  theme(legend.position = "none")
ACC116



set.seed(555)
MD116=ggplot(CORR[ID==116], aes(x = Scenario, y = Mental.demand, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Photopic illuminance [lx]",
       y = "NASA-TLX\nMental demand")+ 
  scale_x_discrete(labels = c("1", "10","100", "1000")) +
  scale_y_continuous(breaks = seq(0,20,5), limits = c(0,20), labels = c("Very    \nlow    1", "", "10", "", "Very 20\nhigh     "))+
  scale_color_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  scale_fill_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  theme_minimal()+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),  
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10),#, hjust = -0.5
        legend.text=element_text(size=10),
        #plot.background = element_rect(fill="lightgrey"),
        panel.background = element_rect(fill = "#F5F3FA"),
        legend.title=element_text(size=10))+ 
  theme(legend.position = "none")
MD116



set.seed(555)
TD116=ggplot(CORR[ID==116], aes(x = Scenario, y = Temporal.demand, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Photopic illuminance [lx]",
       y = "NASA-TLX\nTemporal demand")+ 
  scale_x_discrete(labels = c("1", "10","100", "1000")) +
  scale_y_continuous(breaks = seq(0,20,5), limits = c(0,20), labels = c("Very    \nlow    1", "", "10", "", "Very 20\nhigh     "))+
  scale_color_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  scale_fill_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  theme_minimal()+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),  
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10),#, hjust = -0.25
        legend.text=element_text(size=10),
        #plot.background = element_rect(fill="white"),
        panel.background = element_rect(fill = "#ECEAF2"),
        legend.title=element_text(size=10))+ 
  theme(legend.position = "none")


set.seed(555)
PFC116=ggplot(CORR[ID==116], aes(x = Scenario, y = Success, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Photopic illuminance [lx]",
       y = "NASA-TLX\nPerformance")+ 
  scale_x_discrete(labels = c("1", "10","100", "1000")) +
  scale_y_continuous(breaks = seq(0,20,5), limits = c(0,20), labels = c("Per-    \nfect   1", "","10","", "Fail- 20\nure      ")) +
  scale_color_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  scale_fill_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  theme_minimal()+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),  
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10),#, hjust = -0.75
        legend.text=element_text(size=10),
        #plot.background = element_rect(fill="lightgrey"),
        panel.background = element_rect(fill = "#F0EEF4"),
        legend.title=element_text(size=10))+ 
  theme(legend.position = "none")
PFC116



set.seed(555)
TA116=ggplot(CORR[ID==116], aes(x = Scenario, y = MAS.comfort, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Photopic illuminance [lx]",
       y = "MAS\nTense arousal")+ 
  scale_x_discrete(labels = c("1", "10","100", "1000")) +
  scale_y_continuous(n.breaks = 3, limits = c(-5,+5), labels = c("   Calm\n-5", "0", "+5\n   Tense")) +
  scale_color_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  scale_fill_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  theme_minimal()+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),  
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10),
        legend.text=element_text(size=10),
        #plot.background = element_rect(fill="white"),
        panel.background = element_rect(fill = "#F3FAF5"),
        legend.title=element_text(size=10))+ 
  theme(legend.position = "none")
TA116



set.seed(555)
EA116=ggplot(CORR[ID==116], aes(x = Scenario, y = MAS.alertness, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Photopic illuminance [lx]",
       y = "MAS\nEnergetic arousal")+ 
  scale_x_discrete(labels = c("1", "10","100", "1000")) +
  scale_y_continuous(n.breaks = 3, limits = c(-5,+5), labels = c("Tired\n-5", "0", "En-  +5\nergetic ")) +
  scale_color_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  scale_fill_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  theme_minimal()+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),  
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10),
        legend.text=element_text(size=10),
        #plot.background = element_rect(fill="lightgrey"),
        panel.background = element_rect(fill = "#EBF4EF"),
        legend.title=element_text(size=10))+ 
  theme(legend.position = "none")
EA116


ggarrange(KSS116,NULL,NULL, PVT116,NULL,NULL, ACC116,NULL,NULL, MD116,NULL,NULL, TD116,NULL,NULL, PFC116,NULL,NULL, TA116,NULL,NULL, EA116,NULL,NULL, hjust = -0.1, vjust = 1.2, nrow =8, ncol =3, widths = c(1.33, 1,1), heights = c(1.15,1,1,1,1,1,1,1), common.legend = TRUE, font.label = list(size=10), legend ="none") 
ggsave(filename = "Supplementary_matrixP6.jpg",width = 21, height = 29, unit = "cm", dpi =350)