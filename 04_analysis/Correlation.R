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
            !is_outlier(CORR$Accuracy, coef = 1.5) & !is.na(CORR$Accuracy) &
            !is_outlier(CORR$Mean.RT, coef = 1.5) & !is.na(CORR$Mean.RT) &
            !is_outlier(CORR$Median.RT, coef = 1.5) & !is.na(CORR$Median.RT)  
            ,]

CORR = CORR[, Scenario := fcase(Scenario == 1, 1.10,
                                      Scenario == 10, 10,
                                      Scenario == 100, 69.1,
                                      Scenario == 1000, 593)]

CORR[, `:=` (
  ID = as.numeric(ID),
  Mental.demand = as.numeric(Mental.demand),
  Temporal.demand = as.numeric(Temporal.demand),
  Success = as.numeric(Success),
  MAS.comfort.original = as.numeric(MAS.comfort.original),
  MAS.alertness.original = as.numeric(MAS.alertness.original),
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

jpeg(filename = "Supplementary_Corr-matrix.jpg", width = 10.5 * 300, height = 8.5 * 300, res = 300)

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
    y = "PVT - Reaction time [ms]",
    colour = "mEDI [lx]")+
  scale_color_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                     labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
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
    y = "KSS - Sleepiness",
    colour = "mEDI [lx]")+
  scale_color_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                     labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
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
    y = "KSS - Sleepiness",
    colour = "mEDI [lx]")+
  scale_color_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                     labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
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
    y = "NASA-TLX - Mental demand",
    colour = "mEDI [lx]")+
  scale_color_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                     labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
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
    y = "NASA-TLX - Temporal demand",
    colour = "mEDI [lx]")+
  scale_color_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                     labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
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
    y = "NASA-TLX - Mental demand",
    colour = "mEDI [lx]")+
  scale_color_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                     labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
  theme_minimal()+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),  
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        legend.text=element_text(size=10),
        legend.title=element_text(size=10))+ 
  theme(legend.position = "bottom")
print(ACC.MD)
ggsave(filename = "Supplementary_Corr-Acc.MD.jpg",width = 10.5, height = 8.5, unit = "cm", dpi =350)

ACC.TD=ggplot(TwoCorr, mapping=aes(x=Accuracy, y=Temporal_demand, colour = Scenario))+
  geom_point(size = 2.5,na.rm = TRUE, alpha = 0.8)+
  labs(
    x = "N-back - Accuracy",
    y = "NASA-TLX - Temporal demand",
    colour = "mEDI [lx]")+
  scale_color_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                     labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
  theme_minimal()+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),  
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        legend.text=element_text(size=10),
        legend.title=element_text(size=10))+ 
  theme(legend.position = "bottom")
print(ACC.TD)
ggsave(filename = "Supplementary_Corr-Acc.TD.jpg",width = 10.5, height = 8.5, unit = "cm", dpi =350)







#########Matrix plot for each individual participant
#Page 1 P101-103
CORR = fread("data.csv")

CORR = CORR[!is_outlier(CORR$KSS, coef = 1.5) & !is.na(CORR$KSS) &
              !is_outlier(CORR$Mental.demand, coef = 1.5) & !is.na(CORR$Mental.demand) &
              !is_outlier(CORR$Temporal.demand, coef = 1.5) & !is.na(CORR$Temporal.demand) &
              !is_outlier(CORR$Success, coef = 1.5) & !is.na(CORR$Success) &
              !is_outlier(CORR$MAS.comfort.original, coef = 1.5) & !is.na(CORR$MAS.comfort.original) &
              !is_outlier(CORR$MAS.alertness.original, coef = 1.5) & !is.na(CORR$MAS.alertness.original) &
              !is_outlier(CORR$Accuracy, coef = 1.5) & !is.na(CORR$Accuracy) &
              !is_outlier(CORR$Median.RT, coef = 1.5) & !is.na(CORR$Median.RT)  
            ,]


CORR = CORR[, Scenario := fcase(Scenario == 1, 1.10,
                                Scenario == 10, 10,
                                Scenario == 100, 69.1,
                                Scenario == 1000, 593)]

CORR[, `:=` (
  ID = as.numeric(ID),
  Mental.demand = as.numeric(Mental.demand),
  Temporal.demand = as.numeric(Temporal.demand),
  Success = as.numeric(Success),
  MAS.comfort.original = as.numeric(MAS.comfort.original),
  MAS.alertness.original = as.numeric(MAS.alertness.original),
  Accuracy = as.numeric(Accuracy),
  Median.RT = as.numeric(Median.RT),
  KSS = as.numeric(KSS),
  Scenario = as.factor(Scenario)
)]

for(participant in unique(CORR$ID)){
  data_ID = CORR[ID == participant]
  
  set.seed(555)
  KSS=ggplot(data_ID, aes(x = Scenario, y = KSS, group = Scenario, colour = Scenario, fill = Scenario)) +
    geom_jitter(alpha = 0.6, size = 1) +
    geom_boxplot(alpha = 0.4, linewidth = 0.25) +
    labs(x = "Melanopic EDI [lx]",
         y = "KSS\nSleepiness",
         color = "mEDI [lx]",
         fill = "mEDI [lx]")+ 
    scale_x_discrete(labels = c("1", "10","70", "595")) +
    scale_y_continuous(breaks = c(1, 5, 9), limits = c(1,9), labels = c("  Extr.  \nalert 1","5","  Extr. 9\nsleepy"))+
    scale_color_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                       labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
    scale_fill_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                      labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
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


set.seed(555)
KSS101=ggplot(CORR[ID==101], aes(x = Scenario, y = KSS, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Melanopic EDI [lx]",
       y = "KSS\nSleepiness",
       color = "mEDI [lx]",
       fill = "mEDI [lx]")+ 
  scale_x_discrete(labels = c("1", "10","70", "595")) +
  scale_y_continuous(breaks = c(1, 5, 9), limits = c(1,9), labels = c("  Extr.  \nalert 1","5","  Extr. 9\nsleepy"))+
  scale_color_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                     labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
  scale_fill_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                    labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
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
    labs(x = "Melanopic EDI [lx]",
         y = "PVT\nReaction time\n[ms]",
         color = "mEDI [lx]",
         fill = "mEDI [lx]")+ 
    scale_x_discrete(labels = c("1", "10","70", "595")) +
    scale_y_continuous(limits = c(150,450), breaks = seq(150,450,100), labels = c("  150","  250","  350","  450"))+
    scale_color_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                       labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
    scale_fill_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                      labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
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
  labs(x = "Melanopic EDI [lx]",
       y = "PVT\nReaction time\n[ms]",
       color = "mEDI [lx]",
       fill = "mEDI [lx]")+ 
  scale_x_discrete(labels = c("1", "10","70", "595")) +
  scale_y_continuous(limits = c(150,450), breaks = seq(150,450,100), labels = c("  150","  250","  350","  450"))+
  scale_color_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                     labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
  scale_fill_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                    labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
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
    labs(x = "Melanopic EDI [lx]",
         y = "N-back\nAccuracy",
         color = "mEDI [lx]",
         fill = "mEDI [lx]")+ 
    scale_x_discrete(labels = c("1", "10","70", "595")) +
    scale_y_continuous(breaks = seq(0.70, 1.00, 0.10), limits = c(0.70, 1.00), labels = c("  70%","   80%","   90%","   100%"))+  
    scale_color_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                       labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
    scale_fill_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                      labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
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
  labs(x = "Melanopic EDI [lx]",
       y = "N-back\nAccuracy",
       color = "mEDI [lx]",
       fill = "mEDI [lx]")+ 
  scale_x_discrete(labels = c("1", "10","70", "595")) +
  #scale_y_continuous(label = scales::percent, breaks = seq(0.70, 1.00, 0.10), limits = c(0.70, 1.00))+
  scale_y_continuous(breaks = seq(0.70, 1.00, 0.10), limits = c(0.70, 1.00), labels = c("   70%","   80%","    90%","   100%"))+  
  scale_color_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                     labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
  scale_fill_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                    labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
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
    labs(x = "Melanopic EDI [lx]",
         y = "NASA-TLX\nMental demand",
         color = "mEDI [lx]",
         fill = "mEDI [lx]")+ 
    scale_x_discrete(labels = c("1", "10","70", "595")) +
    scale_y_continuous(breaks = seq(0,20,5), limits = c(0,20), labels = c("Very    \nlow    1", "", "10", "", "Very 20\nhigh     "))+
    scale_color_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                       labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
    scale_fill_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                      labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
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
  labs(x = "Melanopic EDI [lx]",
       y = "NASA-TLX\nMental demand",
       color = "mEDI [lx]",
       fill = "mEDI [lx]")+ 
  scale_x_discrete(labels = c("1", "10","70", "595")) +
  scale_y_continuous(breaks = seq(0,20,5), limits = c(0,20), labels = c("Very    \nlow    1", "", "10", "", "Very 20\nhigh     "))+
  scale_color_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                     labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
  scale_fill_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                    labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
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
    labs(x = "Melanopic EDI [lx]",
         y = "NASA-TLX\nTemporal demand",
         color = "mEDI [lx]",
         fill = "mEDI [lx]")+ 
    scale_x_discrete(labels = c("1", "10","70", "595")) +
    scale_y_continuous(breaks = seq(0,20,5), limits = c(0,20), labels = c("Very    \nlow    1", "", "10", "", "Very 20\nhigh     "))+
    scale_color_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                       labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
    scale_fill_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                      labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
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
  labs(x = "Melanopic EDI [lx]",
       y = "NASA-TLX\nTemporal demand",
       color = "mEDI [lx]",
       fill = "mEDI [lx]")+ 
  scale_x_discrete(labels = c("1", "10","70", "595")) +
  scale_y_continuous(breaks = seq(0,20,5), limits = c(0,20), labels = c("Very    \nlow    1", "", "10", "", "Very 20\nhigh     "))+
  scale_color_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                     labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
  scale_fill_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                    labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
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
    labs(x = "Melanopic EDI [lx]",
         y = "NASA-TLX\nPerformance",
         color = "mEDI [lx]",
         fill = "mEDI [lx]")+ 
    scale_x_discrete(labels = c("1", "10","70", "595")) +
    scale_y_continuous(breaks = seq(0,20,5), limits = c(0,20), labels = c("Per-    \nfect   1", "","10","", "Fail- 20\nure      ")) +
    scale_color_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                       labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
    scale_fill_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                      labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
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
  labs(x = "Melanopic EDI [lx]",
       y = "NASA-TLX\nPerformance",
       color = "mEDI [lx]",
       fill = "mEDI [lx]")+ 
  scale_x_discrete(labels = c("1", "10","70", "595")) +
  scale_y_continuous(breaks = seq(0,20,5), limits = c(0,20), labels = c("Per-    \nfect   1", "","10","", "Fail- 20\nure      ")) +
  scale_color_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                     labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
  scale_fill_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                    labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
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
    labs(x = "Melanopic EDI [lx]",
         y = "MAS\nTense arousal",
         color = "mEDI [lx]",
         fill = "mEDI [lx]")+ 
    scale_x_discrete(labels = c("1", "10","70", "595")) +
    scale_y_continuous(n.breaks = 3, limits = c(-5,+5), labels = c("   Calm\n-5", "0", "+5\n   Tense")) +
    scale_color_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                       labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
    scale_fill_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                      labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
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
  labs(x = "Melanopic EDI [lx]",
       y = "MAS\nTense arousal",
       color = "mEDI [lx]",
       fill = "mEDI [lx]")+ 
  scale_x_discrete(labels = c("1", "10","70", "595")) +
  scale_y_continuous(n.breaks = 3, limits = c(-5,+5), labels = c("   Calm\n-5", "0", "+5\n   Tense")) +
  scale_color_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                     labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
  scale_fill_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                    labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
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
    labs(x = "Melanopic EDI [lx]",
         y = "MAS\nEnergetic arousal",
         color = "mEDI [lx]",
         fill = "mEDI [lx]")+ 
    scale_x_discrete(labels = c("1", "10","70", "595")) +
    scale_y_continuous(n.breaks = 3, limits = c(-5,+5), labels = c("Tired\n-5", "0", "En-  +5\nergetic ")) +
    scale_color_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                       labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
    scale_fill_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                      labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
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
  labs(x = "Melanopic EDI [lx]",
       y = "MAS\nEnergetic arousal",
       color = "mEDI [lx]",
       fill = "mEDI [lx]")+ 
  scale_x_discrete(labels = c("1", "10","70", "595")) +
  scale_y_continuous(n.breaks = 3, limits = c(-5,+5), labels = c("Tired\n-5", "0", "En-  +5\nergetic ")) +
  scale_color_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                     labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
  scale_fill_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                    labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
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


ggarrange(KSS101,KSS102,KSS104, PVT101,PVT102,PVT104, ACC101,ACC102,ACC104, MD101,MD102,MD104, TD101,TD102,TD104, PFC101,PFC102,PFC104, TA101,TA102,TA104, EA101,EA102,EA104, hjust = -0.1, vjust = 1.2, nrow =8, ncol =3, widths = c(1.33, 1,1), heights = c(1.15,1,1,1,1,1,1,1), common.legend = TRUE, font.label = list(size=10), legend ="none") 
ggsave(filename = "Supplementary_matrixP1.jpg",width = 21, height = 29, unit = "cm", dpi =350)



#########Matrix plot with all results
#Page 2 P105-107####################################################################################################################################################################################################################################################



set.seed(555)
KSS105=ggplot(CORR[ID==105], aes(x = Scenario, y = KSS, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Melanopic EDI [lx]",
       y = "KSS\nSleepiness",
       color = "mEDI [lx]",
       fill = "mEDI [lx]")+ 
  scale_x_discrete(labels = c("1", "10","70", "595")) +
  scale_y_continuous(breaks = c(1, 5, 9), limits = c(1,9), labels = c("  Extr.  \nalert 1","5","  Extr. 9\nsleepy"))+
  scale_color_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                     labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
  scale_fill_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                    labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
  theme_minimal()+
  ggtitle("ID 105")+
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
KSS105



set.seed(555)
PVT105=ggplot(CORR[ID==105], aes(x = Scenario, y = Median.RT, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Melanopic EDI [lx]",
       y = "PVT\nReaction time\n[ms]",
       color = "mEDI [lx]",
       fill = "mEDI [lx]")+ 
  scale_x_discrete(labels = c("1", "10","70", "595")) +
  scale_y_continuous(limits = c(150,450), breaks = seq(150,450,100), labels = c("  150","  250","  350","  450"))+
  scale_color_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                     labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
  scale_fill_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                    labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
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
PVT105



set.seed(555)
ACC105=ggplot(CORR[ID==105], aes(x = Scenario, y = Accuracy, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Melanopic EDI [lx]",
       y = "N-back\nAccuracy",
       color = "mEDI [lx]",
       fill = "mEDI [lx]")+ 
  scale_x_discrete(labels = c("1", "10","70", "595")) +
  scale_y_continuous(breaks = seq(0.70, 1.00, 0.10), limits = c(0.70, 1.00), labels = c("   70%","   80%","    90%","   100%"))+  
  scale_color_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                     labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
  scale_fill_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                    labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
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
ACC105



set.seed(555)
MD105=ggplot(CORR[ID==105], aes(x = Scenario, y = Mental.demand, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Melanopic EDI [lx]",
       y = "NASA-TLX\nMental demand",
       color = "mEDI [lx]",
       fill = "mEDI [lx]")+ 
  scale_x_discrete(labels = c("1", "10","70", "595")) +
  scale_y_continuous(breaks = seq(0,20,5), limits = c(0,20), labels = c("Very    \nlow    1", "", "10", "", "Very 20\nhigh     "))+
  scale_color_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                     labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
  scale_fill_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                    labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
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
MD105




set.seed(555)
TD105=ggplot(CORR[ID==105], aes(x = Scenario, y = Temporal.demand, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Melanopic EDI [lx]",
       y = "NASA-TLX\nTemporal demand",
       color = "mEDI [lx]",
       fill = "mEDI [lx]")+ 
  scale_x_discrete(labels = c("1", "10","70", "595")) +
  scale_y_continuous(breaks = seq(0,20,5), limits = c(0,20), labels = c("Very    \nlow    1", "", "10", "", "Very 20\nhigh     "))+
  scale_color_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                     labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
  scale_fill_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                    labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
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
PFC105=ggplot(CORR[ID==105], aes(x = Scenario, y = Success, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Melanopic EDI [lx]",
       y = "NASA-TLX\nPerformance",
       color = "mEDI [lx]",
       fill = "mEDI [lx]")+ 
  scale_x_discrete(labels = c("1", "10","70", "595")) +
  scale_y_continuous(breaks = seq(0,20,5), limits = c(0,20), labels = c("Per-    \nfect   1", "","10","", "Fail- 20\nure      ")) +
  scale_color_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                     labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
  scale_fill_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                    labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
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
PFC105




set.seed(555)
TA105=ggplot(CORR[ID==105], aes(x = Scenario, y = MAS.comfort, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Melanopic EDI [lx]",
       y = "MAS\nTense arousal",
       color = "mEDI [lx]",
       fill = "mEDI [lx]")+ 
  scale_x_discrete(labels = c("1", "10","70", "595")) +
  scale_y_continuous(n.breaks = 3, limits = c(-5,+5), labels = c("   Calm\n-5", "0", "+5\n   Tense")) +
  scale_color_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                     labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
  scale_fill_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                    labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
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
TA105



set.seed(555)
EA105=ggplot(CORR[ID==105], aes(x = Scenario, y = MAS.alertness, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Melanopic EDI [lx]",
       y = "MAS\nEnergetic arousal",
       color = "mEDI [lx]",
       fill = "mEDI [lx]")+ 
  scale_x_discrete(labels = c("1", "10","70", "595")) +
  scale_y_continuous(n.breaks = 3, limits = c(-5,+5), labels = c("Tired\n-5", "0", "En-  +5\nergetic ")) +
  scale_color_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                     labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
  scale_fill_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                    labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
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
EA105


ggarrange(KSS105,KSS106,KSS107, PVT105,PVT106,PVT107, ACC105,ACC106,ACC107, MD105,MD106,MD107, TD105,TD106,TD107, PFC105,PFC106,PFC107, TA105,TA106,TA107, EA105,EA106,EA107, hjust = -0.1, vjust = 1.2, nrow =8, ncol =3, widths = c(1.33, 1,1), heights = c(1.15,1,1,1,1,1,1,1), common.legend = TRUE, font.label = list(size=10), legend ="none") 
ggsave(filename = "Supplementary_matrixP2.jpg",width = 21, height = 29, unit = "cm", dpi =350)



#########Matrix plot with all results
#Page 3 P108-110####################################################################################################################################################################################################################################################


set.seed(555)
KSS108=ggplot(CORR[ID==108], aes(x = Scenario, y = KSS, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Melanopic EDI [lx]",
       y = "KSS\nSleepiness",
       color = "mEDI [lx]",
       fill = "mEDI [lx]")+ 
  scale_x_discrete(labels = c("1", "10","70", "595")) +
  scale_y_continuous(breaks = c(1, 5, 9), limits = c(1,9), labels = c("  Extr.  \nalert 1","5","  Extr. 9\nsleepy"))+
  scale_color_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                     labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
  scale_fill_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                    labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
  theme_minimal()+
  ggtitle("ID 108")+
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
KSS108


set.seed(555)
PVT108=ggplot(CORR[ID==108], aes(x = Scenario, y = Median.RT, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Melanopic EDI [lx]",
       y = "PVT\nReaction time\n[ms]",
       color = "mEDI [lx]",
       fill = "mEDI [lx]")+ 
  scale_x_discrete(labels = c("1", "10","70", "595")) +
  scale_y_continuous(limits = c(150,450), breaks = seq(150,450,100), labels = c("  150","  250","  350","  450"))+
  scale_color_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                     labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
  scale_fill_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                    labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
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
PVT108


set.seed(555)
ACC108=ggplot(CORR[ID==108], aes(x = Scenario, y = Accuracy, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Melanopic EDI [lx]",
       y = "N-back\nAccuracy",
       color = "mEDI [lx]",
       fill = "mEDI [lx]")+ 
  scale_x_discrete(labels = c("1", "10","70", "595")) +
  scale_y_continuous(breaks = seq(0.70, 1.00, 0.10), limits = c(0.70, 1.00), labels = c("   70%","   80%","    90%","   100%"))+  
  scale_color_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                     labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
  scale_fill_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                    labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
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
ACC108



set.seed(555)
MD108=ggplot(CORR[ID==108], aes(x = Scenario, y = Mental.demand, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Melanopic EDI [lx]",
       y = "NASA-TLX\nMental demand",
       color = "mEDI [lx]",
       fill = "mEDI [lx]")+ 
  scale_x_discrete(labels = c("1", "10","70", "595")) +
  scale_y_continuous(breaks = seq(0,20,5), limits = c(0,20), labels = c("Very    \nlow    1", "", "10", "", "Very 20\nhigh     "))+
  scale_color_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                     labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
  scale_fill_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                    labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
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
MD108



set.seed(555)
TD108=ggplot(CORR[ID==108], aes(x = Scenario, y = Temporal.demand, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Melanopic EDI [lx]",
       y = "NASA-TLX\nTemporal demand",
       color = "mEDI [lx]",
       fill = "mEDI [lx]")+ 
  scale_x_discrete(labels = c("1", "10","70", "595")) +
  scale_y_continuous(breaks = seq(0,20,5), limits = c(0,20), labels = c("Very    \nlow    1", "", "10", "", "Very 20\nhigh     "))+
  scale_color_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                     labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
  scale_fill_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                    labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
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
PFC108=ggplot(CORR[ID==108], aes(x = Scenario, y = Success, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Melanopic EDI [lx]",
       y = "NASA-TLX\nPerformance",
       color = "mEDI [lx]",
       fill = "mEDI [lx]")+ 
  scale_x_discrete(labels = c("1", "10","70", "595")) +
  scale_y_continuous(breaks = seq(0,20,5), limits = c(0,20), labels = c("Per-    \nfect   1", "","10","", "Fail- 20\nure      ")) +
  scale_color_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                     labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
  scale_fill_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                    labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
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
PFC108



set.seed(555)
TA108=ggplot(CORR[ID==108], aes(x = Scenario, y = MAS.comfort, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Melanopic EDI [lx]",
       y = "MAS\nTense arousal",
       color = "mEDI [lx]",
       fill = "mEDI [lx]")+ 
  scale_x_discrete(labels = c("1", "10","70", "595")) +
  scale_y_continuous(n.breaks = 3, limits = c(-5,+5), labels = c("   Calm\n-5", "0", "+5\n   Tense")) +
  scale_color_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                     labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
  scale_fill_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                    labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
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
TA108



set.seed(555)
EA108=ggplot(CORR[ID==108], aes(x = Scenario, y = MAS.alertness, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Melanopic EDI [lx]",
       y = "MAS\nEnergetic arousal",
       color = "mEDI [lx]",
       fill = "mEDI [lx]")+ 
  scale_x_discrete(labels = c("1", "10","70", "595")) +
  scale_y_continuous(n.breaks = 3, limits = c(-5,+5), labels = c("Tired\n-5", "0", "En-  +5\nergetic ")) +
  scale_color_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                     labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
  scale_fill_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                    labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
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
EA108


ggarrange(KSS108,KSS109,KSS110, PVT108,PVT109,PVT110, ACC108,ACC109,ACC110, MD108,MD109,MD110, TD108,TD109,TD110, PFC108,PFC109,PFC110, TA108,TA109,TA110, EA108,EA109,EA110, hjust = -0.1, vjust = 1.2, nrow =8, ncol =3, widths = c(1.33, 1,1), heights = c(1.15,1,1,1,1,1,1,1), common.legend = TRUE, font.label = list(size=10), legend ="none") 
ggsave(filename = "Supplementary_matrixP3.jpg",width = 21, height = 29, unit = "cm", dpi =350)



#########Matrix plot with all results
#Page 4 P111-113####################################################################################################################################################################################################################################################


set.seed(555)
KSS111=ggplot(CORR[ID==111], aes(x = Scenario, y = KSS, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Melanopic EDI [lx]",
       y = "KSS\nSleepiness",
       color = "mEDI [lx]",
       fill = "mEDI [lx]")+ 
  scale_x_discrete(labels = c("1", "10","70", "595")) +
  scale_y_continuous(breaks = c(1, 5, 9), limits = c(1,9), labels = c("  Extr.  \nalert 1","5","  Extr. 9\nsleepy"))+
  scale_color_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                     labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
  scale_fill_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                    labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
  theme_minimal()+
  ggtitle("ID 111")+
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
KSS111


set.seed(555)
PVT111=ggplot(CORR[ID==111], aes(x = Scenario, y = Median.RT, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Melanopic EDI [lx]",
       y = "PVT\nReaction time\n[ms]",
       color = "mEDI [lx]",
       fill = "mEDI [lx]")+ 
  scale_x_discrete(labels = c("1", "10","70", "595")) +
  scale_y_continuous(limits = c(150,450), breaks = seq(150,450,100), labels = c("  150","  250","  350","  450"))+
  scale_color_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                     labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
  scale_fill_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                    labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
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
PVT111


set.seed(555)
ACC111=ggplot(CORR[ID==111], aes(x = Scenario, y = Accuracy, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Melanopic EDI [lx]",
       y = "N-back\nAccuracy",
       color = "mEDI [lx]",
       fill = "mEDI [lx]")+ 
  scale_x_discrete(labels = c("1", "10","70", "595")) +
  scale_y_continuous(breaks = seq(0.70, 1.00, 0.10), limits = c(0.70, 1.00), labels = c("   70%","   80%","    90%","   100%"))+  
  scale_color_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                     labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
  scale_fill_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                    labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
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
ACC111



set.seed(555)
MD111=ggplot(CORR[ID==111], aes(x = Scenario, y = Mental.demand, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Melanopic EDI [lx]",
       y = "NASA-TLX\nMental demand",
       color = "mEDI [lx]",
       fill = "mEDI [lx]")+ 
  scale_x_discrete(labels = c("1", "10","70", "595")) +
  scale_y_continuous(breaks = seq(0,20,5), limits = c(0,20), labels = c("Very    \nlow    1", "", "10", "", "Very 20\nhigh     "))+
  scale_color_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                     labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
  scale_fill_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                    labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
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
MD111



set.seed(555)
TD111=ggplot(CORR[ID==111], aes(x = Scenario, y = Temporal.demand, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Melanopic EDI [lx]",
       y = "NASA-TLX\nTemporal demand",
       color = "mEDI [lx]",
       fill = "mEDI [lx]")+ 
  scale_x_discrete(labels = c("1", "10","70", "595")) +
  scale_y_continuous(breaks = seq(0,20,5), limits = c(0,20), labels = c("Very    \nlow    1", "", "10", "", "Very 20\nhigh     "))+
  scale_color_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                     labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
  scale_fill_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                    labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
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
PFC111=ggplot(CORR[ID==111], aes(x = Scenario, y = Success, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Melanopic EDI [lx]",
       y = "NASA-TLX\nPerformance",
       color = "mEDI [lx]",
       fill = "mEDI [lx]")+ 
  scale_x_discrete(labels = c("1", "10","70", "595")) +
  scale_y_continuous(breaks = seq(0,20,5), limits = c(0,20), labels = c("Per-    \nfect   1", "","10","", "Fail- 20\nure      ")) +
  scale_color_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                     labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
  scale_fill_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                    labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
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
PFC111



set.seed(555)
TA111=ggplot(CORR[ID==111], aes(x = Scenario, y = MAS.comfort, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Melanopic EDI [lx]",
       y = "MAS\nTense arousal",
       color = "mEDI [lx]",
       fill = "mEDI [lx]")+ 
  scale_x_discrete(labels = c("1", "10","70", "595")) +
  scale_y_continuous(n.breaks = 3, limits = c(-5,+5), labels = c("   Calm\n-5", "0", "+5\n   Tense")) +
  scale_color_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                     labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
  scale_fill_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                    labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
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
TA111



set.seed(555)
EA111=ggplot(CORR[ID==111], aes(x = Scenario, y = MAS.alertness, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Melanopic EDI [lx]",
       y = "MAS\nEnergetic arousal",
       color = "mEDI [lx]",
       fill = "mEDI [lx]")+ 
  scale_x_discrete(labels = c("1", "10","70", "595")) +
  scale_y_continuous(n.breaks = 3, limits = c(-5,+5), labels = c("Tired\n-5", "0", "En-  +5\nergetic ")) +
  scale_color_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                     labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
  scale_fill_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                    labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
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
EA111


ggarrange(KSS111,KSS112,KSS113, PVT111,PVT112,PVT113, ACC111,ACC112,ACC113, MD111,MD112,MD113, TD111,TD112,TD113, PFC111,PFC112,PFC113, TA111,TA112,TA113, EA111,EA112,EA113, hjust = -0.1, vjust = 1.2, nrow =8, ncol =3, widths = c(1.33, 1,1), heights = c(1.15,1,1,1,1,1,1,1), common.legend = TRUE, font.label = list(size=10), legend ="none") 
ggsave(filename = "Supplementary_matrixP4.jpg",width = 21, height = 29, unit = "cm", dpi =350)



#########Matrix plot with all results
#Page 5 P114-116####################################################################################################################################################################################################################################################


set.seed(555)
KSS114=ggplot(CORR[ID==114], aes(x = Scenario, y = KSS, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Melanopic EDI [lx]",
       y = "KSS\nSleepiness",
       color = "mEDI [lx]",
       fill = "mEDI [lx]")+ 
  scale_x_discrete(labels = c("1", "10","70", "595")) +
  scale_y_continuous(breaks = c(1, 5, 9), limits = c(1,9), labels = c("  Extr.  \nalert 1","5","  Extr. 9\nsleepy"))+
  scale_color_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                     labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
  scale_fill_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                    labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
  theme_minimal()+
  ggtitle("ID 114")+
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
KSS114


set.seed(555)
PVT114=ggplot(CORR[ID==114], aes(x = Scenario, y = Median.RT, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Melanopic EDI [lx]",
       y = "PVT\nReaction time\n[ms]",
       color = "mEDI [lx]",
       fill = "mEDI [lx]")+ 
  scale_x_discrete(labels = c("1", "10","70", "595")) +
  scale_y_continuous(limits = c(150,450), breaks = seq(150,450,100), labels = c("  150","  250","  350","  450"))+
  scale_color_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                     labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
  scale_fill_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                    labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
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
PVT114


set.seed(555)
ACC114=ggplot(CORR[ID==114], aes(x = Scenario, y = Accuracy, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Melanopic EDI [lx]",
       y = "N-back\nAccuracy",
       color = "mEDI [lx]",
       fill = "mEDI [lx]")+ 
  scale_x_discrete(labels = c("1", "10","70", "595")) +
  scale_y_continuous(breaks = seq(0.70, 1.00, 0.10), limits = c(0.70, 1.00), labels = c("   70%","   80%","    90%","   100%"))+  
  scale_color_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                     labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
  scale_fill_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                    labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
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
ACC114



set.seed(555)
MD114=ggplot(CORR[ID==114], aes(x = Scenario, y = Mental.demand, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Melanopic EDI [lx]",
       y = "NASA-TLX\nMental demand",
       color = "mEDI [lx]",
       fill = "mEDI [lx]")+ 
  scale_x_discrete(labels = c("1", "10","70", "595")) +
  scale_y_continuous(breaks = seq(0,20,5), limits = c(0,20), labels = c("Very    \nlow    1", "", "10", "", "Very 20\nhigh     "))+
  scale_color_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                     labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
  scale_fill_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                    labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
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
MD114



set.seed(555)
TD114=ggplot(CORR[ID==114], aes(x = Scenario, y = Temporal.demand, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Melanopic EDI [lx]",
       y = "NASA-TLX\nTemporal demand",
       color = "mEDI [lx]",
       fill = "mEDI [lx]")+ 
  scale_x_discrete(labels = c("1", "10","70", "595")) +
  scale_y_continuous(breaks = seq(0,20,5), limits = c(0,20), labels = c("Very    \nlow    1", "", "10", "", "Very 20\nhigh     "))+
  scale_color_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                     labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
  scale_fill_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                    labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
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
PFC114=ggplot(CORR[ID==114], aes(x = Scenario, y = Success, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Melanopic EDI [lx]",
       y = "NASA-TLX\nPerformance",
       color = "mEDI [lx]",
       fill = "mEDI [lx]")+ 
  scale_x_discrete(labels = c("1", "10","70", "595")) +
  scale_y_continuous(breaks = seq(0,20,5), limits = c(0,20), labels = c("Per-    \nfect   1", "","10","", "Fail- 20\nure      ")) +
  scale_color_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                     labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
  scale_fill_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                    labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
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
PFC114



set.seed(555)
TA114=ggplot(CORR[ID==114], aes(x = Scenario, y = MAS.comfort, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Melanopic EDI [lx]",
       y = "MAS\nTense arousal",
       color = "mEDI [lx]",
       fill = "mEDI [lx]")+ 
  scale_x_discrete(labels = c("1", "10","70", "595")) +
  scale_y_continuous(n.breaks = 3, limits = c(-5,+5), labels = c("   Calm\n-5", "0", "+5\n   Tense")) +
  scale_color_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                     labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
  scale_fill_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                    labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
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
TA114



set.seed(555)
EA114=ggplot(CORR[ID==114], aes(x = Scenario, y = MAS.alertness, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Melanopic EDI [lx]",
       y = "MAS\nEnergetic arousal",
       color = "mEDI [lx]",
       fill = "mEDI [lx]")+ 
  scale_x_discrete(labels = c("1", "10","70", "595")) +
  scale_y_continuous(n.breaks = 3, limits = c(-5,+5), labels = c("Tired\n-5", "0", "En-  +5\nergetic ")) +
  scale_color_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                     labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
  scale_fill_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                    labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
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
EA114


ggarrange(KSS114,KSS115,KSS116, PVT114,PVT115,PVT116, ACC114,ACC115,ACC116, MD114,MD115,MD116, TD114,TD115,TD116, PFC114,PFC115,PFC116, TA114,TA115,TA116, EA114,EA115,EA116, hjust = -0.1, vjust = 1.2, nrow =8, ncol =3, widths = c(1.33, 1,1), heights = c(1.15,1,1,1,1,1,1,1), common.legend = TRUE, font.label = list(size=10), legend ="none") 
ggsave(filename = "Supplementary_matrixP5.jpg",width = 21, height = 29, unit = "cm", dpi =350)









#########Matrix plot with all results
#Page 6 P116####################################################################################################################################################################################################################################################


set.seed(555)
KSS116=ggplot(CORR[ID==116], aes(x = Scenario, y = KSS, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(x = "Melanopic EDI [lx]",
       y = "KSS\nSleepiness",
       color = "mEDI [lx]",
       fill = "mEDI [lx]")+ 
  scale_x_discrete(labels = c("1", "10","70", "595")) +
  scale_y_continuous(breaks = c(1, 5, 9), limits = c(1,9), labels = c("  Extr.  \nalert 1","5","  Extr. 9\nsleepy"))+
  scale_color_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                     labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
  scale_fill_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                    labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
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
  labs(x = "Melanopic EDI [lx]",
       y = "PVT\nReaction time\n[ms]",
       color = "mEDI [lx]",
       fill = "mEDI [lx]")+ 
  scale_x_discrete(labels = c("1", "10","70", "595")) +
  scale_y_continuous(limits = c(150,450), breaks = seq(150,450,100), labels = c("  150","  250","  350","  450"))+
  scale_color_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                     labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
  scale_fill_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                    labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
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
  labs(x = "Melanopic EDI [lx]",
       y = "N-back\nAccuracy",
       color = "mEDI [lx]",
       fill = "mEDI [lx]")+ 
  scale_x_discrete(labels = c("1", "10","70", "595")) +
  scale_y_continuous(breaks = seq(0.70, 1.00, 0.10), limits = c(0.70, 1.00), labels = c("   70%","   80%","    90%","   100%"))+  
  scale_color_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                     labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
  scale_fill_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                    labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
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
  labs(x = "Melanopic EDI [lx]",
       y = "NASA-TLX\nMental demand",
       color = "mEDI [lx]",
       fill = "mEDI [lx]")+ 
  scale_x_discrete(labels = c("1", "10","70", "595")) +
  scale_y_continuous(breaks = seq(0,20,5), limits = c(0,20), labels = c("Very    \nlow    1", "", "10", "", "Very 20\nhigh     "))+
  scale_color_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                     labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
  scale_fill_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                    labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
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
  labs(x = "Melanopic EDI [lx]",
       y = "NASA-TLX\nTemporal demand",
       color = "mEDI [lx]",
       fill = "mEDI [lx]")+ 
  scale_x_discrete(labels = c("1", "10","70", "595")) +
  scale_y_continuous(breaks = seq(0,20,5), limits = c(0,20), labels = c("Very    \nlow    1", "", "10", "", "Very 20\nhigh     "))+
  scale_color_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                     labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
  scale_fill_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                    labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
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
  labs(x = "Melanopic EDI [lx]",
       y = "NASA-TLX\nPerformance",
       color = "mEDI [lx]",
       fill = "mEDI [lx]")+ 
  scale_x_discrete(labels = c("1", "10","70", "595")) +
  scale_y_continuous(breaks = seq(0,20,5), limits = c(0,20), labels = c("Per-    \nfect   1", "","10","", "Fail- 20\nure      ")) +
  scale_color_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                     labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
  scale_fill_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                    labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
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
  labs(x = "Melanopic EDI [lx]",
       y = "MAS\nTense arousal",
       color = "mEDI [lx]",
       fill = "mEDI [lx]")+ 
  scale_x_discrete(labels = c("1", "10","70", "595")) +
  scale_y_continuous(n.breaks = 3, limits = c(-5,+5), labels = c("   Calm\n-5", "0", "+5\n   Tense")) +
  scale_color_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                     labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
  scale_fill_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                    labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
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
  labs(x = "Melanopic EDI [lx]",
       y = "MAS\nEnergetic arousal",
       color = "mEDI [lx]",
       fill = "mEDI [lx]")+ 
  scale_x_discrete(labels = c("1", "10","70", "595")) +
  scale_y_continuous(n.breaks = 3, limits = c(-5,+5), labels = c("Tired\n-5", "0", "En-  +5\nergetic ")) +
  scale_color_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                     labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
  scale_fill_manual(values = c("1.1" = "#9F7E2A", "10" = "#BF9832", "69.1" = "#DFB13B", "593" = "#FFCA43"),
                    labels = c("1.1" = "1", "10" = "10", "69.1" = "70", "593" = "595")) +  
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