library(data.table)
library(ggplot2)
library(patchwork)
library(plotly)
library(dplyr)
library(ggpubr) 
library(Hmisc)
library(lme4)
library(lmerTest)
library(rstatix)


dtNASAMASExp2 = fread("data.csv")
dtNASAMASExp2 = dtNASAMASExp2[ID != 103]
dtNASAMASExp2 = dtNASAMASExp2[!is_outlier(dtNASAMASExp2$Success, coef = 1.5) & !is.na(dtNASAMASExp2$Success), ]


dtNASAMASExp2 = fread("data.csv")

dtNASAMASExp2[, `:=` (
  Scenario = as.factor(Scenario),
  Timeframe = as.factor(Timeframe),
  ID = as.factor(ID),
  MAS.comfort = as.numeric(MAS.comfort),
  MAS.alertness = as.numeric(MAS.alertness),
  Temporal.demand = as.numeric(Temporal.demand)
  
)]


summary_QuestExp2 = dtNASAMASExp2[, .(mean_Mental.demand = mean(Mental.demand, na.rm = TRUE),
                                      sd_Mental.demand = sd(Mental.demand, na.rm = TRUE),
                                      median_Mental.demand = median(Mental.demand, na.rm = TRUE),
                                      mean_Temporal.demand = mean(Temporal.demand, na.rm = TRUE),
                                      sd_Temporal.demand = sd(Temporal.demand, na.rm = TRUE),
                                      median_Temporal.demand = median(Temporal.demand, na.rm = TRUE),
                                      mean_Success = mean(Success, na.rm = TRUE),
                                      sd_Success = sd(Success, na.rm = TRUE),
                                      median_Success = median(Success, na.rm = TRUE),
                                      mean_MAS.comfort = mean(MAS.comfort, na.rm = TRUE),
                                      sd_MAS.comfort = sd(MAS.comfort, na.rm = TRUE),
                                      median_MAS.comfort = median(MAS.comfort, na.rm = TRUE),
                                      mean_MAS.alertness = mean(MAS.alertness, na.rm = TRUE),
                                      sd_MAS.alertness = sd(MAS.alertness, na.rm = TRUE),
                                      median_MAS.alertness = median(MAS.alertness, na.rm = TRUE)),
                                  by = .(Timeframe,Scenario)]


by(summary_QuestExp2$median_Success,summary_QuestExp2$Scenario, shapiro.test)

model = aov(median_Success ~ Scenario, data = summary_QuestExp2)
summary(model)

##### Code for Figures

(Successmeansd = ggplot(summary_QuestExp2, aes(x = Timeframe, y = mean_Success, group = Scenario, colour = Scenario))+ #summary_QuestExp2[!is_outlier(summary_QuestExp2$mean_Success, coef= 1.5)]
  geom_line(aes(x = Timeframe, y = mean_Success))+
  geom_ribbon(aes(ymin = mean_Success - sd_Success, ymax = mean_Success + sd_Success, fill = Scenario),alpha = 0.6, linewidth = 0.25)+
  #scale_y_continuous(breaks = seq(1,20,7), limits = c(1,20), labels = c("1\nPerfect", "10", "20\nFailure")) +
  scale_y_continuous(breaks = seq(1,20,1), limits = c(1,20), labels = c("1\nPerfect","","","","","","","","", "10","","","","","","","","","", "20\nFailure"))+
  scale_x_discrete(labels = c("1","","","", "5","","","","", "10"))+
  labs(#title = "NASA-TLX Performance",
       x = "Time point",
       y = "NASA-TLX Performance",
       color = "Scenario [lx]",
       fill = "Scenario [lx]")+
  facet_wrap(~Scenario, nrow = 1, labeller = as_labeller(c("1" = "1 lx", "10" = "10 lx", "100" = "100 lx", "1000" = "1000 lx")))+
  scale_color_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  scale_fill_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  theme_minimal()+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),  
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        strip.text = element_blank(),
        legend.text=element_text(size=10),
        legend.title=element_text(size=10))+ 
  theme(legend.position = "none"))
print(Successmeansd)


my_comparisonsdtNback = list(c("1 lux", "10 lux"),c("1 lux", "100 lux"),c("1 lux", "1000 lux"),c("10 lux", "100 lux"),c("10 lux", "1000 lux"), c("100 lux", "1000 lux"))

set.seed(555)
(SuccessMedianAll=ggplot(summary_QuestExp2, aes(x = Scenario, y = median_Success, group = Scenario, colour = Scenario, fill = Scenario)) + #[!is_outlier(summary_QuestExp2$median_Success, coef= 1.5)]
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(#title = "NASA-TLX Performance",
       x = "Photopic illuminance [lx]",
       y = "NASA-TLX Performance",
       color = "Scenario [lx]",
       fill = "Scenario [lx]")+
  stat_pwc(hide.ns = TRUE, label = "p.adj.signif", y.position = 15, group.by = "x.var", method = "tukey_hsd", step.increase = 0.25, p.adjust.method = "bonferroni", parse = FALSE)+
  scale_y_continuous(breaks = seq(1,20,1), limits = c(1,20), labels = c("1\nPerfect","","","","","","","","", "10","","","","","","","","","", "20\nFailure"))+
  scale_x_discrete(labels = c("1", "10","100", "1000")) +
  scale_color_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  scale_fill_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  theme_minimal()+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),  
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        legend.text=element_text(size=10),
        legend.title=element_text(size=10))+ 
  theme(legend.position = "none"))
print(SuccessMedianAll)


ggarrange(SuccessMedianAll, NULL, Successmeansd, hjust = -0.1, vjust = 1.2, nrow =3, common.legend = TRUE, legend = "bottom",labels = c("A", "B"), heights = c(2.1,0.1, 1), font.label = list(size=10))
ggsave(filename = "Figure7-NASATLXPerf.jpg",width = 10.5, height = 16.5, unit = "cm", dpi =350)



##### Linear Mixed Model Analysis
PVTNBACKNASATCExp2 = fread("data.csv")
PVTNBACKNASATCExp2 = PVTNBACKNASATCExp2[ID != 103]
PVTNBACKNASATCExp2 = PVTNBACKNASATCExp2[!is_outlier(PVTNBACKNASATCExp2$Success, coef = 1.5) & !is.na(PVTNBACKNASATCExp2$Success), ]

PVTNBACKNASATCExp2[, `:=` (
  Scenario = as.factor(Scenario),               
  Timeframe = as.factor(Timeframe),
  ID = as.factor(ID),
  Means.correct.responses = as.numeric(Means.correct.responses),
  Medians.correct.responses = as.numeric(Medians.correct.responses),
  Accuracy = as.numeric(Accuracy),
  Mental.demand = as.numeric(Mental.demand),
  Temporal.demand = as.numeric(Temporal.demand),
  Success = as.numeric(Success),
  MAS.comfort.original = as.numeric(MAS.comfort.original),
  MAS.alertness.original = as.numeric(MAS.alertness.original),
  Mean.RT = as.numeric(Mean.RT),
  Median.RT = as.numeric(Median.RT),
  Total.Errors = as.numeric(Total.Errors),
  TSV = as.numeric(TSV),
  TCV = as.numeric(TCV),
  TPV = as.numeric(TPV)
)]


PVTNBACKNASATCExp2[, `:=` (
  Scenario = as.numeric(Scenario)
)]

PVTNBACKNASATCExp2[, `:=` (
  Scenario = as.numeric(log10(Scenario)), 
  Timeframe = as.numeric(Timeframe)
)]

formula = "Success ~ Scenario + I(Scenario^2) + Timeframe + Timeframe:Scenario + (1|ID)"
model = lmer(formula = formula, data = PVTNBACKNASATCExp2)
summary(model)
AIC(model)
BIC(model)

num_fixed_effects <- length(fixef(model))
p_values <- summary(model)$coefficients[, "Pr(>|t|)"]
p_values_corrected <- p.adjust(p_values, method = "bonferroni", n = num_fixed_effects)
p_values_corrected
