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


PVTNBACKNASATCExp2 = fread("data.csv")
PVTNBACKNASATCExp2 = PVTNBACKNASATCExp2[ID != 103]
PVTNBACKNASATCExp2 = PVTNBACKNASATCExp2[!is_outlier(PVTNBACKNASATCExp2$Mean.RT, coef = 1.5) & !is.na(PVTNBACKNASATCExp2$Mean.RT), ]
PVTNBACKNASATCExp2 = PVTNBACKNASATCExp2[!is_outlier(PVTNBACKNASATCExp2$Median.RT, coef = 1.5) & !is.na(PVTNBACKNASATCExp2$Median.RT), ]

summary_dtPVT = PVTNBACKNASATCExp2[, .(mean_RT = mean(Mean.RT, na.rm = TRUE),
                          sd_RT = sd(Mean.RT, na.rm = TRUE),
                          median_RT = median(Median.RT, na.rm = TRUE),
                          mean_Error = mean(Total.Errors, na.rm = TRUE),
                          sd_Error = sd(Total.Errors, na.rm = TRUE),
                          median_Error = median(Total.Errors, na.rm = TRUE)),by = .(Timeframe,Scenario)]



summary_dtPVT[, `:=` (
  Scenario = as.factor(Scenario),
  Timeframe = as.factor(Timeframe)
)]

by(summary_dtPVT$median_RT,summary_dtPVT$Scenario, shapiro.test)
by(summary_dtPVT$median_RT,summary_dtPVT$Scenario, shapiro.test)

model = aov(median_Error ~ Scenario, data = summary_dtPVT)
summary(model)

##### Code for Figures

(PVTmeansd = ggplot(summary_dtPVT, aes(x = Timeframe, y = mean_RT, group = Scenario, colour = Scenario))+
  geom_line()+
  geom_ribbon(aes(ymin = mean_RT - sd_RT, ymax =  mean_RT + sd_RT, fill = Scenario),alpha = 0.6, linewidth = 0.25)+
  scale_y_continuous(breaks = seq(150,450,50), limits = c(150,450), oob = scales::squish)+
  scale_x_discrete(labels = c("1","","","", "5","","","","", "10"))+
  labs(#title = "PVT Test",
       x = "Time point",
       y = "PVT Reaction time [ms]",
       color = "Scenario [lx]",
       fill = "Scenario [lx]")+
  facet_wrap(~Scenario, nrow = 1, labeller = as_labeller(c("1" = "1 lx", "10" = "10 lx", "100" = "100 lx", "1000" = "1000 lx")))+ #, scales = "free_y" 
  #  scale_color_manual(values = c("black","black","black","black")) +
  #scale_color_brewer(palette = "RdGy") + #BrBG RdGy Set1
  #scale_fill_brewer(palette = "RdGy") + #BrBG RdGy
  scale_color_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  scale_fill_manual(values = c("#9F7E2A","#BF9832","#DFB13B","#FFCA43")) +
  theme_minimal()+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),  
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        #axis.title.y = element_blank(),
       # strip.text = element_text(size = 10),
        strip.text = element_blank(),
        legend.text=element_text(size=10),
        legend.title=element_text(size=10))+ 
  theme(legend.position = "none"))
print(PVTmeansd)

my_comparisonsdtPVT = list(c("1 lux", "10 lux"),c("1 lux", "100 lux"),c("1 lux", "1000 lux"),c("10 lux", "100 lux"),c("10 lux", "1000 lux"), c("100 lux", "1000 lux"))

set.seed(555)
(PVTMedianRTAll=ggplot(summary_dtPVT, aes(x = Scenario, y = median_RT, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(#title = "PVT Test",
       x = "Photopic illuminance [lx]",
       y = "PVT Reaction time [ms]",
       color = "Scenario [lx]",
       fill = "Scenario [lx]")+ 
  stat_pwc(hide.ns = TRUE, label = "p.adj.signif", group.by = "x.var", method = "tukey_hsd", y.position = 300, step.increase = 0.45, p.adjust.method = "bonferroni", parse = FALSE)+
  scale_x_discrete(labels = c("1", "10","100", "1000")) +
  scale_y_continuous(limits = c(150,450), breaks = seq(150,450,50), labels = seq(150,450,50))+ 
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
print(PVTMedianRTAll)


ggarrange(NULL,PVTMedianRTAll, NULL, PVTmeansd, hjust = -0.1, vjust = -0.5, nrow =4, common.legend = TRUE, legend = "bottom",labels = c("","A", "", "B"), heights = c(0.1,2.1,0.05, 1), font.label = list(size=10)) 
ggsave(filename = "Figure3-PVT.jpg",width = 10.5, height = 16.5, unit = "cm", dpi =350)



##### Linear Mixed Model Analysis

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

formula = "Mean.RT ~ Scenario + I(Scenario^2) + Timeframe + Timeframe:Scenario + (1|ID)"
model = lmer(formula = formula, data = PVTNBACKNASATCExp2)
summary(model)
AIC(model)
BIC(model)

num_fixed_effects = length(fixef(model))
p_values = summary(model)$coefficients[, "Pr(>|t|)"]
p_values_corrected = p.adjust(p_values, method = "bonferroni", n = num_fixed_effects)
p_values_corrected
