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
PVTNBACKNASATCExp2 = PVTNBACKNASATCExp2[!is_outlier(PVTNBACKNASATCExp2$Accuracy, coef = 1.5) & !is.na(PVTNBACKNASATCExp2$Accuracy), ]


summary_dtNASAMASExp2 = PVTNBACKNASATCExp2[, .(mean_Accuracy = mean(Accuracy, na.rm = TRUE),
                                          sd_Accuracy = sd(Accuracy, na.rm = TRUE),
                                          median_Accuracy = median(Accuracy, na.rm = TRUE),
                                          mean_Correct.responses = mean(Means.correct.responses, na.rm = TRUE),
                                          sd_Correct.responses = sd(Means.correct.responses, na.rm = TRUE),
                                          median_Correct.responses = median(Medians.correct.responses, na.rm = TRUE)),
                                      by = .(Timeframe,Scenario)]



summary_dtNASAMASExp2[, `:=` (
  Scenario = as.factor(Scenario),
  Timeframe = as.factor(Timeframe)
)]


by(summary_dtNASAMASExp2$median_Accuracy,summary_dtNASAMASExp2$Scenario, shapiro.test)

model = aov(median_Accuracy ~ Scenario, data = summary_dtNASAMASExp2)
summary(model)



(Nbackmeansd = ggplot(summary_dtNASAMASExp2, aes(x = Timeframe, y = mean_Accuracy, group = Scenario, colour = Scenario))+
  geom_line(aes(x = Timeframe, y = mean_Accuracy))+
  geom_ribbon(aes(ymin = mean_Accuracy - sd_Accuracy, ymax = mean_Accuracy + sd_Accuracy, fill = Scenario),alpha = 0.6, linewidth = 0.25)+
  scale_y_continuous(breaks = seq(0.7,1,0.1), limits = c(0.7,1.02),label = scales::percent)+
  scale_x_discrete(labels = c("1","","","", "5","","","","", "10"))+
  labs(#title = "N-back Test",
       x = "Time point",
       y = "N-back Accuracy",
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
print(Nbackmeansd)

my_comparisonsdtNback = list(c("1 lux", "10 lux"),c("1 lux", "100 lux"),c("1 lux", "1000 lux"),c("10 lux", "100 lux"),c("10 lux", "1000 lux"), c("100 lux", "1000 lux"))


set.seed(555)
(NbackMedianAll=ggplot(summary_dtNASAMASExp2, aes(x = Scenario, y = median_Accuracy, group = Scenario, colour = Scenario, fill = Scenario)) +
  geom_jitter(alpha = 0.6, size = 1) +
  geom_boxplot(alpha = 0.4, linewidth = 0.25) +
  labs(#title = "N-back Test",
       x = "Photopic illuminance [lx]",
       y = "N-back Accuracy",
       color = "Scenario [lx]",
       fill = "Scenario [lx]")+
  stat_pwc(hide.ns = TRUE, label = "p.adj.signif", group.by = "x.var", method = "tukey_hsd", step.increase = 0.15, p.adjust.method = "bonferroni", parse = FALSE)+
  scale_y_continuous(label = scales::percent)+
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
print(NbackMedianAll)

ggarrange(NbackMedianAll, NULL, Nbackmeansd, hjust = -0.1, vjust = 1.2, nrow =3, common.legend = TRUE, legend = "bottom",labels = c("A", "B"), heights = c(2.1,0.01, 1), font.label = list(size=10))
ggsave(filename = "Figure4-NBACK.jpg",width = 10.5, height = 16.5, unit = "cm", dpi =350)



##############Linear Mixed Model Analysis

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

formula = "Accuracy ~ Scenario + I(Scenario^2) + Timeframe + Timeframe:Scenario + (1|ID)" 
model = lmer(formula = formula, data = PVTNBACKNASATCExp2)
summary(model)
AIC(model)
BIC(model)

num_fixed_effects <- length(fixef(model))
p_values <- summary(model)$coefficients[, "Pr(>|t|)"]
p_values_corrected <- p.adjust(p_values, method = "bonferroni", n = num_fixed_effects)
p_values_corrected



