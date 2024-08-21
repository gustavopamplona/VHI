### Illusion indication ###
rm(list = ls())

# if(!require(readxl)){install.packages("readxl")}
# library(dplyr)
# library(lme4)
# if(!require(ggmosaic)){install.packages("ggmosaic")}
# library(ggplot)
# library(ggplot2)
# library(psych)
# library(emmeans)
# library(MuMIn)

data <- read_excel("C:/Gustavo/Dropbox/Postdoc/Project2 - Virtual-hand illusion/Codes/WorkaroundOrdinalRegression/table_subjectiveMeasures_VHI.xlsx")

data=data[!is.na(data$illusion_perceived),]

data=data %>%
  group_by(subj,stim,vis,run) %>%
  summarise(illusion_perceived = mean(illusion_perceived, na.rm=TRUE), .groups = "drop")

data$stim <- factor(data$stim)
data$subj <- factor(data$subj)
data$vis = factor(data$vis,levels = c("low","mid","high"))
data$vis = factor(data$vis,ordered = TRUE)
data$run = factor(data$run,levels = c("1","2","3","4"))

model2 = glmer(illusion_perceived ~ vis*stim*run + (1|subj),data = data, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
car::Anova(model2, type=2)

r2beta(model2, method = "kr")

# Mosaic plot
stim_names <- c('async' = "Async",'sync' = "Sync")
dodge <- position_dodge(width = .7)
data$stim <- factor(data$stim, levels = c("async", "sync"))
data$vis <- factor(data$vis, levels = c("low", "mid", "high"))
data$illusion_perceived <- factor(data$illusion_perceived)

sync_names <- as_labeller(c(`async` = "Async", `sync` = "Sync"))
vis_names <- as_labeller(c(`low` = "Low", `mid` = "Mid", `high` = "High"))
ggplot(data = data)+ 
  # mutate(vis = factor(vis, levels=c("low", "mid", "high"))) %>%
  geom_mosaic(aes(x = product(illusion_perceived,vis), fill = illusion_perceived))+
  theme_bw()+
  labs(x = "", y = "")+
  facet_grid(. ~stim)+
  facet_wrap(~stim, ncol = 2, labeller = sync_names)+
  scale_fill_manual(labels=c("No","Yes"),values = c("#ff3333", "#009933"))+
  # theme(axis.text.y = element_text(size = 20, color="black"))+
  theme(axis.text.y = element_blank())+
  theme(axis.text.x = element_text(size = 15, color="black"))+
  theme(axis.ticks = element_blank())+
  # theme(axis.title.y = element_text(size = 20, color="black"))+
  # theme(axis.title.x = element_text(size = 20, color="black"))+
  theme(strip.text.x = element_text(size = 15))+
  theme(panel.grid.major.x = element_blank())+
  theme(panel.grid.major.y = element_blank())+
  # scale_x_discrete(labels=c("low" = "Low", "mid" = "Mid", "high" = "High"))+
  # theme(legend.position="none")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill = guide_legend(reverse = TRUE))

# Save figure Color
path='D:/VHI/Analysis/SubjectiveMeasures/Figures';
ggsave("Illusion_indication.jpg", path = path, width = 17.5, height = 17.5, units = "cm", device='jpg', dpi=700)

ggplot(data = data)+ 
  # mutate(vis = factor(vis, levels=c("low", "mid", "high"))) %>%
  geom_mosaic(aes(x = product(illusion_perceived,vis), fill = illusion_perceived))+
  theme_bw()+
  labs(x = "", y = "")+
  facet_grid(. ~stim)+
  facet_wrap(~stim, ncol = 2, labeller = sync_names)+
  scale_fill_manual(labels=c("No","Yes"),values = c("lightgray", "darkgrey"))+
  # theme(axis.text.y = element_text(size = 20, color="black"))+
  theme(axis.text.y = element_blank())+
  theme(axis.text.x = element_text(size = 15, color="black"))+
  theme(axis.ticks = element_blank())+
  # theme(axis.title.y = element_text(size = 20, color="black"))+
  # theme(axis.title.x = element_text(size = 20, color="black"))+
  theme(strip.text.x = element_text(size = 15))+
  theme(panel.grid.major.x = element_blank())+
  theme(panel.grid.major.y = element_blank())+
  # scale_x_discrete(labels=c("low" = "Low", "mid" = "Mid", "high" = "High"))+
  # theme(legend.position="none")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  guides(fill = guide_legend(reverse = TRUE))

# Save figure BW
path='D:/VHI/Analysis/SubjectiveMeasures/Figures';
ggsave("Illusion_indication_BW.jpg", path = path, width = 17.5, height = 17.5, units = "cm", device='jpg', dpi=700)

# Results ANOVA
print("ANOVA")
print(car::Anova(model2, type=2))

# Descriptive statistics
print("DESCRIPTIVE STATISTICS - Vis")
tableVis <- table(data$illusion_perceived, data$vis)
print(prop.table(tableVis)*100)

print("DESCRIPTIVE STATISTICS - Stim")
tableStim <- table(data$illusion_perceived, data$stim)
print(prop.table(tableStim)*100)

print("DESCRIPTIVE STATISTICS - Run")
tableRun <- table(data$illusion_perceived, data$run)
print(prop.table(tableRun)*100)

# Eta-square
print("EFFECT SIZE")
print(r.squaredGLMM(model2))

# Post-hoc analysis
print("POST-HOC ANALYSIS - Vis")
emm1 = lsmeans(model2,pairwise ~ vis, adjust="sidak")
emm1
print("POST-HOC ANALYSIS - Stim")
emm2 = lsmeans(model2,pairwise ~ stim, adjust="sidak")
emm2
print("POST-HOC ANALYSIS - Run")
emm3 = lsmeans(model2,pairwise ~ run, adjust="sidak")
emm3

# Effect sizes
print("EFFECT SIZE - PAIRWISE COMPARISON - COHEN'S D - VIS")
eff_size(emm1, sigma=sigma(model2), edf = df.residual(model2))
print("EFFECT SIZE - PAIRWISE COMPARISON - COHEN'S D - STIM")
eff_size(emm2, sigma=sigma(model2), edf = df.residual(model2))
print("EFFECT SIZE - PAIRWISE COMPARISON - COHEN'S D - RUN")
eff_size(emm3, sigma=sigma(model2), edf = df.residual(model2))
