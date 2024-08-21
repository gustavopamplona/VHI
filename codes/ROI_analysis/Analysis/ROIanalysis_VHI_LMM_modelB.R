#############
# ROI analysis VHI study - stimulation second half - contrast varying with factors stimulation, visibility, and illusion

rm(list = ls())

pvals_total = c()

for (roi in 1:21) {
  
  # roi = 5
  
  pathFolder = "D:/VHI/Analysis/ROI_analysis/Tables/ModelB/1stLevel_modelB3/"
  tableFile = list.files(path = pathFolder)
  
  #library(readxl)
  data<-read_excel(paste(pathFolder,tableFile[roi], sep = ""))
  tableFile[roi]
  
  df<-subset(data,!is.na(value))
  # df = df[df$time == "secondHalf",]
  
  df$subj = as.factor(df$subj)
  df$run = as.factor(df$run)
  df$stim = as.factor(df$stim)
  df$vis = as.factor(df$vis)
  df$time = as.factor(df$time)
  df$illusion = as.factor(df$illusion)
  
  #library(lme4)
  #library(lmerTest) # for the p-values
  
  # m01 <-lmer(value ~ stim*vis*illusion + (1|subj) + (1|run), data=df) # crossed model, runs are the same measurement and are not nested in subj
  # # https://stats.stackexchange.com/questions/412628/random-effects-in-repeated-measures-design-using-lme/412989#412989
  # m02 <-lmer(value ~ stim*vis*illusion + (1|subj) + (1|stim) + (1|vis) + (1|illusion) + (1|subj:stim) + (1|subj:vis) + (1|subj:illusion), data=df)
  # m03 <-lmer(value ~ stim*vis*illusion + (1|subj) + (1|vis) + (1|subj:vis), data=df)
  m04 <-lmer(value ~ stim*vis*illusion*time + (1|subj), data=df)
  #m04 <-lmer(gsr_beta ~ stim*vis*illusion + (1|subj), data=df)
  res01 = anova(m04)
  print(res01)
  
  pvals_test = res01$`Pr(>F)`
  
  pvals_total = cbind(pvals_total,pvals_test)
}

pvals_adjusted = c()

for (row in 1:15) {
  pvals_temp = p.adjust(pvals_total[row,], method = "fdr")
  pvals_adjusted = rbind(pvals_adjusted,pvals_temp)
}

#library(ggplot2)
df$vis = factor(df$vis,levels = c("low","mid","high"))
levels(df$illusion) <- c("no Illusion", "Illusion")
ggplot(df, aes(x=stim, y=value, fill=vis)) +
  geom_hline(yintercept=0,linetype="dashed") +
  geom_boxplot()+
  facet_wrap(~illusion, ncol = 2)
# facet_grid(illusion ~ .)

#library(MuMIn)
r.squaredGLMM(m04)

#library(FSA)  
df1<-subset(df,df$illusion=='Illusion')
df0<-subset(df,df$illusion=='no Illusion')
Summarize(value ~ vis+stim,data=df1,digits=4)
Summarize(value ~ vis+stim,data=df0,digits=4)
Summarize(value ~ stim+vis,data=df,digits=4)
Summarize(value ~ stim+illusion,data=df,digits=4)
Summarize(value ~ vis+illusion,data=df,digits=4)
Summarize(value ~ stim,data=df,digits=4)
Summarize(value ~ vis,data=df,digits=4)
Summarize(value ~ illusion,data=df,digits=4)

#library(emmeans)
emmeans(m04, pairwise ~ stim|vis|illusion, adjust = "sidak")
emmeans(m04, pairwise ~ vis|stim|illusion, adjust = "sidak")
emmeans(m04, pairwise ~ illusion|stim|vis, adjust = "sidak")
emmeans(m04, pairwise ~ vis|stim, adjust = "sidak")
emmeans(m04, pairwise ~ stim|vis, adjust = "sidak")
emmeans(m04, pairwise ~ illusion|stim, adjust = "sidak")
emmeans(m04, pairwise ~ stim|illusion, adjust = "sidak")
emmeans(m04, pairwise ~ illusion|vis, adjust = "sidak")
emmeans(m04, pairwise ~ vis|illusion, adjust = "sidak")
emmeans(m04, pairwise ~ vis, adjust = "sidak")
emmeans(m04, pairwise ~ stim, adjust = "sidak")
emmeans(m04, pairwise ~ illusion, adjust = "sidak")



ggplot(df, aes(y=value, fill=stim)) +
  geom_hline(yintercept=0,linetype="dashed") +
  geom_boxplot()

ggplot(df, aes(y=value, fill=vis)) +
  geom_hline(yintercept=0,linetype="dashed") +
  geom_boxplot()

ggplot(df, aes(y=value, fill=illusion)) +
  geom_hline(yintercept=0,linetype="dashed") +
  geom_boxplot()

ggplot(df, aes(x=stim, y=value, fill=vis)) +
  geom_hline(yintercept=0,linetype="dashed") +
  geom_boxplot()

ggplot(df, aes(x=illusion, y=value, fill=stim)) +
  geom_hline(yintercept=0,linetype="dashed") +
  geom_boxplot()

ggplot(df, aes(x=illusion, y=value, fill=vis)) +
  geom_hline(yintercept=0,linetype="dashed") +
  geom_boxplot()

ggplot(df, aes(x=illusion, y=value, fill=stim)) +
  geom_hline(yintercept=0,linetype="dashed") +
  geom_boxplot()
