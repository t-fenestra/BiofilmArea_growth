library(tidyverse)
library(dplyr)
library(ggridges)
library(gridExtra)
library(stringr)
library(modelr)
library(broom)
options(na.action = na.warn)

AreaY<-read_delim(file='ResultTable_SliceY.txt',col_names = TRUE,delim=',')

#========================================================================#
# Prepare tibble, new colums with strain type, frame number
#========================================================================#
AreaY<-AreaY %>% 
  # new colums with strain
  separate(Experiment,c('Strain'),sep='_',remove=FALSE) %>%
  mutate(Strain=as.factor(Strain) )%>%
  # extract data frame
  mutate(frame=as.numeric(str_split(datafile,'_',simplify = TRUE)[,5]))
 
#========================================================================#
# Recalculate units frame in hour to the minutes
#========================================================================#
AreaY<-AreaY %>% 
  mutate(frame=frame)

#========================================================================#
# Filter data with strange output
#========================================================================#

######## choose only 15-first frames from 25
AreaY <- AreaY %>% filter(frame %in% seq(from=1,to=15,by=1)) # frame 1-15

######## eliminate dwss_1_03212019_FR,dwss_1_03232019_FR
AreaY<-AreaY %>% filter(!(Experiment=='dwss_1_03212019_FR' | Experiment=='dwss_1_03232019_FR'))

######## eliminate dwss_1_03132019_FR frame more than 10
maxFrame<-AreaY %>% filter(Experiment=='dwss_1_03132019_FR') %>% summarise(max(frame))

AreaY<-AreaY %>% filter(!(Experiment=='dwss_1_03132019_FR' & frame %in% seq(from=11,to=maxFrame[[1]])))

#========================================================================#
# Integral of Areas across of all layer
#========================================================================#
IntegralArea<-AreaY %>%
  group_by(Strain,Experiment, frame) %>%
  summarise(IntArea=sum(SumSliceY)) %>%
  mutate(LogIntArea=log(IntArea))

PlotIntegralArea<-ggplot(IntegralArea,aes(x=frame,y=LogIntArea,color=Strain))+
  geom_line(aes(x=frame,y=LogIntArea,group=Experiment),size=2)+
  ggtitle('Sum of areas across all Y-layers ') + 
  xlab("time frame in minutes") + ylab("log Area")+
  theme_classic()
#+ ggsave(paste('IntAreaY','.png',sep=""))

#========================================================================#
# Linear regression
#========================================================================#
# model LogIntArea~ Intersept + frame
fit_lm<-function(df){
  lm(LogIntArea~frame,data=df)
}

#=======================#
## nest data by experiments
by_experiment<-IntegralArea %>%
  group_by(Experiment) %>%
  nest()

# per model statistics
glance<-by_experiment %>%
  mutate(glance=map(model,broom::glance)) %>%
  separate(Experiment,c('Strain'),sep='_',remove=FALSE) %>%
  separate(Experiment,c('Label'),sep='_') %>%
  mutate(Strain=as.factor(Strain) ) %>%  
  unnest(glance,.drop=TRUE) %>%
  arrange(r.squared)
glance %>% ggplot(aes(x=Strain,y=r.squared,label=Experiment))+
  geom_jitter(width=0.03)+geom_text(hjust = 0.001, nudge_x = 0.05)
#=======================#
# coefficients
fit_coeff_by_experiment<-by_experiment %>%
  mutate(tidy_model=map(model,broom::tidy)) %>%
  unnest(tidy_model) %>%
  separate(Experiment,c('Strain','1','data'),sep='_',remove=FALSE) %>%
  mutate(Strain=as.factor(Strain) ) %>%
  select(-'1') 


by_experiment <- by_experiment %>% 
   mutate(model = map(data, fit_lm)) %>%
   mutate(resids = map2(data,model, add_residuals)) %>%
   mutate(pred = map2(data,model, add_residuals))


by_experiment_coeff %>% filter(estimate>0.1) %>% 
  mutate(doubl_time=(log(2)/estimate)) %>%
  ggplot(aes(x=Strain,y=doubl_time))+geom_jitter(width=0.01,size=3,alpha=0.7,color='darkblue')+
  labs(title="Doubling time approximation per hour")+
  ylab("doubling time")+
  theme_bw()+
  ggsave(paste('IntAreaY_doubling_time_per_hour','.png',sep=""))


#=======================#
# per model statistics
glance<-by_experiment %>%
  mutate(glance=map(model,broom::glance)) %>%
  unnest(glance,.drop=TRUE) %>%
  arrange(r.squared) %>%
  ggplot(aes(x=Strain,y=r.squared))+geom_jitter(width=0.03)

glance %>% 
  ggplot(aes(Experiment, r.squared)) + 
  geom_jitter(width = 0.5)+coord_flip()  


  
tidy_model<- by_experiment %>% 
  mutate(augment=map(model,broom::augment))

#==========#
#gain-lost
#==========#

split(IntegralArea,IntegralArea$Experiment) %>% map_df(~ (lm(LogIntArea ~ frame, data=.x)))
sim1_mod <- lm(LogIntArea ~ frame*Experiment,IntegralArea) 
coef(sim1_mod)
resids <- unnest(by_experiment, resids)
resids %>% ggplot(aes(frame,resid,color=Strain))+geom_line(aes(group=Experiment))


#==========#
#gain-lost
#==========#
AreaY %>% mutate(g_l=SumSliceYgain-SumSliceYlost) %>%
  group_by(Strain,Experiment, frame) %>%
  summarise(IntArea=sum(g_l)) %>%
  ggplot(aes(x=frame,y=IntArea,group=Experiment,color=Strain))+
  geom_line(size=1)+
  labs(title="SumSlice Ygain -Ylost")+
  geom_hline(aes(yintercept=0),size=1, color="black")+
  theme_classic()+
  ggsave(paste('IntAreaYgain-Ylost','.png',sep=""))

AreaY %>% mutate(g_l=SumSliceYgain-SumSliceYlost) %>%
  group_by(Strain,frame) %>%
  summarise(g_lmedian=median(g_l)) %>%
  ggplot(aes(x=frame,y=g_lmedian,color=Strain))+geom_line(size=1)+
  geom_hline(aes(yintercept=0),size=1, color="black")+
  labs(title="SumSlice Ygain-Ylost")+
  theme_classic()+
  ggsave(paste('IntAreaYgain-Ylostmedian','.png',sep=""))

#==========#          
# best dwss 
AreaY %>% filter(Experiment=='dwss_1_04062019_FR') %>%
  mutate(g_l=SumSliceYgain-SumSliceYlost) %>%
  group_by(frame) %>%
  summarise(g_lmedian=median(g_l)) %>%
  ggplot(aes(x=frame,y=g_lmedian))+
  geom_line(size=1,color='blue')+
  geom_hline(aes(yintercept=0),size=1, color="black")+
  labs(title="dwss_1_04062019_FR SumSlice Ygain-Ylost")+
  theme_classic()+
  ggsave(paste('IntAreaYgain-Ylost_dwss_1_04062019_FR','.png',sep=""))

AreaY %>% filter(Experiment=='dwss_1_04062019_FR') %>%
  group_by(frame) %>%
  summarise(g_lmedian=median(g_l)) %>%
  ggplot(aes(x=frame,y=g_lmedian))+
  geom_line(size=1,color='blue')+
  geom_hline(aes(yintercept=0),size=1, color="black")+
  labs(title="dwss_1_04062019_FR SumSlice Ygain-Ylost")+
  theme_classic()+
  ggsave(paste('IntAreaY_dwss_1_04062019_FR','.png',sep=""))


##=======================#
# calculate approximation coefficients
#by_experiment <- by_experiment %>% 
#  mutate(model = map(data, fit_lm)) %>%
#  mutate(resids = map2(data,model, add_residuals))