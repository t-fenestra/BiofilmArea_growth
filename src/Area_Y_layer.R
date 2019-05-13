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
  mutate(frame=60*frame)

#========================================================================#
# Filter data with strange output
#========================================================================#

######## choose only 15-first frames from 25
AreaY <- AreaY %>% filter(frame %in% seq(from=60,to=900,by=1)) # frame 1-15

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

IntegralArea %>% filter(Experiment=='dwss_1_04062019_FR') %>%
  ggplot(aes(x=frame,y=IntArea))+
  labs(title="Integral Area dwss_1_04062019_FR ")+
  xlab("minutes")+
  geom_line()+theme_bw(base_size = 14)+
  ggsave(paste('IntAreaY_dwss_1_04062019_FR','.png',sep=""))

ggplot(IntegralArea,aes(x=frame,y=IntArea,color=Strain))+
  geom_line(aes(x=frame,y=IntArea,group=Experiment),size=2)+
  ggtitle('Sum of areas across all Y-layers ') + 
  xlab("time frame in minutes") + ylab("log Area")+
  theme_classic(base_size = 12)+
  ggsave(paste('IntAreaY','.png',sep=""))


#============= linear regression #=============
## nest data by experiments
by_experiment<-IntegralArea %>%
  group_by(Experiment) %>%
  nest()

#=======================#
# model LogIntArea~ Intersept + frame
fit_lm<-function(df){
  lm(LogIntArea~frame,data=df)
}

#=======================#
# calculate approximation coefficients
by_experiment <- by_experiment %>% 
  mutate(model = map(data, fit_lm)) %>%
  mutate(resids = map2(data,model, add_residuals))




#=======================#
# coefficients
by_experiment %>%
  mutate(tidy_model=map(model,broom::tidy)) %>%
  unnest(tidy_model) %>%
  separate(Experiment,c('Strain'),sep='_',remove=FALSE) %>%
  mutate(Strain=as.factor(Strain) ) %>%  
  filter(term=="frame")
#%>%
#  ggplot(aes(x=Strain,y=(log(2)/estimate)))+geom_jitter(width=0.03)


#=======================#
# per model statistics
glance<-by_experiment %>%
  mutate(glance=map(model,broom::glance)) %>%
  unnest(glance,.drop=TRUE) %>%
  arrange(r.squared)

glance %>% 
  ggplot(aes(Experiment, r.squared)) + 
  geom_jitter(width = 0.5)+coord_flip()  


  
tidy_model<- by_experiment %>% 
  mutate(augment=map2(data,model,broom::augment))

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
  ggplot(aes(x=frame,y=IntArea))
  
  
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

#========================================================================#
# Growth per layer
#========================================================================#  
ggplot(AreaY,aes(x=frame,y=SumSliceY,color=Strain))+
  geom_line(aes(group=Experiment),size=1)+facet_grid(Layer~.)
  #+theme_classic()+
  #labs(title="Area per layer")
#+ggsave(paste('AreaY_perLayer','.png',sep=""))

#============#
# growth per layer dwss
AreaY %>% filter(Strain=="dwss") %>%
  ggplot(aes(x=frame,y=SumSliceY,color=Experiment))+
  geom_line(size=1)+
  facet_grid(Layer~.)+
  labs(title="Area per layer dwss")+
  theme_classic()+
  ggsave(paste('AreaY_perLayer_dwss','.png',sep=""),width=3, height=3, units="in", scale=3)

#============#
# growth per layer SM
AreaY %>% filter(Strain=="SM") %>%
  ggplot(aes(x=frame,y=SumSliceY,color=Experiment))+
  geom_line(size=1)+
  facet_grid(Layer~.)+
  theme_classic()+
  ggsave(paste('AreaY_perLayer_SM','.png',sep=""),width=3, height=3, units="in", scale=3)
#============#
# growth per layer SM first 15 frames
AreaY_SM_1_10<-AreaY %>%
  filter(Strain=="SM") %>%
  filter(frame %in% c(1:10))
ggplot(AreaY_SM_1_10,aes(x=frame,y=SumSliceY,color=Experiment))+
  geom_line(size=1)+
  facet_grid(Layer~.,scales = "free_y")+
  theme_classic()
ggsave(paste('AreaY_perLayer_SM_1_10','.png',sep=""),width=3, height=3, units="in", scale=3)

##============#
# median Area Layer
AreaY %>% group_by(Strain,Layer,frame) %>%
  summarise(MedianArea=median(SumSliceY))%>%
  ggplot(aes(x=frame,y=MedianArea, color=Strain))+
  geom_line(size=1)+
  theme_classic()+
  facet_grid(Layer~.)+
  labs(title="Median Area Layer")+
  ggsave(paste('MedianAreaLAyer','.png',sep=""),width=3, height=3, units="in", scale=3)

#========================================================================#
# Gain-lost per layer
#========================================================================# 
AreaY %>% mutate(g_l=SumSliceYgain-SumSliceYlost) %>%
  group_by(Strain,Layer,frame) %>%
  ggplot(aes(x=frame,y=g_l,color=Strain,group=Experiment))+
  geom_line(size=1)+
  theme_classic()+
  facet_grid(Layer~.,scales = 'free_y')+
  geom_hline(aes(yintercept=0),color="black")+
  labs(title="Area Layer Gain-lost")
#ggsave(paste('AreaLayer_Gain_lost','.png',sep=""),width=3, height=3, units="in", scale=3)

AreaY %>% filter(Strain=='dwss') %>% 
  mutate(g_l=SumSliceYgain-SumSliceYlost) %>%
  group_by(Strain,Layer,frame) %>%
  summarise(MedianArea=median(g_l))%>%
  ggplot(aes(x=frame,y=MedianArea, color=Strain))+
  geom_line(size=1)+
  theme_classic()+
  facet_grid(Layer~.)+
  labs(title="MedianArea_Gain-Lost_dwss")+
  geom_hline(aes(yintercept=0),size=0.5, color="black")+
  ggsave(paste('MedianArea_Gain-Lost_dwss','.png',sep=""),width=3, height=3, units="in", scale=3)

AreaY %>% filter(Strain=='SM') %>% 
  mutate(g_l=SumSliceYgain-SumSliceYlost) %>%
  group_by(Strain,Layer,frame) %>%
  summarise(MedianArea=median(g_l))%>%
  ggplot(aes(x=frame,y=MedianArea, color=Strain))+
  geom_line(size=1)+
  theme_classic()+
  facet_grid(Layer~.)+
  labs(title="MedianArea_Gain-Lost_SM")+
  geom_hline(aes(yintercept=0),size=0.5, color="black")+
  ggsave(paste('MedianArea_Gain-Lost_SM','.png',sep=""),width=3, height=3, units="in", scale=3)


