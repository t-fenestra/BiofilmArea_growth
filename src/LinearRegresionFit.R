LinearRegressionFit<-function(df,file_prefix){
  ## 1.nest data by experiments
  by_experiment<-df %>%
    #exclude points that produced Nans
    filter(IntAreaTotal>0) %>%
    #converted frames to the minutes
    mutate(frame=60*frame) %>%
    group_by(Experiment) %>%
    nest()
  
  #======================================
  # 2. apply model to the data
  by_experiment <- by_experiment %>%
    mutate(model = map(data, fit_lm)) %>%
    mutate(resids = map2(data,model, add_residuals)) %>%
    mutate(predict = map2(data,model, add_predictions))
  
  #======================================
  # 3. extract coefficients Intersept,frame
  FitCoeff<-by_experiment %>%
    mutate(tidy_model=map(model,broom::tidy)) %>%
    unnest(tidy_model) %>%
    separate(Experiment,c('Strain'),sep='_',remove=FALSE,extra="drop") %>%
    mutate(Strain=as.factor(Strain))
  
  
  #======================================
  # 4. per model statistics
  glance<-by_experiment %>%
    mutate(glance=map(model,broom::glance)) %>%
    unnest(glance,.drop=TRUE) %>%
    select(Experiment,r.squared)
  
  #======================================
  # 5. plot fit to the data and residuals
  
  DataPredict<-by_experiment %>%
    unnest(predict) %>%
    mutate(pred=exp(pred)) %>%
    mutate(res=pred-IntAreaTotal) %>%
    gather(key=Area_type,value=Area,IntAreaTotal,pred)
  
  #plot fit
  PlotFit_dwss<-DataPredict %>% filter(Strain=="dwss") %>%
    ggplot(aes(x=frame,y=Area,color=Area_type,group=Experiment))+
    geom_line(aes(group=Area_type),size=1)+
    scale_color_manual(name="Area_type", values=c("black","red"), labels=c("data", "fit"))+
    facet_wrap(.~Experiment,scales = "free")+
    labs(title=file_prefix,subtitle="dwss")+
    ylab("")
  print(PlotFit_dwss)
  
  PlotFit_SM<-DataPredict %>% filter(Strain=="SM") %>%
    ggplot(aes(x=frame,y=Area,color=Area_type,group=Experiment))+
    geom_line(aes(group=Area_type),size=1)+
    scale_color_manual(name="Area_type", values=c("black","red"), labels=c("data", "fit"))+
    facet_wrap(.~Experiment,scales = "free")+
    labs(title=file_prefix,subtitle="SM")
  print(PlotFit_SM)
  
  #Fig1<-ggarrange(PlotFit_SM,PlotFit_dwss,nrow=2,ncol=1,common.legend = TRUE, legend="bottom")
  #print(Fig1)
  
  # #plot residual
  # p<-ggplot(DataPredict,aes(x=frame,y=res,group=Experiment))+
  #   geom_line(size=1)+
  #   facet_wrap(.~Experiment,scales = "free")+
  #   labs(title=file_prefix,subtitle = "Residuals")
  # print(p)
  
  #plot coefficients
  # frame
  p<-FitCoeff %>% filter(term=="frame") %>%
    ggplot(aes(x=Experiment,y=estimate,fill=Strain,label = Experiment))+
    geom_bar(stat="identity", position=position_dodge())+
    geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=.2,
                  position=position_dodge(.9))+
    ylab("growth rate 1/min")+
    xlab("Experiment")+
    labs(title=file_prefix,subtitle="colonization rate estimate")+
    coord_flip()+
    theme_grey(base_size = 12)
  print(p)
  
  
  # Intersept
  p<-FitCoeff %>% filter(term=="(Intercept)") %>%
    ggplot(aes(x=Experiment,y=estimate,fill=Strain,label = Experiment))+
    geom_bar(stat="identity", position=position_dodge())+
    geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=.2,
                  position=position_dodge(.9))+
    ylab("Intersept")+
    xlab("Experiment")+
    labs(title=file_prefix,subtitle="colonization Intersept estimate")+
    coord_flip()+
    theme_grey(base_size = 12)
  print(p)
  
  # model quality extimate
  p<-glance %>% separate(Experiment,c('Strain'),sep='_',remove=FALSE,extra="drop") %>%
    mutate(Strain=as.factor(Strain)) %>%
    ggplot(aes(y=r.squared,x=Experiment, fill=Strain)) +
    geom_col()+
    xlab("Experiment")+
    coord_flip()+
    labs(title=file_prefix,subtitle = "Quality of model fit R.squared")
  print(p)
  
  # barplot of the colonization rate
  p<-FitCoeff %>% filter(term=="frame") %>%
    ggplot(aes(x=Strain,y=estimate, fill=Strain))+
    geom_boxplot()+
    ylab("colonisation rate 1/min")+
    xlab("Experiment")+
    labs(title=file_prefix,subtitle = "colonization rate estimate")+
    theme_grey(base_size = 12)
  print(p)
  
  return(FitCoeff)
}