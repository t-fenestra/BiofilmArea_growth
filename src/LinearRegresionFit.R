LinearRegressionFit<-function(df){
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
  ggplot(DataPredict,aes(x=frame,y=Area,color=Area_type,group=Experiment))+
    geom_line(aes(group=Area_type),size=1)+
    facet_wrap(.~Experiment,scales = "free")+
    labs(title="Fit to the data")
  
  #plot residual
  ggplot(DataPredict,aes(x=frame,y=res,group=Experiment))+
    geom_line(size=1)+
    facet_wrap(.~Experiment,scales = "free")+
    labs(title="Residuals")
  
  #plot coefficients
  # frame
  FitCoeff %>% filter(term=="frame") %>%
    ggplot(aes(x=Experiment,y=estimate,fill=Strain,label = Experiment))+
    geom_bar(stat="identity", position=position_dodge())+
    geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=.2,
                  position=position_dodge(.9))+
    ylab("growth rate 1/min")+
    xlab("Experiment")+
    labs(title="colonization rate estimate")+
    coord_flip()+
    theme_grey(base_size = 12)
  
  
  # Intersept
  FitCoeff %>% filter(term=="(Intercept)") %>%
    ggplot(aes(x=Experiment,y=estimate,fill=Strain,label = Experiment))+
    geom_bar(stat="identity", position=position_dodge())+
    geom_errorbar(aes(ymin=estimate-std.error, ymax=estimate+std.error), width=.2,
                  position=position_dodge(.9))+
    ylab("Intersept")+
    xlab("Experiment")+
    labs(title="colonization Intersept estimate")+
    coord_flip()+
    theme_grey(base_size = 12)
  
  # model quality extimate
  glance %>% separate(Experiment,c('Strain'),sep='_',remove=FALSE,extra="drop") %>%
    mutate(Strain=as.factor(Strain)) %>%
    ggplot(aes(y=r.squared,x=Experiment, fill=Strain)) +
    geom_col()+
    xlab("Experiment")+
    coord_flip()+
    labs(title="Quality of model fit R.squared")
}