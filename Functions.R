getTopicsList=function(df){#df has two columns: topicID, Word.
  uniqueTopics=sort(unique(df$topicID))
  varList=list()
  for (i in uniqueTopics){
    firstWord=df[df$topicID==i,'Word'][1]
    varList[paste0(as.character(i),':',firstWord)]=i}
  return(varList)}

getTopicDesc=function(df,t){#df has two columns: topicID, Word; t=topic
  prods = df[df$topicID==t,'Word']
  topicProds = data.frame(x=rep(1,length(prods)),y=seq(length(prods),1,-1), prod=prods)
  return(topicProds)
}

getList=function(df){#df has two columns varID, varCat
df=aggregate(varID ~ varCat, df, unique)
df=df[order(df$varID),]
varList=list()
for(i in 1:nrow(df)){varList[df$varCat[i]]=df$varID[i]} 
return(varList)}


getExtraVars=function(varWeight){
varWeight_topic=sqldf('select topicID, avg(weight) t_avg, stdev(weight)/sqrt(count(1)) t_sd from varWeight group by topicID')
varWeight_varCat=sqldf('select varCat, avg(weight) v_avg, stdev(weight)/sqrt(count(1))  v_sd from varWeight group by varCat')
new_varWeight=sqldf('select a.*, 
                    round(weight/t_avg,1) weight_topic_prop, round(weight/v_avg,1) weight_var_prop,
                    case when weight>t_avg+2*t_sd then 1 when weight<t_avg-2*t_sd then -1 else 0 end weight_topic_sgn,
                    case when weight>v_avg+2*v_sd then 1 when weight<v_avg-2*v_sd then -1 else 0 end weight_var_sgn
                    from varWeight a 
                    left outer join varWeight_topic b on a.topicID=b.topicID
                    left outer join varWeight_varCat c on a.varCat=c.varCat')
return(new_varWeight)}

getTopicPlot=function(topicProds){
g=ggplot(topicProds, aes(x, y, label = prod))+
  geom_text(size=3) +
  labs(y="",x="",title='Characteristical Words')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  theme(plot.title = element_text(hjust = 0.5))
return(g)}

getTopicInVarPlot=function(df, varType, plotType){#df has at least two columns: varID,varCat
  dataVar= df[df$type==varType,] 	
  axis_labs= aggregate(varID~ varCat, dataVar, unique) 
  if(plotType==1){ lab='Probability'; g=ggplot(dataVar, aes(x=varID ,y=weight, colour= as.factor(topicID)))}
  if(plotType==2){ lab='Proportions'; g=ggplot(dataVar, aes(x=varID ,y=weight_var_prop, colour= as.factor(topicID)))}
  if(plotType==3){ lab=paste0(varType,' Proportions'); g=ggplot(dataVar, aes(x=varID ,y=weight_topic_prop, colour= as.factor(topicID)))}
  g=g+geom_line()+
    labs(y=lab,x='', colour="Topic ID:",title=varType)+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    scale_x_continuous(minor_breaks= NULL, breaks=axis_labs$varID, labels=axis_labs$varCat)
  return(g)
}

getVarPlot=function(varWeights,varID,plotType){
dataVar <-varWeights[which(varWeights$varID == varID),]
title=unique(dataVar$varCat)[1]
if(plotType==1){ lab='Probability'; g=ggplot(dataVar, aes(x=topicID ,y=weight, fill =as.factor(weight_var_sgn)))}
if(plotType==2){ lab='Proportions'; g=ggplot(dataVar, aes(x=topicID ,y=weight_var_prop, fill =as.factor(weight_var_sgn)))}
if(plotType==3){ lab='Proportions in Topics'; g=ggplot(dataVar, aes(x=topicID ,y=weight_topic_prop, fill =as.factor(weight_topic_sgn)))}
Pallete <- c("-1" = "grey", "0" = "lightblue", "1" = "navyblue")  
g=g+geom_bar(stat="identity",width = 0.1)+
  labs(y=lab,x='', title=title)+
  scale_x_continuous(minor_breaks= NULL, breaks=1:nrow(dataVar), labels=1:nrow(dataVar))+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_manual(values=Pallete, 
                    name="",
                    breaks=c('-1','0','1'),
                    labels=c("Below the Average", "On the Average","Above the Average"))
return(g)
}

mergeLists=function(x,y,dimname){
  vecGrid<- expand.grid(x, y)
  resList <- c()
  for(i in 1:nrow(vecGrid)){
    name=paste0(dimname,"Plot", vecGrid[i,1],"Type",vecGrid[i,2])
    resList<-c(resList,name)}
  return(resList)}
