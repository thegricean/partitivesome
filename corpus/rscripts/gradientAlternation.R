setwd('~/cogsci/projects/partitivesome/corpus/data/some/')

load("ofull.RData")

nooutliers <- subset(o, Outlier == "0")
nooutliers <- droplevels(nooutliers)
nooutliers$residMeanSimilarity1 = resid(lmer(MeanSimilarityRating~logSentenceLength + (1|Speaker_ID),data=nooutliers))

# similarity summary statistics for simple and partitive some
summary(nooutliers[nooutliers$Partitive == "no",]$residMeanSimilarity1)
summary(nooutliers[nooutliers$Partitive == "yes",]$residMeanSimilarity1)

nooutliers[nooutliers$Partitive == "yes" & nooutliers$residMeanSimilarity1 > 0.5,]$residMeanSimilarity1

# first, compare effect sizes on two datasets: cutoff point chosen so both datasets as equal as possible in size (749 vs 746) with cutoff point of 0.3
# 1. all partitives, simple some with residMeanSimilarity > max(partitive residmeansimilarity)
highpart = rbind(nooutliers[nooutliers$Partitive == "yes",] , nooutliers[nooutliers$Partitive == "no" & nooutliers$residMeanSimilarity1 > 0.3,])
nrow(highpart)
centered=cbind(highpart,myCenter(highpart[,c('IPre','IHead','HeadType','redInfoStatus','BinaryGF','redAnimacy','logFreq','revModification')]))
m = lmer(Partitive ~ credInfoStatus +crevModification + cBinaryGF + cHeadType + credInfoStatus:crevModification  + credAnimacy + clogFreq + cIPre + cIHead + (1|Speaker_ID), data=centered, family="binomial")
summary(m)

# 2. all partitives, simple some with residMeanSimilarity <= max(partitive residmeansimilarity)
lowpart = rbind(nooutliers[nooutliers$Partitive == "yes",] , nooutliers[nooutliers$Partitive == "no" & nooutliers$residMeanSimilarity1 <= 0.3,])
nrow(lowpart)
centered=cbind(lowpart,myCenter(lowpart[,c('IPre','IHead','HeadType','redInfoStatus','BinaryGF','redAnimacy','logFreq','revModification')]))
m = lmer(Partitive ~ credInfoStatus +crevModification + cBinaryGF + cHeadType + credInfoStatus:crevModification  + credAnimacy + clogFreq + cIPre + cIHead + (1|Speaker_ID), data=centered, family="binomial")
summary(m)


# now split up into three equally sized simple some bins
# 1. all partitives, simple some with residMeanSimilarity > max(partitive residmeansimilarity)
highpart = rbind(nooutliers[nooutliers$Partitive == "yes",] , nooutliers[nooutliers$Partitive == "no" & nooutliers$residMeanSimilarity1 > 0.3,])
nrow(highpart)
centered=cbind(highpart,myCenter(highpart[,c('IPre','IHead','HeadType','redInfoStatus','BinaryGF','redAnimacy','logFreq','revModification')]))
m = lmer(Partitive ~ credInfoStatus +crevModification + cBinaryGF + cHeadType + credInfoStatus:crevModification  + credAnimacy + clogFreq + cIPre + cIHead + (1|Speaker_ID), data=centered, family="binomial")
summary(m)

# 2. all partitives, simple some with residMeanSimilarity <= max(partitive residmeansimilarity)
low = rbind(nooutliers[nooutliers$Partitive == "yes",] , nooutliers[nooutliers$Partitive == "no" & nooutliers$residMeanSimilarity1 <= 0,])
nrow(low)
centered=cbind(low,myCenter(low[,c('IPre','IHead','HeadType','redInfoStatus','BinaryGF','redAnimacy','logFreq','revModification')]))
m = lmer(Partitive ~ credInfoStatus +crevModification + cBinaryGF + cHeadType + credInfoStatus:crevModification  + credAnimacy + clogFreq + cIPre + cIHead + (1|Speaker_ID), data=centered, family="binomial")
summary(m)

mid = rbind(nooutliers[nooutliers$Partitive == "yes",] , nooutliers[nooutliers$Partitive == "no" & nooutliers$residMeanSimilarity1 > 0 & nooutliers$residMeanSimilarity1 <= 0.5,])
nrow(mid)
centered=cbind(mid,myCenter(mid[,c('IPre','IHead','HeadType','redInfoStatus','BinaryGF','redAnimacy','logFreq','revModification')]))
m = lmer(Partitive ~ credInfoStatus +crevModification + cBinaryGF + cHeadType + credInfoStatus:crevModification  + credAnimacy + clogFreq + cIPre + cIHead + (1|Speaker_ID), data=centered, family="binomial")
summary(m)

high = rbind(nooutliers[nooutliers$Partitive == "yes",] , nooutliers[nooutliers$Partitive == "no" & nooutliers$residMeanSimilarity1 > 0.5,])
nrow(high)
centered=cbind(mid,myCenter(mid[,c('IPre','IHead','HeadType','redInfoStatus','BinaryGF','redAnimacy','logFreq','revModification')]))
m = lmer(Partitive ~ credInfoStatus +crevModification + cBinaryGF + cHeadType + credInfoStatus:crevModification  + credAnimacy + clogFreq + cIPre + cIHead + (1|Speaker_ID), data=centered, family="binomial")
summary(m)

# now go through series of cutoff points, from -0.2 (righ below mean of partitives) to 0.6 (above which there are only 3 more cases of partitives)
d.full = data.frame(SimpleData=factor(levels=c("above","belowequal"),x=rep(NA,18)),BP=rep(0,18),Observations=rep(0,18),BIC=rep(-1,18),Deviance=rep(-1,18),logLik=rep(-1,18),Beta.cIPre=rep(-1,18),SE.cIPre=rep(-1,18),P.cIPre=rep(-1,18),Beta.cIHead=rep(-1,18),SE.cIHead=rep(-1,18),P.cIHead=rep(-1,18),Beta.cHeadType=rep(-1,18),SE.cHeadType=rep(-1,18),P.cHeadType=rep(-1,18),Beta.credAnimacy=rep(-1,18),SE.credAnimacy=rep(-1,18),P.credAnimacy=rep(-1,18),Beta.clogFreq=rep(-1,18),SE.clogFreq=rep(-1,18),P.clogFreq=rep(-1,18),Beta.crevModification=rep(-1,18),SE.crevModification=rep(-1,18),P.crevModification=rep(-1,18),Beta.credInfoStatus=rep(-1,18),SE.credInfoStatus=rep(-1,18),P.credInfoStatus=rep(-1,18),Beta.cBinaryGF=rep(-1,18),SE.cBinaryGF=rep(-1,18),P.cBinaryGF=rep(-1,18),Beta.credInfoStatus.crevModification=rep(-1,18),SE.credInfoStatus.crevModification=rep(-1,18),P.credInfoStatus.crevModification=rep(-1,18),Error=factor(levels=c("error",""),x=rep(NA,18)))

i=1

t = "Partitive~credInfoStatus +crevModification + cBinaryGF+ credInfoStatus:crevModification + +cHeadType+credAnimacy+clogFreq+cIPre+cIHead + (1|Speaker_ID)"

  for (bp in seq(-0.2,0.6,by=0.1))
  {
    print(paste("processing breakpoint ",bp,"...",sep=""))	

    # first the strong simple somes
    dlow = rbind(nooutliers[nooutliers$Partitive == "yes",], nooutliers[nooutliers$Partitive == "no" & nooutliers$residMeanSimilarity1 <= bp,])
    
    centered=cbind(dlow,myCenter(dlow[,c('IPre','IHead','HeadType','redInfoStatus','revModification','BinaryGF','redAnimacy','logFreq')]))	
    m.full = withWarnings(lmer(as.formula(t),data=centered,family="binomial"))
    if (length(m.full$Warnings) > 0)
    {
      w = paste(m.full$Warnings,collapse='')
      if (! w %in% levels(d.full$Error))
      {
        levels(d.full$Error) = c(levels(d.full$Error), w)
      }
      d.full[i,]$Error = w
    } else {
      d.full[i,]$Error = ""			
    }
    
    coefs = as.data.frame(summary(m.full$Value)@coefs)
    coefs[,1] = round(coefs[,1],digits=2)
    coefs[,2] = round(coefs[,2],digits=2)
    coefs[,3] = round(coefs[,3],digits=1)
    coefs[,4] = round(coefs[,4],digits=4)			
    mquality = as.data.frame(summary(m.full$Value)@AICtab)
    
    d.full[i,]$SimpleData = "belowequal"
    d.full[i,]$BP = bp
    d.full[i,]$BIC = mquality$BIC
    d.full[i,]$Deviance = mquality$deviance
    d.full[i,]$logLik = mquality$logLik
    d.full[i,]$Observations = length(fitted(m.full[[1]]))
    
    d.full[i,"Beta.Intercept"] = coefs[1,1]
    d.full[i,"SE.Intercept"] = coefs[1,2]
    d.full[i,"P.Intercept"] = coefs[1,4]
    for (pred in c('cIPre','cIHead','cHeadType','credInfoStatus','cBinaryGF','credAnimacy','clogFreq','crevModification','credInfoStatus:crevModification'))
    {
      if (length(strsplit(pred,":",fixed=TRUE)[[1]]) == 1)
      {
        d.full[i,paste("Beta",pred,sep=".")] = coefs[pred,1]
        d.full[i,paste("SE",pred,sep=".")] = coefs[pred,2]
        d.full[i,paste("P",pred,sep=".")] = coefs[pred,4]
      } else {
        spl = strsplit(pred,":",fixed=TRUE)[[1]]
        d.full[i,paste("Beta",spl[1],spl[2],sep=".")] = coefs[pred,1]
        d.full[i,paste("SE",spl[1],spl[2],sep=".")] = coefs[pred,2]
        d.full[i,paste("P",spl[1],spl[2],sep=".")] = coefs[pred,4]        
      }
    }

    # now the weak simple somes
    i= i +1
    
    dhigh = rbind(nooutliers[nooutliers$Partitive == "yes",], nooutliers[nooutliers$Partitive == "no" & nooutliers$residMeanSimilarity1 > bp,])
    
    centered=cbind(dhigh,myCenter(dhigh[,c('IPre','IHead','HeadType','redInfoStatus','revModification','BinaryGF','redAnimacy','logFreq')]))  
    m.full = withWarnings(lmer(as.formula(t),data=centered,family="binomial"))
    if (length(m.full$Warnings) > 0)
    {
      w = paste(m.full$Warnings,collapse='')
      if (! w %in% levels(d.full$Error))
      {
        levels(d.full$Error) = c(levels(d.full$Error), w)
      }
      d.full[i,]$Error = w
    } else {
      d.full[i,]$Error = ""			
    }
    
    coefs = as.data.frame(summary(m.full$Value)@coefs)
    coefs[,1] = round(coefs[,1],digits=2)
    coefs[,2] = round(coefs[,2],digits=2)
    coefs[,3] = round(coefs[,3],digits=1)
    coefs[,4] = round(coefs[,4],digits=4)			
    mquality = as.data.frame(summary(m.full$Value)@AICtab)
    
    d.full[i,]$SimpleData = "above"
    d.full[i,]$BP = bp
    d.full[i,]$BIC = mquality$BIC
    d.full[i,]$Deviance = mquality$deviance
    d.full[i,]$logLik = mquality$logLik
    d.full[i,]$Observations = length(fitted(m.full[[1]]))
    
    d.full[i,"Beta.Intercept"] = coefs[1,1]
    d.full[i,"SE.Intercept"] = coefs[1,2]
    d.full[i,"P.Intercept"] = coefs[1,4]
    for (pred in c('cIPre','cIHead','cHeadType','credInfoStatus','cBinaryGF','credAnimacy','clogFreq','crevModification','credInfoStatus:crevModification'))
    {
      if (length(strsplit(pred,":",fixed=TRUE)[[1]]) == 1)
      {
        d.full[i,paste("Beta",pred,sep=".")] = coefs[pred,1]
        d.full[i,paste("SE",pred,sep=".")] = coefs[pred,2]
        d.full[i,paste("P",pred,sep=".")] = coefs[pred,4]
      } else {
        spl = strsplit(pred,":",fixed=TRUE)[[1]]
        d.full[i,paste("Beta",spl[1],spl[2],sep=".")] = coefs[pred,1]
        d.full[i,paste("SE",spl[1],spl[2],sep=".")] = coefs[pred,2]
        d.full[i,paste("P",spl[1],spl[2],sep=".")] = coefs[pred,4]        
      }
    } 
    i = i + 1
  }
save(d.full,file="rdata/d.full.gradient.RData")

load("rdata/d.full.gradient.RData")

fe.beta = d.full[,c('SimpleData','Observations','BP','Beta.cIPre','Beta.cIHead','Beta.cHeadType','Beta.credAnimacy','Beta.clogFreq','Beta.crevModification','Beta.credInfoStatus','Beta.cBinaryGF','Beta.credInfoStatus.crevModification')]#,'Beta.Intercept')]
fe.se = d.full[,c('SimpleData','Observations','BP','SE.cIPre','SE.cIHead','SE.cHeadType','SE.credAnimacy','SE.clogFreq','SE.crevModification','SE.credInfoStatus','SE.cBinaryGF','SE.credInfoStatus.crevModification')]#,'SE.Intercept')
fe.p = d.full[,c('SimpleData','Observations','BP','P.cIPre','P.cIHead','P.cHeadType','P.credAnimacy','P.clogFreq','P.crevModification','P.credInfoStatus','P.cBinaryGF','P.credInfoStatus.crevModification')]#,'P.Intercept')
m = melt(fe.beta, id.vars=c('SimpleData','Observations','BP'))
colnames(m) = c('SimpleData','Observations','BP','Predictor','Coefficient')
m$Predictor = gsub("Beta.","",as.character(m$Predictor))
m$Predictor = as.factor(as.character(m$Predictor))
m$SE = melt(fe.se, id.vars=c('SimpleData','Observations','BP'))$value
m$NumP = melt(fe.p, id.vars=c('SimpleData','Observations','BP'))$value
m$P = ifelse(m$NumP < .0001, "<.0001", ifelse(m$NumP < .001, "<.001", ifelse(m$NumP < .01, "<.01", ifelse(m$NumP<.05, "<.05", ifelse(m$NumP <= .1, "<= .1", ">.1")))))
m$P = as.factor(as.character(m$P))
m$YMin = m$Coefficient - m$SE
m$YMax = m$Coefficient + m$SE
limits = aes(ymin=YMin,ymax=YMax)
dodge = position_dodge(.9)


p = ggplot(m, aes(x=BP,y=Coefficient,alpha=Observations,color=P,shape=SimpleData)) +
    geom_point(size=I(4)) +
    geom_errorbar(limits,width=0.05,size=I(1)) +
    geom_hline(color=I("black"),yintercept=0,show_guide=FALSE) +
    scale_colour_manual(values=c("limegreen","limegreen","limegreen","limegreen","yellow","red")) +
    facet_wrap(~Predictor,scales="free_y")
p
pdf(file="graphs/gradientAlternation.pdf",width=12,height=8)
print(p)
dev.off()

# now test what happens if you use the predictors to predict simple weak vs simple/partitive strong. that should identify the factors that are good at identifying the weak/strong distinction but not the simple/partitive. hm, you could also do this with continuous mean similarity...
