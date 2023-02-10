
utils.GetRsq<-function(lm_base, lm_current)
{
  xvarLM1<-as.data.frame(VarCorr(lm_base))
  xtotalVarLM1<-sum(xvarLM1$vcov)
  xresidLM1<-xvarLM1[xvarLM1$grp=='Residual','vcov']
  xvarLM6<-as.data.frame(VarCorr(lm_current))
  xtotalVarLM6<-sum(xvarLM6$vcov)
  xresidLM6<-xvarLM6[xvarLM6$grp=='Residual','vcov']
  xr2lm6<-1-(xtotalVarLM6/xtotalVarLM1); 
  return(xr2lm6)
}

utils.Getfsq<-function(lm_base, lm_current)
{
  Rsq<-utils.GetRsq(lm_base, lm_current)
  return(Rsq/(1-Rsq))
}