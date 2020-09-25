

myf = function(x,xk,coef){
  coef[1]+coef[2]*(x) + coef[3]*(x-xk)*(x-xk>0)
}

rsq = function(xk,data){ # data=spruce.df
  df=within(data, X<-(data$BHDiameter-xk)*(data$BHDiameter>xk))  
  lmp=lm(Height ~ BHDiameter + X, data=df)
  tmp = summary(lmp)
  tmp$r.squared
}

rsqdash1 = function(xk,h,data) {
 (rsq((xk+h/2),data)-rsq((xk-h/2),data))/h
}

rsqdash2 = function(xk,xk2,h,data) {
  (rsq((xk2+h/2),data)-rsq((xk2-h/2),data))/h + (rsq((xk+h/2),data)-rsq((xk-h/2),data))/h
}

myf2 = function(x,xk,xk2,coef){
  coef[1]+coef[2]*(x) + coef[3]*(x-xk)*(x-xk>0)+ coef[4]*(x-xk2)*(x-xk2>0)
}

coeff = function(xk1,xk2,data){ # data=spruce.df
  df=within(data, {
            X1<-(data$BHDiameter-xk1)*(data$BHDiameter>xk1) 
            X2<-(data$BHDiameter-xk2)*(data$BHDiameter>xk2)
  }
            ) 
  lmp=lm(Height ~ BHDiameter + X1 + X2, data=df)
  coef(lmp)
}

rsq2 = function(xk,xk2, data){ # data=spruce.df
  if(xk<xk2){
    X2<-(data$BHDiameter-xk2)*(data$BHDiameter>xk2)
  df=within(data, 
            X<-(data$BHDiameter-xk)*(data$BHDiameter>xk),
            X2<-(data$BHDiameter-xk2)*(data$BHDiameter>xk2))  
  lmp=lm(Height ~ BHDiameter + X + X2, data=df)
  tmp = summary(lmp)
  return(tmp$r.squared)
  }
  if(xk>=xk2){
  return(0)
  }
}
