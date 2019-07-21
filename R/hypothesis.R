#'Hypothesis testing
#'
#'Takes dataframe having binary target variable
#'@param df= dataframe
#'@param y= Binary target variable
#'@author Sunny kumar and Vikas yadav
#'@export

hypothesis<-function(df,y)
{
  if (is.data.frame(df)&&length(unique(y))==2)
  {
    m=c()
    n=c()
    c=c()
    for (i in 1:ncol(df))
    {
      if (class(i)=="factor"|((length(unique(df[,i]))/nrow(df)*100)<10&&(length(unique(df[,i])))>1))
      {
        test<-chisq.test(df[,i],y)
        m[i]=test$p.value
        n[i]=test$statistic
      }
      else if(is.numeric(df[,i]) && (length(unique(df[,i])))>1)
      {
        a<-df[,i][y==unique(y)[1]]
        b<-df[,i][y==unique(y)[2]]
        test=t.test(a,b)
        m[i]=test$p.value
        c[i]=test$statistic
      }
    }

    Table = cbind(m,c,n)
    rownames(Table) = names(df)
    colnames(Table)=c("p-value","t-Statistic","X-Statistic")
    return(Table)




    if (class(y)=="numeric")
    {
      print("Anova Test")
      print(aov(y~as.matrix(df)))
    }

  }
  else
  {
    print("Target variable is not binary")
  }
}


