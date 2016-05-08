# 
library(ggplot2)

### Not- Equal Variance
notequal = as.data.frame(1); notequal$fake <- rnorm(n=1,1,1)
names(notequal) <- c("n","fake")


for (i in 1:500){
  temp = as.data.frame(i); temp$fake <- rnorm(n=1,i,i)
  names(temp) <- c("n","fake")
  notequal <- rbind(notequal,temp)
}

ggplot(data=notequal)+
  geom_point(aes(x=n,y=fake))+
  labs(x="Generic X variable",y="Generic Y Variable",title="A sample of non-equal variance")

notequal_model <- lm(data=notequal, n~fake)
plot(notequal_model$residuals)

res <- as.data.frame(cbind(notequal_model$residuals,notequal_model$model$n))
names(res) <- c("residual","n")
res$residual <- as.numeric(as.character(res$residual))
res$n <- as.numeric(as.character(res$n))

ggplot(data=res)+
  geom_point(aes(x=n,y=residual))+
  labs(x="Generic X variable",y="Generic Y Variable",title="A sample of non-equal variance")


### Equal Variance
equal = as.data.frame(1); equal$fake <- rnorm(n=1,1,1)
names(equal) <- c("n","fake")


for (i in 1:500){
  temp = as.data.frame(i); temp$fake <- rnorm(n=1,i,1)
  names(temp) <- c("n","fake")
  equal <- rbind(equal,temp)
}

ggplot(data=equal)+
  geom_point(aes(x=n,y=fake))+
  labs(x="Generic X variable",y="Generic Y Variable",title="A sample of non-equal variance")

equal_model <- lm(data=equal, n~fake)
plot(equal_model$residuals)

res <- as.data.frame(cbind(equal_model$residuals,equal_model$model$n))
names(res) <- c("residual","n")
res$residual <- as.numeric(as.character(res$residual))
res$n <- as.numeric(as.character(res$n))

ggplot(data=res)+
  geom_point(aes(x=n,y=residual))+
  labs(x="Generic X variable",y="Generic Y Variable",title="A sample of non-equal variance")

