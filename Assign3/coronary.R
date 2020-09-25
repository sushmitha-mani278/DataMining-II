#sushmitha Mani
#assignment- Baysian networks

library(dplyr)
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
#install.packages('fastDummies')
library(fastDummies)
library(ggplot2)
# install.packages("convertr")
# library(convertr)
#BiocManager::install("Rgraphviz")
library(Rgraphviz)
require(bnlearn)
data()

#install.packages("Rcpp")
#install.packages("forecast")
library(Rcpp)
library(forecast)


data(coronary, package="bnlearn")
coronary
data_coronary<- as.data.frame(as(
  object=coronary,
  Class="matrix"
))

view(data_coronary)

glimpse(coronary)





model_hc<-hc(data_coronary)
model_hc
graphviz.plot(x=model_hc)

model_h2pc <-h2pc(data_coronary)
model_h2pc
graphviz.plot(x=model_h2pc)


blacklist <- data.frame(
  from = c("M. Work"),
  to = c("Smoking","Proteins")
)

model_iamb <-iamb.fdr(data_coronary, blacklist = blacklist)
model_iamb
graphviz.plot(x=model_iamb)


model_aracne <-aracne(data_coronary)
model_aracne
graphviz.plot(x=model_aracne,
              shape = 'ellipse')

glimpse(data_coronary)




direct = matrix(c("Smoking","M. Work","M. Work","P. Work", "M. Work", "Family", "M. Work", "Proteins", "Pressure", "Proteins"),
                  ncol = 2, byrow = TRUE,
                  dimnames = list(NULL, c("from", "to")))
direct
arcs(model_aracne)=direct
graphviz.plot(
  x = model_aracne,
  shape = "ellipse"
)




# 
# blacklist <- data.frame(
#   from = c("M. Work"),
#   to = c("Smoking","Proteins")
# )


bnlearn::score(model_hc, data_coronary, type="aic")
bnlearn::score(model_h2pc, data_coronary, type="aic")
bnlearn::score(model_iamb, data_coronary, type="aic")
bnlearn::score(model_aracne, data_coronary, type="aic")



bnlearn::score(model_hc, data_coronary, type="bic")
bnlearn::score(model_h2pc, data_coronary, type="bic")
bnlearn::score(model_iamb, data_coronary, type="bic")
bnlearn::score(model_aracne, data_coronary, type="bic")



bnlearn::score(model_hc, data_coronary, type="loglik")
bnlearn::score(model_h2pc, data_coronary, type="loglik")
bnlearn::score(model_iamb, data_coronary, type="loglik")
bnlearn::score(model_aracne, data_coronary, type="loglik")


glimpse(data_coronary)




data_coronary$Smoking <- as.numeric(data_coronary$Smoking)
model_hc<-hc(data_coronary)
data_coronary$Smoking <- as.numeric(data_coronary$Smoking)
fit = bn.fit(model_hc,data_coronary)
fit$Smoking

pred = predict(fit, "Smoking",data_coronary)
pred

accuracy(object = pred, x =data_coronary$Smoking)


