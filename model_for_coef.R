library(BAS)
library(tidyverse)

original_data = read_csv("./data/df_R.csv")

data = subset(original_data, select = -c(X1, idx))

y_name = c("murdPerPop", "rapesPerPop", "robbbPerPop", "assaultPerPop", "burglPerPop", "larcPerPop", "autoTheftPerPop", "arsonsPerPop")

# error first time
result = tibble(index=coef.gprior$namesx)

for(cnt in y_name){
  set.seed(2020)
  f = paste(cnt, " ~ . -murdPerPop -rapesPerPop -robbbPerPop -assaultPerPop -burglPerPop -larcPerPop -autoTheftPerPop -arsonsPerPop")
  fit.gprior = bas.lm(f,
                      data = data,
                      method = "MCMC", # better than default "BAS"
                      # for large p
                      prior = "ZS-null", # default
                      # "JZS" also can use
                      modelprior = uniform(),
                      include.always = ~1,
                      MCMC.iterations = 100000)
  coef.gprior = coef(fit.gprior)
  result[cnt] = coef.gprior$postmean
}

result%>%
  write_csv('./data/coef.csv')
