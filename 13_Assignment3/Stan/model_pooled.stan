data{
  int<lower=0> npupil;
  int<lower=0> p;
  int School[npupil];
  int Goals[npupil];
}
parameters {
  real<lower = 0, upper = 1> theta;
}
model {
  Goals ~ bernoulli(theta);
  theta ~ beta(1, 1);
}


#for (i in 1:npupil) 
#{ 
# School[i] ~dcat(p[])
#} 

#for(k in 1:nschool)
#{ p[k]<- 1.0/nschool
# }

