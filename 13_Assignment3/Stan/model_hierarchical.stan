data{
  int<lower=0> npupil;
  int<lower=0> p;
  int School[npupil];
  int Goals[npupil];
}
parameters {
  real<lower = 0, upper = 1> theta[p];
  real<lower = 0> a;
  real<lower = 0> b;
}
model {
  Goals ~ bernoulli(theta[School]);
  theta ~ beta(a, b);
  a ~ gamma(.1,.1);
  b ~ gamma(.1,.1);
}





