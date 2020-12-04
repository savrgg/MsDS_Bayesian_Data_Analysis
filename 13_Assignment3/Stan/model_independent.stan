data{
  int<lower=0> npupil;
  int<lower=0> p;
  int School[npupil];
  int Goals[npupil];
}
parameters {
  real<lower = 0, upper = 1> theta[p];
}
model {
  Goals ~ bernoulli(theta[School]);
  theta ~ beta(1, 1);
}
