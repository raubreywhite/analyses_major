data {
  int<lower=0> N;
  vector[N] timeVax;
  vector[N] timeNotVax;
  int y[N];
}
parameters {
  real<lower=0,upper=0.5> thetaVax;
  real<lower=0,upper=0.5> thetaNotVax;
}
model {
  thetaVax ~ beta(0.05, 1);
  thetaNotVax ~ beta(0.05, 1);
  
  for (n in 1:N) 
    y[n] ~ bernoulli(1-((1-thetaVax)^timeVax[n])*((1-thetaNotVax)^timeNotVax[n]));
}
