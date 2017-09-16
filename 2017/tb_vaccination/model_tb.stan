data {
  int<lower=0> N;
  int time[N];
  int y[N];
}
parameters {
  real<lower=0,upper=0.5> thetaLTBI;
  real<lower=0,upper=0.5> thetaATBI;
}
model {
  thetaLTBI ~ beta(0.05, 1);
  thetaATBI ~ beta(0.05, 1);
{
  int personTime;
  real prob_ltbi_no_tb;
  for (n in 1:N){
    prob_ltbi_no_tb = 0;
    personTime = time[n];
    for(t in 1:personTime){
       prob_ltbi_no_tb = (thetaLTBI^t)*(1-thetaATBI)^(personTime-t);
    }
    y[n] ~ bernoulli(1-(1-thetaLTBI)^time[n]-prob_ltbi_no_tb);
  }
}
}
