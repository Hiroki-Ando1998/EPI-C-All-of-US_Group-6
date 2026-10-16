data {
  int<lower=1> N;                 // サンプルサイズ
  array[N] int<lower=0, upper=1> y; // 目的変数 (0または1)
  array[N] int<lower=0, upper=1> exposure;
  
  // 各変数kのダミー変数 (N行 x 2列)
  matrix[N, 1] X1; //Sex
  matrix[N, 1] X2; //Insurance
  matrix[N, 3] X3; //Age
  matrix[N, 3] X4; //Education
  matrix[N, 3] X5; //employment
  matrix[N, 5] X6; //income
  matrix[N, 2] X7; //house own or rent
}

parameters {
  real alpha;             // 切片 (ベースライン)
  real beta_exp;          // 主要関心変数 (exp) の効果
  
  // 7つの変数それぞれのハイパーパラメータ (平均 mu, 標準偏差 sigma)
  vector[7] mu;
  vector<lower=0>[7] sigma;
  
  // 各変数の標準化パラメータ (非中心化サンプリング用: N(0, 1)に従う)
  vector[1] beta_raw1;
  vector[1] beta_raw2;
  vector[3] beta_raw3;
  vector[3] beta_raw4;
  vector[3] beta_raw5;
  vector[5] beta_raw6;
  vector[2] beta_raw7;
}

transformed parameters {
  // 実際の回帰係数 (beta = mu + sigma * beta_raw)
  vector[1] beta1 = mu[1] + sigma[1] * beta_raw1;
  vector[1] beta2 = mu[2] + sigma[2] * beta_raw2;
  vector[3] beta3 = mu[3] + sigma[3] * beta_raw3;
  vector[3] beta4 = mu[4] + sigma[4] * beta_raw4;
  vector[3] beta5 = mu[5] + sigma[5] * beta_raw5;
  vector[5] beta6 = mu[6] + sigma[6] * beta_raw6;
  vector[2] beta7 = mu[7] + sigma[7] * beta_raw7;
}

model {
  // 1. ハイパー事前分布 (各変数の平均とばらつき)
  mu ~ normal(0, 2.5);
  sigma ~ student_t(3, 0, 2.5); // または normal(0, 1)
  
  // 2. パラメータの事前分布 (非中心化)
  beta_raw1 ~ normal(0, 1);
  beta_raw2 ~ normal(0, 1);
  beta_raw3 ~ normal(0, 1);
  beta_raw4 ~ normal(0, 1);
  beta_raw5 ~ normal(0, 1);
  beta_raw6 ~ normal(0, 1);
  beta_raw7 ~ normal(0, 1);
  
  // 切片と主要変数の事前分布
  alpha ~ normal(0, 2.5);
  beta_exp ~ normal(0, 2.5);
  
  // 3. 尤度 (ロジスティック回帰)
  vector[N] logit_p = alpha 
                    + to_vector(exposure) * beta_exp 
                    + X1 * beta1
                    + X2 * beta2
                    + X3 * beta3
                    + X4 * beta4
                    + X5 * beta5
                    + X6 * beta6
                    + X7 * beta7;
                    
  y ~ bernoulli_logit(logit_p);
}

