### Functions fro computations

# 1. Bayesian model
mod <- function(df, MyPrior){
  # SimpleMod <- readRDS("FullETModel.rds")
  fit01 <- brm(data = df,
               family = gaussian(),
               LRes ~ 1 + LTarget + (1 + LTarget|Run),
               iter = 6000, warmup = 1000, thin = 1, chains = 2, cores = 2,
               seed = 5, prior=MyPrior)
  post.fit01 <- as_draws_df(fit01)
  
  return(post.fit01)
}

# 2. Predicted results
predictBayes <- function(post.fit01, Lrel, nRep, nRun, nx){
  L=1-Lrel/100
  U=1/L
  
  
  pred.fit01 <- mutate(post.fit01,
                       Interc= rnorm(n=length(b_Intercept),
                                     mean = b_Intercept,
                                     sd = sd_Run__Intercept/(sqrt(nRun))), 
                       Slope=rnorm(n=length(b_Intercept), 
                                   mean = b_LTarget, 
                                   sd = sd_Run__LTarget/(sqrt(nRun))))
  
  
  t1 <- matrix(nrow=dim(pred.fit01)[1], ncol=length(nx))
  for(j in 1:dim(pred.fit01)[1]) {
    for(t in 1:length(nx)) {
      t1[j,t] <- pred.fit01$Interc[j] + pred.fit01$Slope[j] * nx[t] + rnorm(n=1, mean=0, sd=pred.fit01$sigma[j]/(sqrt(nRep*nRun)))
    }
  }
  
  Pred_res01 <- as.data.frame(t1)
  colnames(Pred_res01) <- as.character(nx)
  
  
  Pred_res01 <- pivot_longer(data=Pred_res01, cols =everything(), names_to="LTarget",
                             values_to = "LRes" ) |>
    mutate(LTarget=as.numeric(LTarget),Lspec=log10(L)+LTarget, 
           Uspec=log10(U)+LTarget)
  
  return(Pred_res01)
}

#3. Probability to be within acceptance criteria
probaSpec <- function(dta){
  Proba_spec01 <- mutate(.data = dta, ID = (LRes > Lspec & LRes < Uspec) ) |>
    group_by(LTarget) |>
    summarise(pi = mean(ID)) 
  return(Proba_spec01)
}

#4. Tolerance limits for Total Error Profile
TISpec <- function(dta, p){
  TI_spec01 <- group_by(.data = dta, LTarget) |>
    summarise(L025 = quantile(LRes, probs=(1-p)/2, na.rm = TRUE), 
              U975 = quantile(LRes, probs=(1+p)/2), na.rm = TRUE)
  TI_spec01 <- mutate(.data = TI_spec01, L025=100*(10**(L025-LTarget)-1), 
                      U975=100*(10**(U975-LTarget)-1) )
  return(TI_spec01)
}

