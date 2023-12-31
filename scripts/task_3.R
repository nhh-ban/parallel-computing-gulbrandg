simTweedieTest <-  
  function(N){
    t.test(
      rtweedie(N, 
               mu    = 10000, 
               phi   = 100, 
               power = 1.9), 
      mu=10000)$p.value 
    
  } 

# Setting max cores (I only have 2)
maxcores <- 2
Cores <- min(parallel::detectCores(), maxcores)
sTT <- simTweedieTest # Set function (could not get tis to work while only setting)
                      # function outside core setting
MTweedieTests <-
  function(N, M, sig) {
    sTT <- simTweedieTest 
    the_sum <- foreach( # Identify task for cores
      i = 1:M,
      .combine = 'rbind', # Row bind after finished
      .packages = c('tweedie', 'tidyverse') # Package needed in process
    ) %dopar% # Execute  below parallel
      tibble(
        t = (sTT(N)) < sig)
    
    return( sum(the_sum) / M)
  }
cl <- makeCluster(Cores)  # Initate clusters (takes some time so I put it outrside)
                          # Should have maybe put it inside to make all parallel 
                          # computations part of the function
MTweedieTests(N = 100, M = 1000, sig = .05)

df <- 
  tibble(N = c(10, 100, 1000, 5000), 
         M = 100, 
         share_reject = NA)

for (i in 1:nrow(df)) {
  
  df$share_reject[i] <-
    MTweedieTests(N   = df$N[i],
                  M   = df$M[i],
                  sig = .05)
}
stopCluster(cl) # Stop cluster when all need for the function is finished



df |> 
  ggplot(aes(x = log(N), 
             y = share_reject)) +
  geom_line() +
  geom_hline(yintercept = .05) +
  theme_bw()

simDat <- function(N, type, mu){
  if(type == "tweedie"){ 
    return(rtweedie(N, 
                    mu    = mu, 
                    phi   = 100, 
                    power = 1.9)) 
  } 
  
  if(type=="normal"){
    return(rnorm(N, mean = mu)) 
  } 
  else{
    stop("invalid distribution") 
  } 
} 

simTest <-  function(N, type, mu){
  t.test(
    simDat(N    = N, 
           type = type, 
           mu   = mu), 
    mu = mu)$p.value
} 

MTests <- function(N, M, type, sig){ 
  sum(
    replicate(M, 
              simTest(N    = N, 
                      type = type, 
                      mu   = 10000)
    ) < sig
  ) / M
} 


df <- 
  expand.grid(N            = c(10, 100, 1000, 5000), 
              M            = 1000,
              type         = c("tweedie","normal"),
              share_reject = NA) %>%
  as_tibble() 


for(i in 1:nrow(df)){
  
  print(i) 
  
  df$share_reject[i] <-  
    MTests(
      df$N[i], 
      df$M[i], 
      df$type[i],
      .05)
} 

