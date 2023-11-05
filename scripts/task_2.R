simTweedieTest <-  
  function(N){
    t.test(
      rtweedie(N, 
               mu    = 10000, 
               phi   = 100, 
               power = 1.9), 
      mu=10000)$p.value 
    
  } 

MTweedieTests <-
  function(N, M, sig) {
    sum(replicate(M, simTweedieTest(N)) < sig) / M
  }

MTweedieTests(N = 100, M = 1000, sig = .05)

df <- 
  tibble(N = c(10, 100, 1000, 5000), 
         M = 100, 
         share_reject = NA)

# Setting max cores (I only have 2)
maxcores <- 2
Cores <- min(parallel::detectCores(), maxcores)

# Instantiate the cores
cl <- makeCluster(Cores)

# Register the cluster
registerDoParallel(cl)

df_2 <- foreach( # Identify task for cores
  i = 1:nrow(df),
  .combine = 'rbind', # Row bind after finished
  .packages = c('tweedie', 'tidyverse') # Package needed in process
) %dopar% # Execute below parallel
  tibble(
    N = df$N[i],
    M = df$M[i],
    share_reject =
      MTweedieTests(
        df$N[i],
        df$M[i],
        0.05
      )
  )
stopCluster(cl) # Stop the cluster

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




