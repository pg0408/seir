require(deSolve)

main <- function(){
  
  ## no intervention
  
  SEIR <- function(time, current_state, params){
    
    with(as.list(c(current_state, params)),{
      N <- S+E+I+R
      dS <- -(beta*S*I)/N
      dE <- (beta*S*I)/N - sigma*E
      dI <- sigma*E - gamma*I - mu*I
      dR <- gamma*I
      dM <- mu*I
      
      return(list(c(dS, dE, dI, dR, dM)))
    })
  }
  
  params <- c(
    beta=0.5, 
    sigma=0.25, 
    gamma=0.2, 
    mu=0.001
    )
  
  initial_state <- c(S=999999, E=1, I=0, R=0, M=0)
  
  times <- 0:365
  
  model <- ode(initial_state, times, SEIR, params)
  
  print(summary(model))
  
  matplot(
    model,
    type="l",
    lty=1,
    main="SEIR model",
    xlab="Time"
    )
  
  legend <- colnames(model)[2:6]
  
  legend("right", legend=legend, col=2:6, lty = 1)
  
  infections <- as.data.frame(model)$I
  
  peak <- max(infections)
  
  match(peak, infections)
  
  print(paste("peak:",peak))
  
  print(paste("peak day:", match(peak, infections)))
  
  
  ### with lockdown
  
  require(deSolve)
  
  SEIR_lockdown <- function(time, current_state, params){
  
    with(as.list(c(current_state, params)),{
      
      beta = ifelse(
        (time <= start_lockdown || time >= end_lockdown),
        0.5, 0.1
        )
      
      N <- S+E+I+R
      dS <- -(beta*S*I)/N
      dE <- (beta*S*I)/N - sigma*E
      dI <- sigma*E - gamma*I - mu*I
      dR <- gamma*I
      dM <- mu*I
      
      return(list(c(dS, dE, dI, dR, dM)))
    })
  }
  
  params <- c(
    sigma=0.25,
    gamma=0.2,
    mu=0.001,
    start_lockdown=90,
    end_lockdown=150
    )
  
  initial_state <- c(S=999999, E=1, I=0, R=0, M=0)
  
  times <- 0:365
  
  model <- ode(initial_state, times, SEIR_lockdown, params)
  
  print(summary(model))
  
  matplot(
    model, 
    type="l",
    lty=1, 
    main="SEIR model (with intervention)", 
    xlab="Time"
    )
  
  legend <- colnames(model)[2:6]
  
  legend("right", legend=legend, col=2:6, lty = 1)
  
  infections <- as.data.frame(model)$I
  
  peak <- max(infections)
  
  match(peak, infections)
  
  print(paste("peak:",peak))
  
  print(paste("peak day:", match(peak, infections)))
}

main()

