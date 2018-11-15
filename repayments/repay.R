
repayment.pg <- function(income){
  if(income>21000){
    income * .06
  } else {
    0
  }
}


repayment.ug <- function(income){
  if(income > (2083*12)){
    return(income * .09)
  } else {
    return(0)
  }
}


payoff.pg <- function(amount, income, rpi, payrise){
  
  starting.salary <- income
  total_repayed <- 0
  years <-  0
  d <- tibble()
  interest.rate <- rpi+3
  while(amount > 0  & years < 40){
    
    pay <- repayment.pg(income)
    
    if(pay < amount){
      amount <-  amount - pay
    } else {
      pay <-  amount
      amount <- 0
    }
    total_repayed <- total_repayed + pay
    income <- income + income * (payrise/100)
    amount <- amount + amount * (interest.rate/100)
    years <-  years + 1
    d <- bind_rows(d, 
                   tibble('salary' = income, 'payments'=pay, 'year' = years))
  }
  d
}


payoff.ug <- function(amount=9500*3, income=22000, rpi=3, payrise){
  
  starting.salary <- income
  total_repayed <- 0
  years <-  0
  d <- tibble()
  
  get.interest.rate = function(inc, rpi=rpi){
    if(inc < 25000){
      return(rpi)
    } 
    if (inc > 45000){
      return (rpi + 3)
    }
    # NewValue = (((OldValue - OldMin) * (NewMax - NewMin)) / (OldMax - OldMin)) + NewMin
    # See http://media.slc.co.uk/repayment/qsg/how-much-do-i-repay.html#s2-5
    extrainterest <- (((inc - 25000) * (3 - 0)) / (45000 - 25000)) + 0
    return(rpi+extrainterest)
  }
  
  interest.rate <- get.interest.rate(income, rpi)
  
  while(amount > 0 & years < 40){
    pay <- repayment.ug(income)
    if(pay < amount){
      amount <-  amount - pay
    } else {
      pay <-  amount
      amount <- 0
    }
    total_repayed <- total_repayed + pay
    income <- income + income * (payrise/100) 
    amount <- amount + amount * (interest.rate/100)
    years <-  years + 1
    d <- bind_rows(d, 
                   tibble('salary' = income, 'payments' = pay, 'year' = years))
  }
  d
}

