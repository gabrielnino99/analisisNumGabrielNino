#De Binario a base 10

calcular_la_parte_entera = function(X){
  cont_potencias = 0
  digito = 0
  num=0
  
  while(X > 0){
    digito = X %% 10
    
    num = num + (digito*(2^cont_potencias))
    
    cont_potencias = cont_potencias + 1
    
    X = X %/% 10
  }
  
  return(num)}
calcular_la_parte_decimal = function(X){
  X_2 = X
  X_2
  cont_potencias_2 = 0
  nums = 0
  digito = 0
  total=0
  
  while(X > 0){
    nums = nums+1
    
    X = X %/% 10
  }
  cont_potencias_2 =-nums;
  
  while(X_2 > 0){
    digito = X_2 %% 10
    
    total = total + (digito*(2^cont_potencias_2))
    
    cont_potencias_2 = cont_potencias_2 + 1
    
    X_2 = X_2 %/% 10
  }
  return(total)}
cat("Numero 1: ",calcular_la_parte_entera(10111) + calcular_la_parte_decimal(010101))
cat("Numero 2: ",calcular_la_parte_entera(1011) + calcular_la_parte_decimal(101),"\n")
cat("Numero 3: ",calcular_la_parte_entera(111) + calcular_la_parte_decimal(111111))
cat("Numero 4: ",calcular_la_parte_entera(101010101),"\n")