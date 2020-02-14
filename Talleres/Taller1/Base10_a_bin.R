#De Base 10 a binaria

calcular_numero_entero_a_binario = function(x)
{
  binario=0
  expo=1
  while(x>0)
  {
    
    digito = x%%2
   binario =  binario+expo*digito
    expo=expo*10
    x = x%/%2
    
  }
  return (binario)
}


calcular_numero_decimal_a_binario = function(x,numero_de_bits)
{
  num=0
  for (i in 1:numero_de_bits)
  {
    x = 2*x;
    
    
    if(x < 1)
    {
      num = paste(num,"0",sep="")
      
    }
    else
    {
      num = paste(num,"1",sep="")
      x = x - 1;
    }
  }
  return(num)
}

cat("1er numero en binario: ", calcular_numero_entero_a_binario(11),".",calcular_numero_decimal_a_binario(0.25,08), "\n")
cat("2do numero en binario: ",calcular_numero_decimal_a_binario(0.6666666666,08), "\n")
cat("3er numero en binario: ", calcular_numero_entero_a_binario(30),".",calcular_numero_decimal_a_binario(0.6,08), "\n")
cat("4to numero en binario: ", calcular_numero_entero_a_binario(99),".",calcular_numero_decimal_a_binario(0.9,08), "\n")