#Numeros Binarios
cat("Numero Pi en binario")

varPi= pi - 3
for (i in 1:14)
{
  varPi = varPi*2;
  
  if(varPi >= 1){
    cat("1")
    varPi = varPi - 1;}
  else{
    cat("0")}
}


