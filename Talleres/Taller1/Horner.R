#Teorema de Horner

polinomio = c(-4,3,-3,0,2)
x_0 = -2
pol_b = polinomio[length(polinomio)]
numDeSumas = 0
numDeMultiplicaciones = 0
ultimaPos = length(polinomio)-1

Horner = function(polinomio,x_0)
  {
  
  for (i in seq(end,1,-1))
    {
    pol_b = polinomio[i] + pol_b*x_0
    numDeSumas = numDeSumas + 1
    numDeMultiplicaciones = numDeMultiplicaciones +1
  }
  
  print(pol_b)
  cat("El numero total de sumas es :",numDeSumas,"\n")
  cat("El numero total de Multiplicaciones es :",numDeMultiplicaciones,"\n")
}

Horner(polinomio,x_0)
