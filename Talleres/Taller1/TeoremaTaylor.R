#Teorema de Taylor
rm(list=ls()) #Borrar los datos guardados en memoria
n<-0 #Valor Inicial
x<-0.5 #Constante
y<-function(x,n){
  x^n/factorial(n)
}
valx<-c(x)
valy<-c(y(x,n)) #Valores que tomara y en cada iteracion
valtay<-c(sum(valy))#Valores de aproximacion del Teorema de Taylor
temp<-sum(valy) #Auxiliar
valn<-c(n) #Valores de n en cada iteracion
valE<-c(abs(exp(x)-mean(valtay))) #Valores del Error Absoluto en cada iteracion
vale<-c(abs(exp(x)-mean(valtay))/mean(valtay)) #Valores del error relativo en cada iteracion
n<-n+1
repeat{
  valx<-c(valx,x)
  valy<-c(valy,y(x,n))
  valtay<-c(valtay,sum(valy))
  valn<-c(valn,n)
  valE<-c(valE,abs(exp(x)-mean(valtay)))
  vale<-c(vale,abs(exp(x)-mean(valtay))/mean(valtay))
  temp<-sum(valy)
  n<-n+1
  if(n>5) break
}
precision<-100-vale
#Guardar resultados en una tabla de datos
resultados<-data.frame("X"=valx,"N"=valn,"Y"=valy,"Taylor"=valtay,"Error_Absoluto"=valE,"Error_Relativo"=vale,"Precision"=precision)
#Exportar los resultados
write.table(resultados,file="ResultadosTeoremaTaylor.txt")
#Imprimir resultados
print(resultados)
#--------------------------------------------