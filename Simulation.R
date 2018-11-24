
library(openxlsx)
library(data.table)
set.seed(42)
dframe=read.xlsx("Práctica de R.xlsx") # upload file

str(dframe) # 1

dframe[,5] <- as.numeric(dframe[,5]) #de char a numeric en EAD LGD y PD
dframe[,4] <- as.numeric(dframe[,4])
dframe[,3] <- as.numeric(dframe[,3])




#megatron = function(n, m, sd, rounding){ funcion para delimitar outliers, despreciada tras no utilizacion
  #base1 = round(rnorm(n, m, sd), rounding)
  #base1[top<base1] = top anteriormente se requeria de top y down valores para eliminar outliers
  #base1[base1<down] = down
# base1
#}

rep.row<-function(x,n){ #funcion para operacion matricial, repite lineas
   matrix(rep(x,each=n),nrow=n)
}
rep.col<-function(x,n){  # repite columnas
   matrix(rep(x,each=n), ncol=n, byrow=TRUE)
}

# 2.1

absolute_zero = function(p_vector, Y_m, Y_sd, wi_m, wi_sd, rows_fm, number_simulations){ #Z = sqrt(p)*Y + sqrt(1-p)* W no for loops
  p = rep(p_vector, number_simulations/2)
  Y = rnorm(number_simulations,Y_m,Y_sd)
  F1 = rep.row(sqrt(p)*Y, rows_fm)
  F2 = rep.row(sqrt(1-p), rows_fm)
  F3 = matrix(rnorm(rows_fm*number_simulations, wi_m, wi_sd), rows_fm, number_simulations)
  return(F1+(F2*F3))

}



absolute_matrix_PD = absolute_zero(c(0.1,0.3),0,1,0,1,nrow(dframe),10000) # matrix and boolean mask
matrix_default = matrix(rep(qnorm(dframe$PD,0,1),10000*103,), 103, 10000)


comparasion_matrix = absolute_matrix_PD < matrix_default   # máscara booleana

collapse = dframe$EAD * dframe$LGD# comparasion_matrix


collapse_matrix = rep.col(collapse, 10000)

comparasion_matrix[comparasion_matrix==TRUE] = collapse_matrix[comparasion_matrix]
comparasion_matrix[comparasion_matrix==FALSE] = 0
comparasion_vector = apply(comparasion_matrix, 2, sum)
comparasion_vector

# 2.2

hist(comparasion_vector, breaks = 50) # lanza el vector en un histograma

# 2.3
quantile_95 =quantile(comparasion_vector, 0.95)



bool_95 = which.min(abs(comparasion_vector - quantile(comparasion_vector, 0.95)))    #función no propia para sacar el percentil más cercano a un abs wich 95




simulation_95 = comparasion_matrix[,bool_95]     #Sacamos el escenario del percentil 95

with_loss_95 = simulation_95[simulation_95>0] # vector perdidas
weighted_95 = with_loss_95/(sum(with_loss_95)) # pesos perdidas

weighted_EAD =  dframe[,3]/sum(dframe[,3])
weighted_EAD = weighted_EAD[which(simulation_95>0)] # pesos EAD

ratio_weighted = weighted_95/weighted_EAD # ratio de pesos
ratio_weighted
#2.4
