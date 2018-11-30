library(datasets)
library("phonTools", lib.loc="~/R/win-library/3.5")
iris.df =data(iris)
data(iris)
data("iris")
iris.df=iris

#options(warn=1) ##### used to display the first warning

total_cost = 0

lr = 0.0000001

J = vector(mode = "numeric" , length = 300 )

iris.df$y1 = 0

iris.df$y2 = 0

iris.df$y3 = 0

iris.df$y1[iris.df$Species =="setosa"] = 1

iris.df$y2[iris.df$Species =="versicolor"] = 1

iris.df$y3[iris.df$Species =="virginica"] = 1

w1b = matrix(rexp(20, rate=.1), ncol=5)  #Declaring weights between 1st layer

w1b = wb1*0.24 - 0.12

w2b = matrix(rexp(12, rate=.1), ncol=4)

w2b = w2b*0.24 - 0.12

d1f = zeros(3,4)

d2f = zeros(4,5)

sigmoid = function(z)
{
  sig = 1/( 1 + exp(-z) )
 
  return(sig)  
}

attach(iris.df)

for (j in 1:300) {
  


for (i in 1:150) {
  

# Forward Propagation For First Layer

a1 = sigmoid( 1*w1b[1,1] + iris.df$Sepal.Length[i]*w1b[1,2] + iris.df$Sepal.Width[i]*w1b[1,3] + iris.df$Petal.Length[i]*w1b[1,4] + iris.df$Petal.Width[i]*w1b[1,5] )
  
a2 = sigmoid( 1*w1b[2,1] + iris.df$Sepal.Length[i]*w1b[2,2] + iris.df$Sepal.Width[i]*w1b[2,3] + iris.df$Petal.Length[i]*w1b[2,4] + iris.df$Petal.Width[i]*w1b[2,5] )

a3 = sigmoid( 1*w1b[3,1] + iris.df$Sepal.Length[i]*w1b[3,2] + iris.df$Sepal.Width[i]*w1b[3,3] + iris.df$Petal.Length[i]*w1b[3,4] + iris.df$Petal.Width[i]*w1b[3,5] )

a4 = sigmoid( 1*w1b[4,1] + iris.df$Sepal.Length[i]*w1b[4,2] + iris.df$Sepal.Width[i]*w1b[4,3] + iris.df$Petal.Length[i]*w1b[4,4] + iris.df$Petal.Width[i]*w1b[4,5] )

#Forward Propagation For Second Layer

o1 = sigmoid( 1*w2b[1,1] + a1*w2b[1,2] + a1*w2b[1,3] + a1*w2b[1,4] )

o2 = sigmoid( 1*w2b[2,1] + a2*w2b[2,2] + a2*w2b[2,3] + a2*w2b[2,4] )

o3 = sigmoid( 1*w2b[3,1] + a3*w2b[3,2] + a3*w2b[3,3] + a3*w2b[3,4] )




#Backward Propagation For First Layer

d1f[1,1] = d1f[1,1] + o1*(1-o1) *(o1-y1[i])  #For Baised Node

d1f[2,1] = d1f[2,1] + o2*(1-o2) *(o2-y2[i])  #For Baised Node

d1f[3,1] = d1f[3,1] + o3*(1-o3) *(o3-y3[i])  #For Baised Node



d1f[1,2] = d1f[1,2] + a1*o1*(1-o1)*(o1-y1[i])

d1f[2,2] = d1f[2,2] + a1*o2*(1-o2)*(o2-y2[i])

d1f[3,2] = d1f[3,2] + a1*o3*(1-o3)*(o3-y3[i])




d1f[1,3] = d1f[1,3] + a2*o1*(1-o1)*(o1-y1[i])

d1f[2,3] = d1f[2,3] + a2*o2*(1-o2)*(o2-y2[i])

d1f[3,3] = d1f[3,3] + a2*o3*(1-o3)*(o3-y3[i])




d1f[1,4] = d1f[1,4] + a3*o1*(1-o1)*(o1-y1[i])

d1f[2,4] = d1f[2,4] + a3*o2*(1-o2)*(o2-y2[i])

d1f[3,4] = d1f[3,4] + a3*o3*(1-o3)*(o3-y3[i])



#Backward Propagation For Second Layer

d2f[1,2] = d2f[1,2] + iris.df$Sepal.Length[i] * a1*(1-a1) * ( (o1-y1[i])*o1*(1-o1)*w2b[1,2] + (o2-y2[i])*o2*(1-o2)*w2b[2,2] + (o3-y3[i])*o3*(1-o3)*w2b[3,2] )

d2f[1,3] = d2f[1,3] + iris.df$Sepal.Width[i] * a1*(1-a1) * ( (o1-y1[i])*o1*(1-o1)*w2b[1,2] + (o2-y2[i])*o2*(1-o2)*w2b[2,2] + (o3-y3[i])*o3*(1-o3)*w2b[3,2] )

d2f[1,4] = d2f[1,4] + iris.df$Petal.Length[i] * a1*(1-a1) * ( (o1-y1[i])*o1*(1-o1)*w2b[1,2] + (o2-y2[i])*o2*(1-o2)*w2b[2,2] + (o3-y3[i])*o3*(1-o3)*w2b[3,2] )

d2f[1,5] = d2f[1,5] + iris.df$Petal.Width[i] * a1*(1-a1) * ( (o1-y1[i])*o1*(1-o1)*w2b[1,2] + (o2-y2[i])*o2*(1-o2)*w2b[2,2] + (o3-y3[i])*o3*(1-o3)*w2b[3,2] )


###########

d2f[1,1] = d2f[1,1] + a1*(1-a1) * ( (o1-y1[i])*o1*(1-o1)*w2b[1,2] + (o2-y2[i])*o2*(1-o2)*w2b[2,2] + (o3-y3[i])*o3*(1-o3)*w2b[3,2] ) #For Biased Node

d2f[2,1] = d2f[2,1] + a2*(1-a2) * ( (o1-y1[i])*o1*(1-o1)*w2b[1,2] + (o2-y2[i])*o2*(1-o2)*w2b[2,2] + (o3-y3[i])*o3*(1-o3)*w2b[3,2] ) #For Biased Node

d2f[3,1] = d2f[3,1] + a3*(1-a3) * ( (o1-y1[i])*o1*(1-o1)*w2b[1,2] + (o2-y2[i])*o2*(1-o2)*w2b[2,2] + (o3-y3[i])*o3*(1-o3)*w2b[3,2] ) #For Biased Node

d2f[4,1] = d2f[4,1] + a4*(1-a4) * ( (o1-y1[i])*o1*(1-o1)*w2b[1,2] + (o2-y2[i])*o2*(1-o2)*w2b[2,2] + (o3-y3[i])*o3*(1-o3)*w2b[3,2] ) #For Biased Node





d2f[2,2] = d2f[2,2] + iris.df$Sepal.Length[i] * a2*(1-a2) * ( (o1-y1[i])*o1*(1-o1)*w2b[1,2] + (o2-y2[i])*o2*(1-o2)*w2b[2,2] + (o3-y3[i])*o3*(1-o3)*w2b[3,2] )

d2f[2,3] = d2f[2,3] + iris.df$Sepal.Width[i] * a2*(1-a2) * ( (o1-y1[i])*o1*(1-o1)*w2b[1,2] + (o2-y2[i])*o2*(1-o2)*w2b[2,2] + (o3-y3[i])*o3*(1-o3)*w2b[3,2] )

d2f[2,4] = d2f[2,4] + iris.df$Petal.Length[i] * a2*(1-a2) * ( (o1-y1[i])*o1*(1-o1)*w2b[1,2] + (o2-y2[i])*o2*(1-o2)*w2b[2,2] + (o3-y3[i])*o3*(1-o3)*w2b[3,2] )

d2f[2,5] = d2f[2,5] + iris.df$Petal.Width[i] * a2*(1-a2) * ( (o1-y1[i])*o1*(1-o1)*w2b[1,2] + (o2-y2[i])*o2*(1-o2)*w2b[2,2] + (o3-y3[i])*o3*(1-o3)*w2b[3,2] )




d2f[3,2] = d2f[3,2] + iris.df$Sepal.Length[i] * a3*(1-a3) * ( (o1-y1[i])*o1*(1-o1)*w2b[1,2] + (o2-y2[i])*o2*(1-o2)*w2b[2,2] + (o3-y3[i])*o3*(1-o3)*w2b[3,2] )

d2f[3,3] = d2f[3,3] + iris.df$Sepal.Width[i] * a3*(1-a3) * ( (o1-y1[i])*o1*(1-o1)*w2b[1,2] + (o2-y2[i])*o2*(1-o2)*w2b[2,2] + (o3-y3[i])*o3*(1-o3)*w2b[3,2] )

d2f[3,4] = d2f[3,4] + iris.df$Petal.Length[i] * a3*(1-a3) * ( (o1-y1[i])*o1*(1-o1)*w2b[1,2] + (o2-y2[i])*o2*(1-o2)*w2b[2,2] + (o3-y3[i])*o3*(1-o3)*w2b[3,2] )

d2f[3][5] = d2f[3,5] + iris.df$Petal.Width[i] * a3*(1-a3) * ( (o1-y1[i])*o1*(1-o1)*w2b[1,2] + (o2-y2[i])*o2*(1-o2)*w2b[2,2] + (o3-y3[i])*o3*(1-o3)*w2b[3,2] )




d2f[4,2] = d2f[4,2] + iris.df$Sepal.Length[i] * a4*(1-a4) * ( (o1-y1[i])*o1*(1-o1)*w2b[1,2] + (o2-y2[i])*o2*(1-o2)*w2b[2,2] + (o3-y3[i])*o3*(1-o3)*w2b[3,2] )

d2f[4][3] = d2f[4,3] + iris.df$Sepal.Width[i] * a4*(1-a4) * ( (o1-y1[i])*o1*(1-o1)*w2b[1,2] + (o2-y2[i])*o2*(1-o2)*w2b[2,2] + (o3-y3[i])*o3*(1-o3)*w2b[3,2] )

d2f[4,4] = d2f[4,4] + iris.df$Petal.Length[i] * a4*(1-a4) * ( (o1-y1[i])*o1*(1-o1)*w2b[1,2] + (o2-y2[i])*o2*(1-o2)*w2b[2,2] + (o3-y3[i])*o3*(1-o3)*w2b[3,2] )

d2f[4,5] = d2f[4,5] + iris.df$Petal.Width[i] * a4*(1-a4) * ( (o1-y1[i])*o1*(1-o1)*w2b[1,2] + (o2-y2[i])*o2*(1-o2)*w2b[2,2] + (o3-y3[i])*o3*(1-o3)*w2b[3,2] )




cost = ( iris.df$y1[i] * log(o1) + (1-y1[i]) * log(1-o1) + iris.df$y2[i] * log(o2) + (1-y2[i]) * log(1-o2) + iris.df$y3[i] * log(o3) + (1-y3[i]) * log(1-o3) )/nrow(iris.df)

total_cost = total_cost + cost

}

############# Weights Updation

  J[j] = total_cost

w1b[1,1] = w1b[1,1] - (lr/nrow(iris.df)) * d2f[1,1]

w1b[1,2] = w1b[1,2] - (lr/nrow(iris.df)) * d2f[1,2]

w1b[1,3] = w1b[1,3] - (lr/nrow(iris.df)) * d2f[1,3]

w1b[1,4] = w1b[1,4] - (lr/nrow(iris.df)) * d2f[1,4]

w1b[1,5] = w1b[1,5] - (lr/nrow(iris.df)) * d2f[1,5]




w1b[2,1] = w1b[2,1] - (lr/nrow(iris.df)) * d2f[2,1]

w1b[2,2] = w1b[2,2] - (lr/nrow(iris.df)) * d2f[2,2]

w1b[2,3] = w1b[2,3] - (lr/nrow(iris.df)) * d2f[2,3]

w1b[2,4] = w1b[2,4] - (lr/nrow(iris.df)) * d2f[2,4]

w1b[2,5] = w1b[2,5] - (lr/nrow(iris.df)) * d2f[2,5]




w1b[3,1] = w1b[3,1] - (lr/nrow(iris.df)) * d2f[3,1]

w1b[3,2] = w1b[3,2] - (lr/nrow(iris.df)) * d2f[3,2]

w1b[3,3] = w1b[3,3] - (lr/nrow(iris.df)) * d2f[3,3]

w1b[3,4] = w1b[3,4] - (lr/nrow(iris.df)) * d2f[3,4]

w1b[3,5] = w1b[3,5] - (lr/nrow(iris.df)) * d2f[3,5]




w1b[4,1] = w1b[4,1] - (lr/nrow(iris.df)) * d2f[4,1]

w1b[4,2] = w1b[4,2] - (lr/nrow(iris.df)) * d2f[4,2]

w1b[4,3] = w1b[4,3] - (lr/nrow(iris.df)) * d2f[4,3]

w1b[4,4] = w1b[4,4] - (lr/nrow(iris.df)) * d2f[4,4]

w1b[4,5] = w1b[4,5] - (lr/nrow(iris.df)) * d2f[4,5]



w2b[1,1] = w2b[1,1] - (lr/nrow(iris.df)) * d1f[1,1]

w2b[1,2] = w2b[1,2] - (lr/nrow(iris.df)) * d1f[1,2]

w2b[1,3] = w2b[1,3] - (lr/nrow(iris.df)) * d1f[1,3]

w2b[1,4] = w2b[1,4] - (lr/nrow(iris.df)) * d1f[1,4]




w2b[2,1] = w2b[2,1] - (lr/nrow(iris.df)) * d1f[2,1]

w2b[2,2] = w2b[2,2] - (lr/nrow(iris.df)) * d1f[2,2]

w2b[2,3] = w2b[2,3] - (lr/nrow(iris.df)) * d1f[2,3]

w2b[2,4] = w2b[2,4] - (lr/nrow(iris.df)) * d1f[2,4]




w2b[3,1] = w2b[3,1] - (lr/nrow(iris.df)) * d1f[3,1]

w2b[3,2] = w2b[3,2] - (lr/nrow(iris.df)) * d1f[3,2]

w2b[3,3] = w2b[3,3] - (lr/nrow(iris.df)) * d1f[3,3]

w2b[3,4] = w2b[3,4] - (lr/nrow(iris.df)) * d1f[3,4]

}


plot(J)

