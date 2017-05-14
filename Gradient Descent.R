library(readr)
kc_house_data <- read_csv("C:/Data Science/Machine Learning/kc_house_data.csv")
#Clean the data
myData=as.data.frame(kc_house_data)
View(myData)
myData=myData[,-1]
myData=myData[,-1]
myData$sqft_area=scale(myData[4])

##################
0.8* nrow(myData)
#Split the data.....
myTrain=myData[0:(0.8 * nrow(myData)),]
myTest=myData[(nrow(myTrain)+1) : nrow(myData),]

View(myTrain)
nrow(myTest)
nrow(myTrain)
# y = mx + b
# m is slope, b is y-intercept
error = function(b, m, myTrain )
  {
  totalError = 0
for (i in 1 : (nrow(myTrain)))
  {
  x = myTrain[i,20]
  y = myTrain[i,1]
totalError = totalError + ((y - (m * x + b)) ^ 2)

}
return (totalError / (2*(nrow(myTrain))))
}

error(500000, 1, myTrain)


grad=function (b_current, m_current, myTrain, learningRate)
{
m_gradient = 0
b_gradient = 0
N = nrow(myTrain)
for (i in 1 : N)
{
x = myTrain[i, 20]
y = myTrain[i, 1]
m_gradient = m_gradient + (-2/N) * (x ) * (y - ((m_current * x) + b_current))
b_gradient = b_gradient + ((-2/N) * (y - ((m_current * x) + b_current)))

}
new_m = m_current - (learningRate * m_gradient)
new_b = b_current - (learningRate * b_gradient)
return(c(new_b,new_m))
}


gradrunner=function(myTrain, starting_b, starting_m, learning_rate)
{
b = starting_b
m = starting_m
all_e=c()
dev.flush()
limit=1e-10
continue=TRUE
while (continue)
{
bm= grad(b, m, myTrain, learning_rate)
continue=((abs(b-bm[1]))>limit) & ((abs(m-bm[2])) > limit)
print(abs(m-bm[2]))
b=bm[1]
m=bm[2]
lines(b)
#points(all_e,i)
#lines(all_e[i])
}

#plot(all_e[1:10], type="o", col="blue") 
return (bm)
}

a=gradrunner(myTrain,500000, 0.25, 0.3)


print(a[1])
print(a[2])

lines(0)
a=c(10)
lines(a)
b=c(2)
lines(a,b)
d=c(a,b)
d
plot(d)
lines(a)
lines(d)
  
  
  
)