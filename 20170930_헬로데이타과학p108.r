df1=data.frame(
   Name=c("Jerry", "Tom","Smith"),
   Math=c(50,60,75))
 df1
 
 write.table(mtcars,"mtcars_new.txt")
 cars = read.table("mtcars_new.txt", header=T)
 
 print(cars)
 
 write.table(cars, "clipboard")
 
 head(cars)
 head(cars, n=10)
 tail(cars)
 tail(cars, n=10)
 rownames(cars)
 colnames(cars)
 cars$mpg
 
 table(cars$cyl)
 table(cars$gear, cars$cyl)
 
 hist(cars$mpg)
 plot(cars$wt, cars$mpg)
 qplot(wt, mpg, data=cars, shape=factor(cyl))
 

 
for (i in 1:10) {
  print(i)
}
 
 i <- 1
 while (i <= 10) {
   print(i)
   i <- i+1 
 }
 
 
 
 
 
 