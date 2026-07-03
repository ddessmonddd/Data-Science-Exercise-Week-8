#week 8
install.packages("dplyr")
install.packages("readxl")
library(dplyr)
library(readxl)
student_data <- read_excel("C:/Users/Bernice Ng/Downloads/student_data.xlsx")
Student_fail<- Student_fail<- student_data[student_data$final_exam_mark< 40,]
View(Student_fail)
View(student_data)

Student_fail<- student_data %>% filter(final_exam_mark< 40)
View(Student_fail)

mydata1<-student_data%>% filter(final_exam_mark> 40) %>% arrange(desc(final_exam_mark))
View(mydata1)

mydata2 <- student_data%>% select(student_id,coursework_mark,final_exam_mark)
View(mydata2)

mydata4 = student_data%>% mutate(Total_Mark=(coursework_mark +
                                               final_exam_mark/200*100))
View(mydata4)

data(iris)
head(iris)
tail(iris)
str(iris)

min(iris$Sepal.Length) 
max(iris$Sepal.Length) 
range(iris$Sepal.Length)
sd(iris$Sepal.Length)
var(iris$Sepal.Length)
summary(iris)

#quantile
A<-c(170.2, 181.5, 188.9, 163.9, 166.4, 163.7, 160.4, 175.8, 181.5)
quantile(A)
sort_A<-sort(A)
quantile(A,0.25)
quantile(A,0.75)

#counting column and row
nrow(iris)
ncol(iris)
sum(is.na(iris$Sepal.Length))
hist(iris$Sepal.Length,
     main = "Histogram of Sepal Length",
     xlab = "Sepal Length (cm)",
     ylab = "Frequency",
     col = "lightblue",
     border = "black")

data<-c(30,24,26,28,29,28,27,26,32,34,13,15,14,31,29,28,24,25,30,34,35,27,30,34,44,48)
boxplot(data, main = "Boxplot")
first_q <-quantile(data,0.25)
third_q <-quantile(data,0.75)
inter_q <-quantile(data,0.5)

#Quartile
iqr<-IQR(data)
#Lower extreme
lower_extreme <-first_q - 1.5 * iqr
upper_extreme<-third_q + 1.5 * iqr

#how to deal with outlier

#remove the outlier
data_new<-data
data_new <- data_new[!data_new<lower_extreme]
data_new <- data_new[!data_new>upper_extreme]
data_new

#replace with upper extreme and lower extreme value
data_new<- data
avg <- round(mean(data_new))#for the purpose of example we round up value
data_new[data_new<lower_extreme] <- avg
data_new[data_new>upper_extreme] <- avg
data_new

