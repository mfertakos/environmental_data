a <- "Matt"
b1 <- 45.6
b2 <- "45.6"
c1 <- c(0:3)

#Q1: the data within a is a character
#Q2: b1 is numeric data
#Q3: the data within b2 is a character
#Q4: There is an error - you cannot add numeric data with character data
#Q5: No, b1 is a numeric class while c1 is an integer class.
#Q6: 45.6 (a numeric vector) is added to each integer from 0 to 3 resulting in 4 outputs: 45.6 + 0 = 45.6, 45.6 + 1 = 46.6, 45.6 + 2 = 47.6, 45.6 + 3 = 48.6.

#Q7
v1<-c(-2:2)
#Q8
v2<-3*v1
#Q9
sum(v2)

vec_4<-c(1:12)
#Q10
mat_1<-matrix(vec_4,byrow=TRUE,nrow=3,ncol=4)
#Q11
mat_2<-matrix(vec_4,byrow=FALSE,nrow=3,ncol=4)

#Q12
my_list_1<-list(5.2,"five point two",0:5)
names(my_list_1)<-c("two","one","three")
#Q13
my_list_1$three
#Q14
my_list_1$one #or
my_list_1[[2]]

#Q15
my_vec = rep(1:3,5)
my_bool_vec <- my_vec == 3
data.frame(my_vec,my_bool_vec)
#Q16
my_vec[which(my_bool_vec==TRUE)]

