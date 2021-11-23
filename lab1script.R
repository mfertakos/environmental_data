my_vec <- c(1,2,3)
mat_1 = matrix(my_vec)

mat_2 = matrix(my_vec,nrow=2,ncol=3)
mat_3=matrix(my_vec,nrow=3,ncol=2)

my_vec <- c(1,2,3,4)
mat_4=matrix(my_vec,nrow=3,ncol=2)

my_list_1 = list(5.2,"five point two",0:5)

my_list_1[["one"]]
#should have returned a value - but where in the list is the element called "one"?

my_list_1$one
#should have returned a value - but where in the list is the element called "one"?

my_list_1$"one"

names(my_list_1)<-c("two","one","three")
