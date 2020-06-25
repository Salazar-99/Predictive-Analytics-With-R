#First we load the data
mustangs = read.table("Mustangs.csv", header=TRUE, sep=",")
manhattan = read.table("Manhattan.csv", header=TRUE, sep=",")

#Function for computing Bootstrap of given parameter
#Returns vector of Bootstrapped values
bootstrap = function(parameter, data, sample_size){
  parameter_vector = numeric(sample_size)
  for (i in 1:sample_size) {
    x = sample(data, sample_size, replace=TRUE)
    parameter_vector[i] = parameter(x)
  }
  parameter_vector
}

#Storing Mustang Prices column
mustang_prices = mustangs[["Price"]]

#Performing bootstraps for Mustangs Price
mustang_means = bootstrap(mean, mustang_prices, 10000)
mustang_sds = bootstrap(sd, mustang_prices, 10000)
mustang_medians = bootstrap(median, mustang_prices, 10000)

#Storing Manhattan Rents column
rents = manhattan[["Rent"]]

#Performing bootstraps for Manhattan Apt Rents
rents_means = bootstrap(mean, rents, 10000)
rents_sds = bootstrap(sd, rents, 10000)
rents_medians = bootstrap(median, rents, 10000)

#Plotting bootstraps
par(mfrow=c(2,3))
hist(mustang_means)
hist(mustang_sds)
hist(mustang_medians)
hist(rents_means)
hist(rents_sds)
hist(rents_medians)