####### Setup ######

data(airquality) #load the data to see it better


###### 1. Rows With Missing Values ######

#complete cases checks if a row has any NA, returns false if NA
missing_rows <- airquality[!complete.cases(airquality), ] #Find all NA rows
#print(missing_rows) #Prints the actual missing rows
#rownames(missing_rows) #This gives all the rows as characters
missing_rows_numbers <- as.character(rownames(missing_rows)) #Stores as numbers 
print(missing_rows_numbers) # Prints to display 


###### 2. Ozone and Temperature Analysis ######
# Find a way to only use Ozone and Temperature rows with valid values
valid_ozone <- airquality$Ozone[complete.cases(airquality$Ozone)]
print(valid_ozone)

#All temperatures have values, but this is to ensure it (might be useless)
valid_temp <- airquality$Temp[complete.cases(airquality$Temp)]
print(valid_temp)

#Statistical analysis 
mean_ozone <- mean(valid_ozone)
sd_ozone <- sd(valid_ozone)
min_ozone <- min(valid_ozone)
max_ozone <- max(valid_ozone)
ozone_stats <- c(mean_ozone, sd_ozone, min_ozone, max_ozone) #Vector for df

mean_temp <- mean(valid_temp)
sd_temp <- sd(valid_temp)
min_temp <- min(valid_temp)
max_temp <- max(valid_temp)
temp_stats <- c(mean_temp, sd_temp, min_temp, max_temp) #Vector for df

#Creating a dataframe of the stats for printing purposes
df <- data.frame(ozone_stats, temp_stats)
rownames(df) <- c("Mean", "SD", "Min", "Max")
colnames(df) <- c("Ozone", "Temperature")
print(df)

################################################################################

############## CARS DATASET ###############
##### SETUP #####
data(cars)
class(cars)

##### VARIABLE DECLARATIONS #####
y_vec <- cars$dist #dependent
x_matrix <- model.matrix(dist ~ speed, data = cars) #design matrix

print(x_matrix)

##### COMPUTING X^TX #####
xtx_matrix <- t(x_matrix) %*% x_matrix #transpose then matrix multiply
print(xtx_matrix)

##### INVERSE #####
inv_matrix <- solve(xtx_matrix) #no b value therefore solves for inverse
print(inv_matrix)

##### COMPUTING X^TY #####
xty <- t(x_matrix) %*% y_vec
print(xty)

##### PARAMETER ESTIMATION USING GIVEN FORMULA #####
beta <- inv_matrix %*% xty
print(beta)

y_hat <- x_matrix %*% beta
print(y_hat)
resids <- y_hat - y_vec

##### RESIDUAL VARIANCE #####
p <- ncol(x_matrix)
resvar <- sum(resids^2)/(n_obs-p)
sigma <- sqrt(resvar)
var_covar_matrix <- resvar * inv_matrix
print(var_covar_matrix)

##### STANDARD ERROR #####
stderr <- sqrt(diag(var_covar_matrix))
print(stderr)

##### T STAT #####
t_stat <- beta/stderr
print(t_stat)

##### P-VALUES #####
p_val <- 2*(1-pt(abs(t_stat), df = n_obs - p))
print(p_val)

##### DATAFRAME FOR PRINTING #####
results <- data.frame(row.names = c("(Intercept)", "Speed"), 
                      Estimate = beta, 
                      Std_Error = stderr, 
                      t_statistic = t_stat, 
                      p_value = p_val)

print(results)

##### COMPARE WITH LM() #####
lin_reg_model = lm(dist ~ speed, data = cars)
summary(lin_reg_model)

##### SHIFTING ELEMENTS IN A ######
#n_obs <- nrow(cars)
#reverse_matrix <- with(cars, matrix(c(sum(speed^2), -sum(speed), -sum(speed), n_obs), nrow = 2))
#print(reverse_matrix)

##### SCALAR CONSTANT FOR MATRIX INVERSION #####
#constant <- (with(cars, sum((speed-mean(speed))^2))*n_obs)^-1
#constant*reverse_matrix
