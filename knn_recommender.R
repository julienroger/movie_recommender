## Julien Roger <julienroger@gmail.com>
## COMP 652 - Machine Learning


##############################
# f-fold Validation Function #
##############################

# Description: This function runs f-fold validation on the ML100K dataset (The k variable was assigned to the number of neighbours). The function takes as inputs for the k-NN algorithm k, the number of neighbours to test against, sim_func, the similarity measure between users, and mean_method, the method for calculating hte mean rating of the k nearest neighbours. The function returns the mean RMSE.


fold_val <- function(f = 5, k = 5, sim_func = "cos_sim", mean_method = "simple_mean") {

  rmse_vec <- rep(NA, 5)

  ## Similarity function
    # Cosine Similarity
    cos_sim <- function(i, j){
      (sum(ratings.matrix[i,] * ratings.matrix[j,], na.rm = TRUE)) / (sqrt(sum((ratings.matrix[i,] + ratings.matrix[j,] - ratings.matrix[j,]) * ratings.matrix[i,], na.rm = TRUE)) * sqrt(sum((ratings.matrix[j,] + ratings.matrix[i,] - ratings.matrix[i,]) * ratings.matrix[j,], na.rm = TRUE)))
    }

    # Pearson correlation
    p_corr <- function(i, j){

      value <- try(suppressWarnings((cor(ratings.matrix[i,], ratings.matrix[j,], use = "complete.obs", method = "pearson")+1)/2), silent = TRUE)

      if(class(value) == "try-error"){
        return(0)
      } else if(is.na( value )) {
        return(0)
      } else if(value == 0){
        return(0)
      } else {
        return(value)
      }
    }

    # Euclidean distance
    euc_dis <- function(i, j){

      value <- dist(rbind(ratings.matrix[i,], ratings.matrix[j,]), method = "euclidean")

      if(is.na(value)){
        return(0)
      } else if(value == 0){
        return(1)
      } else {
        return((1/value[1]))
      }
    }


  ## Recommender
    predict_rating <- function( user, movie, method = mean_method ){

      # Methods
        # mean
        simple_mean <- function ( x ){
          mean(x[,2])
        }

        # weighted mean
        weighted_mean <- function( x ){
          sum(x[,1]*x[,2])/sum(x[,1])
        }

      # Find the user's k nearest neighbours who have rated movie
        rating.pairs <- data.frame(users.matrix[user, ], ratings.matrix[, movie])
        rating.pairs <- rating.pairs[complete.cases(rating.pairs),]
        rating.pairs <- rating.pairs[with(rating.pairs, order(-rating.pairs[, 1])),]

      # Find average rating of these users ratings
        return(do.call(method, list(head(rating.pairs, n=k))))

    }


  ## Update rating
    update_rating <- function( user, movie, rating ) {

      # Check to see if user or movie exist, and if ratings is valid
      if( user > dim(ratings.matrix)[1] || movie > dim(ratings.matrix)[2]){
        return(cat(" Error: User or movie does not exist."))
      } else if( !is.numeric( rating ) || !((rating <= 5) && (rating >= 1))) {
        return(cat(" Error: Not a valid rating."))
      }

      # Update rating
      ratings.matrix[user][movie] <<- rating

    }


  ## Update user
    update_user_rowcol <- function( user ) {

      # Update row and col
      for(i in 1:max(ratings.users)){
        if( i != user ){
            users.matrix[user, i] <- do.call(sim_func, list(i = user, j = i))
            users.matrix[i, user] <- users.matrix[user, i]
        }
      }
    }


  for(q in 1:f){
    cat("Validation: ", q, "\n")
    ## Data Init
      ratings <- do.call(rbind,strsplit(readLines(paste("ml-100k/u", q, ".base", sep="")),'\t',fixed=T))
      ratings <- data.frame(as.integer(ratings[,1]), as.integer(ratings[,2]), as.integer(ratings[,3]), as.integer(ratings[,4]))
      colnames(ratings) <- c('user.id','movie.id','rating','timestamp')


    ## Ratings matrix
      ratings.users <- sort(unique(ratings[,1]))
      ratings.movies <- sort(unique(ratings[,2]))

      ratings.matrix <- matrix(NA, nrow=943, ncol=1682)

      # Build ratings matrix based on ratings data
        for(i in 1:dim(ratings)[1]) {
          ratings.matrix[ratings[i,1],ratings[i,2]] <- ratings[i,3]
          if(i %% 20000 == 0) {cat(" Rating row ", i, "\n")}
        }


    ## User similarity matrix
      users.matrix <- matrix(NA, nrow=943, ncol=943)

      # Create symmetric matrix based on similarity function
      for(i in 1:943){
        for(j in i:943){
          if( i != j){
            users.matrix[i, j] <- do.call(sim_func, list(i = i, j = j))
            users.matrix[j, i] <- users.matrix[i, j]
          }
        }
        if(i%% 100 == 0) {cat(" Users row ", i, "\n")}
      }


    ## RMSE
      # Import testing data
      test.ratings <- do.call(rbind,strsplit(readLines(paste("ml-100k/u", q, ".test", sep="")),'\t',fixed=T))
      test.ratings <- data.frame(as.integer(test.ratings[,1]), as.integer(test.ratings[,2]), as.integer(test.ratings[,3]), as.integer(test.ratings[,4]))
      colnames(test.ratings) <- c('user.id','movie.id','rating','timestamp')

      # Predict ratings
      test.predictions <- data.frame(rep(NA, dim(test.ratings)[1]))

      for(i in 1:dim(test.ratings)[1]){
        test.predictions[i,] <- predict_rating( test.ratings[i, 1], test.ratings[i, 2], method = mean_method )
      }

      # Calculate RMSE of predictions
      mis_values <- sum(is.na((test.ratings[,3] - test.predictions)^2)[,1])

      rmse_vec[q] <- sqrt( sum(((test.ratings[,3] - test.predictions)^2)[,1], na.rm = TRUE) / (dim(test.predictions)[1] - mis_values) )

    }

    return(mean(rmse_vec))

}
