## Julien Roger <julienroger@gmail.com>
## COMP 652 - Machine Learning


##############################
#   Online Recommendations   #
##############################

# Description: This function takes training data to learn the user preferences, and then uses a second dataset to calculate RMSE and performance online. The functions returns a matrix of true ratings, predicted ratings, and CPU time


online_rate <- function(k = 50, sim_func = "p_corr", mean_method = "simple_mean", train_data = "ml-100k/ua.base", testing_data = "ml-100k/ua.test") {

  set.seed(53)

    ## Data Init
      # Training data
      ratings <- do.call(rbind,strsplit(readLines( train_data ),'\t',fixed=T))
      ratings <- data.frame(as.integer(ratings[,1]), as.integer(ratings[,2]), as.integer(ratings[,3]), as.integer(ratings[,4]))
      colnames(ratings) <- c('user.id','movie.id','rating','timestamp')

      # Testing data
      test.ratings <- do.call(rbind,strsplit(readLines( testing_data ),'\t',fixed=T))
      test.ratings <- data.frame(as.integer(test.ratings[,1]), as.integer(test.ratings[,2]), as.integer(test.ratings[,3]), as.integer(test.ratings[,4]))
      test.ratings <- test.ratings[sample(nrow(test.ratings)),]
      colnames(test.ratings) <- c('user.id','movie.id','rating','timestamp')


  perf_mat <- matrix(rep(NA, 4*dim(test.ratings)[1]), nrow=4)
  rownames(perf_mat) <- c("true rating", "pred. rating", "user time", "sys time")


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
      if(dist(rbind(ratings.matrix[i,], ratings.matrix[j,]), method = "euclidean") == 0){
        return(1)
      } else {
        1/dist(rbind(ratings.matrix[i,], ratings.matrix[j,]), method = "euclidean")
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

      # Find the user's k nearest neighbour who have rated movie
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
      ratings.matrix[user, movie] <<- rating

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


    ## At each step,
      for(w in 1:dim(test.ratings)[1]){

        # Start time clock
        start_time <- proc.time()
       
        # Pull user and movie from dataset
        user_id <- test.ratings[w, 1]
        movie_id <- test.ratings[w, 2]
        movie_rating <- test.ratings[w, 3]

        # Predict rating for user
        predicted_rating <- round(predict_rating(user_id, movie_id), 5)

        # Update ratings matrix
        update_rating(user_id, movie_id, movie_rating)

        # Update user colum/row
        update_user_rowcol(user_id)

        # Stop time clock
        stop_time <- proc.time() - start_time


        if(w %% 500 == 0) {cat( "Trial: ", w, "; true: ", perf_mat[1, w-1], "; pred.: ", perf_mat[2, w-1], "\n")}

        # Update perf_mat
        perf_mat[1, w] <- movie_rating
        perf_mat[2, w] <- predicted_rating
        perf_mat[3, w] <- stop_time[1]
        perf_mat[4, w] <- stop_time[2]

    }


    return( perf_mat )

}
