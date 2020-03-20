## Import packages
from flask import Flask, request, jsonify
import pandas

## Initialize app
app = Flask(__name__)

## Read in predictions.csv and movies.csv using Pandas
predictions = pandas.read_csv("Data/predictions.csv").dropna()

movies = pandas.read_csv("Data/movies.csv").dropna()

## Combine both files to allow to return complete information about the movie
predictions = predictions.merge(movies, on="movieId", how="left").sort_values(['userId', 'prediction'], ascending=[True, False])

## How this is done in Spark (information)
#movies = spark.read.option("header", "true").csv("movies.csv")\
#                    .select("movieId", "title")\
#                    .repartition("movieId")
            
#predictions = predictions.join(movies, "movieId", "left")\
#                    .orderBy(col("userId"), col("prediction").desc())\
#                    .cache()

## The application definiton
### Endpoint - one route /ratings/top - one HTTP verb = GET
@app.route("/ratings/top", methods=["GET"])
def top_ratings():
    ## read the parameters of the API call
    userId_str = request.args.get("userId")
    try:
        userId = int(userId_str)
    except:
        return "'userId' is required and should be an Integer."
        sys.exit("'userId' is required and should be an Integer.")
       
    count_str = request.args.get("count")
    try:
        count = int(count_str)
    except:
        count = 5
    
    # filter predictions for the given userId
    predict = predictions[predictions.userId == userId].head(count)
    
    # select movieId, title and prediction and transform to list
    top_ratings = list(predict[["movieId", "title", "prediction"]].T.to_dict().values())
    
    # Return the result to the API
    return jsonify(top_ratings)

### Put endpoint online
if __name__ == '__main__':
    app.run(host='localhost', port=6000)
