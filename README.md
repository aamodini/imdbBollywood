# imdbBollywood
Using data from IMDb to find influential spheres in Indian cinema

## Datasets:

1. IMDb - https://www.imdb.com/interfaces/
The description of the 7 dataset used in this are listed on IMDb at this link.

2. `staff_movies` - Assuming a list of movies that are known to be of Indian origin, the dataset `title.principals` was used to extract a list of nameIDs. The columns in this dataset are nameIDs and each row corresponds to the movie title each nameID has worked in.

3. `IC-nodes` - Dataset listing the names and nameIDs of people and the families they belong to.

4. `edgelist_kapoor` -

5. `edgelist_anand` - 

## Main functions:

`functionsTitle.R`

1. `get.nameID(name.input)` - This function accepts a user input (non-unique strings of characters) and matches it with the attribute `primaryName` in the dataset `name.basic`. The function will output a list of possible unique nameIDs corresponding to each of the names provided by the user in the form of a matrix. Note: User needs to supply the correct first and last names of the actors (according to IMDb).

2. `name.title.id(nameID1, nameID2)` - This function accepts 2 nameIDs and uses the dataset `staff_movies`. It find the titleIDs that are a result of the intersection between the 2 nameIDs.

3. `get.title(titleIDs)` - This function takes titleIDs and, using the `title.basic` dataset, outputs readable strings of movie titles.

`Get.Unique.NameID.R`

4. `specNameID(name.input, title.input)` - Input is a string provided by the user. One is the name and the other is a specific movie name that the person is known for. This function is necessary as name strings are not unique. Using the column `knownForTitles` in `name.basic`, a unique nameID can be identified. 

`intersectionFunc.R`

5. `name.title.characters(names, titlesKnown)` - Takes in 2 names and the corresponding movie titles they are known for. This uses the functions `specNameID(name.input, title.input)`, `get.nameID(name.input)`, `name.title.id(nameID1, nameID2)` and  `get.title(titleIDs)`

`functionsGraph.R`

6. `get_first_degree(family)` -  

## Test Functions:

1. `get.name(nameIDs)` - The function accepts unique nameIDs and returns the corresponding name. This function was created to test `get.nameID(names)`.
