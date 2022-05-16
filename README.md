# DATA-332-Final-Project

 Ladybug and Butterfly Projects

We analyzed data on butterflies and ladybugs documented by Augustana College. We used RStudio to clean and visualize the data of the ladybugs and butterflies to see the distribution of the insects over decades, countries, and institutions. We also performed statistical tests to find averages of different values. We have added the charts we created along with descriptions as to why these charts are helpful in the overall analysis of the data. 

# LADYBUG PROJECT
The first step we did in cleaning the data was by making values in the stateProvince consistent. For example, if the value was "IL", we changed it to be "Illinois." We did the same step for other values as well. This was because we wanted the names of the states to be clear in our visuals. 

Next, we created a new column called "decade" to condense the years into their respective decade so that we could narrow our analysis to only the more recent decades of 2000-2020. 

The first bar chart we created shows the amount of ladybug species found during the decades of 2000-2020
![Species Found during the decades of 2000-2020](https://user-images.githubusercontent.com/63571919/168665102-f89f1b1d-23bc-44ef-a81b-2415b6f769a1.png)

* We chose to visualize the data in this way to see the distribution of the speicies in recent decades.
* This chart helps us see which species are more present during these decades and which are not.
* This data can help scientists recognize if the populations of certain species are diminishing or if there is an overpopulation.

The next chart we created shows the distribution of ladybug species found in Illinois and Iowa during the decades of 2000-2020
![Total Number of Insects Found in Illinois   Iowa during 2000-2020](https://user-images.githubusercontent.com/104874375/168444475-8835e0aa-14c0-4deb-8736-e964b9629f49.png)
* We chose to visualize the data in this way because the majority of the ladybugs were found in Illinois and Iowa and believed that those states are of the most importance to the scientists using the data
* The chart shows that Iowa has the larger amount of ladybugs found and this can be helpful to scientists because they might want to research as to why there are more in Iowa than Illinois. 
* The difference could be interesting to scientists because the cliamte is so similar in Illinois and Iowa that it might be expected that the amount of ladybugs found in both states be more alike. 

The last chart we created shows the total amount of ladybugs found by the decades of 2000-2020
![Number of Insects Found by Decade](https://user-images.githubusercontent.com/104874375/168444649-9685a6e5-df6f-4f7b-8b53-49704d4e07d5.png)
* This chart shows that the majority of ladybugs were found in the decade of 2020
* This might be because there is an overpopulation of ladybugs or a larger distribution of species during 2020
* The other reason there might be such a discrepancy between the amount of ladybugs in 2020 versus the other decades is that more research was done in 2020.  

# BUTTERFLY PROJECT
The first step we did was cleaning the dataset. This step was very similar to what we did in the ladybug project and it is a very important part of the process. We started by renaming a column from "dwc:country" to "Country", and we did this so that it was easier to read the column. 
We then selected the columns that we wanted to work with in order to make the data more tidy and concise. 
After selecting the columns we wanted, we then changed value names in columns to be more consistent. For example, in the sexUpdated column, we made all the male butterfly values equal to "male" by changing others like "M" and "male?". We did this with a couple other columns as well. 
Our last step in cleaning the data was to filter out all of the blank values for the columns we have selected. So if there was a blank country in our "Country" column, it was removed from our dataframe. 

The first chart we created shows amount of male and female butterflies by country.  
![Countries Where Male and Female Butterflies Were Found](https://user-images.githubusercontent.com/104874375/168444943-11e15c86-c486-46ab-9d51-8afd93d399cd.png)
* This chart shows that there might be a certain climate that the butterflies prefer, and that is why they might be found more in UK and the USA.
* To support this reasoning, we believe that there are so few butterflies in Mexico possibly due to the heat.
* The scienctists might want to look in this further. 

The next chart shows the Institutions that found the butterflies
![Institution Where Male and Female Butterflies Were Found](https://user-images.githubusercontent.com/104874375/168445169-d6dbc411-8548-4299-b879-84775d869482.png)
* This chart can help scientists determnine which institution are doing the most research on butterflies.
* If the scientists need help with their research then they can use this chart to determine which institute to reach out to for help.

The last chart we created shows the number of female and male butterflies found by decade. 
![Number of Male and Female Butteflies by Decade](https://user-images.githubusercontent.com/104874375/168445287-9caa81ae-851a-456a-b64d-3b9483af7b80.png)
* The chart shows us that the majority of the butterflies in the decades of 1960-2010 were found in 1960.
* The chart also shows us that male butterflies found always exceed the amount of female butterflies found. 
* Scientists can use this data to do further research as to why there are less female butterflies found and if this is normal for the butterflies. 
