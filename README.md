# Spring2018


# Project 3: Algorithm Implementation and Evaluation

----


### [Project Description](doc/)

Term: Spring 2018

+ Project title: Collaborative Filtering and Neighborhood Selection
+ Team Number: 5
+ Team Members: Minzi Keem, Yang He, Yujie Hu, Sile Yang, Chuyuan Zhou
+ Project summary: Our group was tasked with considering different similarity weights, and from there to see whether prediction accuracy improves by using different similarity weights than that based on the Pearson correlation. In addition, we had to figure out whether selecting the best "neighbors" for an active user improves prediction accuracy, since there is evidence that selecting a subset improves accuracy.

### Similarity Weights
We calculated similarity weights for each of the datasets using the Pearson, Spearman, Cosine, MSD, Entropy, and Simrank similarity weights.

### Model-based Algorithm

### Memory-based Algorithm

### Evaluation
For the purposes of our evaluation, we used the Mean Absolute Error and the Root Mean Square Error. For both values, the smaller the value, the more accurate our predictions are.

### Neighborhood Selection
For the purposes of our neighborhood selection, we tried filtering the data by minimum threshold only, and then by the nearest number of neighbors. For both the EM data and the MS data, we tried a minimum threshold of 0.1, 0.2, 0.3, 0.4, 0.5. We did this filtering by setting all the similarity weights under the desired weights to 0.

We then tried a combined method with a minimum threshold of 0.1 and a number of neighbors at twenty, and then forty. To do this, we found the top twenty and forty similarity weights and filtered out anything that did not fall under that category. 

Our results showed that for the Microsoft data, the MAE and RMSE were smallest for Min Abs Corr = 0.1, and Number of Neighbors at 20. Extending the number of neighbors further from 20 or so seemed to decrease the accuracy, though more data points would be needed to confirm that. For the EachMovie data, however, the MAE was smallest for Min Abs Corr = 0.2, and the RMSE was smallest for Min Abs Corr = 0.1, and Number of Neighbors 40. However, the MAE for the Number of Neighbors at 40 was quite small as well, which leads us to believe that the neighborhood selection should be set with a higher number of neighbors, perhaps 50 or 60. More selection can be run to test for this.

Contribution statement: [default](doc/a_note_on_contributions.md) All team members contributed equally in all stages of this project. All team members approve our work presented in this GitHub repository including this contributions statement.

Following [suggestions](http://nicercode.github.io/blog/2013-04-05-projects/) by [RICH FITZJOHN](http://nicercode.github.io/about/#Team) (@richfitz). This folder is orgarnized as follows.

```
proj/
├── lib/
├── data/
├── doc/
├── figs/
└── output/
```

Please see each subfolder for a README file.
