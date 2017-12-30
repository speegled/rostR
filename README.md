# rostR
Builds rosters of teams which maximize a score function. 

See [rostR](https://speegled.shinyapps.io/rostr/) for the Shiny app hosted by RStudio. 

### Contribution Guidelines
I would love to have contributors help on this project. If you see a mistake, please submit a pull request. If you have an idea for improvement, I would appreciate it if you would create an issue. If you have some time on your hands and want to learn more about MCMC and/or reactive programming using Shiny, then I would also appreciate help implementing a couple of ideas that I have for improvement. 

1. I would like to have an option that allows the user to let rostR create a finsihed roster for them. This way, they wouldn't have to fiddle around with the fine controls unless they don't like the final roster for some reason.
2. I would like to improve the UI on the graph page. It still seems clunky.
3. I would like to add the names and/or Id's on the Diagnostics tab who don't get assigned any baggage.

Code of Conduct: All contributors to this package will treat other contributors with respect. All contributions are valued.

### Discussion of algorithm

When players sign up for the [ultimate league](http://www.slua.org) that I organize, they provide information on their skill level, their gender, and the other players in the league that they would like to be on the same team with. I am then faced with the problem of coming up with teams so that the skill levels on each team are not too different from one another, the gender balance of the teams are similar, and players get to play with the people they requested. In the particular league that I organize, players are allowed to request to play with up to 7 different players. Needless to say, this is a time-consuming task to do by hand.

A few years ago, my friend [David Letscher](http://www.cs.slu.edu/~letscher) wrote a program that automated this process. His program started with a random partition of the playes into teams, and then made various trades: eg 1 for 1, 1 for 2, 2 for 2, 1 for 1 for 1 (three teams), and made the trade if the trade made the teams better. This algorithm works very well, and there was no real need to try to improve upon it, except that it seemed messy.

Inspired by Persi Diaconis' article [The MCMC Revolution](http://www.ams.org/journals/bull/2009-46-02/S0273-0979-08-01238-X/), I decided to implement a Markov Chain Monte Carlo algorithm to partition the players into teams. More specifically, I used a simple Metropolis-Hastings algorithm. The transitions allowed are simple trades of players between teams. I have created a "roster score" of the current partition that combines the mean power of all players on the teams, the mean power of the best 7 players on the teams (default is 5 best men and 2 best women, since that is my league's gender ratio), the mean number of women on the teams, the total number of baggage requests that are granted *and* the total number of players who received no baggage requests. The only tricky part was scaling all of these so that the score more or less matches what I think of as a "good" roster. 

For each proposed swap of players, the program determines whether the score increases or decreases. (Here, decreasing is better. Why? Good question.) If the score *decreases*, i.e., the roster is better, the program makes the switch. If the score *increases*, i.e. gets worse, the program makes the switch with a probability that is related to the scale assigned in the Fine Tuning tab of the program and the amount of difference in the scores. 

What I like about this algorithm is that there is a theoretical guarantee that, if you run the program long enough, then the proportion of time that the rosters will have various scores is determined by the scores and the scale. So, the best roster, in the sense of the one with the lowest score, is guaranteed to be the one that appears most often. Now, in practice, this is not always the case, because we don't have that long to wait, but by playing around with the importance values, I have been able to get very good rosters for a variety of different starting points.

Intiutively, this algorithm takes a directed graph such that vertex $i$ has properties associated with it $P_{i,1}, \ldots, P_{i,n}$, and partitions the graph into a fixed number of groups[teams] $G_1, \ldots, G_K$ in such a way as to minimize the number of edges[baggage requests] between groups while also for each $m$ minimizing the differences
\[
\max_{1\le j,k \le K} |\frac{1}{|G_j|} \sum_{i \in G_j} P_{i,m} - \frac{1}{|G_k|} \sum_{i \in G_k} P_{i,m}|
\]
If you look at the details, you will see that this algorithm does not do exactly the above, but perhaps it should?
