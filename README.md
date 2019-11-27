
This repo stores some ad-hoc analysis and visualizations of personal
fantasy football (FF) data. Right now, it’s only for a public league on
ESPN.

## Getting Started

To repeat the same analysis for your own ESPN FF league, you’ll need to
manually download your league’s team and box score information (in JSON
format) using the following URL templates (where the curly braces “{}”
indicate values to substitute):

  - scores:
    [http://fantasy.espn.com/apis/v3/games/ffl/seasons/{season}/segments/0/leagues/{leagueid}?view=mMatchup\&view=mMatchupScore\&scoringPeriodId={scoreingPeriodId}](http://fantasy.espn.com/apis/v3/games/ffl/seasons/%7Bseason%7D/segments/0/leagues/%7Bleagueid%7D?view=mMatchup&view=mMatchupScore&scoringPeriodId=%7BscoreingPeriodId%7D)
    
      - For 2018, I used the URL
        <http://fantasy.espn.com/apis/v3/games/ffl/seasons/2018/segments/0/leagues/453218?view=mMatchup&view=mMatchupScore&scoringPeriodId=20>
        to download all of my league’s data.
    
      - For 2019, when I started to play in a dynasty league, I used the
        URL
        <http://fantasy.espn.com/apis/v3/games/ffl/seasons/2019/segments/0/leagues/899513?view=mMatchup&view=mMatchupScore&scoringPeriodId=20>.

  - teams:
    [http://fantasy.espn.com/apis/v3/games/ffl/seasons/{season}/segments/0/leagues/{leagueId}?scoringPeriodId={scoringPeriodId}\&view=mRoster\&view=mTeam](http://fantasy.espn.com/apis/v3/games/ffl/seasons/%7Bseason%7D/segments/0/leagues/%7BleagueId%7D?scoringPeriodId=%7BscoringPeriodId%7D&view=mRoster&view=mTeam)

If you’re going to be downloading data from a completed season, then I
would recommend setting the `scoringPeriodId` to a number larger than
the last week of the season (including playoffs). I believe that the
data returned is the same for any number greater than the number
corresponding to the last week of your FF season (so something like `20`
should always work).

For whatever reason, I have not been successful in my attempts to send
an HTTP request to return the same data (even though the league is
“public”). This is why the data needs to be downloaded manually. (If
you figure out how to download the data via a request, then please send
a pull request.)

The teams data can also be retrieved from the scores JSON without making
any modifications to the code originally written specifically to extract
teams, so retrieving the JSON file corresponding to the teams URL above
is not completely necessary.

The link for the scores data can also be found in the Network tab when
browser inspecting on the Standing page of your league. For example, the
correct link would look something like
<https://fantasy.espn.com/apis/v3/games/ffl/seasons/2019/segments/0/leagues/899513?scoringPeriodId=13&view=mMatchup&view=mMatchupScore&view=mRoster&view=mScoreboard&view=mSettings&view=mStatus&view=mTeam&view=modular&view=mNav>.

## Highlights

Below are some hastily-made visualizations for the “Texas Fantasy
Football” league on ESPN. (Yes, it’s a super creative name, I know.)

### 2019

![](output/2019-12/viz_bump_anim.gif)

![](output/2019-12/viz_tornado.png)

![](output/2019-12/viz_scores_cusum_both.png)

### 2018

![](output/2018-20/viz_scores_cusum_pf.png)

![](output/2018-20/viz_scores_cusum_both_2.png)
