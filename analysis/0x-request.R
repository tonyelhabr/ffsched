
# http://fantasy.espn.com/apis/v3/games/ffl/seasons/2018/segments/0/leagues/453218?view=mMatchup&view=mMatchupScore&scoringPeriodId=14
resp <-
  httr::GET(
    url = 'http://fantasy.espn.com/apis/v3/games/ffl/seasons/2018/segments/0/leagues/453218',
    httr::user_agent('Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/75.0.3770.142 Safari/537.36'),
    body = list(
      view = 'mMatchup',
      view = 'mMatchupScore',
      scoringPeriodId = 14
    )
  )
resp
resp$url
resp$respuest
resp$content
