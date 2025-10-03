vector compute_mu(array[] int home_team,
                  array[] int away_team,
                  array[] int week_idx,
                  array[] int season_idx,
                  array[] int hfa,
                  array[] vector team_strength,
                  array[] vector team_hfa,
                  int N_games) {
  vector[N_games] mu;
  for (g in 1:N_games) {
    int w = week_idx[g];
    int s = season_idx[g];
    int h = home_team[g];
    int a = away_team[g];
    mu[g] = team_strength[w][h] - team_strength[w][a];
    if (hfa[g] == 1) {
      mu[g] += team_hfa[s][h];
    }
  }
  return mu;
}

array[] int compute_week_to_season(int N_weeks,
                                   int N_games,
                                   array[] int week_idx,
                                   array[] int season_idx) {
  array[N_weeks] int week_to_season = rep_array(1, N_weeks);
  for (g in 1:N_games) {
    int w = week_idx[g];
    int s = season_idx[g];
    if (s > week_to_season[w]) {
      week_to_season[w] = s;
    }
  }
  return week_to_season;
}

array[] int compute_is_first_week(int N_weeks,
                                  int N_games,
                                  array[] int week_idx,
                                  array[] int fw_season_idx) {
  array[N_weeks] int is_first_week = rep_array(0, N_weeks);
  for (g in 1:N_games) {
    if (fw_season_idx[g] == 1) {
      is_first_week[week_idx[g]] = 1;
    }
  }
  if (N_weeks > 0) {
    is_first_week[1] = 1;
  }
  return is_first_week;
}

array[] int compute_is_last_week(int N_weeks,
                                 int N_games,
                                 array[] int week_idx,
                                 array[] int lw_season_idx) {
  array[N_weeks] int is_last_week = rep_array(0, N_weeks);
  for (g in 1:N_games) {
    if (lw_season_idx[g] == 1) {
      is_last_week[week_idx[g]] = 1;
    }
  }
  return is_last_week;
}
