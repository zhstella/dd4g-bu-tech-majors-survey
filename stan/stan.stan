data {
  int<lower=1> N;                     // Number of observations

  int<lower=1> D;                     // Number of departments
  int<lower=1> L;                     // Number of course levels
  int<lower=1> K;                     // Number of courses

  int<lower=1, upper=D> course_dept[K]; // Department for each course
  int<lower=1, upper=L> course_level[K]; // Level for each course
  int<lower=1, upper=K> course[N];    // Vector of course indicators

  int<lower=1> R;
  int<lower=1, upper=R> race[N]; //vector of races

  int<lower=1> G;
  int<lower=1, upper=R> gender[N]; //vector of races


  int<lower=1> C;                     // Number of response levels
  int<lower=1, upper=C> response[N];  // Vector of responses

  // Matrices to lift independent priors to zero-sample-mean priors
  matrix[D,D-1] dept_chol; // departments are mean zero
  matrix[K,K-D] course_chol; // courses are mean zero within each department
  matrix[G,G-1] gender_chol; // departments are mean zero
  matrix[R,R-1] race_chol; // courses are mean zero within each department

}

parameters {
  vector[D-1] dept_offset_raw;      // Department level offset
  vector[L-1] level_offset_raw;     // Level level offset
  vector[K-D] course_offset_raw;  // course level offset

  real<lower=0> sigma_dept; // Standard deviation of department offsets
  // currently all departments have the same prior
  // course variance within department. The below
  // would be needed to allow for varying variances.
  // real<lower=0> sigma_dept_course[D];
  real<lower=0> sigma_course; // Standard deviation of course offsets
  real<lower=0> sigma_level;  // Standard deviation of level offsets

  vector[R-1] race_offset_raw;
  vector[G-1] gender_offset_raw;

  real<lower=0> sigma_race;
  real<lower=0> sigma_gender;  // Standard deviation of level offsets

  ordered[C - 1] c;
}

transformed parameters {

  vector[L] level_offset;
  level_offset[2:L] = sigma_level * level_offset_raw;
  level_offset[1] = 0;

  vector[D] dept_offset = tan(sigma_dept) * dept_chol * dept_offset_raw;

  vector[K] course_offset = tan(sigma_course) * course_chol * course_offset_raw;

  vector[K] course_offset_composite = course_offset +
       dept_offset[course_dept] +
       level_offset[course_level];

  vector[R] race_offset = tan(sigma_race) * race_chol * race_offset_raw;
  vector[G] gender_offset = tan(sigma_gender) * gender_chol * gender_offset_raw;

}

model {
  dept_offset_raw ~ std_normal();
  level_offset_raw ~ std_normal();
  course_offset_raw ~ std_normal();
  race_offset_raw ~ std_normal();
  gender_offset_raw ~ std_normal();


  sigma_dept ~ uniform(0, pi() / 2);
  // sigma_dept_course ~ cauchy(0, 1);
  sigma_level ~ std_normal(); // uniform(0, pi() / 2);
  sigma_course ~ uniform(0, pi() / 2);
  sigma_race ~ uniform(0, pi() / 2);
  sigma_gender ~ uniform(0, pi() / 2);



  // Likelihood
  response ~ ordered_logistic(
    course_offset_composite[course] + race_offset[race] + gender_offset[gender],
    c
  );
}
