xlog = function(x) x * log(x)
l_lambda = list(
  'entr' = function(v_tau, a, b) xlog(v_tau[a] + v_tau[b]) - xlog(v_tau[a]) - xlog(v_tau[b]),
  'demp' = function(v_tau, a, b) if(which.max(v_tau) == b) 1 else 0,
  'demp.mod' = function(v_tau, a, b) v_tau[b] * (v_tau[a] + v_tau[b])^-1,
  'coda' = function(v_tau, a, b) log(v_tau[b] / v_tau[a]),
  'coda.norm' = function(v_tau, a, b) -log(v_tau[b] / v_tau[a])^2,
  'prop' = function(v_tau, a, b) v_tau[b] )

# Weitghing functions
l_omega = list(
  'cnst' = function(v_tau, a) 1,
  'prop' = function(v_tau, a) v_tau[a],
  'dich' = function(v_tau, a) if(which.max(v_tau) == a) 1 else 0
)