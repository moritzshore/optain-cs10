import pandas as pd
from scipy.optimize import curve_fit

def api_model(P,alpha=0.97,ini=36.8):
  api = [ini]
  for t in range(1,len(P)):
    append_val = api[t-1]*alpha + P[t]
    if append_val > 40:
      append_val = 40
    api.append(append_val)
  return api


df = pd.read_csv('model_data/field_sites/output/twc.csv')
df = df[['date','value','pcp']]

# calc the optimal parameters
par_opt, par_cov = curve_fit(api_model, df['pcp'], df['value'], p0=[0.95,100])

# use the API params
storage_optimized = api_model(df['pcp'],par_opt[0],par_opt[1])

# write the used API params
dates = df[['date']]
api_optimized = pd.DataFrame(dates.values.tolist(),storage_optimized)
api_optimized.to_csv("model_data/field_sites/output/api_optimized.csv")

del(dates)
del(api_optimized)
del(df)

alpha_value = round(par_opt[0],2)
