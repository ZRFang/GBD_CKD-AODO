import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
file_daly = r"D:\GBD_Project\Decomposition_analysis\input_daly.csv" 
file_pop = r"D:\GBD_Project\Decomposition_analysis\input_pop.csv" 
target_cause = "Chronic kidney disease"
sdi_order = ['Global', 'High SDI', 'High-middle SDI', 'Middle SDI', 'Low-middle SDI', 'Low SDI']
try:
    df_daly = pd.read_csv(file_daly)
    df_pop = pd.read_csv(file_pop)
    print()
except FileNotFoundError:
    print()
    exit()
df_daly = df_daly[
    (df_daly['cause_name'] == target_cause) &
    (df_daly['sex_name'] == 'Both') &
    (df_daly['location_name'].isin(sdi_order)) &
    (df_daly['metric_name'] == 'Number')
    ]
df_pop = df_pop[
    (df_pop['sex_name'] == 'Both') &
    (df_pop['location_name'].isin(sdi_order)) &
    (df_pop['metric_name'] == 'Number')
    ]
df_merged = pd.merge(
    df_daly,
    df_pop[['location_name', 'year', 'age_name', 'val']],
    on=['location_name', 'year', 'age_name'],
    how='inner',
    suffixes=('_daly', '_pop')
)

if len(df_merged) == 0:
    print()
    exit()
decomposition_results = []
for region in sdi_order:
    region_data = df_merged[df_merged['location_name'] == region]
    Index=Age, Columns=Year
   Pop_1990, Pop_2021, DALY_1990, DALY_2021
    pop_pivot = region_data.pivot(index='age_name', columns='year', values='val_pop')
    daly_pivot = region_data.pivot(index='age_name', columns='year', values='val_daly')
    try:
        pop_1990 = pop_pivot[1990]
        pop_2021 = pop_pivot[2021]
        daly_1990 = daly_pivot[1990]
        daly_2021 = daly_pivot[2021]
        continue
    total_pop_1990 = pop_1990.sum()
    total_pop_2021 = pop_2021.sum()
    struct_1990 = pop_1990 / total_pop_1990
    struct_2021 = pop_2021 / total_pop_2021
    rate_1990 = daly_1990 / pop_1990
    rate_2021 = daly_2021 / pop_2021
    change_growth = (total_pop_2021 - total_pop_1990) * (rate_1990 * struct_1990).sum()
    change_aging = total_pop_2021 * ((rate_1990 * struct_2021).sum() - (rate_1990 * struct_1990).sum())
    change_epi = total_pop_2021 * ((rate_2021 * struct_2021).sum() - (rate_1990 * struct_2021).sum())
    net_change = daly_2021.sum() - daly_1990.sum()
    baseline = daly_1990.sum()
    decomposition_results.append({
        'Location': region,
        'Population growth': (change_growth / baseline) * 100,
        'Population aging': (change_aging / baseline) * 100,
        'Epidemiological change': (change_epi / baseline) * 100,
        'Net change': (net_change / baseline) * 100
    })
res_df = pd.DataFrame(decomposition_results)
print(res_df)
fig, ax = plt.subplots(figsize=(12, 7))
x = np.arange(len(res_df))
width = 0.6
bottom_val = np.zeros(len(res_df))
p1 = ax.bar(x, res_df['Population growth'], width, label='Population growth', color=color_growth, alpha=0.9)
bottom_val += res_df['Population growth']
p2 = ax.bar(x, res_df['Population aging'], width, bottom=bottom_val, label='Population aging', color=color_aging,
            alpha=0.9)
bottom_val += res_df['Population aging']
p3 = ax.bar(x, res_df['Epidemiological change'], width, bottom=bottom_val, label='Epidemiological change',
            color=color_epi, alpha=0.9)
ax.scatter(x, res_df['Net change'], color='black', s=80, zorder=10, label='Net change')
ax.set_xticks(x)
ax.set_xticklabels(res_df['Location'], fontsize=11)
ax.set_ylabel('Change in DALYs (%)', fontsize=12)
ax.set_title(f'Drivers of Change in {target_cause} DALYs (1990-2021)', fontsize=14, fontweight='bold')
ax.axhline(0, color='black', linewidth=0.8)
ax.grid(axis='y', linestyle='--', alpha=0.3)
ax.legend(loc='upper left', frameon=False)
plt.tight_layout()
plt.savefig('Figure5_Decomposition.png', dpi=300)
plt.show()
