import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
filename = r"D:\GBD_Project\APC\CKD_Global_Results.xlsx"
print(f"📂 正在读取 Excel 文件: {filename} ...")
try:
    df_drift = pd.read_excel(filename, sheet_name='LocalDrifts')
    df_age = pd.read_excel(filename, sheet_name='LongAge')
    df_period = pd.read_excel(filename, sheet_name='PeriodRR')
    df_cohort = pd.read_excel(filename, sheet_name='CohortRR')
    print("✅ 读取成功！所有数据已准备就绪。")
    exit()
df_drift.columns = df_drift.columns.str.strip()
df_age.columns = df_age.columns.str.strip()
df_period.columns = df_period.columns.str.strip()
df_cohort.columns = df_cohort.columns.str.strip()
colors = {
    'drift': '#E41A1C', 
    'age': '#377EB8', 
    'period': '#4DAF4A',
    'cohort': '#984EA3'
}

fig, axes = plt.subplots(2, 2, figsize=(14, 10))
plt.subplots_adjust(wspace=0.3, hspace=0.35)

def plot_apc(ax, df, x_col, y_col, lo_col, hi_col, color, title, xlabel, ylabel, ref_line=None):
    x = df[x_col]
    y = df[y_col]
    lower = df[lo_col]
    upper = df[hi_col]
    ax.plot(x, y, marker='o', markersize=5, color=color, linewidth=2)
    ax.fill_between(x, lower, upper, color=color, alpha=0.15)
    if ref_line is not None:
        ax.axhline(ref_line, linestyle='--', color='gray', alpha=0.7)
    ax.set_title(title, fontweight='bold', fontsize=12)
    ax.set_xlabel(xlabel, fontsize=10)
    ax.set_ylabel(ylabel, fontsize=10)
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)
    ax.grid(axis='y', linestyle='--', alpha=0.3)

# (A) Local Drift
plot_apc(
    axes[0, 0], df_drift,
    'Age', 'Percent per Year', 'CILo', 'CIHi',
    colors['drift'],
    'Local Drift (Annual % Change)',
    'Age Group', 'Percentage Change (%)',
    ref_line=0
)

# (B) Age Effect
plot_apc(
    axes[0, 1], df_age,
    'Age', 'Rate', 'CILo', 'CIHi',
    colors['age'],
    'Age Effect (Longitudinal Age Curve)',
    'Age Group', 'Incidence Rate (per 100,000)'
)

# (C) Period Effect
plot_apc(
    axes[1, 0], df_period,
    'Period', 'Rate Ratio', 'CILo', 'CIHi',
    colors['period'],
    'Period Effect (Relative Risk)',
    'Period', 'Relative Risk (RR)',
    ref_line=1
)

# (D) Cohort Effect
plot_apc(
    axes[1, 1], df_cohort,
    'Cohort', 'Rate Ratio', 'CILo', 'CIHi',
    colors['cohort'],
    'Cohort Effect (Relative Risk)',
    'Birth Cohort', 'Relative Risk (RR)',
    ref_line=1
)

outfile = 'Figure6_APC_Global_Excel.png'
plt.savefig(outfile, dpi=300, bbox_inches='tight')
print(f"🎉 绘图完成！图片已保存为: {outfile}")
plt.show()
