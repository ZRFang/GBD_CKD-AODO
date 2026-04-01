import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import matplotlib.colors as mcolors
import os
import tkinter as tk
from tkinter import filedialog
custom_vibrant_colors = [
    '#3288bd', '#66c2a5', '#abdda4', '#e6f598', '#ffffbf',
    '#fee08b', '#fdae61', '#f46d43', '#d53e4f', '#9e0142'
]
vibrant_rainbow_cmap = mcolors.LinearSegmentedColormap.from_list("VibrantRainbow", custom_vibrant_colors, N=256)
def get_file_path(filename_hint):
    if os.path.exists(filename_hint):
        print(f"✅ 自动找到文件: {filename_hint}")
        return filename_hint
    print(f"⚠️ 找不到 {filename_hint}，请手动选择...")
    root = tk.Tk();
    root.withdraw()
    file_path = filedialog.askopenfilename(title=f"请选择 {filename_hint}", filetypes=[("CSV Files", "*.csv")])
    return file_path
def plot_final_layout_heatmap(target_filename, disease_name, year):
    csv_file = get_file_path(target_filename)
    if not csv_file: return
    try:
        df = pd.read_csv(csv_file)
    except Exception as e:
        return
    df_plot = df[df['year'] == year].copy()
    if df_plot.empty: print(f"❌ 无 {year} 数据"); return
    try:
        pivot_df = df_plot.pivot_table(index='rei_name', columns='location_name', values='val')
    except KeyError:
        print("❌ 列名错误：CSV中必须包含 rei_name, location_name, val 字段");
        return
    cols = list(pivot_df.columns)
    if 'Global' in cols:
        cols.remove('Global')
        pivot_df = pivot_df[['Global'] + sorted(cols)]
    if 'Global' in pivot_df.columns:
        pivot_df = pivot_df.sort_values(by='Global', ascending=False)
    data_out_name = f"Heatmap_Data_{disease_name}_{year}.csv"
    pivot_df.to_csv(data_out_name, encoding='utf-8-sig')
    print(f"📊 [成功] 对应的数据表格已保存为: {data_out_name}")
    plt.figure(figsize=(18, 12))

    sns.heatmap(data=pivot_df,
                cmap=vibrant_rainbow_cmap,
                annot=True,
                fmt=".1f",
                linewidths=0.5, linecolor='white',
                robust=True,
                cbar_kws={'label': 'Age-standardized DALYs per 100,000'})

    plt.title(f'{disease_name} Risk Factors in {year}', fontsize=18, fontweight='bold', pad=20)
    plt.xlabel('');
    plt.ylabel('')
    plt.xticks(rotation=90, ha='center', fontsize=11)
    plt.yticks(rotation=0, fontsize=12)
    plt.tight_layout()
    out_name = f"Heatmap_{disease_name}_{year}_HorizontalY.png"
    plt.savefig(out_name, dpi=300)
    plt.show()
print("\n=== Chronic kidney disease 绘图 ===")
plot_final_layout_heatmap('risk_ckd.csv', 'Chronic kidney disease', 1990)
plot_final_layout_heatmap('risk_ckd.csv', 'Chronic kidney disease', 2021)
