import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

# ==========================================
# 1. 基础配置
# ==========================================
file_daly = r"D:\GBD_Project\Decomposition_analysis\input_daly.csv"  # 疾病数据文件
file_pop = r"D:\GBD_Project\Decomposition_analysis\input_pop.csv"  # 人口数据文件

# 目标疾病 (一次跑一个)
target_cause = "Alzheimer's disease and other dementias"
# target_cause = "Alzheimer's disease and other dementias"

# SDI 顺序
sdi_order = ['Global', 'High SDI', 'High-middle SDI', 'Middle SDI', 'Low-middle SDI', 'Low SDI']

# ==========================================
# 2. 读取与合并数据
# ==========================================
try:
    df_daly = pd.read_csv(file_daly)
    df_pop = pd.read_csv(file_pop)
    print("两个文件读取成功！")
except FileNotFoundError:
    print("错误：找不到文件。请确保你下载了 input_daly.csv 和 input_pop.csv 并放在了项目里。")
    exit()

# --- 清洗 DALY 数据 ---
df_daly = df_daly[
    (df_daly['cause_name'] == target_cause) &
    (df_daly['sex_name'] == 'Both') &
    (df_daly['location_name'].isin(sdi_order)) &
    (df_daly['metric_name'] == 'Number')
    ]

# --- 清洗 Population 数据 ---
# 注意：人口数据通常没有 cause_name 列，或者 cause_name 是 'All causes'
df_pop = df_pop[
    (df_pop['sex_name'] == 'Both') &
    (df_pop['location_name'].isin(sdi_order)) &
    (df_pop['metric_name'] == 'Number')
    ]

# --- 合并数据 (Merge) ---
# 我们需要把 Population 列拼接到 DALY 表上
# 关联键：location_name, year, age_name
df_merged = pd.merge(
    df_daly,
    df_pop[['location_name', 'year', 'age_name', 'val']],  # 只取这几列
    on=['location_name', 'year', 'age_name'],
    how='inner',
    suffixes=('_daly', '_pop')  # 以此区分：val_daly 是病, val_pop 是人
)

if len(df_merged) == 0:
    print("错误：合并后数据为空！请检查两个文件的 年份、地区、年龄组 是否完全对应一致？")
    exit()

print(f"数据合并成功，共 {len(df_merged)} 行数据。开始分解分析...")

# ==========================================
# 3. 核心分解算法 (Das Gupta Method)
# ==========================================
decomposition_results = []

for region in sdi_order:
    # 提取该地区数据
    region_data = df_merged[df_merged['location_name'] == region]

    # 透视表: Index=Age, Columns=Year
    # 我们需要: Pop_1990, Pop_2021, DALY_1990, DALY_2021

    # 1. 提取 Population 矩阵
    pop_pivot = region_data.pivot(index='age_name', columns='year', values='val_pop')
    # 2. 提取 DALY 矩阵
    daly_pivot = region_data.pivot(index='age_name', columns='year', values='val_daly')

    try:
        pop_1990 = pop_pivot[1990]
        pop_2021 = pop_pivot[2021]
        daly_1990 = daly_pivot[1990]
        daly_2021 = daly_pivot[2021]
    except KeyError:
        print(f"跳过 {region}，缺少年份数据。")
        continue

    # --- 准备变量 ---
    # 总人口
    total_pop_1990 = pop_1990.sum()
    total_pop_2021 = pop_2021.sum()

    # 年龄结构 (Structure)
    struct_1990 = pop_1990 / total_pop_1990
    struct_2021 = pop_2021 / total_pop_2021

    # 患病率 (Rate)
    rate_1990 = daly_1990 / pop_1990
    rate_2021 = daly_2021 / pop_2021

    # --- 计算贡献 (Stepwise) ---

    # A. 人口增长 (Growth): 仅总人口变，结构和率不变(用1990)
    change_growth = (total_pop_2021 - total_pop_1990) * (rate_1990 * struct_1990).sum()

    # B. 人口老龄化 (Aging): 总人口不变(用2021)，率不变(用1990)，结构变
    change_aging = total_pop_2021 * ((rate_1990 * struct_2021).sum() - (rate_1990 * struct_1990).sum())

    # C. 流行病学变化 (Epidemiology): 剩下的部分 (率变了)
    change_epi = total_pop_2021 * ((rate_2021 * struct_2021).sum() - (rate_1990 * struct_2021).sum())

    # D. 净变化
    net_change = daly_2021.sum() - daly_1990.sum()

    # 转百分比 (相对于 1990 年 DALYs)
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

# ==========================================
# 4. 绘图 (Figure 5)
# ==========================================
fig, ax = plt.subplots(figsize=(12, 7))

# 设置 X 轴位置
x = np.arange(len(res_df))
width = 0.6

# 颜色设置
color_growth = '#4575b4'  # 蓝
color_aging = '#fc8d59'  # 橙
color_epi = '#91bfdb'  # 浅绿/灰蓝

# 绘制柱子 (累加堆积)
# 为了处理正负值显示问题，我们简单地把它们画在对应位置
# 这里用的是一种通用的视觉技巧：Bottom 累加
bottom_val = np.zeros(len(res_df))

# 1. Growth
p1 = ax.bar(x, res_df['Population growth'], width, label='Population growth', color=color_growth, alpha=0.9)
bottom_val += res_df['Population growth']

# 2. Aging
p2 = ax.bar(x, res_df['Population aging'], width, bottom=bottom_val, label='Population aging', color=color_aging,
            alpha=0.9)
bottom_val += res_df['Population aging']

# 3. Epidemiology
p3 = ax.bar(x, res_df['Epidemiological change'], width, bottom=bottom_val, label='Epidemiological change',
            color=color_epi, alpha=0.9)

# 4. Net Change (黑点)
ax.scatter(x, res_df['Net change'], color='black', s=80, zorder=10, label='Net change')

# 美化
ax.set_xticks(x)
ax.set_xticklabels(res_df['Location'], fontsize=11)
ax.set_ylabel('Change in DALYs (%)', fontsize=12)
ax.set_title(f'Drivers of Change in {target_cause} DALYs (1990-2021)', fontsize=14, fontweight='bold')
ax.axhline(0, color='black', linewidth=0.8)
ax.grid(axis='y', linestyle='--', alpha=0.3)
ax.legend(loc='upper left', frameon=False)

plt.tight_layout()
plt.savefig('Figure5_Decomposition.png', dpi=300)
print("Figure 5 绘制完成！")
plt.show()