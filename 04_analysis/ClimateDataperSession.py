# -*- coding: utf-8 -*-
"""
Created on Sun Sep 15 17:12:53 2024

@author: kobas
"""
import os
import pandas as pd
import matplotlib.pyplot as plt
from matplotlib.ticker import LogLocator

# Set the working directory
os.chdir(r'\\workinddirection')

# Read the CSV file once
df = pd.read_csv('TimeClimaAct.csv')

# Calculate the 'top' variable as the average of 'tdb' and 'tg'
df['top'] = (df['tdb'] + df['tg']) / 2

df = df[df['Exclude'] == 0]

# Extract unique session information (assuming Participant and Scenario are constant per SessionID)
session_info = df[['SessionID', 'Participant', 'Scenario']].drop_duplicates()

# Compute aggregated climate statistics for 'tdb', 'tg', 'rh', and 'top'
climate_stats = df.groupby('SessionID').agg({
    'tdb': ['mean', 'std'],
    'tg': ['mean', 'std'],
    'rh': ['mean', 'std'],
    'top': ['mean', 'std'],
    'AMB LIGHT_lux': ['mean', 'std']
}).reset_index()

# Flatten the MultiIndex columns
climate_stats.columns = [
    'SessionID', 
    'tdb_mean', 'tdb_std', 
    'tg_mean', 'tg_std', 
    'rh_mean', 'rh_std', 
    'top_mean', 'top_std',
    'AMB LIGHT_lux', 'lux_std'
]


# Merge the climate and lux statistics with session info
merged_df = pd.merge(climate_stats, session_info, on='SessionID', how='left')

# Sort the rows by Participant and then Scenario
merged_df.sort_values(['Participant', 'Scenario'], inplace=True)

merged_df.to_csv('99_ClimaStatsperSession_luxtop.csv', index=False)

#%% Plot and save
df = merged_df.copy()
color = 'black'
capsize = 5
markersize = 5

# Ensure 'Participant' and 'Scenario' are strings
df['Participant'] = df['Participant'].astype(str)
df['Scenario'] = df['Scenario'].astype(str)

# Create a numeric plotting index
df['plot_index'] = range(len(df))

# (Optional) Build a list of custom labels if needed later
x_labels = []
for i in range(len(df)):
    label = df['Scenario'].iloc[i]
    if i % 4 == 0:  # Every 4th value gets the Participant label appended
        label += '\n' + df['Participant'].iloc[i]
    x_labels.append(label)

# Create subplots with a shared x-axis
fig, axs = plt.subplots(2, 1, figsize=(14, 12), sharex=True)

# Top (operative temperature) plot
axs[0].errorbar(
    df['plot_index'] + 0.5,
    df['top_mean'],
    yerr=df['top_std'],
    fmt='o',
    color=color,
    ecolor='red',
    capsize=capsize,
    markersize=markersize,
    label='Operative temperature mean±std'
)
axs[0].set_ylabel('Operative Temperature (°C)')
axs[0].legend(loc='upper right', frameon=False)  # remove legend frame
axs[0].grid(True)
axs[0].set_ylim(26, 28)

# Draw horizontal reference lines
axs[0].axhline(y=26.5, color='black', linewidth=2)
axs[0].axhline(y=27, color='black', linewidth=1, linestyle="--")
axs[0].axhline(y=27.5, color='black', linewidth=2)

# Ambient light plot
axs[1].errorbar(
    df['plot_index'] + 0.5,
    df['AMB LIGHT_lux'],
    yerr=df['lux_std'],
    fmt='o',
    color=color,
    ecolor='blue',
    capsize=capsize,
    markersize=markersize,
    label='Ambient light mean±std'
)
axs[1].set_ylabel('Ambient Light (lx)')
axs[1].set_xlabel('Participant')
axs[1].legend(loc='upper right', frameon=False)  # remove legend frame
axs[1].grid(True)
axs[1].set_yscale('log')
axs[1].set_ylim(0, 10000)
axs[1].yaxis.set_major_locator(LogLocator(base=10.0))
axs[1].yaxis.set_major_formatter(plt.FuncFormatter(lambda y, _: '{:.0f}'.format(y)))
for y_value in [1, 10, 100, 1000]:
    axs[1].axhline(y=y_value, color='black', linewidth=1, linestyle="--")

# Remove default x-tick labels from the second subplot
axs[1].set_xticks([])

# Add custom x-tick labels using text (rotated Scenario labels with Participant every 4th data point)
x_positions = df['plot_index']
for i, (x, scenario, participant_id) in enumerate(zip(x_positions, df['Scenario'], df['Participant'])):
    axs[1].text(
        x + 0.5,
        -0.05,  # position for scenario label (in axis coordinates)
        scenario,
        ha='center',
        va='top',
        rotation=90,
        fontsize=9,
        transform=axs[1].get_xaxis_transform()
    )
    if i % 4 == 0:
        axs[1].text(
            x + 0.5,
            -0.15,  # position for participant label (in axis coordinates)
            participant_id,
            ha='center',
            va='top',
            rotation=0,
            fontsize=9,
            transform=axs[1].get_xaxis_transform()
        )

# Move the x-axis label further down (adjust the y-coordinate as needed)
axs[1].xaxis.set_label_coords(0.5, -0.2)
axs[1].set_xlabel("Participant ID", labelpad=20)  # Adjust labelpad to move it down

# Adjust x-axis limits to ensure labels are visible
axs[1].set_xlim(df['plot_index'].min() - 1, df['plot_index'].max() + 1)

# Add vertical lines for scenario changes and participant labels
# Identify positions where the Scenario changes
scenario_change_positions = []
for i in range(1, len(df)):
    if df['Scenario'].iloc[i] != df['Scenario'].iloc[i - 1]:
        x = df['plot_index'].iloc[i]
        scenario_change_positions.append(x)

# Positions for Participant labels (every 4 data points)
participant_id_positions = [df['plot_index'].iloc[i] for i in range(len(df)) if i % 4 == 0]

# Draw vertical lines on both subplots
for ax in axs:
    for x in scenario_change_positions:
        ax.axvline(x=x, color='gray', linewidth=0.5)
    for x in participant_id_positions:
        ax.axvline(x=x, color='gray', linewidth=1.5)

# Add alternating background shading for every 4 groups
num_points = len(df)
for i in range(0, num_points, 4):
    group_index = i // 4
    if group_index % 2 == 0:  # Shade even groups (0, 2, 4, ...)
        left_bound = i 
        # Ensure the right bound does not exceed the maximum x-axis value
        right_bound = min(i + 4 , df['plot_index'].max() + 1)
        for ax in axs:
            ax.axvspan(left_bound, right_bound, facecolor='lightgray', alpha=0.3, zorder=0)

# Final layout adjustments and saving the plot
plt.tight_layout()
plt.subplots_adjust(bottom=0.25)  # Adjust the bottom margin to accommodate custom x-tick labels
plt.savefig('top_lux.png')
plt.show()
