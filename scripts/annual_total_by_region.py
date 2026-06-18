# -----------------------------
# SETUP
# -----------------------------
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt


import glob

files = glob.glob("../data/raw/PREC_*.csv") # list of all precipitation files

df_list = [] # empty list to hold dataframes

for f in files: # loop to read each file and append to list
    df = pd.read_csv(f, sep=";", encoding="latin-1")
    df["source_file"] = f
    df_list.append(df)

df = pd.concat(df_list, ignore_index=True)
print(df)

# -----------------------------
# BASIC CLEANING
# -----------------------------
df.columns = df.columns.str.strip().str.lower()
print(df.columns)

# -----------------------------
# FILTER GALICIA REGIONS
# -----------------------------
galicia_regions = ["A CORUÑA", "LUGO", "OURENSE", "PONTEVEDRA"]

df_galicia = df_long[df_long["región"].isin(galicia_regions)]

# -----------------------------
# CLEAN DATA
# -----------------------------
df_galicia["precipitation"] = pd.to_numeric(df_galicia["precipitation"], errors="coerce")

# -----------------------------
# AGGREGATE (2019–2024 mean monthly precipitation)
# -----------------------------
region_avg = (
    df_galicia
    .groupby("región")["precipitation"]
    .mean()
    .sort_values()
)

# -----------------------------
# PLOT
# -----------------------------
plt.figure(figsize=(8, 5))
region_avg.plot(kind="barh")

plt.title("Average Monthly Precipitation in Galicia (2019–2024)")
plt.xlabel("Average monthly precipitation (mm)")
plt.ylabel("Region")

plt.tight_layout()
plt.show()