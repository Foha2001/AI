# Energy Price Forecasting Using AI Models Amidst Global Transitions

This repository provides code, data references, and documentation for the paper:

**"Forecasting Energy Commodity Prices Amidst Worldwide Energy Transitions Using Artificial Intelligence Models"**  
*Foued Hamouda, Nadia Arfaoui, and Muhammad Abubakr Naeem*  
Published in *The Energy Journal (2025)*  
[DOI: 10.1177/01956574251340012](https://doi.org/10.1177/01956574251340012)

## 📌 Overview

This project evaluates the effectiveness of machine learning models in forecasting the prices of energy commodities (Crude Oil, Natural Gas, and Coal) during global disruptions and energy transitions. We develop and compare several models, including:

- **Nonlinear Auto-Regressive model with Exogenous inputs (NARX)**  
- **Artificial Neural Networks (ANN)**  
- **Long Short-Term Memory (LSTM)**  
- **XGBoost**

These models are tested on a comprehensive dataset spanning from **2006 to 2023**, integrating economic, political, environmental, and renewable energy indicators.

## 📊 Key Contributions

- Integration of **renewable energy transition variables** (e.g., Levelized Cost of Energy, patent data) into forecasting models.
- Demonstration that **NARX outperforms** traditional ANN and XGBoost in terms of RMSE, MAE, and R².
- Evidence that **technological and environmental factors** (e.g., solar innovation, CO₂ emissions) now drive price dynamics as much as, or more than, traditional macroeconomic indicators.
- Robust feature importance analysis using:
  - Neural connection weights
  - Permutation-based impact measures

## 🧠 Methodology

The methodology involves:

- Data normalization using **Min-Max scaling**
- Splitting data into **pre- and post-COVID-19** subsets
- Modeling temporal dependencies using **feedback architectures** (NARX)
- Performance evaluation via:
  - RMSE (Root Mean Squared Error)
  - MAE (Mean Absolute Error)
  - R² (Coefficient of Determination)
- **Diebold-Mariano tests** for comparing forecasting accuracy across models

## 📈 Data Sources

- [FRED – Federal Reserve Economic Data](https://fred.stlouisfed.org/)
- [EIA – U.S. Energy Information Administration](https://www.eia.gov/)
- [IRENA – International Renewable Energy Agency](https://www.irena.org/)
- [Our World in Data](https://ourworldindata.org/)
- [Economic Policy Uncertainty Index](https://www.policyuncertainty.com/)

> Note: Some datasets are proprietary or restricted. See `data/README.md` for guidance on how to download or access.

## 📊 Results Summary

| Model      | MAE (Oil) | RMSE (Gas) | R² (Coal) |
|------------|-----------|------------|-----------|
| NARX       | **0.0086** | **0.0112**  | **0.9915** |
| ANN        | 0.0241    | 0.0209     | 0.9148    |
| LSTM       | 0.0058    | 0.0098     | 0.9990    |
| XGBoost    | 0.0374    | 0.0621     | 0.8049    |

> NARX consistently performs best, particularly when modeling exogenous effects such as technological innovation, CO₂ emissions, and renewable capacity shifts.

## 📌 Citation

If you use this code or dataset, please cite the original paper:

@article{hamouda2025forecasting,
title={Forecasting Energy Commodity Prices Amidst Worldwide Energy Transitions Using Artificial Intelligence Models},
author={Hamouda, Foued and Arfaoui, Nadia and Naeem, Muhammad Abubakr},
journal={The Energy Journal},
volume={00},
pages={1–30},
year={2025},
publisher={IAEE},
doi={10.1177/01956574251340012}
}




## ⚙️ Instructions

- Please **install all required packages** before running the program.
- ⚠️ Some packages (especially R-based or specialized ones) may require **manual installation**.
- 🔄 For some blocks of code you may need to **restart your R session** before execution to avoid conflicts or memory issues.

---

----------------------
📬 Contact

For questions or collaborations:
	•	Foued Hamouda – foha2001@gmail.com
	•	Paper [DOI]([https://journals.sagepub.com/doi/10.1177/01956574251340012)
