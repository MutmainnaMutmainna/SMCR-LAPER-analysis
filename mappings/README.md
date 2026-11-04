# Mappings and CFA model syntax

To be added from Supplementary Tables S1 to S3.

- `factor_sources.csv` (Table S1) with instrument, number of factors, number of variables, basis, and source.
- `mbt_factors.csv` (Table S2) with factor, MBT items, and topic. Excluded items MBT9, 11, 12, 13, 14, 17, 18, 23, 24, and 25.
- `flipcoin_mapping.csv` (Table S3) with original item, model variables V1 to V40, dataset columns, and remarks.
- `cfa_models.R` with the lavaan model syntax of the eight CFA models using the final column names (`CLASS1` to `CLASS42`, `FCI2-1` to `FCI2-30`) and factor names such as `CLS_F1` to `CLS_F8`.
