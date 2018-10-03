# metabarcoding_Rvisuals
## Summary

This repository contains two scripts to show alternative visualisation of !iime outputs.

## Scripts

[visualiseFamilytable.R](visualiseFamilytable.R) visualises Gammaproteobacteria content on the hands
[visualizePCOA.R](visualizePCOA.R) offer a publication ready alternative frome Qiime PCOA output

## Data files


[bray_curtis_pcoa_ordination.txt](bray_curtis_pcoa_ordination.txt). Generated by:
```
qiime tools extract bray_curtis_pcoa_results.qza --output-dir diversity_exports
```
[barplot_export_family_level_resolution.csv](barplot_export_family_level_resolution.csv) . Generated by:

```
qiime tools view taxa-bar-plots.qzv #then in Firefox, adjust to family level and click on CSV
```
