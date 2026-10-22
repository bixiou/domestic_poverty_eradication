# Website: Shortfall of Domestic Resources to Eradicate Extreme Poverty

Static website presenting the results of Fabre, A. (2025), "Shortfall of Domestic Resources to Eradicate Extreme Poverty by 2030", *Journal of Development Studies*, https://doi.org/10.1080/00220388.2025.2493807, updated with the September 2026 World Bank PIP data (2021 PPP).

Everything is static (HTML/CSS/JS + JSON/CSV). All indicators are computed in the browser (`js/model.js`) from the percentile distributions of 171 countries, so any poverty line, taxation threshold, tax rate, growth scenario, horizon year and data source can be chosen. Plotly.js (geo bundle) is loaded from the plot.ly CDN; country shapes come from Plotly's CDN too (ISO-3 codes).

## Deploy

Upload the content of this folder to any static host (GitHub Pages, Netlify, an Apache/Nginx directory...). Nothing to build. For GitHub Pages, e.g. publish the `website/` folder (Settings > Pages, or copy it to a `gh-pages` branch / `docs/` folder).

To preview locally (opening `index.html` directly from disk does not work, as browsers block `fetch` of local files):

    cd website && python3 -m http.server 8000   # then open http://localhost:8000

The current parameters and view are stored in the URL hash, so links can be shared.

## Files

- `index.html`, `css/style.css`, `js/app.js`: user interface.
- `js/model.js`: indicators (port of the R functions of `code_poverty/preparation.R`: poverty rate, poverty gap, antipoverty cap, antipoverty tax, income floor / `compute_min_funded`), usable in the browser and in Node.
- `data/countries.json`: compact data used by the site (per country: survey year, welfare type, 100 percentile means and 99 upper bounds at survey year, growth parameters, HFCE scaling factor, population 2022-2035).
- `data/percentiles.csv`, `data/countries.csv`: full underlying dataset, offered for download.
- `data/validation.json`: values computed in R, used by the test.
- `test/validate.js`: checks that the JavaScript reproduces the R values (`node test/validate.js`, from `website/`).
- `test/browser_check.mjs`: optional headless-Chrome check (JS errors, screenshots).

## Regenerate the data

After running `code_poverty/update.R` (which saves `code_poverty/.RData_update`):

    cd code_poverty
    Rscript website_data.R          # ~3 min (most of it computing the validation values)
    cd ../website && node test/validate.js

## Conventions

- Distribution in year T = survey distribution x observed GDP per capita growth from the survey year to `growth_start` (2025 for most countries, or the survey year if later / no GDP data) x g^(T - growth_start), with g = 1.03, 1.07, custom, the country's 2015-2025 trend, or the autoregressive projection. Horizons: 2026, 2030, 2035, 2040, 2050, 2100 (population from UN WPP 2022 projections, exported for 2022-2035, 2040, 2050, 2100). The model also supports 2022 (observed growth only, the `now` scenario) and any year with exported population; these are used by the validation.
- Map colours: discrete bins and RdBu colours of the paper's `plot_world_map` (a continuous scale is available as an option). Numbers always use "." as decimal separator.
- As in the R code: with the trend scenario, countries without GDP data (SSD, VEN, YEM) keep their survey distribution; with the autoregressive scenario, countries without projection (DJI, SSD, VEN, XKX, YEM) are missing.
- HFCE variant: the gap between household final consumption per capita and the survey mean (when positive) is added to the top percentile.
- Aggregates (world, Sub-Saharan Africa, low-income countries) are population-weighted with the population of the horizon year; aggregate poverty gaps are in % of the group's total consumption.
- Country view display units: per day / month (365/12 days) / year (365 days); PPP $ (2021), local currency at 2026 prices (x WDI PA.NUS.PRVT.PP x local CPI 2026/2021 from `../data/cpi_2021_2026.csv`: WDI FP.CPI.TOTL, extended with IMF WEO Oct. 2026 PCPIPCH; fetched by `website_data.R` if absent) or US$ at 2021 market exchange rates (x PA.NUS.PRVT.PP / PA.NUS.FCRF), from `../data/price_level_2021.csv`. Conversion happens at display time only; computations stay in PPP $/day. Currency names come from the WDI country metadata (`data/currency_units.csv`, fetched from the World Bank API by `website_data.R` if the file is absent; delete it to refresh).
