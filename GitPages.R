library(dplyr)
library(ggplot2)
library(glue)
library(scales)

house_preds    <- readRDS("model_artifacts/house_preds.rds")
house_metrics  <- readRDS("model_artifacts/house_metrics.rds")
var_importance <- readRDS("model_artifacts/var_importance.rds")

dir.create("docs/images", recursive = TRUE, showWarnings = FALSE)


p_actual_pred <- ggplot(house_preds, aes(x = sold_price, y = .pred)) +
  geom_point(alpha = 0.4, color = "#2c7fb8") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40") +
  scale_x_continuous(labels = label_dollar()) +
  scale_y_continuous(labels = label_dollar()) +
  labs(
    title = "Actual vs. Predicted Sold Price",
    x = "Actual Sold Price", y = "Predicted Sold Price"
  ) +
  theme_minimal(base_size = 13)

ggsave("docs/images/actual_vs_predicted.png", p_actual_pred, width = 7, height = 5, dpi = 150)


house_preds <- house_preds %>% mutate(resid = sold_price - .pred)

p_resid <- ggplot(house_preds, aes(x = resid)) +
  geom_histogram(bins = 40, fill = "#2c7fb8", color = "white") +
  scale_x_continuous(labels = label_dollar()) +
  labs(
    title = "Prediction Residuals (Actual - Predicted)",
    x = "Residual ($)", y = "Count"
  ) +
  theme_minimal(base_size = 13)

ggsave("docs/images/residuals.png", p_resid, width = 7, height = 5, dpi = 150)


p_vip <- var_importance %>%
  slice_max(Importance, n = 15) %>%
  ggplot(aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_col(fill = "#2c7fb8") +
  coord_flip() +
  labs(
    title = "Top 15 Variable Importances (Impurity)",
    x = NULL, y = "Importance"
  ) +
  theme_minimal(base_size = 13)

ggsave("docs/images/variable_importance.png", p_vip, width = 7, height = 6, dpi = 150)


get_metric <- function(name) {
  house_metrics %>% filter(.metric == name) %>% pull(.estimate) %>% round(2)
}

rmse_val <- get_metric("rmse")
rsq_val  <- get_metric("rsq")
mae_val  <- get_metric("mae")


html <- glue('
<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>FM Housing Sold Price Model - Performance Report</title>
<style>
  body {{
    font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
    max-width: 900px;
    margin: 0 auto;
    padding: 2rem 1.5rem 4rem;
    color: #1a1a1a;
    background: #fafafa;
  }}
  h1 {{ font-size: 1.9rem; margin-bottom: 0.25rem; }}
  .subtitle {{ color: #555; margin-top: 0; margin-bottom: 2rem; }}
  .metrics {{
    display: flex;
    gap: 1rem;
    flex-wrap: wrap;
    margin-bottom: 2.5rem;
  }}
  .metric-card {{
    background: white;
    border: 1px solid #e0e0e0;
    border-radius: 10px;
    padding: 1.2rem 1.5rem;
    flex: 1;
    min-width: 150px;
    text-align: center;
  }}
  .metric-card .value {{ font-size: 1.6rem; font-weight: 700; color: #2c7fb8; }}
  .metric-card .label {{ font-size: 0.85rem; color: #666; margin-top: 0.25rem; }}
  section {{ margin-bottom: 2.5rem; }}
  img {{ width: 100%; border-radius: 10px; border: 1px solid #e0e0e0; }}
  footer {{ color: #888; font-size: 0.85rem; margin-top: 3rem; }}
</style>
</head>
<body>

<h1>Fargo-Moorhead Home Sold Price Model</h1>
<p class="subtitle">Random forest regression - performance on held-out test data</p>

<div class="metrics">
  <div class="metric-card">
    <div class="value">${format(rmse_val, big.mark=",")}</div>
    <div class="label">RMSE</div>
  </div>
  <div class="metric-card">
    <div class="value">{rsq_val}</div>
    <div class="label">R-squared</div>
  </div>
  <div class="metric-card">
    <div class="value">${format(mae_val, big.mark=",")}</div>
    <div class="label">MAE</div>
  </div>
</div>

<section>
  <h2>Actual vs. Predicted Sold Price</h2>
  <img src="images/actual_vs_predicted.png" alt="Actual vs predicted sold price scatter plot">
</section>

<section>
  <h2>Residuals</h2>
  <img src="images/residuals.png" alt="Histogram of prediction residuals">
</section>

<section>
  <h2>Variable Importance</h2>
  <img src="images/variable_importance.png" alt="Bar chart of top variable importances">
</section>

<footer>
  Model: random forest (ranger engine, tidymodels workflow). Report generated automatically.
</footer>

</body>
</html>
')

writeLines(html, "docs/index.html")

cat("\nReport written to docs/index.html and docs/images/\n")