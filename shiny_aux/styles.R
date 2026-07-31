# ---- CSS classes ----
filter_message_css <- "
  .filter-message {
    margin-top: 12px;
    padding: 8px 12px;
    background-color: #e6f2ff;
    border-left: 5px solid #0066cc;
    font-weight: bold;
    font-size: 14px;
    color: #003366;
    border-radius: 4px;
    margin-bottom: 20px;
  }
"

filter_message_css <- paste0(
  filter_message_css,
  '
  .filter-row {
    display: flex;
    flex-wrap: wrap;
  }
  .filter-row > [class*="col-sm"] {
    display: flex;
    flex-direction: column;
  }
  .filter-row .filter-message {
    margin-top: auto;
  }
  '
)
