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
  /* Sidebar collapse */
  #studies_layout.sidebar-hidden .col-sm-3 {
    display: none !important;
  }
  #studies_layout.sidebar-hidden .col-sm-9 {
    flex: 0 0 100% !important;
    max-width: 100% !important;
  }
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
  .dataTables_wrapper .top {
    display: flex;
    justify-content: space-between;
    align-items: center;
  }
  
  .table-responsive {
  width: 100%;
  overflow-x: auto;
  background-color: #ffffff;
  border: 1px solid #d0d0d0;
  border-radius: 8px;
  padding: 12px;
  box-shadow: 0 1px 4px rgba(0, 0, 0, 0.08);
  margin-top: 10px;
  -webkit-overflow-scrolling: touch;
}

.table-responsive .dataTables_wrapper {
  width: 100% !important;
  box-sizing: border-box;
}

.table-responsive .dataTables_scrollBody {
  max-width: 100%;
  overflow-x: auto;
}
  '
)
