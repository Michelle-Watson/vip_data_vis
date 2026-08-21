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
  r"(
  /* Sticky navigation tabs + fixed logo */
  .nav-tabs {
    position: -webkit-sticky;
    position: sticky;
    top: 0;
    z-index: 999;
    background-color: #ffffff !important;
    padding-right: 160px;
    border-bottom: 1px solid #dee2e6;
  }

  .top-controls {
    position: fixed;
    top: 0;
    right: 24px;
    height: 41px;
    display: flex;
    align-items: center;
    gap: 12px;
    z-index: 1000;
  }

  .app-logo {
    height: 30px;
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
  
  .bslib-sidebar-layout > .sidebar {
    background-color: #ffffff;
    border: 1px solid #d0d0d0;
    border-radius: 8px;
    padding: 8px;
    box-shadow: 0 1px 4px rgba(0, 0, 0, 0.08);
    box-sizing: border-box;
    margin-top: 3rem;
    max-height: 90vh;
  }
  
  

  .table-responsive .dataTables_wrapper {
    width: 100% !important;
    box-sizing: border-box;
  }
  
  /* DataTables mobile adjustments */
  @media (max-width: 576px) {
    .dataTables_wrapper .top {
      display: flex;
      flex-direction: row;
      justify-content: flex-end;
      align-items: center;
      gap: 10px;
    }

    .dataTables_filter {
      text-align: right;
    }

    .dataTables_filter input {
      width: 100%;
      max-width: 200px;
      display: inline-block;
    }
  }
  
  .table-responsive .dataTables_scrollBody {
    max-width: 100%;
    overflow-x: auto;
  }

  /* Remove bslib resize handle and its helper text */
  .bslib-sidebar-resize-handle {
    display: none !important;
  }
  )"
)
