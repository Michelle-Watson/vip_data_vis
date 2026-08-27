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
  /* Only reserve scrollbar space when About tab is active */
  html:has(.tab-pane.active .about-page-content) {
    scrollbar-gutter: stable;
  }

  /* Make the scrollbar track/thumb transparent on About */
  html:has(.tab-pane.active .about-page-content) {
    scrollbar-color: transparent transparent;
  }

  html:has(.tab-pane.active .about-page-content)::-webkit-scrollbar {
    width: 12px;
  }

  html:has(.tab-pane.active .about-page-content)::-webkit-scrollbar-thumb {
    background: transparent;
  }

  html:has(.tab-pane.active .about-page-content)::-webkit-scrollbar-track {
    background: transparent;
  }
  )"
)


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


filter_message_css <- paste0(
  filter_message_css,
  r"(
  /* About page logo card */
  .about-logo-card {
    display: flex;
    justify-content: flex-start;
    margin: 10px 0 20px 0;
  }
  
  .about-logo {
    max-width: 180px;
    width: 100%;
    height: auto;
  }
  )"
)

filter_message_css <- paste0(
  filter_message_css,
  r"(
  /* Collapsible definitions */
  .definitions-box {
    margin-top: 30px;
    margin-bottom: 16px;
  }

  .definitions-box details {
    border: 1px solid #d0d0d0;
    border-radius: 8px;
    background-color: #ffffff;
    padding: 12px 14px;
    box-shadow: 0 1px 4px rgba(0, 0, 0, 0.08);
  }

  .definitions-box summary {
    cursor: pointer;
    font-weight: bold;
    font-size: 1rem;          /* match the About body text */
    line-height: 1.4;
    color: #003366;
    list-style: none;
  }

  .definitions-box summary::-webkit-details-marker {
    display: none;
  }

  .definitions-box summary::before {
    content: "▸ ";
    display: inline-block;
    margin-right: 6px;
    transition: transform 0.2s;
  }

  .definitions-box details[open] summary::before {
    transform: rotate(90deg);
  }

  .definitions-content {
    margin-top: 10px;
    border-top: 1px solid #e5e5e5;
    padding-top: 10px;
  }

  .definition-item {
    margin-bottom: 8px;
    line-height: 1.4;
  }

  .definition-term {
    font-weight: bold;
  }

  .definition-sep {
    color: #666;
  }

  .definition-text {
    color: #333;
  }
  )"
)


filter_message_css <- paste0(
  filter_message_css,
  r"(
  /* Definitions table */
  .definitions-content table {
    width: 100%;
    border-collapse: collapse;
  }

  .definitions-content th {
    text-align: left;
    font-weight: bold;
    color: #003366;
    border-bottom: 2px solid #d0d0d0;
    padding: 8px 6px;
  }

  .definitions-content td {
    border-bottom: 1px solid #e5e5e5;
    padding: 8px 6px;
    vertical-align: top;
  }

  .definitions-content tr:last-child td {
    border-bottom: none;
  }
  )"
)


# max-height: 150px;
filter_message_css <- paste0(
  filter_message_css,
  r"(
  .definitions-content {
    overflow-y: auto;
    margin-top: 10px;
    margin-bottom: 16px;
    border-top: 1px solid #e5e5e5;
    padding-top: 10px;
  }
  )"
)

filter_message_css <- paste0(
  filter_message_css,
  r"(
  .definitions-search {
    display: flex;
    justify-content: flex-end;
    margin-bottom: 8px;
  }

  .definitions-search input[type="text"] {
    width: 100%;
    max-width: 250px;
    padding: 6px 8px;
    font-size: 0.95rem;
    border: 1px solid #d0d0d0;
    border-radius: 4px;
  }
  )"
)
