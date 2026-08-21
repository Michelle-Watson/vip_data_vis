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

.table-responsive .dataTables_wrapper {
  width: 100% !important;
  box-sizing: border-box;
}
  '
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
    padding-right: 160px;   /* keep tabs clear of the logo */
    border-bottom: 1px solid #dee2e6;
  }

  .app-logo {
    top: 4px;
    height: 35px;
  }
  )"
)

# .app-logo {
#   position: fixed;
#   top: 4px;
#   right: 24px;
#   height: 35px;
#   z-index: 1000;
# }

filter_message_css <- paste0(
  filter_message_css,
  r"(
.top-controls {
  position: fixed;
  top: 0;               /* align with nav bar top */
  right: 24px;
  height: 41px;         /* same height as the nav tabs bar */
  display: flex;
  align-items: center;  /* vertically centre inside the nav bar */
  gap: 12px;
  z-index: 1000;
}

.app-logo {
  height: 30px;         /* smaller than the nav bar height */
}


)"
)

filter_message_css <- paste0(
  filter_message_css,
  r"(
  /* Sidebar internal scroll: only when content is taller than viewport */
  .sidebar-scroll {
    max-height: calc(100vh - 117px);
    overflow-y: auto;
    padding-right: 5px;
    -webkit-overflow-scrolling: touch;
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
    padding-right: 160px;   /* keep tabs clear of the logo */
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

  /* Sidebar internal scroll */
  .sidebar-scroll {
    max-height: calc(100vh - 117px);
    overflow-y: auto;
    padding-right: 5px;
    -webkit-overflow-scrolling: touch;
  }
  .sidebar-scroll {
  max-height: calc(100vh - 120px);
  overflow-y: auto;
  }
  
  
  .bslib-sidebar-toggle {
  color: transparent;
}

.bslib-sidebar-toggle i,
.bslib-sidebar-toggle svg {
  color: #333;
}

  )"
)

filter_message_css <- paste0(
  filter_message_css,
  r"(

  /* Main content flex column */
  .main-content {
    display: flex;
    flex-direction: column;
    height: 100%;
    min-height: 0;
  }

  /* Sidebar internal scroll */
  .sidebar-scroll {
    max-height: calc(100vh - 160px);
    overflow-y: auto;
    padding-right: 5px;
    -webkit-overflow-scrolling: touch;
  }

  /* Table wrapper fills remaining space */
  .table-responsive {
    flex: 1 1 auto;
    min-height: 0;
    overflow: auto;
    background-color: #ffffff;
    border: 1px solid #d0d0d0;
    border-radius: 8px;
    padding: 12px;
    box-shadow: 0 1px 4px rgba(0, 0, 0, 0.08);
    margin-top: 10px;
  }

  /* Stack filters above table on smaller screens */
  @media (max-width: 991px) {
    .bslib-sidebar-layout {
      flex-direction: column !important;
    }

    .bslib-sidebar-layout > .sidebar {
      width: 100% !important;
      max-height: 45vh;
      overflow-y: auto;
    }

    .bslib-sidebar-layout > .main {
      width: 100% !important;
    }
  }

  /* DataTables controls stack nicely on mobile */
  @media (max-width: 576px) {
    .dataTables_wrapper .top {
      flex-direction: column;
      align-items: flex-start;
    }
  }
  )"
)


filter_message_css <- paste0(
  filter_message_css,
  r"(
  /* Remove bslib resize handle and the "Use arrow keys" hidden text */
  .bslib-sidebar-resize-handle {
    display: none !important;
  }
  )"
)
