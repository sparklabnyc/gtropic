# print.gtropic_data() output is stable

    Code
      print(gtropic_example())
    Message
      
      -- G-TROPIC data ---------------------------------------------------------------
      * Historical dataset: doi:10.0000/EXAMPLE (version 1.0)
      * Genesis dates: "2020-08-01" to "2020-08-31"
      * Storms: 3
      * ADM2 units: 6
      
      -- Tables 
      * wind: 15 rows x 12 columns
      * precip: 60 rows x 3 columns
      * flood: 5 rows x 7 columns
      * zero_pairs: 3 rows x 5 columns
      * tracks: 8 rows x 10 columns
      * storm_metadata: 3 rows x 4 columns
      * adm2: 6 rows x 5 columns
      * geometry: 6 rows x 2 columns
      * codebook: 1 entries (list)
      * links: 18 rows x 11 columns
      
      i Use `summary()` for detail.

# summary.gtropic_data() output is stable

    Code
      summary(gtropic_example())
    Message
      
      -- G-TROPIC data summary -------------------------------------------------------
      
      -- Sources --
      
      * Server: "example.invalid"
      * Historical dataset: doi:10.0000/EXAMPLE v1.0
      * Files retrieved: 9 (43.9 KB)
      * Retrieved at: 2026-01-05 12:00:00 UTC
      
      -- Tables --
      
      * wind: 15 rows x 12 columns
      * precip: 60 rows x 3 columns
      * flood: 5 rows x 7 columns
      * zero_pairs: 3 rows x 5 columns
      * tracks: 8 rows x 10 columns
      * storm_metadata: 3 rows x 4 columns
      * adm2: 6 rows x 5 columns
      * geometry: 6 rows x 2 columns
      * codebook: 1 entries (list)
      * links: 18 rows x 11 columns
      
      -- Resolved filters --
      
      * ADM2 units: 6
      * Storms: 3
      * Genesis window: 2020-08-01 to 2020-08-31
      * Wind radius: 500 km
      * Precipitation window: -2/ +1 days within 500 km
      * Flood window: -2/+1 days within 500 km (match: "start")

# print.gtropic_manifest() output is stable

    Code
      print(gtropic_manifest(gtropic_example()))
    Message
      
      -- gtropic version manifest ----------------------------------------------------
      * Server: "example.invalid"
      * Historical dataset: doi:10.0000/EXAMPLE (version 1.0)
      * Files recorded: 9
      * Total size: 43.9 KB
      * Retrieved: 2026-01-05 12:00:00 UTC
      * Package version: "<version>"
      
      i Pass this object as `manifest` to `gtropic_data()` to
      reproduce the same retrieval.

# print.gtropic_filters() output is stable

    Code
      print(gtropic_filters(gtropic_example()), n = 3)
    Message
      
      -- G-TROPIC resolved filters ---------------------------------------------------
      * adm2_ids (6 values): ["PLOT.1", "PLOT.2", "PLOT.3"] (3 values omitted)
      * storm_ids (3 values): ["PLOT_A", "PLOT_B", "PLOT_C"]
      * date_start (1 value): [2020-08-01]
      * date_end (1 value): [2020-08-31]
      * date_order (1 value): ["ymd"]
      * years_scanned (1 value): [2020]
      * wind_radius_km (1 value): [500]
      * precip_days_before (1 value): [2]
      * precip_days_after (1 value): [1]
      * precip_radius_km (1 value): [500]
      * flood_days_before (1 value): [2]
      * flood_days_after (1 value): [1]
      * flood_radius_km (1 value): [500]
      * flood_match (1 value): ["start"]
      * inconsistent_adm2_ids (0 values): <empty>

