port_talbot <-
  openair::importMeta("waqn", all = TRUE) |>
  dplyr::filter(grepl("Talbot", site)) |>
  dplyr::mutate(end_date = lubridate::as_date(end_date)) |>
  dplyr::summarise(
    start_date = min(start_date),
    end_date = max(end_date),
    .by = c(code, site, latitude, longitude, site_type)
  ) |>
  dplyr::mutate(site = gsub("Port Talbot ", "", site)) |>
  dplyr::arrange(desc(start_date)) |>
  dplyr::slice_head(n = 1, by = c(latitude, longitude)) |>
  dplyr::filter(
    site != "CO",
    site != "Prince Street",
    site != "Prince Street (FDMS only)"
  ) |>
  dplyr::mutate(open = is.na(end_date), .after = site_type)

usethis::use_data(port_talbot, overwrite = TRUE)
