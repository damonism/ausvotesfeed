# ausvotesfeed (delevelopment version)

# ausvotesfeed 0.1.3

## 7 July 2022

* Added totals (formal, informal, total) to `get_mediafeed_votes_type`.

# ausvotesfeed 0.1.2

## 6 July 2022

* Added calculation of TCP swings to `get_mediafeed_votes_type`.

## 21 June 2022

* Re-worked `download_mediafeed_api` to return the file with the original 
  filename, and in the process implemented a bit of error checking to make sure
  the file downloaded and was re-locaed correctly.
* Made a couple of improvements to the `explorer` Shiny app, including using the
  last downloaded data and sorting the TCP page by changed seats.

## 20 June 2022

* Added initial TCP tab to explorer Shiny app.

# ausvotesfeed 0.1.1

## 20 June 2022

* `get_mediafeed_votes_sen` now correctly accounts for Senate groups without a
  group name, and includes the `NoAffiliation` candidate variable.

# ausvotesfeed 0.1.0

## 16 June 2022

* Added a `NEWS.md` file to track changes to the package.
* Renamed variables from "Id" to "ID" for consistency with `ausvotes`.
