# ausvotesfeed (delevelopment version)

## 21 June 2022

* Re-worked `download_mediafeed_api` to return the file with the original 
  filename, and in the process implemented a bit of error checking to make sure
  the file downloaded and was re-locaed correctly.

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
