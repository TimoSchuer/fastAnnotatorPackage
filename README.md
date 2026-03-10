# fastAnnotator

An R package wrapping the [fastAnnotator](https://github.com/TimoSchuer/fastAnnotator) Shiny app for annotating linguistic data stored in a MariaDB database.

## Features

- Interactive annotation of tokens with custom category sets
- Audio playback for tokens and intonation phrases (via howler.js)
- Praat integration: open audio snippets with TextGrids directly in Praat
- Transcript display with time-range based audio playback
- Background audio preloading for smooth navigation
- Keyboard shortcuts for fast annotation workflows

## Installation

```r
# install.packages("remotes")
remotes::install_github("TimoSchuer/fastAnnotatorPackage")
```

## Usage

```r
library(fastAnnotator)

# Connect to your MariaDB database
con <- DBI::dbConnect(
  RMariaDB::MariaDB(),
  dbname = "my_annotation_db",
  host = "localhost",
  port = 3306,
  user = "user",
  password = "password"
)

# Launch the annotator
fastAnnotator(
  con = con,
  pathPraat = "C:/Praat",
  pathAudio = "D:/audio/",
  sample = "mySample",
  sampleOrder = "myCategory"
)
```

## Arguments

| Argument | Description |
|---|---|
| `con` | A DBI database connection (created with `DBI::dbConnect()`) |
| `pathPraat` | Path to the folder containing `Praat.exe` |
| `pathAudio` | Path to the folder where audio files are stored |
| `sample` | (optional) Name of the subsample to load on start |
| `sampleOrder` | (optional) Name of the annotation category to order the sample by |

## License

MIT