# Recognizing guitar chords from audio

## The company

You work for a company that makes a guitar-practice app. A student plays along with a chord chart while the phone's microphone records. The app should show, as the student plays, which chord it hears, so the student can see when they have played the wrong one. The company has a set of studio recordings in which every chord has been labeled by hand.

## The question

From the audio alone, how accurately can you name the chord being played at each moment, **for a guitarist the model has never heard**? Which mistakes does the model make, and are they ones a guitar teacher would consider close (for example, A major for A7) or far off?

## The data

[GuitarSet](https://zenodo.org/records/3371780) (Xi et al., 2018): recordings of 6 guitarists, each playing the same 30 short excerpts (5 styles: bossa nova, funk, jazz, rock and singer-songwriter; 3 chord progressions per style; each at two tempos), once as accompaniment (`comp`) and once as a solo. Use the 180 accompaniment recordings, about 91 minutes of audio in total. Each recording comes with:

- A mono microphone recording (`.wav`, 44.1 kHz).
- A `.jams` file (JSON) with the chord chart: start time, duration and chord label, such as `A:maj`, `D:min`, `E:7` or `B:hdim7`. The chord qualities are major, minor, dominant seventh and half-diminished seventh.

File names carry the player, style, tempo and key: `03_Jazz2-187-F#_comp` is player 03, jazz piece 2, 187 beats per minute, key of F#.

## What to deliver

- **Target:** the chord label (root and quality) every 0.1 seconds, from the first chord annotation in each `.jams` file (the chord chart). Note that `C#` and `Db` name the same pitch.
- **Inputs:** features computed from the audio up to and including that moment, since the app runs live. Spectral features such as the short-time Fourier transform or chroma (energy folded into the 12 pitch classes) are the place to start. Downsampling to 11,025 Hz with `tuneR::downsample()` keeps the range that matters for chords and makes the transforms about four times faster.
- **Test set:** all recordings by player `05`. Choose model settings on a validation set of other players taken from the training data.
- **Metrics:** the share of frames labeled correctly on the test set (root and quality), the share with the correct root only, and a confusion table of the most common errors.

## Load the data

The audio is about 650 MB, so download it once into a folder next to your `.qmd`. The repository is set to ignore the `guitarset/` folder, so the data will not be uploaded to GitHub.

```r
library(tidyverse)
library(jsonlite)
library(tuneR)

dir <- "guitarset"
if (!dir.exists(dir)) {
  dir.create(dir)
  options(timeout = 3600)
  base <- "https://zenodo.org/records/3371780/files/"
  for (f in c("annotation.zip", "audio_mono-mic.zip")) {
    download.file(paste0(base, f), file.path(dir, f), mode = "wb")
    unzip(file.path(dir, f), exdir = file.path(dir, sub("\\.zip$", "", f)))
  }
}

# the 180 accompaniment recordings
recordings <- tibble(name = sub("\\.jams$", "",
                                list.files(file.path(dir, "annotation"), "_comp\\.jams$"))) |>
  separate_wider_regex(name, c(player = "\\d+", "_", style = "[A-Za-z]+", piece = "\\d",
                               "-", tempo = "\\d+", "-", key = "[A-G][b#]?", "_comp"),
                       cols_remove = FALSE) |>
  mutate(tempo = as.numeric(tempo))

# chord chart for one recording
read_chords <- function(name) {
  j <- fromJSON(file.path(dir, "annotation", paste0(name, ".jams")), simplifyVector = FALSE)
  ns <- map_chr(j$annotations, "namespace")
  chords <- j$annotations[[which(ns == "chord")[1]]]$data
  tibble(start = map_dbl(chords, "time"),
         end   = start + map_dbl(chords, "duration"),
         chord = map_chr(chords, "value"))
}

# audio for one recording
read_audio <- function(name) {
  readWave(file.path(dir, "audio_mono-mic", paste0(name, "_mic.wav")))
}

name <- recordings$name[1]
read_chords(name)
wav <- read_audio(name)
wav
```
