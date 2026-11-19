# Final Project Guidelines

### Presentations: Thursday December 3
### Report due: Tuesday December 15

## Overview

For the midterm you found your own data. For the final, the data and the question come to you, the way they would in a job. Each of you has been assigned a problem in `assigned-problem/` (look in the folder with your initials). Treat it as a request from a company: they have handed you their data, and they want an answer they can act on.

The data are messier than the series we used in class. Expect to spend real time on exploratory analysis and on building features before any model is fit. The final project has two parts, graded separately:

- A 20 minute presentation, with slides made in Quarto, given in class on Thursday December 3
- A written report that answers every question listed below, due Tuesday December 15

Both should use core material presented during lecture and extended material from textbooks or other resources.

## Presentation

Each presentation should be a "mini lecture" on the analysis, given to an audience that includes the company's managers. Presentations will be evaluated on clarity and on a thoughtful exposition of the problem, the analysis and the answer. You choose what to show and how to tell the story. The report questions below are examples of what you might include, but you do not need to cover all of them in 20 minutes.

- Produce slides using Quarto, and format code and output so they look nice on the screen
- Add `embed-resources: true` to the YAML header so the slides render to a single `.html` file
- Students will be randomly selected for presentation order

## Written report

The report is the complete record of your analysis. Write it as a single Quarto (`.qmd`) or R Markdown (`.Rmd`) document and answer all of the questions below. The code must run from start to finish, downloading the data with the code in your problem.

1. **The problem**
   - What is the company's question, in your own words?
   - What decision depends on the answer, and how will the quality of an answer be measured?
   - What information is available at the moment a prediction is made, and what is not?

2. **Exploratory data analysis**
   - How are the data structured: units, sampling rate, groups (people, players, teams) and time span?
   - What missing values, impossible values or changes in how the data were recorded did you find, and how did you deal with them?
   - Which plots shaped your modeling choices, and what did each one tell you?

3. **Feature engineering**
   - What inputs do your models use (lags, rolling summaries, spectral features, ratings, and so on)?
   - Why should each group of features carry information about the target?
   - How did you make sure no feature uses information from after the time of the prediction?

4. **Modeling**
   - What simple baseline did you fit, and how did it do?
   - What models did you fit? Give the notation and the mathematical background for at least one model we have learned in class and at least one model that extends what we have learned.
   - Why did you choose these models?
   - What do the diagnostics show?

5. **Validation and results**
   - How did you split the data, and why does your test set look like the future the company cares about?
   - How did you choose model settings using only the training data?
   - How do the models compare on the metrics given in your problem? Where does each one do well, and where does it fail?

6. **Recommendation**
   - What is your answer to the company's question?
   - How much should the company trust that answer, and what could go wrong?
   - What would you do next with more time or more data?

## What to upload

Upload the following to your folder in `final/assigned-problem/` on GitHub:

- Your slides, before class on December 3: the `.qmd` and the rendered `.html`
- Your report, by December 15: the `.qmd` or `.Rmd` and the rendered `.html` (for a `.qmd`, add `embed-resources: true`; an `.Rmd` rendered to `html_document` is already a single file)

Do not upload the data.
