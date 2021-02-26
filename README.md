Linking User Opinion Dynamics and Online Discussions
================

This repository contains code and data accompanying the publication
“Linking User Opinion Dynamics and Online Discussions” [\[Largeron et
al,
    ’21\]](https://arxiv.org/abs/2101.09852).

# Reference:

    [Largeron et al, '21] Largeron, C., Mardale, A., & Rizoiu, M.-A. Linking User Opinion Dynamics and Online Discussions. In Proceedings of the Symposium on Intelligent Data Analysis, 2021.

# Repository content:

This repository contains the following data – the Reddit discussions
around Brexit (submissions and comments):

  - `Data/diffusions_submissions_extra.csv.xz` – contains the Reddit
    submission (posts) that initiate discussion threads around Brexit
    (CSV compressed using LZMA). Lines are individual submissions,
    columns are features of the submissions (e.g., author, text, URL
    etc). The function `readSubmissions()` in the file `utils.R` reads
    submissions into an R object.
  - `Data/diffusions_comments_extra.csv.xz` – contains the comments to
    each of the Reddit submissions contained in the above file (CSV
    compressed using LZMA). Lines are individual comments, columns are
    features similar to the submissions. The function `readComments()`
    in the file `utils.R` reads comments into an R object. Comments and
    submissions can be merged using the function
    `mergeSubmissionsAndComments()` in `utils.R`.
  - `Data/correct_model_no_hashtags.rds` – contains the Brexit stance
    detector: a trained Naive Bayes model (trained on Twitter data) for
    labeling whether a text is Pro- or Against-Brexit.

We also provide the following code scripts:

  - `scripts/crawlReddit.py` – Python script to crawl the `r/brexit`
    subreddit. Creates the submissions and comments files here above. R
    script that starts from `data/all_users_data.csv.xz`, and builds the
    profession profiles (stored in the file
    `data/profession-profiles.csv`);
  - `scripts/construct-feature-set_FX.R` (where X is 0-3) – R scripts to
    build the textual description (FS0) and activity descriptors (FS1 to
    FS3) to predict the future Brexit stance (see paper for details).
    These scripts generate the files
    `Data/feature-sets/FX_improved_data.csv` (where X is 0-3), which are
    the datasets used to train the next stance classifiers.
  - `scripts/library_loader.R` – R script that to load all required
    librarie for execution and check their versions.
  - `scripts/utils.R` – additional functions for reading, writing data
    and plotting.

# License

Both data set and code are distributed under the General Public License
v3 (GPLv3) license, a copy of which is included in this repository, in
the LICENSE file. If you require a different license and for other
questions, please contact us at <Marian-Andrei@rizoiu.eu>
