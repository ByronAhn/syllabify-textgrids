# syllabify-textgrids
Using R and Python to syllabify Praat textgrids, using a syllabification algorithm fit for English

## Usage

Load up `syllabify.R` in your favorite R environment to use this tool.

**The R script depends on three things**

1. `syllabify-core.py` in the same directory
    - _This is the syllabification algorithm for English, as found at https://github.com/kylebgorman/syllabify_
2. .Textgrid files in a folder called `input/` in the same directory
3. those TextGrids *MUST* have a "Words" tier and a "Phones" tier, with the "Phones" tier containing ARPABET transcriptions
    - _(ARPABET Phones can be added through a forced aligner like the Montreal Forced Aligner: https://montreal-forced-aligner.readthedocs.io/)_

**The R script then creates two things**

1. *modified* textgrids in a subdirectory called `syllabified/`
2. an R dataframe called `syllableDF` that contains all the syllable information

## License

Attribution-ShareAlike 2.5 license
