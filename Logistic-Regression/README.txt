Dataset: sprache.asc is based on a perceptional experiment, in which test subjects should decide whether a speaker did smile while speaking or not. The speaker had to say twelve different German words and had to repeat each one eight times. For every one of these record- ings some measurements of certain acoustical quantities were done and each recording was played to 50 test subjects, which had to decide if they could hear a smile or not. Additionally it is known if the speaker was really smiling or not. All variables are listed as follows.
• nummer: Reference number
• wort: Spoken Word
• sprecher: Speaker is smiling: 1: yes, 0: no
• segtyp: Indicates segment of word, for which physical measure- ments were done
• meanf0: Mean of fundamental frequency
• slopef0: Slope of regression through single measurements of fundamental frequency
• meanf1: Frequency of first formant
• meanf2: Frequency of second formant
• meanf3: Frequency of third formant

Fitted a logistic regression to predict whether the speaker smiles (“sprecher=1”) from the other variables.
