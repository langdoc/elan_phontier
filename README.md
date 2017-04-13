# elan_phontier

Idea of this investigation is find an ideal way to apply a regular expression pattern into an ELAN file and generate a new tier with content already preprocessed to the level wanted for further analysis, which would often be phonetic in this case. With morphology and syntax it should usually be satisfactory and easier to annotate the search results in some other environment than ELAN itself. I think one relatively frequent situation is that there are some tokens on transcription tiers which would desiderably be time aligned, but the time alignation is not needed for all of the tokens.

I set this up in a preliminary manner for my [SLE2016 presentation](https://github.com/nikopartanen/izva_sibilants/blob/master/sle2016/sle2016partanen.pdf), but I have now tried to optimize this to work in wider number of situations. I have also used R as the programming language, but naturally this can be written in any language, since the logic is fairly straighforward. I have lots of R functions ready to interact with ELAN files in different ways, so this was also good reason to work on that. I have marked into list below on bold all the work phases where someone has to do something, rest computer can take care of.

- **Come up with a regular expression that locates the wanted elements**
- For each matching word token, create an annotation and new tier of the type `included_in`
- Populate that tier with approximate locations of the word tokens
    - In principle one could use here much more complex methods too, everything from string length counting to voice recognition or machine learning
    - Anyway, before populating that tier one should probably do something to transliterate the string into phonemic representation from the orthography
    - **Every recognized instance should be manually verified**

I do think it is necessary to go file through manually and annotate the wanted phonetic properties. Of course also here there is some space for automatization, but on the other hand this is the point where the linguistic expertise has time to kick in once again. Often with phonetic variation we talking about very nuanced phenomena which demands proper training and knowledge on topic. I think it is very essential that we as researchers spend the time to familiarize ourselves with the data we have at hand, and this is particularly doable if we can automatize tedious and annoying parts of the workflow, and concentrate into those questions which demand the expert knowledge we have.