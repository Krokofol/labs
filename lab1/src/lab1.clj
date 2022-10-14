(ns lab1)
(def startLength 4)
(def alphabet ["a" "b" "c"])

; creates all able words from start word and unused symbols
(defn generateWords [word, unusedSymbols]
  (remove #{(last word)} unusedSymbols)
  (if (> (count unusedSymbols) 1)
    ;then
    ; 1. takes the first symbol from unused symbols
    ; 2. adds all other words into array
    ; 3. adds the word build by using first symbol into array
    (let [firstSymbol (first unusedSymbols)]
      (concat [(str word firstSymbol)] (generateWords word (remove #{firstSymbol} unusedSymbols)))
      )
    ;else
    ; 1. creates the string using the last symbol
    (concat [(str word (first unusedSymbols))] [])
    )
  )

; removes unable symbols from unused symbols
(defn generateAbleWords [word, unusedSymbols]
  (let [ableSymbols (remove #{(str (last word))} unusedSymbols)]
    (generateWords word ableSymbols)
    )
  )

; generates all able words from entered words and symbols
(defn generateAllWords [words, symbols]
  (if (> (count words) 1)
    ;then
    ; 1. takes first word
    ; 2. generate all able words for this
    ; 3. generate all words for other words
    ; 4. connects all words
    (let [firstWord (first words), newWords (generateAbleWords firstWord symbols), otherWords (remove #{firstWord} words)]
      (concat newWords (generateAllWords otherWords symbols))
      )
    ;else
    ; 1. generate all able words from the last word
    (generateAbleWords (first words) symbols)
    )
  )

; generates all able words for entered length and symbols level by level
(defn allWordsBuilder [words, symbols, length]
  (if (> length 1)
    ;then
    ; 1. generates all words for this level
    ; 2. go to next level
    (allWordsBuilder (generateAllWords words symbols) symbols (- length 1))
    ;else
    ; 1. generates all words for this level
    (generateAllWords words symbols)
    )
  )

; generates all able words for entered length and symbols
(defn start [symbols, length]
  (allWordsBuilder [] symbols length)
  )

; main function
(println (start alphabet startLength))