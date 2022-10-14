(ns lab1)
(def startLength 3)
(def alphabet ["a" "b" "c"])

; creates all able words from start word and unused symbols
(defn generateWords [word, unusedSymbols, index, accumulator]
  (if (> (count unusedSymbols) index)
    ;then
    ; 1. takes the first symbol from unused symbols
    ; 2. adds all other words into array
    ; 3. adds the word build by using first symbol into array
    (
      (def newAccumulator
        (let [newSymbol (get unusedSymbols index)]
          (if (= (str (last word)) newSymbol )
            ;then
            accumulator
            ;else
            (concat [(str word newSymbol)] accumulator)
            )
          )
        )
      (recur word unusedSymbols (+ index 1) newAccumulator)
      )
    )
  )

; generates all able words from entered words and symbols
(defn generateAllWords [words, symbols, index, accumulator]
  (if (> (count words) 1)
    ;then
    ; 1. takes first word
    ; 2. generate all able words for this
    ; 3. generate all words for other words
    ; 4. connects all words
    (let [firstWord (first words), newWords (generateWords firstWord symbols 0 []), otherWords (remove #{firstWord} words)]
      (concat newWords (recur otherWords symbols (+ index 1) newAccumulator))
      )
    ;else
    ; 1. generate all able words from the last word
    (generateWords (first words) symbols 0 [])
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