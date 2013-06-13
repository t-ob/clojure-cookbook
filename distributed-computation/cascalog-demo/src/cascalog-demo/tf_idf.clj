(ns cascalog-demo.tf-idf
  (:require [cascalog-demo.tf-idf.samples :as samples]
            [clojure.string :as str]
            [cascalog.api :refer :all]
            [cascalog.ops :as c]))

(def stopwords
  #{"a" "is" "of" "i" "the" "this" "you" "off" "ok" "should" "dont" "your" "youre" "on" "to" "but" "his" "it" "all" "him"})

(defn stopword? [s]
  (contains? stopwords s))

(defn word-splitter [s]
  (remove stopword?
          (str/split s #"\s+")))

(defmapcatop [split-terms [split-fn]]
  [c]
  (map vector
       (split-fn c)))

(defmapop [idf [docs]]
  [freq]
  (/ (Math/log (/ docs freq))
     (Math/log 2)))

;; First, we take each document and create a count of each individual
;; word per document.
(defn document-word-frequencies [source]
  (<- [?label ?word ?freq]
      (source ?label ?document)
      (split-terms #'word-splitter ?document :> ?word)
      (c/count ?freq)))

;; To normalise our calculations, we require the maximum word
;; frequency for each document.
(defn document-max-word-frequency [source]
  (<- [?label ?max-freq]
      (source ?label _ ?freq)
      (c/max ?freq :> ?max-freq)))

;; We can now calculate the term frequencies for each document.
(defn term-frequency [word-freqs max-word-freqs]
  (<- [?label ?word ?tf]
      (word-freqs ?label ?word ?freq)
      (max-word-freqs ?label ?max-freq)
      (div ?freq ?max-freq :> ?tf)))



;; For calculating IDF, we first need the total frequencies of all
;; words across all documents.
(defn total-frequencies [source]
  (<- [?word ?total-freq]
      (source _ ?word _)
      (c/count ?total-freq)))

(defn inverse-document-frequency [n source]
  (<- [?word ?idf]
      (source _ ?word _)
      (c/count ?total-freq)
      (idf n ?total-freq :> ?idf)))

(defn tf-idf [term-freq inv-doc-freq]
  (<- [?label ?word ?tf-idf]
      (term-freq ?label ?word ?tf)
      (inv-doc-freq ?word ?idf)
      (* ?tf ?idf :> ?tf-idf)))

(defbufferop [top-n [n]]
  [tuples]
  (take n
        (sort-by last
                 >
                 tuples)))

;; (let [source samples/documents
;;       n (count source)
;;       word-freqs (document-word-frequencies source)
;;       max-word-freqs (document-max-word-frequency word-freqs)
;;       term-freq (term-frequency word-freqs (document-max-word-frequency word-freqs))
;;       inv-doc-freq (inverse-document-frequency n word-freqs)
;;       source (tf-idf term-freq inv-doc-freq)]
;;   (?- (stdout)
;;       (<- [?document ?b ?c]
;;           (source ?document ?word ?tf-idf)
;;           (top-n 10 ?word ?tf-idf :> ?b ?c))))
