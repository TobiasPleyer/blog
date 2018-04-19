(ns nyt-clojure.core
  (:gen-class)
  (:require [net.cgrand.enlive-html :as html]
            [clojure.string :as str]))


;; Global Variables

(def nyt-url "https://www.nytimes.com")

(def article-selector [[:div.collection
                        (html/but :.headlines)]
                       [:article.story
                        :article.theme-summary
                        (html/but :.banner)]])
(def headline-selector [:h2.story-heading])
(def author-selector   [:p.byline])
(def url-selector      [:h2 :> :a])

;; Utility functions

(defn fetch-url [url]
  (html/html-resource (java.net.URL. url)))

(defn get-articles []
  "Fetches the website of the New York Times and then extracts the articles."
  (html/select (fetch-url nyt-url) article-selector))

(defn extract [node]
  "Extracts the headline, author and URL of an article"
  (let [headline-node (first (html/select [node] headline-selector))
        author-node   (first (html/select [node] author-selector))
        url-node      (first (html/select [node] url-selector))
        headline      (html/text headline-node)
        author        (html/text author-node)
        url           (:href (:attrs url-node))]
    {:headline headline :author author :url url}))

(defn -main
  "Parses the website of the New York Times and prints a list of the front page
  articles."
  [& args]
  (def parsed-articles (map extract (get-articles)))
  (def articles (filter
                  #(every? false? (map str/blank? (vals %)))
                  parsed-articles))
  (doseq [article articles]
    (println "")
    (println (:headline article))
    (println (:author article))
    (println (str "Url: " (:url article)))))
