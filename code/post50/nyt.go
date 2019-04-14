package main

import (
        "fmt"
        "log"
        "github.com/PuerkitoBio/goquery"
)


const NytUrl = "https://www.nytimes.com"

func getArticles(doc *goquery.Document) *goquery.Selection {
    return doc.Find("div.collection").
               Not(".headlines").
               Find("article.story.theme-summary").
               Not(".banner")
}

func extract(sel *goquery.Selection) map[string]string {
    headline := sel.Find("h2.story-heading").Text()
    author := sel.Find("p.byline").Text()
    url, found := sel.Find("h2 > a").Attr("href")
    if headline == "" || author == "" || !found {
        return nil
    } else {
        var m = map[string]string{
            "headline": headline,
            "author": author,
            "url": url,
        }
        return m
    }
}

func main() {
        doc, err := goquery.NewDocument(NytUrl)
        if err != nil {
            log.Fatal(err)
        }

        var articles *goquery.Selection = getArticles(doc)

        articles.Each(func(i int, s *goquery.Selection) {
            m := extract(s)
            if m != nil {
                fmt.Printf("\n")
                fmt.Printf("%s\n", m["headline"])
                fmt.Printf("%s\n", m["author"])
                fmt.Printf("Url: %s\n", m["url"])
            }
        })
}
