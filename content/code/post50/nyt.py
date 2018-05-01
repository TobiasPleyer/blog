from bs4 import BeautifulSoup
from urllib.request import urlopen


# Global Variables

nyt_url = "https://www.nytimes.com"

headline_selector = "h2.story-heading"
author_selector = "p.byline"
url_selector = "h2 > a"
summary_selector = "p.summary"

# Utility Functions

def has_class(whitelist=[], blacklist=[]):
    def _helper(cls):
        if cls:
            whitelist_ok = all(map(lambda k: k in cls, whitelist))
            blacklist_ok = all(map(lambda k: k not in cls, blacklist))
            return whitelist_ok and blacklist_ok
        return False
    return _helper

def fetch_url(url):
    return urlopen(url).read()

def get_articles():
    soup = BeautifulSoup(fetch_url(nyt_url), 'html.parser')
    divs = soup.find_all("div", class_=has_class(whitelist=['collection'],
                                                 blacklist=['headlines']))
    articles = [div.find_all("article", class_=has_class(whitelist=['story', 'theme-summary'],
                                                         blacklist=['banner']))
                    for div in divs]
    return [article for submatch in articles for article in submatch if article]

def extract(article):
    headline = article.select_one(headline_selector)
    if headline:
        headline = headline.string
    author = article.select_one(author_selector)
    if author:
        author = author.string
    url = article.select_one(url_selector)
    if url:
        url = url['href']
    return {'headline': headline, 'author': author, 'url': url}


if __name__ == '__main__':
    parsed_articles = map(extract, get_articles())
    articles = filter(lambda a: all(a.values()), parsed_articles)
    for article in articles:
        print()
        print(article['headline'])
        print(article['author'])
        print("Url: {}".format(article['url']))
