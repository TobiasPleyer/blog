---
title:  "Announcing the chefkoch tool"
date: 2018-03-25
tags: haskell, html, web, crawler
category: Programming
authors: Tobias Pleyer
summary: "A simple command line tool to extract recipes from a cooking website."
---

Announcing the chefkoch tool
============================

I am happy to announce that yesterday I released version 1.0 of
**chefkoch**. Chefkoch is a simple command line script written in
[Haskell](https://www.haskell.org/). Its purpose is to download recipe
information from the cooking website
[www.chefkoch.de](https://www.chefkoch.de/), thus the name for the
script. You can find the [source code on
github](https://github.com/TobiasPleyer/chefkoch).

Motivation
----------

I am not a very skilled cook. I am neither very creative with the choice
of my dishes and ingredients, neither am I very good in remembering
recipes.

Thus for people like me cooking websites are a very nice option to bring
some variety to the kitchen without investing to much time. My website
of choice is the above mentioned website (in German, sorry...).

But I, personally, don't like two things:

> 1.  The website is way to overloaded with flashy, colorful media,
>     distracting from the really important information
> 2.  It is hard to find nice recipes because you don't know the
>     keywords to look for

I am not critizing the first point so much, because it is important to
make a website look nice and of course they want to make a bit money off
of it, so there is advertisement. But as a programmer I know I have a
way to get around all this clutter...

The second point is actually more a problem of me than of the website.
If you don't know what to look for, it is hard to find it :P

The website offers tags and a search option of course, but just looking
for *'potatoes'* is a bit of a too broad search. But the website is
offering another nice feature that I like: **recipe of the day**. For
every calendar day they choose one dish and present it. I am actually
using this possibility to find a wide variety of meals in a realitvely
easy way.

So these were the two intentions I had when I started writing this
script:

> 1.  Only extract the importatnt information of the recipe (ingredients
>     and instructions) along with a bit meta data
> 2.  Provide a programmatic way to access the *recipe of the day*
>     recipes

Usage
-----

The fact that its possible to output the recipes in JSON or YAML syntax
opens a wide range of post processing and pipeline options. From the
start of the project it was a goal of mine to support these output
formats. In addition the YAML output pretty prints nicely to for
reading.

There exist 3 options to access a recipe:

> 1.  By providing a link
> 2.  By providing the year, month and day
> 3.  Choosing at random

### By Link

``` {.sourceCode .bash}
$ chefkoch --link https://www.chefkoch.de/rezepte/1316541236346986/Kaesekuchen-von-Tante-Gertrud.html --output -
- Url: https://www.chefkoch.de/rezepte/1316541236346986/Kaesekuchen-von-Tante-Gertrud.html
  Instructions: ! 'Für eine 28er Springform.Aus den Teigzutaten einen Knetteig herstellen
    und diesen gleichmäßig in einer Springform verteilen und bis an den Rand hochziehen.Die
    Zutaten für die Füllung miteinander mischen und die Quarkmasse (Achtung, sie ist
    dünnflüssig!) in die mit dem Teig ausgekleidete Form füllen. Im vorgeheizten Backofen
    bei 200 Grad (Umluft 175 Grad) gut eine Stunde backen.Achtung: Den Kuchen erst
    nach dem völligen Erkalten aus der Form nehmen, da unmittelbar nach dem Herausnehmen
    aus dem Backofen die Konsistenz der Quarkmasse noch zu weich ist.Belohnt werdet
    Ihr mit einem Käsekuchen, wie Ihr ihn sicherlich noch nie gegessen habt und wie
    Ihr ihn auch mit diesem Geschmack bei keinem Bäcker zu kaufen bekommt.Ich habe
    dieses Rezept schon x-mal gemacht. Der Kuchen ist einfach super! Das Beste an
    ihm ist, dass er wirklich nicht zusammenfällt, obwohl die Quarkmasse am Anfang
    flüssig ist. Er bleibt auch nach dem Backen so hoch, wie er in der Kuchenform
    ist.Das ist für mich das beste Käsekuchenrezept, das es gibt.'
  Ingredients:
  - ! 'Für den Teig:'
  - 330 g Mehl
  - 130 g Zucker
  - 130 g Butter, oder Margarine
  - 2 m.-große Ei(er)
  - 2 Pck. Vanillezucker
  - 1 Pck. Backpulver
  - ! 'Für die Füllung:'
  - 1 kg Magerquark
  - 300 g Zucker
  - 2 Pck. Puddingpulver, Vanille zum Kochen
  - 100 ml Öl (Sonnenblumenöl)
  - 600 ml Milch
  - 4 Ei(er)
  - evtl. Zitronenschale, abgeriebene, oder Rum-Aroma (für Quarkmasse)
  Day: null
  Name: ''
  Year: null
  Month: null
  Weekday: null
```

### By Date

``` {.sourceCode .bash}
$ chefkoch --year 2017 --month 07 --day 22 --output -
- Url: https://www.chefkoch.de/rezepte/1730171282126244/
  Instructions: Die Melone vierteln und entkernen. Mit einem Kugelausstecher das Fruchtfleisch
    ausstechen und mit den zerrupften Minzeblättern und Thymianblättchen im Kühlschrank
    marinieren lassen. Die Obstschale mit einem Messer ein wenig begradigen und aufheben.Das
    lässt sich sehr gut vorbereiten und kurz vor dem Servieren braucht man nur noch
    in den Schalen-Schiffchen der Melone den Rucola, die Melonenkugeln mit Kräutern,
    den Schafskäse und die Oliven ansprechend verteilen.Salz, Pfeffer, Olivenöl und
    Balsamico stelle ich separat auf den Tisch.Sehr gut passt ein Haselnuss-Knoblauch-Krokant,
    ein paar angeröstete Kürbiskerne als Topping und Fladenbrot, auf türkische Art
    oder Baguette dazu.
  Ingredients:
  - 1 Wassermelone(n)
  - Minze, frisch, nach Belieben
  - Thymian, frisch, nach Belieben
  - 200 g Schafskäse, gewürfelt
  - 12 Oliven, schwarz
  - Salz
  - Pfeffer, frisch gemahlen
  - Olivenöl
  - Balsamico, nach Belieben
  - 100 g Rucola
  Day: 22
  Name: Melonen - Schafskäse Salatschiffchen
  Year: 2017
  Month: July
  Weekday: Saturday
```

### Random

``` {.sourceCode .bash}
$ chefkoch --random --output -
- Url: https://www.chefkoch.de/rezepte/118681050423161/
  Instructions: Fischfilets säubern, säuern und würzen. Mehl mit Salz vermischen,
    die trockengetupften Filets erst im Mehl und dann im (mit 2 EL Wasser) verquirlten
    Ei wenden und in ca. 40 g heißer Butter goldgelb braten.In eine feuerfeste, gebutterte
    Form legen. Paprika in der Bratbutter kurz dünsten, würzen und über die Filets
    verteilen. Das ganze erst mit dem vermischten Käse, dann mit Bröseln bestreuen,
    zum Schluss Butterflöckchen draufsetzen. Bei 200° ca. 15 Minuten überbacken
  Ingredients:
  - 600 g Fischfilet(s) (Seehechtfilet)
  - 2 EL Zitronensaft
  - Salz
  - Pfeffer
  - 1 Paprikaschote(n), rot, gewürfelt
  - 1 Paprikaschote(n), gelb, gewürfelt
  - 5 EL Semmelbrösel
  - 100 g Käse, gerieben (Emmentaler)
  - 100 g Parmesan, gerieben
  - Mehl
  - 2 Ei(er)
  - 65 g Butter
  Day: 20
  Name: Gratiniertes Seehechtfilet
  Year: 2012
  Month: January
  Weekday: Friday
```