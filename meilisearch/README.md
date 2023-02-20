Trying things out…

https://docs.meilisearch.com/learn/getting_started/quick_start.html#search

Meilisearch is a very fast search API.

```lisp
;; Storing 2 movies, in our default namespace:
(post-document (dict "name" "Matrix" "id" 1))
(post-document (dict "name" "Matrix reloaded" "id" 2))

;; Searching them:
(search-document "ma")

;; =>

"Matrix reloaded"
"Matrix"

 (dict
  "hits" #(
  (dict
   "id" 2
   "name" "Matrix reloaded"
  )

  (dict
   "id" 1999
   "name" "Matrix"
  ) )
  "estimatedTotalHits" 2
  "query" "ma"
  "limit" 20
  "offset" 0
  "processingTimeMs" 0
 )
```

There's a built-in movies test data set. We can search inside the "movies" document collection:

```lisp
(with-document ("movies")
  (search-document "ma"))
```
=>
```txt

"Ma"
"Ma and Pa Kettle"
"Ma Ma"
"Ma famille t'adore déjà !"
"Make Way for Tomorrow"
"Magnetic Rose"
"Mars Attacks!"
"Match Point"
"Mambo Italiano"
"Magnolia"
"Mala Noche"
"Mary Poppins"
"Maria Full of Grace"
"Marnie"
"Manhattan"
"Madagascar"
"March of the Penguins"
"Max Dugan Returns"
"Marlowe"
"Man on the Moon"

 (dict
  "hits" #(
  (dict
   "id" 334685
   "title" "Ma"
   "overview" "In this modern-day vision of Mother Mary's pilgrimage, a woman crosses the American Southwest playfully deconstructing the woman’s role in a world of roles."
   "genres" #("Drama")
   "poster" "https://image.tmdb.org/t/p/w500/gZ32qFZ5zoIEyAe8io7D0yZJxaA.jpg"
   "release_date" 1441411200
  )
[…]
```

Add and search data with the built-in web view: http://localhost:7700/

Using CIEL's built-in libraries: Dexador, Shasht (json), Serapeum's `dict`, access, Quri, Alexandria…
