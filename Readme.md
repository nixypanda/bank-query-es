# Elastic search query DSL (Bank example)

I came across [Bloodhound](https://github.com/bitemyapp/bloodhound) and [ES Bank Example](https://www.elastic.co/guide/en/elasticsearch/reference/current/_exploring_your_data.html).
Which coupled with my intrest in DSL's, compilers led to this project. A query DSL for the bank example. This can probably even be useful example to some. Given my super beginner
understanding of haskell the code base could be painful to read but hopefully that will improve.


## Usage
- Run `elasticsearch`. Load up the bank data.
- Run `stack build`.
- Run `stack exec app-search-exe`.
- App will run on port `8081`
- You can now execute queries on it like `curl -XGET -G http://localhost:8081/search -d 'query=gender:%20M'`
