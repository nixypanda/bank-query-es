# Elastic search query DSL (Bank example)

I came across [Bloodhound](https://github.com/bitemyapp/bloodhound) and [ES Bank Example](https://www.elastic.co/guide/en/elasticsearch/reference/current/_exploring_your_data.html).
Which coupled with my intrest in DSL's, compilers led to this project. A query DSL for the bank example. This can probably even be useful example to some. Given my super beginner
understanding of haskell the code base could be painful to read but hopefully that will improve.


## Usage
- Run `elasticsearch`. Load up the bank data. (on `localhost:9200`)
- Run `stack build`.
- Run `stack exec app-search-exe`. (Now the app is running)
- App will run on port `8081`
- You can now execute queries on it like `curl -XGET -G http://localhost:8081/search -d 'query=gender:F'`


## Query syntax
- You can use `gender`, `age`, `balance`, `and`, `or`.

## Examples
- `query=(balance%20%20>48000)%20and%20age<=24%20and%20gender:F`
- `query=(balance%20%20<48000)%20or%20age<=24%20and%20gender:F`
- and so on

## Note
- Config is being handled in the most naive way (Need to look into how to stack monads)
- See `src/Utils.hs` to see what can be configured

## Config
|Env Var|Desc|Default|
|:-----:|:--:|:-----:|
SCOTTY_PORT|port|8081|
ES_USERNAME|elasticsearch username|-|
ES_PASSWORD|elasticsearch password|-|
ES_URL|elasticsearch url|localhost|
ES_PORT|elasticsearch port|9200|-|
ES_INDEX|elasticsearch index|bank|
KEY_MAP*|key mapping file path|-|
FILTER_MAP*|filter mapping file path|-|

`*` - multiline files seperated with spaces (txt)
e.g. filters.txt
```
c ct
nam user org
addr address
```
