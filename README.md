# od-crawler

For a given URL it traverses recursively all the links pointing to folders and outputs all the file links found.

```
./od-crawler-exe -h
Usage: od-crawler-exe URL [-p|--profile PROFILE] [-v|--verbose]
  Crawls open directories for tasty links

Available options:
  URL                      The target URL
  -p,--profile PROFILE     Profile for allowed extensions (Videos, Pictures, Music, Docs)
  -v,--verbose             Enable verbose mode
  -h,--help                Show this help text
```