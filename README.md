# od-crawler

For a given URL it traverses recursively all the links pointing to folders and outputs all the file links found.

```
./od-crawler-exe -h
Usage: od-crawler-exe TARGET [-p|--profile PROFILE] [-v|--verbose]
                      [-d|--directory DIRECTORY]
  Crawls open directories for tasty links

Available options:
  TARGET                   The target URL or the path to the file containing the
                           target URLs
  -p,--profile PROFILE     Profile for allowed extensions (Videos, Pictures,
                           Music, Docs, SubTitles)
  -v,--verbose             Enable verbose mode
  -d,--directory DIRECTORY The folder where to persist results - only new
                           entries will be shown
  -h,--help                Show this help text
```